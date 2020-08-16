{-# LANGUAGE LambdaCase #-}
module Codegen.C where
import           AST.C
import           AST.Simple
import           Codegen.C.CompilerMonad
import           Codegen.C.CUtils
import           Codegen.C.Memory                (bvNum, initMem)
import           Codegen.C.Utils
import           Control.Applicative
import           Control.Monad                   (replicateM_)
import           Control.Monad.State.Strict      (forM, forM_, gets, liftIO,
                                                  unless, void, when)
import qualified Data.BitVector                  as Bv
import           Data.List                       (intercalate)
import qualified Data.Map                        as Map
import           Data.Maybe                      (fromJust, fromMaybe, isJust,
                                                  isNothing, listToMaybe)
import qualified IR.SMT.Assert                   as Assert
import qualified IR.SMT.TySmt                    as Ty
import           Language.C.Analysis.AstAnalysis
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
--import Debug.Trace

fieldToInt :: Ident -> Int
fieldToInt = undefined

typedefSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Compiler ()
typedefSMT (Ident name _ _) tys ptrs = do
  ty <- ctype tys ptrs
  typedef name ty

declareVarSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Compiler ()
declareVarSMT (Ident name _ _) tys ptrs = do
  ty <- ctype tys ptrs
  declareVar name ty

genVarSMT :: Ident -> Compiler CTerm
genVarSMT (Ident name _ _) = getTerm name

genNumSMT :: CConstant a -> Compiler CTerm
genNumSMT c = case c of
  CIntConst   (CInteger i _ _) _ -> return $ cppIntLit S32 i
  CCharConst  (CChar  c _    ) _ -> error ""
  CCharConst  (CChars c _    ) _ -> error ""
  CFloatConst (CFloat str    ) _ -> error ""
  CStrConst   (CString str _ ) _ -> error ""

data CLVal = CLVar VarName
           | CLAddr CTerm
           deriving (Show)

evalLVal :: CLVal -> Compiler CTerm
evalLVal location = case location of
  CLVar  v -> getTerm v
  CLAddr a -> liftMem $ cppLoad a

genLValueSMT :: CExpr -> Compiler CLVal
genLValueSMT expr = case expr of
  CVar (Ident name _ _) _ -> return $ CLVar name
  CUnary CIndOp addr _    -> CLAddr <$> genExprSMT addr
  CIndex base   idx  _    -> do
    base' <- genExprSMT base
    idx'  <- genExprSMT idx
    addr  <- liftMem $ cppPtrOffset base' idx'
    return $ CLAddr addr
  _ -> error $ unwords ["Not yet impled:", show expr]

genAssign :: CLVal -> CTerm -> Compiler CTerm
genAssign location value = case location of
  CLVar  varName -> ssaAssign varName value
  CLAddr addr    -> do
    guard <- getGuard
    liftMem $ cppStore addr value guard
    return value

genExprSMT :: CExpr -> Compiler CTerm
--genExprSMT expr = case trace ("genExprSMT " ++ show expr) expr of
genExprSMT expr = case expr of
  CVar id _            -> genVarSMT id
  CConst c             -> genNumSMT c
  CAssign op lhs rhs _ -> do
    lval <- genLValueSMT lhs
    rval <- genExprSMT rhs
    case lval of
      CLVar  varName -> ssaAssign varName rval
      CLAddr addr    -> do
        guard <- getGuard
        liftMem $ cppStore addr rval guard
        return rval
  CBinary op left right _ -> do
    left'  <- genExprSMT left
    right' <- genExprSMT right
    getBinOp op left' right'
  CUnary op   arg   _ -> getUnaryOp op arg
  CIndex base index _ -> do
    base'  <- genExprSMT base
    index' <- genExprSMT index
    let baseTy = cppType base'
    case baseTy of
      --Array{}              -> liftMem $ getIdx base' index'
      _ | isPointer baseTy -> liftMem $ do
        addr <- cppPtrOffset base' index'
        cppLoad addr
  --CMember struct field _ _ -> do
  --  struct' <- genExprSMT struct
  --  liftMem $ getField struct' $ fieldToInt field
  CCast decl expr _ -> case decl of
    CDecl specs _ _ -> do
      ty    <- baseTypeFromSpecs specs
      expr' <- genExprSMT expr
      return $ cppCast ty expr'
    _ -> error "Expected type in cast"
  CCall fn args _ -> case fn of
    CVar fnIdent _ -> do
      let fnName = identToVarName fnIdent
      actualArgs <- traverse genExprSMT args
      f          <- getFunction fnName
      retTy      <- ctype (baseTypeFromFunc f) (ptrsFromFunc f)
      pushFunction fnName retTy
      forM_ (argsFromFunc f) (genDeclSMT Nothing)
      let
        formalArgs =
          map identToVarName
            $ concatMap
                (\decl -> case decl of
                  CDecl _ decls _ -> do
                    map
                      (\(Just dec, mInit, _) ->
                        let mName = identFromDeclr dec
                        in  if isJust mName
                              then fromJust mName
                              else error "Expected identifier in decl"
                      )
                      decls
                )
            $ argsFromFunc f
      unless (length formalArgs == length actualArgs)
        $  error
        $  "Wrong arg count: "
        ++ show expr
      forM_ (zip formalArgs actualArgs) (uncurry argAssign)
      let body = bodyFromFunc f
      case body of
        CCompound{} -> genStmtSMT body
        _           -> error "Expected C statement block in function definition"
      returnValue <- getReturn
      popFunction
      return $ returnValue
    _ -> error $ unwords ["Fn call of", show fn, "is unsupported"]
  CCond cond mTrueBr falseBr _ -> do
    cond' <- genExprSMT cond
    true' <- if isJust mTrueBr
             then genExprSMT $ fromJust mTrueBr
             else return cond'
    false' <- genExprSMT falseBr
    return $ cppCond cond' true' false'
  _ -> error $ unwords ["We do not support", show expr, "right now"]


getUnaryOp :: CUnaryOp -> CExpr -> Compiler CTerm
getUnaryOp op arg = case op of
  CPreIncOp -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    genAssign lval (cppAdd (cppIntLit (cppType rval) 1) rval)
  CPreDecOp -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    genAssign lval (cppSub rval (cppIntLit (cppType rval) 1))
  CPostIncOp -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    genAssign lval (cppAdd (cppIntLit (cppType rval) 1) rval)
    return rval
  CPostDecOp -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    genAssign lval (cppSub rval (cppIntLit (cppType rval) 1))
    return rval
  -- CAdrOp ->
  -- The '*' operation
  CIndOp -> do
    arg <- genExprSMT arg
    liftMem $ cppLoad arg
  CPlusOp -> error $ unwords ["Do not understand:", show op]
  CMinOp  -> cppNeg <$> genExprSMT arg
  -- One's complement: NOT CORRECT
  CCompOp -> cppBitNot <$> genExprSMT arg
  -- Logical negation: NOT CORRECT
  CNegOp  -> cppNot <$> genExprSMT arg
  _       -> error $ unwords [show op, "not supported"]

getBinOp :: CBinaryOp -> CTerm -> CTerm -> Compiler CTerm
getBinOp op left right =
  let f = case op of
        CMulOp -> cppMul
        CDivOp -> cppDiv
        CRmdOp -> cppRem
        CAddOp -> cppAdd
        CSubOp -> cppSub
        CShlOp -> cppShl
        CShrOp -> cppShr
        CLeOp  -> cppLt
        CGrOp  -> cppGt
        CLeqOp -> cppLe
        CGeqOp -> cppGe
        CEqOp  -> cppEq
        CNeqOp -> cppNe
        CAndOp -> cppBitAnd
        CXorOp -> cppBitXor
        COrOp  -> cppBitOr
        CLndOp -> cppAnd
        CLorOp -> cppOr
  in  return $ f left right

-- | Assign operation
-- eg x += 1
-- aka x = x + 1
getAssignOp :: CAssignOp -> CLVal -> CTerm -> Compiler CTerm
getAssignOp op l r = case op of
  CAssignOp -> genAssign l r
  _ ->
    let f = case op of
          CMulAssOp -> cppMul
          CAddAssOp -> cppAdd
          CSubAssOp -> cppSub
          CShlAssOp -> cppShl
          CShrAssOp -> cppShr
          CAndAssOp -> cppBitAnd
          CXorAssOp -> cppBitXor
          COrAssOp  -> cppBitOr
          o         -> error $ unwords ["Cannot handle", show o]
    in  do
          lvalue <- evalLVal l
          genAssign l (f lvalue r)

---
--- Statements
---

genStmtSMT :: CStat -> Compiler ()
genStmtSMT stmt = case stmt of
--genStmtSMT stmt = case trace ("genStmtSMT " ++ show stmt) stmt of
  CCompound ids items _ -> do
    enterLexScope
    forM_ items $ \case
        CBlockStmt stmt -> genStmtSMT stmt
        CBlockDecl decl -> void $ genDeclSMT (Just True) decl
        CNestedFunDef{} -> error "Nested function definitions not supported"
    exitLexScope
  CExpr e _                 -> when (isJust e) $ void $ genExprSMT $ fromJust e
  CIf cond trueBr falseBr _ -> do
    trueCond <- cppBool <$> genExprSMT cond
    -- Guard the true branch with the true condition
    pushGuard trueCond
    genStmtSMT trueBr
    popGuard
    -- Guard the false branch with the false condition
    when (isJust falseBr) $ do
      pushGuard (Ty.Not trueCond)
      genStmtSMT $ fromJust falseBr
      popGuard
  CWhile{}                    -> liftIO $ print "while"
  CFor init check incr body _ -> do
    case init of
      Left  (Just expr) -> void $ genExprSMT expr
      Right decl        -> void $ genDeclSMT (Just True) decl
      _                 -> return ()
    -- Make a guard on the bound to guard execution of the loop
    -- Execute up to the loop bound
    bound <- getLoopBound
    forM_ [0 .. bound] $ \_ -> do
      guard <- case check of
        Just b -> genExprSMT b
        _      -> error "NYI"
      pushGuard (cppBool guard)
      genStmtSMT body
      -- printComp
      -- increment the variable
      case incr of
        Just inc -> void $ genExprSMT inc
        _        -> error "Not yet supported"
    replicateM_ (bound + 1) popGuard
    -- TODO: assert end
  CReturn expr _ -> when (isJust expr) $ do
    toReturn <- genExprSMT $ fromJust expr
    doReturn toReturn
  _ -> liftIO $ print $ unwords ["other", show stmt]

-- Returns the declaration's variable name
genDeclSMT :: Maybe Bool -> CDecl -> Compiler String
genDeclSMT undef (CDecl specs decls _) = do
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        (isStorageSpec firstSpec) && (isTypedef $ storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  names <- forM decls $ \(Just dec, mInit, _) -> do
    let mName   = identFromDeclr dec
        name    = fromMaybe (error "Expected identifier in decl") mName
        ptrType = derivedFromDeclr dec

    if isTypedefDecl
      then typedefSMT name baseType ptrType >> return "TYPEDEF"
      else do
        declareVarSMT name baseType ptrType
        lhs <- genVarSMT name
        whenM (gets findUB)
          $ forM_ undef
          $ (liftAssert . Assert.assert . Ty.Eq (udef lhs) . Ty.BoolLit)
        case mInit of
          Just (CInitExpr e _) -> do
            rhs <- genExprSMT e
            a   <- getAssignment
            let (assertion, value) = a lhs rhs
            setValue (identToVarName name) value
            void $ liftAssert $ Assert.assert assertion
          _ -> return ()
        return $ identToVarName name
  return $ head names


---
--- High level codegen (translation unit, etc)
---

-- Returns the variable names corresponding to inputs and the return
genFunDef
  :: CFunDef -> Maybe (Map.Map String Integer) -> Compiler ([String], String)
genFunDef f inVals = do
  -- Declare the function and setup the return value
  let name = nameFromFunc f
      ptrs = ptrsFromFunc f
      tys  = baseTypeFromFunc f
  retTy <- ctype tys ptrs
  pushFunction name retTy
  -- Declare the arguments and execute the body
  inputsNames    <- forM (argsFromFunc f) (genDeclSMT (Just False))
  fullInputNames <- map ssaVarAsString <$> forM inputsNames getSsaVar
  def            <- gets defaultValue
  case inVals of
    Just i -> forM_ inputsNames $ \n -> initAssign n $ fromMaybe
      (error $ "Missing value for input " ++ n)
      ((i Map.!? n) <|> def)
    Nothing -> return ()

  let body = bodyFromFunc f
  case body of
    CCompound{} -> genStmtSMT body
    _           -> error "Expected C statement block in function definition"
  returnValue <- getReturn
  whenM (gets findUB) $ liftAssert $ Assert.assert $ udef returnValue
  popFunction
  return (fullInputNames, fromJust $ asVar returnValue)

genAsm :: CStringLiteral a -> Compiler ()
genAsm = undefined

registerFns :: [CExtDecl] -> Compiler ()
registerFns decls = forM_ decls $ \case
  CFDefExt f -> registerFunction (nameFromFunc f) f
  _          -> pure ()

codegenAll :: CTranslUnit -> Compiler ()
codegenAll (CTranslUnit decls _) = do
  liftMem initMem
  registerFns decls
  forM_ decls $ \case
    CDeclExt decl -> void $ genDeclSMT Nothing decl
    CFDefExt fun  -> void $ genFunDef fun Nothing
    CAsmExt asm _ -> genAsm asm

findFn :: String -> [CExtDecl] -> CFunDef
findFn name decls =
  let nameFnPair (CFDefExt f) = [(nameFromFunc f, f)]
      nameFnPair _            = []
      namesToFns = Map.fromList $ concatMap nameFnPair decls
  in  fromMaybe
        (  error
        $  "No function `"
        ++ name
        ++ "`. Available functions: {"
        ++ intercalate "," (Map.keys namesToFns)
        ++ "}."
        )
        (namesToFns Map.!? name)


codegenFn
  :: CTranslUnit
  -> String
  -> Maybe (Map.Map String Integer)
  -> Compiler ([String], String)
codegenFn (CTranslUnit decls _) name inVals = do
  registerFns decls
  genFunDef (findFn name decls) inVals


-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
checkFn :: CTranslUnit -> String -> IO (Maybe String)
checkFn tu name = do
  assertions <- Assert.execAssert $ evalCodegen True $ codegenFn tu name Nothing
  Ty.evalZ3 $ Ty.BoolNaryExpr Ty.And (Assert.asserted assertions)

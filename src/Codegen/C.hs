{-# LANGUAGE LambdaCase #-}
module Codegen.C where
import           AST.C
import           AST.Simple
import           Codegen.CompilerMonad
import           Codegen.Utils
import           Control.Monad.State.Strict     ( forM
                                                , forM_
                                                , liftIO
                                                , unless
                                                , void
                                                , when
                                                , gets
                                                )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                , fromMaybe
                                                , listToMaybe
                                                )
import           IR.CUtils
import           IR.Memory                      ( bvNum )
import qualified Targets.SMT.Assert            as Assert
import qualified IR.TySmt                      as Ty
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
      forM_ (argsFromFunc f) (genDeclSMT True)
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
      forM_ (zip formalArgs actualArgs) (uncurry ssaAssign)
      let body = bodyFromFunc f
      case body of
        CCompound{} -> genStmtSMT body
        _           -> error "Expected C statement block in function definition"
      returnValue <- getReturn
      popFunction
      return $ returnValue
    _ -> error $ unwords ["Fn call of", show fn, "is unsupported"]
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
    genAssign lval (cppSub (cppIntLit (cppType rval) 1) rval)
  CPostIncOp -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    genAssign lval (cppSub (cppIntLit (cppType rval) 1) rval)
    return rval
  CPostDecOp -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    genAssign lval (cppSub (cppIntLit (cppType rval) 1) rval)
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
  CCompound ids items _ -> forM_ items $ \item -> do
    case item of
      CBlockStmt stmt -> genStmtSMT stmt
      CBlockDecl decl -> genDeclSMT True decl
      CNestedFunDef{} -> error "Nested function definitions not supported"
  CExpr e _                 -> when (isJust e) $ void $ genExprSMT $ fromJust e
  CIf cond trueBr falseBr _ -> do
    trueCond <- genExprSMT cond
    let falseCond = cppBitNot trueCond
    -- Guard the true branch with the true condition
    pushGuard (cppBool trueCond)
    enterLexScope
    genStmtSMT trueBr
    exitLexScope
    popGuard
    -- Guard the false branch with the false condition
    when (isJust falseBr) $ do
      pushGuard (cppBool falseCond)
      enterLexScope
      genStmtSMT $ fromJust falseBr
      exitLexScope
      popGuard
  CWhile{}                    -> liftIO $ print "while"
  CFor init check incr body _ -> do
    case init of
      Left  (Just expr) -> void $ genExprSMT expr
      Right decl        -> genDeclSMT True decl
      _                 -> return ()
    -- Make a guard on the bound to guard execution of the loop
    -- Execute up to the loop bound
    bound <- getLoopBound
    forM_ [0 .. bound] $ \_ -> do
      guard <- case check of
        Just b -> genExprSMT b
        _      -> error "NYI"
      pushGuard (cppBool guard)
      enterLexScope
      genStmtSMT body
      -- printComp
      -- increment the variable
      case incr of
        Just inc -> void $ genExprSMT inc
        _        -> error "Not yet supported"
      exitLexScope
    forM_ [0 .. bound] $ \_ -> do
      popGuard
    -- TODO: assert end
  CReturn expr _ -> when (isJust expr) $ do
    toReturn <- genExprSMT $ fromJust expr
    doReturn toReturn
  _ -> liftIO $ print $ unwords ["other", show stmt]

genDeclSMT :: Bool -> CDecl -> Compiler ()
genDeclSMT undef (CDecl specs decls _) = do
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        (isStorageSpec firstSpec) && (isTypedef $ storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  forM_ decls $ \(Just dec, mInit, _) -> do
    let mName = identFromDeclr dec
        name  = fromMaybe (error "Expected identifier in decl") mName
        ptrType = derivedFromDeclr dec

    if isTypedefDecl
      then typedefSMT name baseType ptrType
      else do
        declareVarSMT name baseType ptrType
        lhs <- genVarSMT name
        liftAssert $ Assert.assert $ Ty.Eq (udef lhs) (Ty.BoolLit undef)
        case mInit of
          Just (CInitExpr e _) -> do
            lhs <- genVarSMT name
            rhs <- genExprSMT e
            liftAssert $ cppAssign lhs rhs
            return ()
          _ -> return ()

---
--- High level codegen (translation unit, etc)
---

genFunDef :: CFunDef -> Bool -> Compiler ()
genFunDef f checkUndef = do
  -- Declare the function and setup the return value
  let name = nameFromFunc f
      ptrs = ptrsFromFunc f
      tys  = baseTypeFromFunc f
  retTy <- ctype tys ptrs
  pushFunction name retTy
  -- Declare the arguments and execute the body
  forM_ (argsFromFunc f) (genDeclSMT False)
  let body = bodyFromFunc f
  case body of
    CCompound{} -> genStmtSMT body
    _           -> error "Expected C statement block in function definition"
  when checkUndef $ do
    returnValue <- getReturn
    liftAssert $ Assert.assert $ udef returnValue
  popFunction

genAsm :: CStringLiteral a -> Compiler ()
genAsm = undefined

registerFns :: [CExtDecl] -> Compiler ()
registerFns decls = forM_ decls $ \case
  CFDefExt f -> do
    liftIO $ putStrLn $ nameFromFunc f
    registerFunction (nameFromFunc f) f
  _          -> pure ()

codegenAll :: CTranslUnit -> Compiler ()
codegenAll (CTranslUnit decls _) = do
  registerFns decls
  forM_ decls $ \case
    CDeclExt decl -> genDeclSMT True decl
    CFDefExt fun  -> genFunDef fun True
    CAsmExt asm _ -> genAsm asm

codegenFn :: CTranslUnit -> String -> Compiler ()
codegenFn (CTranslUnit decls _) name = do
  registerFns decls
  let f = fromMaybe (error $ "No " ++ name) $ listToMaybe $ concatMap
        (\case
          CFDefExt f -> if nameFromFunc f == name then [f] else []
          _          -> []
        )
        decls
  genFunDef f True

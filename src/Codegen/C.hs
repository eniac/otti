{-# LANGUAGE LambdaCase #-}
module Codegen.C where
import           AST.C
import           AST.Simple
import           Codegen.C.CompilerMonad
import           Codegen.C.CUtils
import           Codegen.C.Memory               ( bvNum
                                                , initMem
                                                )
import           Codegen.C.Utils
import           Control.Applicative
import           Control.Monad                  ( replicateM_
                                                , join
                                                )
import           Control.Monad.State.Strict     ( forM
                                                , forM_
                                                , gets
                                                , liftIO
                                                , unless
                                                , void
                                                , when
                                                )
--import Control.Monad.Trans.Class
import qualified Data.BitVector                as Bv
import           Data.Char                      ( toLower
                                                , ord
                                                )
import qualified Data.Char                     as Char
import           Data.Either                    ( fromRight )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , isNothing
                                                , listToMaybe
                                                , maybeToList
                                                )
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import           Language.C.Analysis.AstAnalysis
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import           Util.Log
--import Debug.Trace

fieldToInt :: Ident -> Int
fieldToInt = undefined

typedefSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Compiler ()
typedefSMT (Ident name _ _) tys ptrs = do
  ty <- ctype tys ptrs
  forM_ ty $ typedef name

declareVarSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Compiler ()
declareVarSMT (Ident name _ _) tys ptrs = do
  ty <- ctype tys ptrs
  forM_ ty $ declareVar name

genVarSMT :: Ident -> Compiler CTerm
genVarSMT (Ident name _ _) = getTerm name

genConstSMT :: CConst -> Compiler CTerm
genConstSMT c = case c of
--genConstSMT c = liftIO (("Const: " ++) <$> nodeText c >>= putStrLn) >> case c of
  CIntConst (CInteger i _ _) _ -> return $ cppIntLit S32 i
  CCharConst (CChar c _) _ -> return $ cppIntLit S8 $ toInteger $ Char.ord c
  CCharConst (CChars c _) _ -> error "Chars const unsupported"
  CFloatConst (Language.C.Syntax.Constants.CFloat str) _ ->
    case toLower (last str) of
      'f' -> return $ cppFloatLit (read $ init str)
      'l' -> return $ cppDoubleLit (read $ init str)
      _   -> return $ cppDoubleLit (read str)
  CStrConst (CString str _) _ -> liftMem $ cppArrayLit
    S8
    (map (cppIntLit S8 . toInteger . ord) str ++ [cppIntLit S8 0])

data CLVal = CLVar VarName
           | CLAddr CTerm
           deriving (Show)

evalLVal :: CLVal -> Compiler CTerm
evalLVal location = case location of
  CLVar  v -> getTerm v
  CLAddr a -> load a

genLValueSMT :: CExpr -> Compiler CLVal
genLValueSMT expr = case expr of
  CVar (Ident name _ _) _ -> return $ CLVar name
  CUnary CIndOp addr _    -> CLAddr <$> genExprSMT addr
  CIndex base   idx  _    -> do
    base' <- genExprSMT base
    idx'  <- genExprSMT idx
    addr  <- liftMem $ cppIndex base' idx'
    return $ CLAddr addr
  _ -> error $ unwords ["Not yet impled:", show expr]

genAssign :: CLVal -> CTerm -> Compiler CTerm
genAssign location value = case location of
  CLVar  varName -> ssaAssign varName value
  CLAddr addr    -> store addr value >> return value

unwrap :: Show l => Either l r -> r
unwrap e = case e of
  Left  l -> error $ "Either is not right, it is: Left " ++ show l
  Right r -> r

genExprSMT :: CExpr -> Compiler CTerm
--genExprSMT expr = liftIO (("Expr: " ++) <$> nodeText expr >>= putStrLn) >> case expr of
genExprSMT expr = case expr of
  CVar id _            -> genVarSMT id
  CConst c             -> genConstSMT c
  CAssign op lhs rhs _ -> do
    lval <- genLValueSMT lhs
    rval <- genExprSMT rhs
    genAssignOp op lval rval
  CBinary op left right _ -> case op of
    CLndOp -> do
      left'  <- genExprSMT left
      right' <- guarded (cppBool left') $ genExprSMT right
      return $ cppAnd left' right'
    CLorOp -> do
      left'  <- genExprSMT left
      right' <- guarded (Ty.Not $ cppBool left') $ genExprSMT right
      return $ cppOr left' right'
    _ -> do
      left'  <- genExprSMT left
      right' <- genExprSMT right
      getBinOp op left' right'
  CUnary op   arg   _ -> getUnaryOp op arg
  CIndex base index _ -> do
    base'  <- genExprSMT base
    index' <- genExprSMT index
    offset <- liftMem $ cppIndex base' index'
    load offset
  --CMember struct field _ _ -> do
  --  struct' <- genExprSMT struct
  --  liftMem $ getField struct' $ fieldToInt field
  CCast decl expr _ -> case decl of
    CDecl specs _ _ -> do
      ty    <- unwrap <$> baseTypeFromSpecs specs
      expr' <- genExprSMT expr
      return $ cppCast ty expr'
    _ -> error "Expected type in cast"
  CCall fn args _ -> case fn of
    CVar fnIdent _ -> do
      let fnName = identToVarName fnIdent
      actualArgs <- traverse genExprSMT args
      f          <- getFunction fnName
      retTy      <- unwrap <$> ctype (baseTypeFromFunc f) (ptrsFromFunc f)
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
  CSizeofExpr e _ -> do
    -- Evaluate in false context, to get type, but avoid side-effects
    e' <- guarded (Ty.BoolLit False) (genExprSMT e)
    return $ cppIntLit U32 (toInteger $ numBits (cppType e') `div` 8)
  CSizeofType decl _ -> do
    ty <- unwrap <$> cDeclToType decl
    return $ cppIntLit U32 (toInteger $ numBits ty `div` 8)
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
    load arg
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
genAssignOp :: CAssignOp -> CLVal -> CTerm -> Compiler CTerm
genAssignOp op l r = case op of
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
    guarded trueCond $ genStmtSMT trueBr
    -- Guard the false branch with the false condition
    forM_ falseBr $ \br -> guarded (Ty.Not trueCond) $ genStmtSMT br
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
  CWhile check body isDoWhile _ -> do
    bound <- getLoopBound
    let addGuard = genExprSMT check >>= pushGuard . cppBool
    forM_ [0 .. bound] $ \_ -> do
      unless isDoWhile addGuard
      genStmtSMT body
      when isDoWhile addGuard
    replicateM_ (bound + 1) popGuard
  CReturn expr _ -> when (isJust expr) $ do
    toReturn <- genExprSMT $ fromJust expr
    doReturn toReturn
  _ -> error $ unwords ["Unsupported: ", show stmt]

-- Returns the declaration's variable name
genDeclSMT :: Maybe Bool -> CDecl -> Compiler [String]
genDeclSMT undef d@(CDecl specs decls _) = do
  -- At the top level, we ignore types we don't understand.
  skipBadTypes <- gets (null . callStack)
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        (isStorageSpec firstSpec) && (isTypedef $ storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  forM decls $ \(Just dec, mInit, _) -> do
    let mName   = identFromDeclr dec
        ident   = fromMaybe (error "Expected identifier in decl") mName
        name    = identToVarName ident
        ptrType = derivedFromDeclr dec

    if isTypedefDecl
      then typedefSMT ident baseType ptrType >> return "TYPEDEF"
      else do
        declareVarSMT ident baseType ptrType
        lhs <- genVarSMT ident
        case mInit of
          Just init -> do
            ty <- ctype baseType ptrType
            case ty of
              Left  err -> if skipBadTypes then return () else error err
              Right ty  -> do
                rhs <- genInitSMT ty init
                void $ argAssign name rhs
          Nothing -> whenM (gets findUB) $ forM_
            undef
            (liftAssert . Assert.assert . Ty.Eq (udef lhs) . Ty.BoolLit)
        return name

genInitSMT :: Type -> CInit -> Compiler CTerm
genInitSMT ty i = case (ty, i) of
  (ty             , CInitExpr e _ ) -> cppCast ty <$> genExprSMT e
  (Array _ innerTy, CInitList is _) -> do
    values <- forM is $ \(_, i) -> genInitSMT innerTy i
    liftMem $ cppArrayLit innerTy values
  _ -> error $ unwords ["Cannot initialize type", show ty, "from", show i]

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
  retTy <- unwrap <$> ctype tys ptrs
  pushFunction name retTy
  -- Declare the arguments and execute the body
  inputNamesList <- forM (argsFromFunc f) (genDeclSMT (Just False))
  let inputNames = join inputNamesList
  fullInputNames <- map ssaVarAsString <$> forM inputNames getSsaVar
  def            <- gets defaultValue
  case inVals of
    Just i -> forM_ inputNames $ \n -> initAssign n $ fromMaybe
      (error $ "Missing value for input " ++ n)
      ((i Map.!? n) <|> def)
    Nothing -> return ()

  let body = bodyFromFunc f
  case body of
    CCompound{} -> genStmtSMT body
    _           -> error "Expected C statement block in function definition"
  returnValue <- getReturn
  whenM (gets findUB) $ bugIf $ udef returnValue
  popFunction
  return (fullInputNames, fromJust $ asVar returnValue)

genAsm :: CStringLiteral a -> Compiler ()
genAsm = undefined

registerFns :: [CExtDecl] -> Compiler ()
registerFns decls = forM_ decls $ \case
  CFDefExt f -> registerFunction (nameFromFunc f) f
  CDeclExt d -> do
    void $ genDeclSMT Nothing d
    printComp
  CAsmExt asm _ -> genAsm asm

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
  assertions <- Assert.execAssert
    $ evalCodegen True (codegenFn tu name Nothing >> assertBug)
  liftIO $ Ty.evalZ3 $ Ty.BoolNaryExpr Ty.And (Assert.asserted assertions)

evalFn :: CTranslUnit -> String -> IO (Map.Map String Ty.Val)
evalFn tu name = do
  assertions <- Assert.execAssert $ evalCodegen False $ codegenFn tu
                                                                  name
                                                                  Nothing
  liftIO $ Ty.evalZ3Model $ Ty.BoolNaryExpr Ty.And (Assert.asserted assertions)

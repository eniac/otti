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

declareVarSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> Compiler (Either String Type)
declareVarSMT (Ident name _ _) tys ptrs = do
  ty <- ctype tys ptrs
  forM_ ty $ declareVar name
  return ty

genVarSMT :: Ident -> Compiler SsaVal
genVarSMT (Ident name _ _) = getTerm $ SLVar name

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

data CLVal = CLVar SsaLVal
           -- TODO: SsaVal?
           | CLAddr SsaVal
           | CLField CLVal String
           deriving (Show)

evalLVal :: CLVal -> Compiler SsaVal
evalLVal location = case location of
  CLVar  v -> getTerm v
  CLAddr a -> case a of
    Base c -> Base <$> load c
    RefVal r -> getTerm (SLRef r)
  CLField s f -> ssaStructGet f <$> evalLVal s

genLValueSMT :: CExpr -> Compiler CLVal
genLValueSMT expr = case expr of
  CVar (Ident name _ _) _ -> return $ CLVar $ SLVar name
  CUnary CIndOp addr _    -> CLAddr <$> genExprSMT addr
  CIndex base   idx  _    -> do
    base' <- genExprSMT base
    idx'  <- genExprSMT idx
    addr  <- liftMem $ liftCFun2M "cppIndex" cppIndex base' idx'
    return $ CLAddr addr
  CMember struct ident False _ -> flip CLField (identToVarName ident) <$> genLValueSMT struct
  CMember struct ident True _ -> do
    s <- genExprSMT struct
    return $ CLField (CLAddr s) (identToVarName ident)
  _ -> error $ unwords ["Not yet impled:", show expr]

genRefSMT :: CExpr -> Compiler SsaVal
genRefSMT expr = case expr of
  CVar (Ident name _ _) _ -> getRef $ SLVar name
  _ -> error $ unwords ["Not yet impled:", show expr]


-- TODO: This may be broken
-- The approach in `modLocation` is the right one, but when run on addresses it
-- performs accesses to the underlying storage, even when we're doing an
-- overwrite. This may not agree with our uninit tracking system.
genAssign :: CLVal -> SsaVal -> Compiler SsaVal
genAssign location value = case location of
  CLVar  varName -> ssaAssign varName value
  CLAddr addr    -> case addr of
    Base a -> ssaStore addr value >> return value
    RefVal r -> ssaAssign (SLRef r) value
  CLField struct field -> modLocation location (const value)
   where
    -- Apply a modification function to a location
    modLocation :: CLVal -> (SsaVal -> SsaVal) -> Compiler SsaVal
    modLocation location modFn = case location of
      CLVar varName -> getTerm varName >>= ssaAssign varName . modFn
      CLAddr addr   -> case addr of
        Base c -> do
          old <- ssaLoad addr
          let new = modFn old
          ssaStore addr new
          return new
        RefVal r -> let v = SLRef r in getTerm v >>= ssaAssign v . modFn
      CLField struct field ->
        modLocation struct (\t -> ssaStructSet field (modFn $ ssaStructGet field t) t)

unwrap :: Show l => Either l r -> r
unwrap e = case e of
  Left  l -> error $ "Either is not right, it is: Left " ++ show l
  Right r -> r

genExprSMT :: CExpr -> Compiler SsaVal
--genExprSMT expr = liftIO (("Expr: " ++) <$> nodeText expr >>= putStrLn) >> case expr of
genExprSMT expr = do
  liftLog $ logIfM "expr" $ do
    t <- liftIO $ nodeText expr
    return $ "Expr: " ++ t
  case expr of
    CVar id _            -> genVarSMT id
    CConst c             -> Base <$> genConstSMT c
    CAssign op lhs rhs _ -> do
      lval <- genLValueSMT lhs
      rval <- genExprSMT rhs
      genAssignOp op lval rval
    CBinary op left right _ -> case op of
      CLndOp -> do
        left'  <- genExprSMT left
        right' <- guarded (ssaBool left') $ genExprSMT right
        return $ liftCFun2 "cppAnd" cppAnd left' right'
      CLorOp -> do
        left'  <- genExprSMT left
        right' <- guarded (Ty.Not $ ssaBool left') $ genExprSMT right
        return $ liftCFun2 "cppOr" cppOr left' right'
      _ -> do
        left'  <- genExprSMT left
        right' <- genExprSMT right
        getBinOp op left' right'
    CUnary op   arg   _ -> getUnaryOp op arg
    CIndex base index _ -> do
      base'  <- genExprSMT base
      index' <- genExprSMT index
      offset <- liftMem $ liftCFun2M "cppIndex" cppIndex base' index'
      ssaLoad offset
    CMember struct ident isArrow _ -> do
      e <- genExprSMT struct
      -- If this is a ->, then derefence the left first.
      s <- if isArrow
              then getTerm (deref e)
              else return $ e
      return $ ssaStructGet (identToVarName ident) s
    CCast decl expr _ -> case decl of
      CDecl specs _ _ -> do
        ty    <- unwrap <$> baseTypeFromSpecs specs
        expr' <- genExprSMT expr
        return $ liftCFun "cppCast" (cppCast ty) expr'
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
            map (SLVar . identToVarName)
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
        return $ Base $ returnValue
      _ -> error $ unwords ["Fn call of", show fn, "is unsupported"]
    CCond cond mTrueBr falseBr _ -> do
      cond' <- genExprSMT cond
      true' <- maybe (return cond') genExprSMT mTrueBr
      false' <- genExprSMT falseBr
      return $ liftCFun3 "cppCond" cppCond cond' true' false'
    CSizeofExpr e _ -> do
      -- Evaluate in false context, to get type, but avoid side-effects
      e' <- guarded (Ty.BoolLit False) (genExprSMT e)
      let bits = case e' of
                  Base c -> numBits (cppType c)
                  RefVal {} -> numBits $ Ptr32 U8
      return $ Base $ cppIntLit U32 (toInteger $ bits `div` 8)
    CSizeofType decl _ -> do
      ty <- unwrap <$> cDeclToType decl
      return $ Base $ cppIntLit U32 (toInteger $ numBits ty `div` 8)
    _ -> error $ unwords ["We do not support", show expr, "right now"]

isIncDec :: CUnaryOp -> Bool
isIncDec o = o `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp]

isDec :: CUnaryOp -> Bool
isDec o = o `elem` [CPreDecOp, CPostDecOp]

isPre :: CUnaryOp -> Bool
isPre o = o `elem` [CPreDecOp, CPreDecOp]

getUnaryOp :: CUnaryOp -> CExpr -> Compiler SsaVal
getUnaryOp op arg = case op of
  _ | isIncDec op -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    let one = Base $ cppIntLit (ssaType rval) 1
    let new = liftCFun2 (show op) (if isDec op then cppSub else cppAdd) rval one
    genAssign lval new
    return $ if isPre op
      then new
      else rval
  CIndOp  -> do
    l <- genExprSMT arg
    case l of
      Base c -> Base <$> load c
      RefVal r -> getTerm (SLRef r)
  CPlusOp -> error $ unwords ["Do not understand:", show op]
  CMinOp  -> liftCFun "cppNeg" cppNeg <$> genExprSMT arg
  CCompOp -> liftCFun "cppBitNot" cppBitNot <$> genExprSMT arg
  CNegOp  -> liftCFun "cppNot" cppNot <$> genExprSMT arg
  CAdrOp  -> genRefSMT arg
  _       -> error $ unwords [show op, "not supported"]

getBinOp :: CBinaryOp -> SsaVal -> SsaVal -> Compiler SsaVal
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
  in  return $ liftCFun2 (show op) f left right

-- | Assign operation
-- eg x += 1
-- aka x = x + 1
genAssignOp :: CAssignOp -> CLVal -> SsaVal -> Compiler SsaVal
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
          genAssign l (liftCFun2 (show op) f lvalue r)

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
    trueCond <- ssaBool <$> genExprSMT cond
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
      pushGuard (ssaBool guard)
      genStmtSMT body
      -- increment the variable
      case incr of
        Just inc -> void $ genExprSMT inc
        _        -> error "Not yet supported"
    replicateM_ (bound + 1) popGuard
    -- TODO: assert end
  CWhile check body isDoWhile _ -> do
    bound <- getLoopBound
    let addGuard = genExprSMT check >>= pushGuard . ssaBool
    forM_ [0 .. bound] $ \_ -> do
      unless isDoWhile addGuard
      genStmtSMT body
      when isDoWhile addGuard
    replicateM_ (bound + 1) popGuard
  CReturn expr _ -> when (isJust expr) $ do
    toReturn <- genExprSMT $ fromJust expr
    doReturn $ ssaValAsCTerm "return" toReturn
  _ -> error $ unwords ["Unsupported: ", show stmt]

-- Returns the declaration's variable name
genDeclSMT :: Maybe Bool -> CDecl -> Compiler [String]
genDeclSMT undef d@(CDecl specs decls _) = do
  liftLog $ logIf "decls" "genDeclSMT:"
  liftLog $ logIfM "decls" $ liftIO $ nodeText d
  -- At the top level, we ignore types we don't understand.
  skipBadTypes <- gets (null . callStack)
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        (isStorageSpec firstSpec) && (isTypedef $ storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  -- Even for not declarators, process the type. It may be a struct that needs to be recorded!
  when (null decls) $ void $ baseTypeFromSpecs baseType

  forM decls $ \(Just dec, mInit, _) -> do
    let mName   = identFromDeclr dec
        ident   = fromMaybe (error "Expected identifier in decl") mName
        name    = identToVarName ident
        ptrType = derivedFromDeclr dec

    if isTypedefDecl
      then do
        typedefSMT ident baseType ptrType
        return "TYPEDEF"
      else do
        -- TODO: kick this into the Nothing case below, use better thing in the
        -- Just case?
        mTy <- declareVarSMT ident baseType ptrType
        case mTy of
          Right ty -> do
            lhs <- genVarSMT ident
            case mInit of
              Just init -> do
                ty <- ctype baseType ptrType
                case ty of
                  Left  err -> if skipBadTypes then return () else error err
                  Right ty  -> do
                    rhs <- genInitSMT ty init
                    void $ argAssign (SLVar name) rhs
              Nothing -> whenM (gets findUB) $ forM_
                undef
                (liftAssert . Assert.assert . Ty.Eq (udef $ ssaValAsCTerm "undef settting in genDeclSMT" lhs) . Ty.BoolLit)
          Left {} -> return ()
        return name

genInitSMT :: Type -> CInit -> Compiler SsaVal
genInitSMT ty i = case (ty, i) of
  (tt             , CInitExpr e _ ) -> do
    t <- genExprSMT e
    return $ case t of
      Base c -> Base $ cppCast ty c
      RefVal {} -> t
  (Array _ innerTy, CInitList is _) -> do
    values <- forM is $ \(_, i) -> genInitSMT innerTy i
    let cvals = map (ssaValAsCTerm "Cannot put refs in arrays") values
    liftMem $ Base <$> cppArrayLit innerTy cvals
  (Struct fields, CInitList is _) -> do
    values <- forM (zip fields is) $ \((_, fTy), (_, i)) -> genInitSMT fTy i
    let cvals = map (ssaValAsCTerm "Cannot put refs in structs") values
    liftMem $ Base <$> cppStructLit ty cvals
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
  fullInputNames <- map ssaVarAsString <$> forM inputNames (getSsaVar . SLVar)
  def            <- gets defaultValue
  case inVals of
    Just i -> forM_ inputNames $ \n -> initAssign (SLVar n) $ fromMaybe
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
  CFDefExt f    -> registerFunction (nameFromFunc f) f
  CDeclExt d    -> void $ genDeclSMT Nothing d
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

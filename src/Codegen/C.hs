{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TupleSections              #-}
module Codegen.C where
import           AST.C
import           Codegen.C.CUtils
import           Codegen.C.Utils
import           Codegen.Circify
import           Codegen.Circify.Memory         ( MonadMem
                                                , liftMem
                                                , MemState
                                                )
import           Control.Monad                  ( join
                                                , replicateM_
                                                , forM
                                                )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Char                      ( ord
                                                , toLower
                                                )
import qualified Data.Char                     as Char
import           Data.Either                    ( isRight )
import           Data.List                      ( intercalate )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , catMaybes
                                                )
import           IR.SMT.Assert                  ( MonadAssert
                                                , liftAssert
                                                )
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import           Util.Cfg                       ( Cfg
                                                , MonadCfg(..)
                                                , _loopBound
                                                )
import           Util.Log


data CState = CState { funs          :: Map.Map FunctionName CFunDef
                     , loopBound     :: Int
                     , findUB        :: Bool
                     , bugConditions :: [Ty.TermBool]
                     }

newtype C a = C (StateT CState (Circify Type CTerm) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadIO, MonadLog, MonadAssert, MonadMem, MonadCircify Type CTerm, MonadCfg)

emptyCState :: Bool -> CState
emptyCState findBugs = CState { funs          = Map.empty
                              , loopBound     = 5
                              , findUB        = findBugs
                              , bugConditions = []
                              }

cfgFromEnv :: C ()
cfgFromEnv = do
  bound <- liftCfg $ asks _loopBound
  modify $ \s -> s { loopBound = bound }
  liftLog $ logIf "loop" $ "Setting loop bound to " ++ show bound

-- Loops

getLoopBound :: C Int
getLoopBound = gets loopBound

setLoopBound :: Int -> C ()
setLoopBound bound = modify (\s -> s { loopBound = bound })

-- Functions

registerFunction :: FunctionName -> CFunDef -> C ()
registerFunction name function = do
  s0 <- get
  case Map.lookup name $ funs s0 of
    Nothing -> put $ s0 { funs = Map.insert name function $ funs s0 }
    _       -> error $ unwords ["Already declared", name]

getFunction :: FunctionName -> C CFunDef
getFunction funName = do
  functions <- gets funs
  case Map.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords ["Called undeclared function", funName]

-- Bugs

-- Record that a bug happens if some condition is met (on this path!)
bugIf :: Ty.TermBool -> C ()
bugIf c = do
  g <- liftCircify getGuard
  modify $ \s ->
    s { bugConditions = Ty.BoolNaryExpr Ty.And [g, c] : bugConditions s }

-- Assert that some recorded bug happens
assertBug :: C ()
assertBug = do
  cs <- gets bugConditions
  liftAssert $ Assert.assert $ Ty.BoolNaryExpr Ty.Or cs

-- Lift some CUtils stuff to the SSA layer
ssaBool :: CSsaVal -> Ty.TermBool
ssaBool = cBool . ssaValAsTerm "cBool"

ssaType :: CSsaVal -> Type
ssaType = cType . ssaValAsTerm "cType"

ssaLoad :: CSsaVal -> C CSsaVal
ssaLoad addr = case addr of
  Base cterm -> do
    (oob, val) <- liftMem $ cLoad cterm
    whenM (gets findUB) $ bugIf oob
    return $ Base val
  RefVal inner -> liftCircify $ getTerm (SLRef inner)

ssaStore :: CSsaVal -> CSsaVal -> C ()
ssaStore ref val = case (ref, val) of
  (Base addr, Base cval) -> do
    g   <- liftCircify getGuard
    oob <- liftMem $ cStore addr cval g
    whenM (gets findUB) $ bugIf oob
  _ -> error $ "Cannot store " ++ show (ref, val)

ssaStructGet :: String -> CSsaVal -> CSsaVal
ssaStructGet n = liftTermFun "cStructGet" (`cStructGet` n)

ssaStructSet :: String -> CSsaVal -> CSsaVal -> CSsaVal
ssaStructSet n = liftTermFun2 "cStructSet" (cStructSet n)

typedefSMT :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> C (Maybe Type)
typedefSMT (Ident name _ _) tys ptrs = do
  ty <- liftCircify $ ctype tys ptrs
  case ty of
    Right ty' -> liftCircify $ typedef name ty' >> return (Just ty')
    Left  _   -> return Nothing

declareVarSMT
  :: Ident -> [CDeclSpec] -> [CDerivedDeclr] -> C (Either String Type)
declareVarSMT (Ident name _ _) tys ptrs = do
  ty <- liftCircify $ ctype tys ptrs
  forM_ ty $ liftCircify . declareVar name
  return ty

genVarSMT :: Ident -> C CSsaVal
genVarSMT (Ident name _ _) = liftCircify $ getTerm $ SLVar name

genConstSMT :: CConst -> C CTerm
genConstSMT c = case c of
  CIntConst  (CInteger i _ _) _ -> return $ cIntLit S32 i
  CCharConst (CChar c _     ) _ -> return $ cIntLit S8 $ toInteger $ Char.ord c
  CCharConst (CChars{}      ) _ -> error "Chars const unsupported"
  CFloatConst (Language.C.Syntax.Constants.CFloat str) _ ->
    case toLower (last str) of
      'f' -> return $ cFloatLit (read $ init str)
      'l' -> return $ cDoubleLit (read $ init str)
      _   -> return $ cDoubleLit (read str)
  CStrConst (CString str _) _ -> liftMem
    $ cArrayLit S8 (map (cIntLit S8 . toInteger . ord) str ++ [cIntLit S8 0])

type CSsaVal = SsaVal CTerm

data CLVal = CLVar SsaLVal
           | CLAddr CSsaVal
           | CLField CLVal String
           deriving (Show)

evalLVal :: CLVal -> C CSsaVal
evalLVal location = case location of
  CLVar  v    -> liftCircify $ getTerm v
  CLAddr a    -> ssaLoad a
  CLField s f -> ssaStructGet f <$> evalLVal s

genLValueSMT :: CExpr -> C CLVal
genLValueSMT expr = case expr of
  CVar (Ident name _ _) _ -> return $ CLVar $ SLVar name
  CUnary CIndOp addr _    -> CLAddr <$> genExprSMT addr
  CIndex base   idx  _    -> do
    base' <- genExprSMT base
    idx'  <- genExprSMT idx
    addr  <- liftMem $ liftTermFun2M "cIndex" cIndex base' idx'
    return $ CLAddr addr
  CMember struct ident False _ ->
    flip CLField (identToVarName ident) <$> genLValueSMT struct
  CMember struct ident True _ -> do
    s <- genExprSMT struct
    return $ CLField (CLAddr s) (identToVarName ident)
  _ -> error $ unwords ["Not yet impled:", show expr]

genRefSMT :: CExpr -> C CSsaVal
genRefSMT expr = case expr of
  CVar (Ident name _ _) _ -> liftCircify $ getRef $ SLVar name
  _                       -> error $ unwords ["Not yet impled:", show expr]


-- TODO: This may be broken
-- The approach in `modLocation` is the right one, but when run on addresses it
-- performs accesses to the underlying storage, even when we're doing an
-- overwrite. This may not agree with our uninit tracking system.
genAssign :: CLVal -> CSsaVal -> C CSsaVal
genAssign location value = case location of
  CLVar  varName -> liftCircify $ ssaAssign varName value
  CLAddr addr    -> case addr of
    Base{}   -> ssaStore addr value >> return value
    RefVal r -> liftCircify $ ssaAssign (SLRef r) value
  CLField{} -> modLocation location (const value)
   where
    -- Apply a modification function to a location
    modLocation :: CLVal -> (CSsaVal -> CSsaVal) -> C CSsaVal
    modLocation location modFn = case location of
      CLVar varName ->
        liftCircify $ getTerm varName >>= ssaAssign varName . modFn
      CLAddr addr -> case addr of
        Base _ -> do
          old <- ssaLoad addr
          let new = modFn old
          ssaStore addr new
          return new
        RefVal r ->
          let v = SLRef r in liftCircify $ getTerm v >>= ssaAssign v . modFn
      CLField struct field -> modLocation
        struct
        (\t -> ssaStructSet field (modFn $ ssaStructGet field t) t)

unwrap :: Show l => Either l r -> r
unwrap e = case e of
  Left  l -> error $ "Either is not right, it is: Left " ++ show l
  Right r -> r

noneIfVoid :: Type -> Maybe Type
noneIfVoid t = if Void == t then Nothing else Just t

genExprSMT :: CExpr -> C CSsaVal
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
        return $ liftTermFun2 "cAnd" cAnd left' right'
      CLorOp -> do
        left'  <- genExprSMT left
        right' <- guarded (Ty.Not $ ssaBool left') $ genExprSMT right
        return $ liftTermFun2 "cOr" cOr left' right'
      _ -> do
        left'  <- genExprSMT left
        right' <- genExprSMT right
        getBinOp op left' right'
    CUnary op   arg   _ -> getUnaryOp op arg
    CIndex base index _ -> do
      base'  <- genExprSMT base
      index' <- genExprSMT index
      offset <- liftMem $ liftTermFun2M "cIndex" cIndex base' index'
      ssaLoad offset
    CMember struct ident isArrow _ -> do
      e <- genExprSMT struct
      -- If this is a ->, then derefence the left first.
      s <- if isArrow then liftCircify $ getTerm (deref e) else return e
      return $ ssaStructGet (identToVarName ident) s
    CCast decl expr _ -> case decl of
      CDecl specs _ _ -> do
        ty    <- liftCircify $ unwrap <$> baseTypeFromSpecs specs
        expr' <- genExprSMT expr
        return $ liftTermFun "cCast" (cCast ty) expr'
      _ -> error "Expected type in cast"
    CCall fn args _ -> case fn of
      CVar fnIdent _ -> do
        let fnName = identToVarName fnIdent
        actualArgs <- traverse genExprSMT args
        f          <- getFunction fnName
        retTy      <- liftCircify $ unwrap <$> ctype (baseTypeFromFunc f)
                                                     (ptrsFromFunc f)
        liftCircify $ pushFunction fnName (noneIfVoid retTy)
        forM_ (argsFromFunc f) (genDeclSMT Nothing)
        let
          formalArgs =
            map (SLVar . identToVarName)
              $ concatMap
                  (\case
                    CDecl _ decls _ -> map
                      (\(Just dec, _, _) ->
                        let mName = identFromDeclr dec
                        in  fromMaybe (error "Expected identifier in decl")
                                      mName
                      )
                      decls
                    _ -> error "Missing case in formalArgs"
                  )
              $ argsFromFunc f
        unless (length formalArgs == length actualArgs)
          $  error
          $  "Wrong arg count: "
          ++ show expr
        liftCircify $ forM_ (zip formalArgs actualArgs) (uncurry argAssign)
        let body = bodyFromFunc f
        case body of
          CCompound{} -> genStmtSMT body
          _ -> error "Expected C statement block in function definition"
        returnValue <- liftCircify popFunction
        return $ Base $ fromMaybe
          (error "Getting the return value of a void fn")
          returnValue
      _ -> error $ unwords ["Fn call of", show fn, "is unsupported"]
    CCond cond mTrueBr falseBr _ -> do
      cond'  <- genExprSMT cond
      true'  <- maybe (return cond') genExprSMT mTrueBr
      false' <- genExprSMT falseBr
      return $ liftTermFun3 "cCond" cCond cond' true' false'
    CSizeofExpr e _ -> do
      -- Evaluate in false context, to get type, but avoid side-effects
      e' <- guarded (Ty.BoolLit False) (genExprSMT e)
      let bits = case e' of
            Base c   -> numBits (cType c)
            RefVal{} -> numBits $ Ptr32 U8
      return $ Base $ cIntLit U32 (toInteger $ bits `div` 8)
    CSizeofType decl _ -> do
      ty <- liftCircify $ unwrap <$> cDeclToType decl
      return $ Base $ cIntLit U32 (toInteger $ numBits ty `div` 8)
    _ -> error $ unwords ["We do not support", show expr, "right now"]


getUnaryOp :: CUnaryOp -> CExpr -> C CSsaVal
getUnaryOp op arg = case op of
  _ | isIncDec op -> do
    lval <- genLValueSMT arg
    rval <- evalLVal lval
    let one = Base $ cIntLit (ssaType rval) 1
    let new = liftTermFun2 (show op) (if isDec op then cSub else cAdd) rval one
    _ <- genAssign lval new
    return $ if isPre op then new else rval
  CIndOp -> do
    l <- genExprSMT arg
    ssaLoad l
  CPlusOp -> error $ unwords ["Do not understand:", show op]
  CMinOp  -> liftTermFun "cNeg" cNeg <$> genExprSMT arg
  CCompOp -> liftTermFun "cBitNot" cBitNot <$> genExprSMT arg
  CNegOp  -> liftTermFun "cNot" cNot <$> genExprSMT arg
  CAdrOp  -> genRefSMT arg
  _       -> error $ unwords [show op, "not supported"]
 where
  isIncDec o = o `elem` [CPreIncOp, CPreDecOp, CPostIncOp, CPostDecOp]
  isDec o = o `elem` [CPreDecOp, CPostDecOp]
  isPre o = o `elem` [CPreDecOp, CPreDecOp]

getBinOp :: CBinaryOp -> CSsaVal -> CSsaVal -> C CSsaVal
getBinOp op left right =
  let f = case op of
        CMulOp -> cMul
        CDivOp -> cDiv
        CRmdOp -> cRem
        CAddOp -> cAdd
        CSubOp -> cSub
        CShlOp -> cShl
        CShrOp -> cShr
        CLeOp  -> cLt
        CGrOp  -> cGt
        CLeqOp -> cLe
        CGeqOp -> cGe
        CEqOp  -> cEq
        CNeqOp -> cNe
        CAndOp -> cBitAnd
        CXorOp -> cBitXor
        COrOp  -> cBitOr
        CLndOp -> cAnd
        CLorOp -> cOr
  in  return $ liftTermFun2 (show op) f left right

-- | Assign operation
-- eg x += 1
-- aka x = x + 1
genAssignOp :: CAssignOp -> CLVal -> CSsaVal -> C CSsaVal
genAssignOp op l r = case op of
  CAssignOp -> genAssign l r
  _ ->
    let f = case op of
          CMulAssOp -> cMul
          CAddAssOp -> cAdd
          CSubAssOp -> cSub
          CShlAssOp -> cShl
          CShrAssOp -> cShr
          CAndAssOp -> cBitAnd
          CXorAssOp -> cBitXor
          COrAssOp  -> cBitOr
          o         -> error $ unwords ["Cannot handle", show o]
    in  do
          lvalue <- evalLVal l
          genAssign l (liftTermFun2 (show op) f lvalue r)

---
--- Statements
---

genStmtSMT :: CStat -> C ()
genStmtSMT stmt = do
  liftLog $ logIfM "stmt" $ do
    t <- liftIO $ nodeText stmt
    return $ "Stmt: " ++ t
  case stmt of
    CCompound _ items _ -> do
      liftCircify enterLexScope
      forM_ items $ \case
        CBlockStmt stmt -> genStmtSMT stmt
        CBlockDecl decl -> void $ genDeclSMT (Just True) decl
        CNestedFunDef{} -> error "Nested function definitions not supported"
      liftCircify exitLexScope
    CExpr e _ -> when (isJust e) $ void $ genExprSMT $ fromJust e
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
      replicateM_ bound $ do
        test <- genExprSMT $ fromMaybe (error "Missing test in for-loop") check
        liftCircify $ pushGuard (ssaBool test)
        genStmtSMT body
        -- increment the variable
        forM_ incr $ \inc -> genExprSMT inc
      replicateM_ bound (liftCircify popGuard)
      -- TODO: assert end
    CWhile check body isDoWhile _ -> do
      bound <- getLoopBound
      let addGuard = genExprSMT check >>= liftCircify . pushGuard . ssaBool
      replicateM_ bound $ do
        unless isDoWhile addGuard
        genStmtSMT body
        when isDoWhile addGuard
      replicateM_ bound (liftCircify popGuard)
    CReturn expr _ -> when (isJust expr) $ do
      toReturn <- genExprSMT $ fromJust expr
      liftCircify $ doReturn $ ssaValAsTerm "return" toReturn
    _ -> error $ unwords ["Unsupported: ", show stmt]

-- Returns the names of all declared variables, and their types
genDeclSMT :: Maybe Bool -> CDecl -> C [(String, Type)]
genDeclSMT undef d@(CDecl specs decls _) = do
  liftLog $ logIf "decls" "genDeclSMT:"
  liftLog $ logIfM "decls" $ liftIO $ nodeText d
  -- At the top level, we ignore types we don't understand.
  skipBadTypes <- liftCircify $ gets (null . callStack)
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        isStorageSpec firstSpec && isTypedef (storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  -- Even for not declarators, process the type. It may be a struct that needs to be recorded!
  when (null decls) $ void $ liftCircify $ baseTypeFromSpecs baseType

  ms <- forM decls $ \(Just dec, mInit, _) -> do
    let mName   = identFromDeclr dec
        ident   = fromMaybe (error "Expected identifier in decl") mName
        name    = identToVarName ident
        ptrType = derivedFromDeclr dec

    if isTypedefDecl
      then do
        ty <- typedefSMT ident baseType ptrType
        return $ ("TYPEDEF", ) <$> ty
      else do
        -- TODO: kick this into the Nothing case below, use better thing in the
        -- Just case?
        ty <- case mInit of
          Just init -> do
            ty <- liftCircify $ ctype baseType ptrType
            case ty of
              Left  err -> if skipBadTypes then return Nothing else error err
              Right ty  -> do
                rhs <- genInitSMT ty init
                liftCircify $ declareInitVar name ty rhs
                return $ Just ty
          Nothing -> do
            mTy <- declareVarSMT ident baseType ptrType
            when (not skipBadTypes || isRight mTy) $ do
              lhs <- genVarSMT ident
              whenM (gets findUB) $ forM_
                undef
                ( liftAssert
                . Assert.assert
                . Ty.Eq
                    (udef $ ssaValAsTerm "undef settting in genDeclSMT" lhs)
                . Ty.BoolLit
                )
            return $ either (const Nothing) Just mTy
        return $ (name, ) <$> ty
  return $ catMaybes ms
genDeclSMT _ _ = error "Missing case in genDeclSMT"

genInitSMT :: Type -> CInit -> C CSsaVal
genInitSMT ty i = case (ty, i) of
  (_, CInitExpr e _) -> do
    t <- genExprSMT e
    return $ case t of
      Base c   -> Base $ cCast ty c
      RefVal{} -> t
  (Array _ innerTy, CInitList is _) -> do
    values <- forM is $ \(_, i) -> genInitSMT innerTy i
    let cvals = map (ssaValAsTerm "Cannot put refs in arrays") values
    liftMem $ Base <$> cArrayLit innerTy cvals
  (Struct fields, CInitList is _) -> do
    values <- forM (zip fields is) $ \((_, fTy), (_, i)) -> genInitSMT fTy i
    let cvals = map (ssaValAsTerm "Cannot put refs in structs") values
    liftMem $ Base <$> cStructLit ty cvals
  _ -> error $ unwords ["Cannot initialize type", show ty, "from", show i]

---
--- High level codegen (translation unit, etc)
---

-- Returns the variable names corresponding to inputs and the return
genFunDef :: CFunDef -> Maybe InMap -> C ([String], Maybe String)
genFunDef f inVals = do
  -- Declare the function and setup the return value
  let name = nameFromFunc f
      ptrs = ptrsFromFunc f
      tys  = baseTypeFromFunc f
  retTy <- liftCircify $ unwrap <$> ctype tys ptrs
  liftCircify $ pushFunction name $ noneIfVoid retTy
  -- Declare the arguments and execute the body
  inputNamesAndTys <- join <$> forM (argsFromFunc f) (genDeclSMT (Just False))
  let inputNames = map fst inputNamesAndTys
  fullInputNames <- map ssaVarAsString
    <$> forM inputNames (liftCircify . getSsaVar . SLVar)
  case inVals of
    Just pathMap -> forM_ inputNamesAndTys $ \(n, ty) -> do
      let v = parseVar pathMap n ty
      liftLog $ logIf "inputs" $ "Input: " ++ n ++ " -> " ++ show v
      liftCircify $ setValue (SLVar n) v
    Nothing -> return ()

  let body = bodyFromFunc f
  case body of
    CCompound{} -> genStmtSMT body
    _           -> error "Expected C statement block in function definition"
  returnValue <- liftCircify popFunction
  whenM (gets findUB) $ forM_ returnValue $ \r -> bugIf $ udef r
  return (fullInputNames, fmap (fromJust . asVar) returnValue)

genAsm :: CStringLiteral a -> C ()
genAsm = undefined

registerFns :: [CExtDecl] -> C ()
registerFns decls = forM_ decls $ \case
  CFDefExt f    -> registerFunction (nameFromFunc f) f
  CDeclExt d    -> void $ genDeclSMT Nothing d
  CAsmExt asm _ -> genAsm asm

codegenAll :: CTranslUnit -> C ()
codegenAll (CTranslUnit decls _) = do
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

codegenFn :: CTranslUnit -> String -> Maybe InMap -> C ([String], Maybe String)
codegenFn (CTranslUnit decls _) name inVals = do
  when (isJust inVals) $ liftCircify initValues
  registerFns decls
  genFunDef (findFn name decls) inVals

cLangDef :: Bool -> LangDef Type CTerm
cLangDef findBugs = LangDef { declare   = cDeclVar findBugs
                            , assign    = cCondAssign findBugs
                            , setValues = cSetValues findBugs
                            , termInit  = ctermInit
                            }

runC :: Bool -> C a -> Assert.Assert (a, CircifyState Type CTerm, MemState)
runC findBugs c = do
  let (C act) = cfgFromEnv >> c
  (((x, _), circState), memState) <- runCodegen (cLangDef findBugs)
    $ runStateT act (emptyCState findBugs)
  return (x, circState, memState)

evalC :: Bool -> C a -> Assert.Assert a
evalC findBugs act = do
  (r, _, _) <- runC findBugs act
  return r

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
checkFn
  :: CTranslUnit -> String -> Cfg ([String], Maybe (Map.Map String Ty.Val))
checkFn tu name = do
  (ins, assertions) <- Assert.runAssert $ evalC True $ do
    (ins, _) <- codegenFn tu name Nothing
    assertBug
    return ins
  model <- liftIO $ Ty.evalZ3Model $ Ty.BoolNaryExpr
    Ty.And
    (Assert.asserted assertions)
  return (ins, if Map.null model then Nothing else Just model)

evalFn :: Bool -> CTranslUnit -> String -> Cfg (Map.Map String Ty.Val)
evalFn findBug tu name = do
  assertions <- Assert.execAssert $ evalC findBug $ do
    _ <- codegenFn tu name Nothing
    when findBug assertBug
  liftIO $ Ty.evalZ3Model $ Ty.BoolNaryExpr Ty.And (Assert.asserted assertions)

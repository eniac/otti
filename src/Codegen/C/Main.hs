{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
module Codegen.C.Main
  ( CInputs(..)
  , evalFn
  , checkFn
  )
where
import           Codegen.C.Type
import           Codegen.C.Term
import           Codegen.C.AstUtil
import           Codegen.Circify
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify.Memory         ( MonadMem
                                                , liftMem
                                                )
import           Codegen.FrontEnd
import           Codegen.LangVal
import           Control.Monad                  ( replicateM_
                                                , forM
                                                , join
                                                )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Char                     as Char
import qualified Data.Foldable                 as Fold
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                )
import qualified Data.Set                      as Set
import           IR.SMT.Assert                  ( MonadAssert
                                                , liftAssert
                                                )
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.TySmt.Alg              as TyAlg
import qualified Targets.SMT.Z3                as ToZ3
import qualified Targets.BackEnd               as Back
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import qualified Util.Cfg                      as Cfg
import           Util.Cfg                       ( MonadCfg(..) )
import           Util.Control
import           Util.Log


data CState = CState
  { funs          :: Map.Map FunctionName CFunDef
  , loopBound     :: Int
  , findUB        :: Bool
  , bugConditions :: [Ty.TermBool]
  , assumptions   :: [Ty.TermBool]
  , nonDetCtr     :: Int
  , breakDepth    :: Int
  }

type CCircState = CircifyState Type CTerm (Maybe InMap, Bool)

newtype C a = C (StateT CState (Circify Type CTerm (Maybe InMap, Bool)) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadIO, MonadLog, MonadAssert, MonadMem, MonadCircify Type CTerm (Maybe InMap, Bool), MonadCfg, MonadDeepState (((Assert.AssertState, Mem.MemState), CCircState), CState))

emptyCState :: Bool -> CState
emptyCState findBugs = CState { funs          = Map.empty
                              , loopBound     = 5
                              , findUB        = findBugs
                              , bugConditions = []
                              , assumptions   = []
                              , nonDetCtr     = 0
                              , breakDepth    = 0
                              }

enterBreak :: C ()
enterBreak = do
  d <- (+ 1) <$> gets breakDepth
  modify $ \s -> s { breakDepth = d }
  liftCircify $ pushBreakable $ "break" ++ show d

exitBreak :: C ()
exitBreak = do
  liftCircify popGuard
  modify $ \s -> s { breakDepth = -1 + breakDepth s }

cBreak :: C ()
cBreak = do
  d <- gets breakDepth
  liftCircify $ doBreak $ "break" ++ show d

cfgFromEnv :: C ()
cfgFromEnv = do
  bound <- Cfg.liftCfg $ asks Cfg._loopBound
  modify $ \s -> s { loopBound = bound }
  logIf "loop" $ "Setting loop bound to " ++ show bound

-- Loops

-- | Right indicates this bound is constant
getForLoopBound :: CStat -> CStat -> C (Either Int Int)
getForLoopBound loop body = do
  s <- liftCfg $ asks (Cfg._constLoops . Cfg._cCfg)
  d <- gets loopBound
  if s
    then do
      cI <- constIterations loop body
      logIf "c::const::iter" $ "constant iterations: " ++ show cI
      return $ maybe (Left d) Right cI
    else return $ Left d

constIterations :: CStat -> CStat -> C (Maybe Int)
constIterations stmt body = do
  logIf "c::const::iter" "isConst?"
  case getConstIterations stmt of
    Just (ident, nIter) -> do
      oldState <- deepGet -- Save state, all the way down to assertions
      oldV     <- liftCircify $ getVer (SLVar ident)
      genStmt body
      newV <- liftCircify $ getVer (SLVar ident)
      deepPut oldState -- Restore old state.
      return $ if oldV == newV then Just (fromInteger nIter) else Nothing
    Nothing -> return Nothing

-- Functions

registerFunction :: FunctionName -> CFunDef -> C ()
registerFunction name function = do
  s0 <- get
  case Map.lookup name $ funs s0 of
    Nothing -> put $ s0 { funs = Map.insert name function $ funs s0 }
    _       -> error $ unwords ["Already declared fn:", name]

getFunction :: FunctionName -> C CFunDef
getFunction funName = do
  functions <- gets funs
  case Map.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords ["Called undeclared function:", funName]

-- Bugs

-- Record that a bug happens if some condition is met (on this path!)
bugIf :: Ty.TermBool -> C ()
bugIf c = do
  logIf "bugIf" $ "Bug if: " ++ show c
  g <- liftCircify getGuard
  modify $ \s ->
    s { bugConditions = Ty.BoolNaryExpr Ty.And [g, c] : bugConditions s }

assume :: Ty.TermBool -> C ()
assume c = do
  g <- liftCircify getGuard
  modify
    $ \s -> s { assumptions = Ty.BoolBinExpr Ty.Implies g c : assumptions s }

-- Assert that some recorded bug happens
assertBug :: C ()
assertBug = do
  cs <- gets bugConditions
  liftAssert $ Assert.assert $ Ty.BoolNaryExpr Ty.Or cs
  as <- gets assumptions
  liftAssert $ forM_ as Assert.assert

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

genVar :: Ident -> C CSsaVal
genVar (Ident name _ _) = liftCircify $ getTerm $ SLVar name

-- https://en.cppreference.com/w/c/language/integer_constant
cIntConstant :: Integer -> Flags CIntFlag -> CTerm
cIntConstant i fs = case tys of
  ty : _ -> cIntLit ty i
  []     -> error $ show i ++ " does not fit in any int type"
 where
  l   = testFlag FlagLong fs
  ll  = testFlag FlagLongLong fs
  s   = not $ testFlag FlagUnsigned fs
  two = 2 :: Integer
  fitsIn i t =
    let n = numBits t
    in  if isSignedInt t
          then (i >= -(two ^ (n - 1))) && i < (two ^ (n - 1))
          else i >= 0 && i < (two ^ n)
  tys = filter (fitsIn i) $ map (flip makeIntTy s) $ filter
    (\size -> (not l && not ll) || size >= 64)
    [32, 64]

genConst :: CConst -> C CTerm
genConst c = case c of
  CIntConst  (CInteger i _ f) _ -> return $ cIntConstant i f
  CCharConst (CChar c _     ) _ -> return $ cIntLit S8 $ toInteger $ Char.ord c
  CCharConst CChars{}         _ -> error "Chars const unsupported"
  CFloatConst (Language.C.Syntax.Constants.CFloat str) _ ->
    case Char.toLower (last str) of
      'f' -> return $ cFloatLit (read $ init str)
      'l' -> return $ cDoubleLit (read $ init str)
      _   -> return $ cDoubleLit (read str)
  CStrConst (CString str _) _ -> do
    svExtensions <- Cfg.liftCfg $ asks (Cfg._svExtensions . Cfg._cCfg)
    let p = "__SMT_assert:"
    if svExtensions && List.isPrefixOf p str
      then do
        let t = either error id $ TyAlg.checkSortDeep
              (read (drop (length p) str) :: Ty.TermBool)
        logIf "SMT_assert" $ "User assertion: " ++ show t
        t' <- localizeVars t
        logIf "SMT_assert" $ "SMT  assertion: " ++ show t'
        whenM (gets findUB) $ bugIf $ Ty.Not t'
        return $ cIntLit U32 0
      else liftMem $ cArrayLit
        S8
        (map (cIntLit S8 . toInteger . Char.ord) str ++ [cIntLit S8 0])

-- | Given a term with user-visible variables in it, replaces them with their
-- (current version) names
localizeVars :: Ty.SortClass s => Ty.Term s -> C (Ty.Term s)
localizeVars = TyAlg.mapTermM visit
 where
  visit :: Ty.SortClass s => Ty.Term s -> C (Maybe (Ty.Term s))
  visit (Ty.Var n s) = do
    t  <- liftCircify $ ssaValAsTerm "localize" <$> getTerm (SLVar n)
    vs <- liftMem $ ctermGetVars n t
    let v = head $ Set.toList vs
    return $ Just $ Ty.Var v s
  visit _ = return Nothing

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

genLValue :: CExpr -> C CLVal
genLValue expr = case expr of
  CVar (Ident name _ _) _ -> return $ CLVar $ SLVar name
  CUnary CIndOp addr _    -> CLAddr <$> genExpr addr
  CIndex base   idx  _    -> do
    base' <- genExpr base
    idx'  <- genExpr idx
    addr  <- liftMem $ liftTermFun2M "cIndex" cIndex base' idx'
    return $ CLAddr addr
  CMember struct ident False _ ->
    flip CLField (identToVarName ident) <$> genLValue struct
  CMember struct ident True _ -> do
    s <- genExpr struct
    return $ CLField (CLAddr s) (identToVarName ident)
  _ -> error $ unwords ["Not yet impled:", show expr]

genRef :: CExpr -> C CSsaVal
genRef expr = case expr of
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

-- | Handle special functions, returning whether this function was special
genSpecialFunction :: VarName -> [CSsaVal] -> C (Maybe CSsaVal)
genSpecialFunction fnName args = do
  specifialPrintf <- Cfg.liftCfg $ asks (Cfg._printfOutput . Cfg._cCfg)
  svExtensions    <- Cfg.liftCfg $ asks (Cfg._svExtensions . Cfg._cCfg)
  bugs            <- gets findUB
  case fnName of
    "printf" | specifialPrintf -> do
      -- skip fstring
      when bugs $ forM_ (tail args) (bugIf . udef . ssaValAsTerm "printf udef")
      -- Not quite right. Should be # chars.
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_error" | svExtensions -> do
      when bugs $ bugIf (Ty.BoolLit True)
      return $ Just $ Base $ cIntLit S32 1
    "reach_error" | svExtensions -> do
      when bugs $ bugIf (Ty.BoolLit True)
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_assert" | svExtensions -> do
      if bugs
        then bugIf $ Ty.Not $ ssaBool $ head args
        else assume $ ssaBool $ head args
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_assume" | svExtensions -> do
      when bugs $ assume $ ssaBool $ head args
      return $ Just $ Base $ cIntLit S32 1
    "assume_abort_if_not" | svExtensions -> do
      when bugs $ assume $ ssaBool $ head args
      return $ Just $ Base $ cIntLit S32 1
    _ | isNonDet fnName -> do
      let ty = nonDetTy fnName
      n <- gets nonDetCtr
      modify $ \s -> s { nonDetCtr = n + 1 }
      let name = fnName ++ "_" ++ show n
      liftCircify $ declareVar True name ty
      liftCircify $ Just <$> getTerm (SLVar name)
    _ -> return Nothing
 where
  nonDetTy :: String -> Type
  nonDetTy s = case drop (length "__VERIFIER_nondet_") s of
    "char"   -> S8
    "uchar"  -> U8
    "int"    -> S32
    "uint"   -> U32
    "long"   -> S64
    "ulong"  -> U64
    "float"  -> Float
    "double" -> Double
    _        -> error $ "Unknown nondet suffix in: " ++ s
  isNonDet = List.isPrefixOf "__VERIFIER_nondet_"

genExpr :: CExpr -> C CSsaVal
genExpr expr = do
  logIfM "expr" $ do
    t <- liftIO $ nodeText expr
    return $ "Expr: " ++ t
  case expr of
    CVar id _            -> genVar id
    CConst c             -> Base <$> genConst c
    CStatExpr s _        -> genStmt s >> return (Base (cIntLit U32 1))
    CAssign op lhs rhs _ -> do
      lval <- genLValue lhs
      rval <- genExpr rhs
      genAssignOp op lval rval
    CBinary op left right _ -> case op of
      CLndOp -> do
        left'  <- genExpr left
        right' <- guarded (ssaBool left') $ genExpr right
        return $ liftTermFun2 "cAnd" cAnd left' right'
      CLorOp -> do
        left'  <- genExpr left
        right' <- guarded (Ty.Not $ ssaBool left') $ genExpr right
        return $ liftTermFun2 "cOr" cOr left' right'
      _ -> do
        left'  <- genExpr left
        right' <- genExpr right
        getBinOp op left' right'
    CUnary op   arg   _ -> getUnaryOp op arg
    CIndex base index _ -> do
      base'  <- genExpr base
      index' <- genExpr index
      offset <- liftMem $ liftTermFun2M "cIndex" cIndex base' index'
      ssaLoad offset
    CMember struct ident isArrow _ -> do
      e <- genExpr struct
      -- If this is a ->, then derefence the left first.
      s <- if isArrow then liftCircify $ getTerm (deref e) else return e
      return $ ssaStructGet (identToVarName ident) s
    CCast decl expr _ -> case decl of
      CDecl specs _ _ -> do
        ty    <- liftCircify $ unwrap <$> baseTypeFromSpecs specs
        expr' <- genExpr expr
        return $ liftTermFun "cCast" (cCast ty) expr'
      _ -> error "Expected type in cast"
    CCall fn args _ -> case fn of
      CVar fnIdent _ -> do
        let fnName = identToVarName fnIdent
        actualArgs <- traverse genExpr args
        s          <- genSpecialFunction fnName actualArgs
        case s of
          Just r  -> return r
          Nothing -> do
            f     <- getFunction fnName
            retTy <- liftCircify $ unwrap <$> fnRetTy f
            let (_, args, body) = fnInfo f
            liftCircify $ pushFunction fnName (noneIfVoid retTy)
            forM_ args (genDecl FnArg)
            formalArgs <-
              liftCircify
              $   map (\(name, _, _) -> SLVar name)
              .   join
              .   map (either error id)
              <$> forM args cSplitDeclaration
            unless (length formalArgs == length actualArgs)
              $  error
              $  "Wrong arg count: "
              ++ show expr
            liftCircify $ forM_ (zip formalArgs actualArgs) (uncurry argAssign)
            genStmt body
            returnValue <- liftCircify popFunction
            return $ Base $ fromMaybe
              (error "Getting the return value of a void fn")
              returnValue
      _ -> error $ unwords ["Fn call of", show fn, "is unsupported"]
    CCond cond mTrueBr falseBr _ -> do
      cond'  <- genExpr cond
      true'  <- maybe (return cond') genExpr mTrueBr
      false' <- genExpr falseBr
      return $ liftTermFun3 "cCond" cCond cond' true' false'
    CSizeofExpr e _ -> do
      -- Evaluate in false context, to get type, but avoid side-effects
      e' <- guarded (Ty.BoolLit False) (genExpr e)
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
    lval <- genLValue arg
    rval <- evalLVal lval
    let one = Base $ cIntLit (ssaType rval) 1
    let new = liftTermFun2 (show op) (if isDec op then cSub else cAdd) rval one
    _ <- genAssign lval new
    return $ if isPre op then new else rval
  CIndOp -> do
    l <- genExpr arg
    ssaLoad l
  CPlusOp -> error $ unwords ["Do not understand:", show op]
  CMinOp  -> liftTermFun "cNeg" cNeg <$> genExpr arg
  CCompOp -> liftTermFun "cBitNot" cBitNot <$> genExpr arg
  CNegOp  -> liftTermFun "cNot" cNot <$> genExpr arg
  CAdrOp  -> genRef arg
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

-- | Should we skip the current path?
skipPath :: C Bool
skipPath = andM (liftCfg $ asks (Cfg._smtBoundLoops . Cfg._cCfg)) -- enabled
                (liftCircify $ not <$> reachable)                 -- unreachable

genStmt :: CStat -> C ()
genStmt stmt = do
  logIfM "stmt" $ do
    t <- liftIO $ nodeText stmt
    return $ "Stmt: " ++ t
  case stmt of
    CCompound _ items _ -> scoped $ forM_ items $ \case
      CBlockStmt stmt -> genStmt stmt
      CBlockDecl decl -> void $ genDecl Local decl
      CNestedFunDef{} -> error "Nested function definitions not supported"
    CExpr e _                 -> when (isJust e) $ void $ genExpr $ fromJust e
    CIf cond trueBr falseBr _ -> do
      trueCond <- ssaBool <$> genExpr cond
      -- Guard the true branch with the true condition
      guarded trueCond $ genStmt trueBr
      -- Guard the false branch with the false condition
      forM_ falseBr $ \br -> guarded (Ty.Not trueCond) $ genStmt br
    CFor init check incr body _ -> do
      enterBreak
      case init of
        Left  (Just expr) -> void $ genExpr expr
        Right decl        -> void $ genDecl Local decl
        _                 -> return ()
      -- Make a guard on the bound to guard execution of the loop
      -- Execute up to the loop bound
      bound <- getForLoopBound stmt body
      case bound of
        Right b -> replicateM_ b $ do
          genStmt body
          forM_ incr $ \inc -> genExpr inc
        Left b -> do
          replicateM_ b $ do
            test <- genExpr $ fromMaybe (error "Missing test in for-loop") check
            liftCircify $ pushGuard (ssaBool test)
            -- TODO: could skip more
            unlessM skipPath $ do
              genStmt body
              -- increment the variable
              forM_ incr $ \inc -> genExpr inc
          -- TODO: assert end, skip dependent
          replicateM_ b (liftCircify popGuard)
      exitBreak
    CWhile check body isDoWhile _ -> do
      bound <- gets loopBound
      let addGuard = genExpr check >>= liftCircify . pushGuard . ssaBool
      enterBreak
      replicateM_ bound $ do
        unless isDoWhile addGuard
        -- TODO: could skip more
        unlessM skipPath $ genStmt body
        when isDoWhile addGuard
      -- TODO: assert end, skip dependent
      replicateM_ bound (liftCircify popGuard)
      exitBreak
    CReturn expr _ -> forM_ expr $ \e -> do
      toReturn <- genExpr e
      logIf "return" $ "Returning: " ++ show toReturn
      liftCircify $ doReturn $ ssaValAsTerm "return" toReturn
    CLabel _ inner _ _ -> genStmt inner
    CBreak _           -> cBreak
    _                  -> do
      text <- liftIO $ nodeText stmt
      error $ unlines ["Unsupported:", text]

-- Kind of declaration
data DeclType = FnArg -- ^ Argument to a called function. Internally defined.
              | Local -- ^ Local variable. Not defined if uninitialized.
              | EntryFnArg -- ^ Top level function argument. Externally defined.
              deriving (Eq)

-- | Returns the names of all declared variables, and their types
-- @isInput@: whether the declared variables are inputs to the constraint system (vs. witnesses)
genDecl :: DeclType -> CDecl -> C ()
genDecl dType d@(CDecl specs decls _) = do
  logIf "decls" "genDecl:"
  logIfM "decls" $ liftIO $ nodeText d
  -- At the top level, we ignore types we don't understand.
  skipBadTypes <- liftCircify $ gets (null . callStack)
  when (null specs) $ error "Expected specifier in declaration"
  let firstSpec = head specs
      isTypedefDecl =
        isStorageSpec firstSpec && isTypedef (storageFromSpec firstSpec)
      baseType = if isTypedefDecl then tail specs else specs

  -- Even for not declarators, process the type. It may be a struct that needs to be recorded!
  when (null decls) $ void $ liftCircify $ baseTypeFromSpecs baseType

  forM_ decls $ \(Just dec, mInit, _) -> do
    let ident   = identFromDeclr dec
        name    = identToVarName ident
        ptrType = derivedFromDeclr dec
    eTy <- liftCircify $ ctype baseType ptrType
    if isTypedefDecl
      then forM_ eTy $ liftCircify . typedef name
      else case eTy of
        Left  err -> unless skipBadTypes $ error err
        Right ty  -> case mInit of
          Just init -> do
            rhs <- genInit ty init
            liftCircify $ declareInitVar name ty rhs
          Nothing -> do
            liftCircify $ declareVar (dType == EntryFnArg) name ty
            whenM (gets findUB) $ when (dType /= FnArg) $ do
              lhs <- genVar ident
              liftAssert
                $  Assert.assert
                $  Ty.Eq (udef $ ssaValAsTerm "undef settting in genDecl" lhs)
                $  Ty.BoolLit
                $  dType
                == Local
genDecl _ _ = error "Missing case in genDecl"

genInit :: Type -> CInit -> C CSsaVal
genInit ty i = case (ty, i) of
  (_, CInitExpr e _) -> do
    t <- genExpr e
    return $ case t of
      Base c   -> Base $ cCast ty c
      RefVal{} -> t
  (Array _ innerTy, CInitList is _) -> do
    values <- forM is $ \(_, i) -> genInit innerTy i
    let cvals = map (ssaValAsTerm "Cannot put refs in arrays") values
    liftMem $ Base <$> cArrayLit innerTy cvals
  (Struct fields, CInitList is _) -> do
    values <- forM (zip fields is) $ \((_, fTy), (_, i)) -> genInit fTy i
    let cvals = map (ssaValAsTerm "Cannot put refs in structs") values
    liftMem $ Base <$> cStructLit ty cvals
  _ -> error $ unwords ["Cannot initialize type", show ty, "from", show i]

---
--- Pequin conventions
---

pequinOutStructName = "Out"
pequinOutGlobalName = "output_global"
pequinOutLocalName = "output"
pequinInStructName = "In"
pequinInGlobalName = "input_global"
pequinInLocalName = "input"

pequinSetup :: C ()
pequinSetup = liftCircify $ do
  -- Declare a global input, and create a local reference to it.
  inTy <- fromMaybe (error $ "No struct " ++ pequinInStructName)
    <$> getStruct pequinInStructName
  declareGlobal True pequinInGlobalName inTy
  inRef <- getRef (SLVar pequinInGlobalName)
  declareInitVar pequinInLocalName (Ptr32 inTy) inRef
  -- Same for output.
  outTy <- fromMaybe (error $ "No struct " ++ pequinInStructName)
    <$> getStruct pequinOutStructName
  declareGlobal False pequinOutGlobalName outTy
  outRef <- getRef (SLVar pequinOutGlobalName)
  declareInitVar pequinOutLocalName (Ptr32 outTy) outRef
  logIf "pequin" "Done with pequin setup"
  return ()

pequinTeardown :: C ()
pequinTeardown = do
  outTerm <- liftCircify $ getTerm (SLVar pequinOutGlobalName)
  logIf "pequin" "Done with pequin teardown"
  pubVars <- liftMem $ ctermGetVars pequinOutGlobalName $ ssaValAsTerm
    "pequin return"
    outTerm
  liftAssert $ forM_ (Set.toList pubVars) Assert.publicize

---
--- High level codegen (translation unit, etc)
---

-- Returns the variable names corresponding to inputs and the return
genFunDef :: CFunDef -> C ()
genFunDef f = do
  -- Declare the function and get the return type
  let (name, args, body) = fnInfo f
  retTy <- liftCircify $ unwrap <$> fnRetTy f
  liftCircify $ pushFunction name $ noneIfVoid retTy
  -- Declare the arguments and execute the body
  pequinIo <- liftCfg $ asks (Cfg._pequinIo . Cfg._cCfg)
  if pequinIo then pequinSetup else forM_ args (genDecl EntryFnArg)
  logIf "funDef" $ "Starting: " ++ name
  genStmt body
  logIf "funDef" $ "Popping: " ++ name
  returnValue <- liftCircify popFunction
  if pequinIo
    then pequinTeardown
    else do
      logIf "funDef" $ "Ret: " ++ show returnValue
      forM_ returnValue $ \rv -> do
        pubVars <- liftMem $ ctermGetVars "return" rv
        liftAssert $ forM_ (Set.toList pubVars) Assert.publicize
      svExtensions <- Cfg.liftCfg $ asks (Cfg._svExtensions . Cfg._cCfg)
      ub           <- gets findUB
      when (ub && not svExtensions) $ forM_ returnValue $ \r ->
        bugIf $ ctermIsUndef r

genAsm :: CStringLiteral a -> C ()
genAsm = undefined

registerFns :: [CExtDecl] -> C ()
registerFns decls = forM_ decls $ \case
  CFDefExt f    -> registerFunction (nameFromFunc f) f
  CDeclExt d    -> void $ genDecl Local d
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
        ++ List.intercalate ", " (Map.keys namesToFns)
        ++ "}."
        )
        (namesToFns Map.!? name)

genFn :: CTranslUnit -> String -> C ()
genFn (CTranslUnit decls _) name = do
  registerFns decls
  genFunDef (findFn name decls)

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
checkFn :: CTranslUnit -> String -> Log ToZ3.Z3Result
checkFn tu name = do
  assertState <- liftCfg $ Assert.execAssert $ compile $ CInputs tu
                                                                 name
                                                                 True
                                                                 Nothing
  Back.target assertState

evalFn :: Bool -> CTranslUnit -> String -> Log (Map.Map String ToZ3.Val)
evalFn findBug tu name = do
  -- TODO: inputs?
  assertState <- liftCfg $ Assert.execAssert $ compile $ CInputs tu
                                                                 name
                                                                 findBug
                                                                 Nothing
  let a = Fold.toList $ Assert.asserted assertState
  z3res <- ToZ3.evalZ3Model $ Ty.BoolNaryExpr Ty.And a
  return $ ToZ3.model z3res

data CInputs = CInputs CTranslUnit String Bool (Maybe InMap)

instance FrontEndInputs CInputs where
  compile (CInputs tu fnName findBugs inMap) =
    let C act = do
          cfgFromEnv
          when (isJust inMap) $ liftAssert Assert.initValues
          genFn tu fnName
          when findBugs $ do
            assertBug
            liftAssert $ modify $ \s -> s { Assert.public = Set.empty }
    in  void $ runCircify (inMap, findBugs) $ runStateT act
                                                        (emptyCState findBugs)

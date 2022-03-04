{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
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

import           Control.Monad.Fail             ( MonadFail )
import           Control.Monad                  ( replicateM_
                                                , forM
                                                , join
                                                )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Time.Clock.System         ( getSystemTime
                                                , SystemTime(..)
                                                )

import qualified Data.Char                     as Char
import qualified Data.Foldable                 as Fold
import qualified Data.List                     as List
import qualified Data.Map                      as Map
import qualified Data.BitVector                as BV
import           Data.Maybe                     ( fromJust
                                                , fromMaybe
                                                , isJust
                                                , catMaybes
                                                )
import qualified Data.Set                      as Set
import           IR.SMT.Assert                  ( MonadAssert
                                                , liftAssert
                                                )
import qualified System.Process                as Proc
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import           IR.SMT.TySmt                   ( )
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
import           Data.Tuple.Extra
import           Data.Dynamic
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Foreign.Marshal.Array
import qualified Foreign.C.Types               as FTypes
import qualified Foreign                       as F
import           Data.Typeable
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Targets.R1cs.Main              ( R1CS(..) )
import qualified Targets.R1cs.Opt.Main         as R1csOpt
import           Targets.R1cs.Output            ( r1csShow )
import           Targets.BackEnd
import qualified IR.SMT.Opt                    as SmtOpt
import qualified IR.SMT.Opt.Assert             as OptAssert
import qualified IR.SMT.ToPf                   as ToPf
import           GHC.TypeNats                   ( KnownNat )
import           Language.C.Pretty
import           Codegen.C.LP
import           Codegen.C.SGD
import           Codegen.C.SDP
import           Debug.Trace



data CState = CState
  { _funs          :: Map.Map FunctionName CFunDef
  , _loopBound     :: Int
  , _findUB        :: Bool
  , _bugConditions :: [Ty.TermBool]
  , _assumptions   :: [Ty.TermBool]
  , _nonDetCtr     :: Int
  , _breakDepth    :: Int
  } deriving Show

$(makeLenses ''CState)

type CCircState = CircifyState Type CTerm (Maybe InMap, Bool)

newtype C a = C (StateT CState (Circify Type CTerm (Maybe InMap, Bool)) a)
    deriving (Functor, Applicative, Monad, MonadState CState, MonadIO, MonadLog, MonadAssert, MonadMem, MonadFail, MonadCircify Type CTerm (Maybe InMap, Bool), MonadCfg, MonadDeepState (((Assert.AssertState, Mem.MemState), CCircState), CState))


emptyCState :: Bool -> CState
emptyCState findBugs = CState { _funs          = Map.empty
                              , _loopBound     = 5
                              , _findUB        = findBugs
                              , _bugConditions = []
                              , _assumptions   = []
                              , _nonDetCtr     = 0
                              , _breakDepth    = 0
                              }

findBugs :: C Bool
findBugs = view findUB <$> get

enterBreak :: C ()
enterBreak = do
  d <- view breakDepth <$> get
  modify $ over breakDepth (+ 1)
  liftCircify $ pushBreakable $ "break" ++ show d

exitBreak :: C ()
exitBreak = do
  liftCircify popGuard
  modify $ over breakDepth (\x -> x - 1)

cBreak :: C ()
cBreak = do
  d <- view breakDepth <$> get
  liftCircify $ doBreak $ "break" ++ show d

cfgFromEnv :: C ()
cfgFromEnv = do
  bound <- Cfg.liftCfg $ asks Cfg._loopBound
  modify $ set loopBound bound
  logIf "loop" $ "Setting loop bound to " ++ show bound

-- Loops

-- | Right indicates this bound is constant
getForLoopBound :: CStat -> CStat -> C (Either Int Int)
getForLoopBound loop body = do
  s <- liftCfg $ asks (Cfg._constLoops . Cfg._cCfg)
  d <- view loopBound <$> get
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
  fs <- view funs <$> get
  case Map.lookup name fs of
    Nothing -> modify . over funs $ Map.insert name function
    _       -> error $ unwords ["Already declared fn:", name]

getFunction :: FunctionName -> C CFunDef
getFunction funName = do
  fs <- view funs <$> get
  case Map.lookup funName fs of
    Just function -> return function
    Nothing       -> error $ unwords ["Called undeclared function:", funName]

-- Bugs

-- Record that a bug happens if some condition is met (on this path!)
bugIf :: Ty.TermBool -> C ()
bugIf c = do
  logIf "bugIf" $ "Bug if: " ++ show c
  g <- liftCircify getGuard
  modify $ over bugConditions (Ty.BoolNaryExpr Ty.And [g, c] :)

assume :: Ty.TermBool -> C ()
assume c = do
  g <- liftCircify getGuard
  modify $ over assumptions (Ty.BoolBinExpr Ty.Implies g c :)

-- Assert that some recorded bug happens
assertBug :: C ()
assertBug = do
  cs <- view bugConditions <$> get
  liftAssert $ Assert.assert $ Ty.BoolNaryExpr Ty.Or cs
  as <- view assumptions <$> get
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
    whenM findBugs $ bugIf oob
    return $ Base val
  RefVal inner -> liftCircify $ getTerm (SLRef inner)

ssaStore :: CSsaVal -> CSsaVal -> C ()
ssaStore ref val = case (ref, val) of
  (Base addr, Base cval) -> do
    g   <- liftCircify getGuard
    oob <- liftMem $ cStore addr cval g
    whenM findBugs $ bugIf oob
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
  []     -> error $ show i ++ " does not fit in any int type; tys=" ++ show
    (filter (fitsIn i) $ [S32, S64])
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
    (\size -> (not l && not ll) || size >= 64) -- JESS EDIT
    [32, 64, 96]

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
        whenM (view findUB <$> get) $ bugIf $ Ty.Not t'
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

unwrapConstInt :: CTerm -> Integer
unwrapConstInt cterm = case cterm of
  (CTerm (CInt _ _ (Ty.DynBvLit bv)) _) -> BV.int bv
  _ -> error $ "Input int variable not read correctly"

unwrapConstDouble :: CTerm -> FTypes.CDouble
unwrapConstDouble cterm = case cterm of
  (CTerm (CDouble (Ty.FpUnExpr Ty.FpNeg (Ty.Fp64Lit d))) _) ->
    realToFrac (-1 * d)
  (CTerm (CDouble (Ty.Fp64Lit d)) _) -> realToFrac d
  _ -> error $ "Input double variable not read correctly"

-- | Handle special functions, returning whether this function was special
genSpecialFunction :: VarName -> [CExpr] -> C (Maybe CSsaVal)
genSpecialFunction fnName cargs = do
  specifialPrintf <- Cfg.liftCfg $ asks (Cfg._printfOutput . Cfg._cCfg)
  svExtensions    <- Cfg.liftCfg $ asks (Cfg._svExtensions . Cfg._cCfg)
  bugs            <- view findUB <$> get
  computingVals   <- liftAssert $ Assert.isStoringValues
  case fnName of
    "printf" | specifialPrintf -> do
      -- skip fstring
      args <- traverse genExpr . tail $ cargs
      when bugs $ forM_ args (bugIf . udef . ssaValAsTerm "printf udef")
      -- Not quite right. Should be # chars.
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_error" | svExtensions -> do
      when bugs $ bugIf (Ty.BoolLit True)
      return $ Just $ Base $ cIntLit S32 1
    "reach_error" | svExtensions -> do
      when bugs $ bugIf (Ty.BoolLit True)
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_assert" | svExtensions -> do
      prop <- genExpr . head $ cargs
      if bugs then bugIf . Ty.Not . ssaBool $ prop else assume . ssaBool $ prop
      return $ Just $ Base $ cIntLit S32 1
    "__VERIFIER_assume" | svExtensions -> do
      prop <- genExpr . head $ cargs
      when bugs . assume . ssaBool $ prop
      return $ Just $ Base $ cIntLit S32 1
    "__GADGET_exist" ->
      error "Runaway intrinsic __GADGET_exist should be caught earlier"

    "__GADGET_sgd" -> if computingVals
      then do
        CConst (CIntConst dc _) <- return . head $ cargs
        CConst (CIntConst nc _) <- return . head . tail $ cargs
        let
          dataset =
            map
                (\x -> case x of
                  (CConst (CIntConst cn _)) -> show $ getCInteger cn
                  (CUnary CMinOp (CConst (CIntConst cn _)) _) ->
                    show $ -1 * getCInteger cn
                  (CConst (CFloatConst (Language.C.Syntax.Constants.CFloat fs) _))
                    -> fs
                  (CUnary CMinOp (CConst (CFloatConst (Language.C.Syntax.Constants.CFloat fs) _)) _)
                    -> "-" ++ fs
                  o -> error $ show o
                )
              . tail
              . tail
              $ cargs
        wvals <- liftLog $ sgd_train (getCInteger dc) (getCInteger nc) dataset
        forM_
          (zip [0 :: Int ..] $ traceShowId wvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "W" ++ show id) (cIntLit S64 d)
          )
        return . Just . Base $ cIntLit S32 1
      else do
        liftLog $ logIf "gadgets::user::verification"
                        "Runs SGD training in prove mode only"
        return . Just . Base $ cIntLit S32 1
    "__GADGET_lpsolve" -> if computingVals
      then do
        liftLog
          . logIf "gadgets::user::verification"
          $ "Starting external LPsolver (primal) ..."
        CConst (CStrConst cstr _) <- return . head $ cargs
        pvals                     <- liftLog $ lp_solve (getCString cstr) Primal
        dvals                     <- liftLog $ lp_solve (getCString cstr) Dual
        forM_
          (zip [0 :: Int ..] pvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "X" ++ show id) (double2fixpt d)
          )
        forM_
          (zip [0 :: Int ..] $ dvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "Y" ++ show id) (double2fixpt d)
          )
        return . Just . Base $ cIntLit S32 1
      else do
        liftLog $ logIf "gadgets::user::verification"
                        "Runs linear programming solver in prove mode only"
        return . Just . Base $ cIntLit S32 1

    "__GADGET_maximize" -> if computingVals
      then do
        liftLog
          . logIf "gadgets::user::verification"
          $ "Starting optimization solver..."
        start    <- liftIO getSystemTime
        maybeRes <- liftIO $ ToZ3.evalOptimizeZ3 True (tail cargs) (head cargs)
        let res = fromMaybe
              (error $ "Could not maximize " ++ (prettys . head $ cargs))
              maybeRes
        end <- liftIO getSystemTime
        liftLog . logIf "gadgets::user::verification" $ "Finished solving"
        let seconds =
              (fromInteger (ToZ3.tDiffNanos end start) :: Double) / 1.0e9
        z3result <- liftIO $ ToZ3.parseZ3Model res seconds
        liftLog $ logIf "gadgets::user::verification" (show z3result)
        forM_
          (Map.toList $ ToZ3.model z3result) -- Valuation
          (\case
            (id, ToZ3.DVal d) ->
              liftCircify $ setValue (SLVar id) (double2fixpt d)
          )
        return . Just . Base $ cIntLit S32 1
      else do
        liftLog $ logIf "gadgets::user::verification"
                        "Runs linear programming solver in prove mode only"
        return . Just . Base $ cIntLit S32 1
    "__GADGET_minimize" -> if computingVals
      then do
        liftLog
          . logIf "gadgets::user::verification"
          $ "Starting optimization solver..."
        start    <- liftIO getSystemTime
        maybeRes <- liftIO $ ToZ3.evalOptimizeZ3 False (tail cargs) (head cargs)
        let res = fromMaybe
              (error $ "Could not minimize " ++ (prettys . head $ cargs))
              maybeRes
        end <- liftIO getSystemTime
        let seconds =
              (fromInteger (ToZ3.tDiffNanos end start) :: Double) / 1.0e9
        z3result <- liftIO $ ToZ3.parseZ3Model res seconds
        liftLog $ logIf "gadgets::user::verification" (show z3result)
        forM_
          (Map.toList $ ToZ3.model z3result) -- Valuation
          (\case
            (id, ToZ3.DVal d) ->
              liftCircify $ setValue (SLVar id) (double2fixpt d)
          )
        return . Just . Base $ cIntLit S32 1
      else do
        liftLog $ logIf "gadgets::user::verification"
                        "Runs linear programming solver in prove mode only"
        return . Just . Base $ cIntLit S32 1
    "__GADGET_sdp" -> if computingVals
      then do
	expr_n <- genExpr $ cargs !! 0
        let n = unwrapConstInt $ ssaValAsTerm "sdp val extraction" $ expr_n

        expr_m <- genExpr $ cargs !! 1
        let m = unwrapConstInt $ ssaValAsTerm "sdp val extraction" $ expr_m

        liftLog
          . logIf "gadgets::user::verification"
          $ "Starting external SDPsolver ..."
        CConst (CStrConst cstr _) <- return $ cargs !! 2
       
	vals                     <- liftLog $ sdp_solve (getCString cstr)
        
	let xvals = take (fromIntegral $ n * n) vals
	forM_
          (zip [0 :: Int ..] xvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "X" ++ show id) (double2fixpt d)
          )

	let yvals = take (fromIntegral $ m) (drop (fromIntegral $ (n * n)) vals)
        forM_
          (zip [0 :: Int ..] $ yvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "Y" ++ show id) (double2fixpt d)
          )

	let xlvals = take (fromIntegral $ div (n * (n+1)) 2) (drop (fromIntegral $ m + (n * n)) vals)
	forM_
          (zip [0 :: Int ..] $ xlvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "XL" ++ show id) (double2fixpt d)
          )

	let slvals = take (fromIntegral $ div (n * (n+1)) 2) (drop (fromIntegral $ (div (n * (n+1))  2) + m + (n * n)) vals)
	forM_
          (zip [0 :: Int ..] $ slvals) -- Valuation
          (\case
            (id, d) ->
              liftCircify $ setValue (SLVar $ "SL" ++ show id) (double2fixpt d)
          )

        return . Just . Base $ cIntLit S32 1
      else do
        liftLog $ logIf "gadgets::user::verification"
                        "Runs semidefinite programming solver in prove mode only"
        return . Just . Base $ cIntLit S32 1


{-
do

      expr_n <- genExpr $ cargs !! 0
      let n = unwrapConstInt $ ssaValAsTerm "sdp val extraction" $ expr_n

      expr_m <- genExpr $ cargs !! 1
      let m = unwrapConstInt $ ssaValAsTerm "sdp val extraction" $ expr_m

      expr_fp <- genExpr $ cargs !! 2
      let fp = unwrapString $ ssaValAsTerm "sdp fp extraction" $ expr_fp

      error $ "Out sdp fp: " ++ fp
-}
      {-
      let expr_c = take (fromIntegral $ n * n) (drop 2 cargs)
      c <- forM expr_c $ \q -> do
        expr <- genExpr $ q
        let vars = unwrapConstDouble $ ssaValAsTerm "sdp val extraction" $ expr
        return vars

      let
        expr_x =
          take (fromIntegral $ n * n) (drop (fromIntegral $ 2 + (n * n)) cargs)
      x <- forM expr_x $ \q -> do
        expr <- genExpr $ q
        let vars = unwrapConstDouble $ ssaValAsTerm "sdp val extraction" $ expr
        return vars

      let expr_a = take (fromIntegral $ m * n * n)
                        (drop (fromIntegral $ 2 + 2 * (n * n)) cargs)
      a <- forM expr_a $ \q -> do
        expr <- genExpr $ q
        let vars = unwrapConstDouble $ ssaValAsTerm "sdp val extraction" $ expr
        return vars

      let expr_b = take
            (fromIntegral m)
            (drop (fromIntegral $ 2 + 2 * (n * n) + (m * n * n)) cargs)
      b <- forM expr_b $ \q -> do
        expr <- genExpr $ q
        let vars = unwrapConstDouble $ ssaValAsTerm "sdp val extraction" $ expr
        return vars

      -- compute SDP
      c1 <- liftIO $ cArrayAlloc c
      x1 <- liftIO $ cArrayAlloc x
      a1 <- liftIO $ cArrayAlloc a
      b1 <- liftIO $ cArrayAlloc b

      let pre = replicate (fromIntegral $ n * n + m) 0
      sol <- liftIO $ cArrayAlloc pre


      -- N, M, C, X, big array of A's, b, y
      liftIO $ sdpSolve (fromIntegral n) (fromIntegral m) c1 x1 a1 b1 sol
      sol1 <- liftIO $ cArrayPeek sol (n * n + m)

      let sol_x = take (fromIntegral $ n * n) sol1
      let sol_y = drop (fromIntegral $ n * n) sol1

      --error $ "Out EXPR CTEN " ++ show sol_x ++ "sol y " ++ show sol_y

      forM (zip sol_x [0 .. (n * n - 1)]) $ \(q, i) -> do
        liftCircify
          $ setValue (SLVar ("x" ++ show i)) (double2fixpt $ realToFrac q)
        return ()

      forM (zip sol_y [0 .. (m - 1)]) $ \(q, i) -> do
        liftCircify
          $ setValue (SLVar ("y" ++ show i)) (double2fixpt $ realToFrac q)
        return ()
      -}
    "__GADGET_compute" -> do
      -- Enter scratch state
      s    <- deepGet
      expr <- genExpr . head $ cargs
      -- Give an l-var to the gadget expression
      let exprname = "__gadget_out"
      let ctype = cType . ssaValAsTerm "gadget evaluation" $ expr
      liftCircify $ declareInitVar exprname ctype expr -- This one?
      vexpr <- liftCircify . getValue . SLVar $ exprname
      logIfPretty "gadget::user::analytics"
                  ("Gadget computed for " ++ show vexpr ++ " from expr ")
                  (head cargs)
      -- Erase scratch state
      deepPut s
      -- Add checker assertion
      liftCircify $ declareVar False exprname ctype
      -- Add witness if computing values
      forM_ vexpr $ liftCircify . setValue (SLVar exprname)

      Just <$> (liftCircify . getTerm . SLVar $ exprname)
    "__GADGET_check" -> do
      -- Parse propositional arguments see `test/Code/C/max.c`
      args <- traverse genExpr cargs
      let numbered = zip3 [1 :: Int ..] cargs args
      -- Evaluate proposition bools and check if they hold
      forM_ numbered (uncurry3 assumeGadgetAssertion)
      return . Just . Base $ cIntLit S32 1

    "assume_abort_if_not" | svExtensions -> do
      prop <- genExpr . head $ cargs
      when bugs . assume . ssaBool $ prop
      return $ Just $ Base $ cIntLit S32 1
    _ | isNonDet fnName -> do
      let ty = nonDetTy fnName
      n <- view nonDetCtr <$> get
      modify $ over nonDetCtr (+ 1)
      let name = fnName ++ "_" ++ show n
      liftCircify $ declareVar True name ty
      liftCircify $ Just <$> getTerm (SLVar name)
    _ -> return Nothing
 where
  assumeGadgetAssertion :: Int -> CExpr -> CSsaVal -> C ()
  assumeGadgetAssertion n e cv = do
    -- Assign to an l-value
    -- gadget_prop_1 = max_check(a,b,out)
    let lname = "__gadget_prop_" ++ show n
    liftCircify $ declareInitVar lname Bool cv
    -- Compute value with inputs if given, or Nothing
    cterm <- liftCircify . getValue . SLVar $ lname

    -- Generate circuitry to check at verifier runtime
    assume . ssaBool $ cv
    -- Check at compile time
    case fmap (asBool . term) cterm of
      (Just (Ty.BoolLit True)) -> do
        logIfPretty "gadgets::user::verification" "Verified assertion" e
      (Just other) -> do
        vs <- liftAssert $ Assert.printValues
        logIf "gadgets::user::verification"
              "============ Valuation ============"
        logIf "gadgets::user::verification" $ vs
        logIfPretty "gadgets::user::verification" "Failed assertion" e
        fail
          $  "Failed assertion, enable gadgets::user::verification for details"
          ++ show other
      -- Must mean inputs are not given at compile-time
      (Nothing) -> do
        logIf "gadgets::user::verification"
              "Inputs are not given, no verification performed."

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
        s <- genSpecialFunction fnName args
        case s of
          Just r  -> return r
          Nothing -> do
            f          <- getFunction fnName
            retTy      <- liftCircify $ unwrap <$> fnRetTy f
            actualArgs <- traverse genExpr args
            let (_, args, body) = fnInfo f
            liftCircify $ pushFunction fnName (noneIfVoid retTy)
            forM_ args (genDecl FnArg)
            formalArgs <-
              liftCircify
              $   map (\(name, _, _) -> SLVar name)
              .   join
              .   map (either error id)
              <$> forM args cSplitDeclaration
            unless (length formalArgs == length args)
              $  error
              $  "Wrong arg count: "
              ++ show expr
            liftCircify $ forM_ (zip formalArgs actualArgs) (uncurry hardAssign)
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
      bound <- view loopBound <$> get
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
          Just init -> case init of
            (CInitExpr (CCall (CVar fnIdent _) _ _) _)
              | identToVarName fnIdent == "__GADGET_exist" -> do
                liftCircify $ compilerExistVar name
                liftCircify $ declareVar False name ty
            (_) -> do
              rhs <- genInit ty init
              liftCircify $ declareInitVar name ty rhs
          Nothing -> do
            liftCircify $ declareVar (dType == EntryFnArg) name ty
            whenM (view findUB <$> get) $ when (dType /= FnArg) $ do
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
  existentials <- liftCircify getExistentials
  unless (List.null existentials) $ error $ unlines
    ["Could not discharge all existential values", show existentials]
  returnValue <- liftCircify popFunction
  -- Collect all existentials
  if pequinIo
    then pequinTeardown
    else do
      logIf "funDef" $ "Ret: " ++ show returnValue
      forM_ returnValue $ \rv -> do
        pubVars <- liftMem $ ctermGetVars "return" rv
        liftAssert $ forM_ (Set.toList pubVars) Assert.publicize
      svExtensions <- Cfg.liftCfg $ asks (Cfg._svExtensions . Cfg._cCfg)
      ub           <- view findUB <$> get
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

evalFn :: CTranslUnit -> String -> Maybe InMap -> Log (Map.Map String ToZ3.Val)
evalFn tu name ins = do
  assertState <- liftCfg $ Assert.execAssert $ compile $ CInputs tu
                                                                 name
                                                                 False
                                                                 ins
  -- Create an assertion from given values
  let vs = (catMaybes . fmap (uncurry mkValEq) . Map.toList)
        <$> Assert.vals assertState
  let a = Fold.toList $ Assert.asserted assertState
  z3res <- ToZ3.evalZ3Model $ Ty.BoolNaryExpr Ty.And (a ++ fromMaybe [] vs)
  return $ ToZ3.model z3res
 where
  mkValEq :: String -> Dynamic -> Maybe Ty.TermBool
  mkValEq name v = do
    term <- TyAlg.valueToTerm . Ty.ValDynBv <$> fromDyn v
    let sort = Ty.sort term
    return $ Ty.Eq (Ty.Var name sort) term
  fromDyn :: Dynamic -> Maybe BV.BV
  fromDyn v = Fold.asum
    [ (fromDynamic v :: Maybe (Ty.Value Ty.DynBvSort))
        >>= (\case
              (Ty.ValDynBv b) -> Just b
            )
    ]

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

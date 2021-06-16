{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE StandaloneDeriving     #-}
module Targets.SMT.Z3
  ( toZ3
  , Z3Result(..)
  , valToZ3
  , sortToZ3
  , evalZ3
  , evalOptimizeZ3
  , parseZ3Model
  , evalZ3Model
  , tDiffNanos
  , cToZ3
  , Val(..)
  , i_
  , b_
  , d_
  , nz
  , nan
  )
where

import           Control.Monad                  ( forM
                                                , foldM
                                                )
import           Control.Monad.IO.Class         ( liftIO )
import           Control.Monad.Reader
import qualified Data.Binary.IEEE754           as IEEE754
import           Data.Bits
import qualified Data.BitVector                as Bv
import           Data.Char                      ( digitToInt )
import qualified Data.Foldable                 as Fold
import           Data.List                      ( foldl'
                                                , isInfixOf
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Maybe                     ( catMaybes )
import           Data.Proxy                     ( Proxy(..) )
import           Data.Time.Clock.System         ( getSystemTime
                                                , SystemTime(..)
                                                )
import           GHC.TypeLits
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.Opt                    as Opt
import qualified IR.SMT.Opt.Assert             as OptAssert
import           Targets.BackEnd
import           IR.SMT.TySmt
import           Z3.Monad                       ( MonadZ3
                                                , MonadOptimize
                                                )
import qualified Z3.Monad                      as Z
import           Util.Log
import qualified Util.Cfg                      as Cfg
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import qualified Data.Text                     as Text

sortToZ3 :: forall z . MonadZ3 z => Sort -> z Z.Sort
sortToZ3 s = case s of
  SortBool        -> Z.mkBoolSort
  SortInt         -> Z.mkIntSort
  SortBv w        -> Z.mkBvSort w
  SortPf _        -> error "Prime fields"
  SortFp    11 53 -> Z.mkDoubleSort
  SortFp    8  24 -> Z.mkFloatSort
  SortFp    e  si -> error $ unwords ["Fp", show e, show si, "unsupported"]
  SortArray k  v  -> do
    k' <- sortToZ3 k
    v' <- sortToZ3 v
    Z.mkArraySort k' v'

valToZ3 :: forall s z . (SortClass s, MonadZ3 z) => Value s -> z Z.AST
valToZ3 v = case v of
  ValBool   b  -> Z.mkBool b
  ValInt    i  -> Z.mkInteger i
  ValBv     bv -> Z.mkBvNum (Bv.size bv) bv
  ValDynBv  bv -> Z.mkBvNum (Bv.size bv) bv
  ValDouble d  -> Z.mkDoubleSort >>= Z.mkFpFromDouble d
  ValFloat  d  -> Z.mkFloatSort >>= Z.mkFpFromFloat d
  ValPf{}      -> error "Prime fields not supported in z3"
  ValArray{}   -> error "Array values not supported in z3"

cToZ3 :: (MonadZ3 z) => CExpr -> z Z.AST
cToZ3 expr = case expr of
  CVar (Ident id _ _) _ -> do
    name <- Z.mkStringSymbol id
    Z.mkRealSort >>= Z.mkVar name
  CConst c -> case c of
    CIntConst (CInteger i _ _) _ -> Z.mkRealSort >>= Z.mkNumeral (show i)
    CFloatConst (Language.C.Syntax.Constants.CFloat str) _ ->
      Z.mkRealSort >>= Z.mkNumeral str
    e -> error $ "Not a real " ++ (prettys e)
  CBinary op left right _ -> case op of
    CMulOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkMul [l, r]
    CAddOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkAdd [l, r]
    CSubOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkSub [l, r]
    CLeOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkLt l r
    CGrOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkGt l r
    CLeqOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkLe l r
    CGeqOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkGe l r
    CEqOp -> do
      l <- cToZ3 left
      r <- cToZ3 right
      Z.mkEq l r
    e -> error $ "Not a linear operation " ++ (prettys e)
  CUnary op arg _ -> case op of
    CPlusOp -> cToZ3 arg
    CMinOp  -> do
      minone <- Z.mkRealSort >>= Z.mkNumeral ("-1.0")
      a      <- cToZ3 arg
      Z.mkMul [minone, a]
    e -> error $ "Not supported operation " ++ (prettys e)
  e -> error $ "Not supported operation " ++ (prettys e)

toZ3 :: forall s z . (SortClass s, MonadZ3 z) => Term s -> z Z.AST
toZ3 t = case t of
  BoolLit b               -> Z.mkBool b
  BoolBinExpr Implies l r -> tyBinZ3Bin Z.mkImplies l r
  BoolNaryExpr o a        -> case o of
    Xor -> tyNaryZ3Bin Z.mkXor a
    And -> tyNaryZ3Nary Z.mkAnd a
    Or  -> tyNaryZ3Nary Z.mkOr a
  Not s       -> toZ3 s >>= Z.mkNot

  Ite c tt ff -> tyTernZ3Tern Z.mkIte c tt ff
  Var name s' -> do
    s''   <- sortToZ3 s'
    name' <- Z.mkStringSymbol name
    Z.mkVar name' s''
  Exists{}       -> error "NYI"
  Let _x _s _t'  -> error "NYI"
  Eq       tt ff -> tyBinZ3Bin Z.mkEq tt ff

  BvConcat a  b  -> tyBinZ3Bin Z.mkConcat a b
  BvUnExpr o  b  -> toZ3 b >>= case o of
    BvNeg -> Z.mkBvneg
    BvNot -> Z.mkBvnot
  BvExtract start' t' -> bvExtract start' t t'
   where
    -- We pass the original term in to get its width, encoded in its type.
    bvExtract
      :: forall n i
       . (KnownNat n, KnownNat i, i <= n)
      => Int
      -> TermBv i
      -> TermBv n
      -> z Z.AST
    bvExtract start _term innerTerm = toZ3 innerTerm >>= Z.mkExtract start end
     where
      end     = start + newSize - 1
      newSize = fromInteger $ natVal $ Proxy @i
  BvBinExpr o l r -> tyBinZ3Bin (bvBinOpToZ3 o) l r
  BvNaryExpr o l  -> tyNaryZ3Bin (bvNaryOpToZ3 o) l
  BvBinPred o l r -> tyBinZ3Bin (bvBinPredToZ3 o $ bvWidth l) l r
  IntToBv i       -> toZ3 i >>= Z.mkInt2bv (width t)
   where
    width :: forall n . KnownNat n => TermBv n -> Int
    width _ = fromInteger $ natVal (Proxy @n)
  FpToBv tt            -> toZ3 tt >>= Z.mkFpIEEEBv

  DynamizeBv _ i       -> toZ3 i
  StatifyBv i          -> toZ3 i
  RoundFpToDynBv w s i -> do
    let w' = fromIntegral w
    i' <- toZ3 i
    rm <- Z.mkFpRoundToNearestTiesToEven
    if s then Z.mkFpToBv rm i' w' else Z.mkFpToUbv rm i' w'
  DynBvBinExpr o _ l r -> tyBinZ3Bin (bvBinOpToZ3 o) l r
  DynBvNaryExpr o _ l  -> tyNaryZ3Bin (bvNaryOpToZ3 o) l
  DynBvUnExpr   o _ b  -> toZ3 b >>= case o of
    BvNeg -> Z.mkBvneg
    BvNot -> Z.mkBvnot
  DynBvLit bv            -> Z.mkBvNum (Bv.size bv) bv
  DynBvSext   _ wDelta b -> toZ3 b >>= Z.mkSignExt wDelta
  DynBvUext   _ wDelta b -> toZ3 b >>= Z.mkZeroExt wDelta
  DynBvConcat _ l      r -> tyBinZ3Bin Z.mkConcat l r
  DynBvBinPred o w l r   -> tyBinZ3Bin (bvBinPredToZ3 o w) l r
  DynBvExtract s w i     -> toZ3 i >>= Z.mkExtract (w + s - 1) s
  DynBvExtractBit i b    -> toZ3 $ mkIte
    (mkEq (mkDynBvExtract i 1 b) (DynBvLit $ Bv.ones 1))
    (BoolLit True)
    (BoolLit False)
  IntToDynBv w i -> toZ3 i >>= Z.mkInt2bv w

  IntLit i       -> Z.mkInteger i
  IntUnExpr o t' -> case o of
    IntNeg -> toZ3 t' >>= Z.mkUnaryMinus
    IntAbs -> nyi o
  IntBinExpr o l r -> case o of
    IntDiv -> tyBinZ3Bin Z.mkDiv l r
    IntMod -> tyBinZ3Bin Z.mkMod l r
    IntSub -> tyBinZ3Nary Z.mkSub l r
    _      -> nyi o
  IntNaryExpr o as -> tyNaryZ3Nary
    (case o of
      IntAdd -> Z.mkAdd
      IntMul -> Z.mkMul
    )
    as
  IntBinPred o l r ->
    let f = case o of
          IntLt -> Z.mkLt
          IntLe -> Z.mkLe
          IntGt -> Z.mkGt
          IntGe -> Z.mkGe
    in  tyBinZ3Bin f l r
  BvToInt       tt -> toZ3 tt >>= flip Z.mkBv2int False
  SignedBvToInt tt -> toZ3 tt >>= flip Z.mkBv2int True
  BoolToInt     t' -> toZ3 $ Ite t' (IntLit 1) (IntLit 0)
  PfToInt{}        -> nyi "Prime fields"
  PfToDynBv{}      -> nyi "Prime fields"
  BoolToDynBv b -> toZ3 $ Ite b (DynBvLit $ Bv.ones 1) (DynBvLit $ Bv.zeros 1)

  Fp64Lit     d    -> Z.mkDoubleSort >>= Z.mkFpFromDouble d
  Fp32Lit     d    -> Z.mkFloatSort >>= Z.mkFpFromFloat d
  FpBinExpr o l r ->

    let wrapRound g a b = do
          m <- Z.mkFpRoundToNearestTiesToEven
          g m a b
        f :: Z.AST -> Z.AST -> z Z.AST = case o of
          FpAdd -> wrapRound Z.mkFpAdd
          FpSub -> wrapRound Z.mkFpSub
          FpMul -> wrapRound Z.mkFpMul
          FpDiv -> wrapRound Z.mkFpDiv
          FpRem -> Z.mkFpRem
          FpMax -> Z.mkFpMax
          FpMin -> Z.mkFpMin
    in  tyBinZ3Bin f l r
  FpUnExpr o l ->
    let f = case o of
          FpNeg   -> Z.mkFpNeg
          FpAbs   -> Z.mkFpAbs
          FpSqrt  -> nyi o
          FpRound -> nyi "Fp rounding"
    in  toZ3 l >>= f
  FpBinPred o l r ->
    let f = case o of
          FpLe -> Z.mkFpLeq
          FpLt -> Z.mkFpLt
          FpEq -> Z.mkFpEq
          FpGe -> Z.mkFpGeq
          FpGt -> Z.mkFpGt
    in  tyBinZ3Bin f l r

  FpUnPred o l ->
    let f = case o of
          FpIsNormal    -> nyi o
          FpIsSubnormal -> nyi o
          FpIsZero      -> Z.mkFpIsZero
          FpIsInfinite  -> Z.mkFpIsInf
          FpIsNaN       -> Z.mkFpIsNan
          FpIsNegative  -> Z.mkFpIsNeg
          FpIsPositive  -> Z.mkFpIsPos
    in  toZ3 l >>= f

  FpFma{}   -> nyi "fused multiply-add"
  IntToFp{} -> nyi "IntToFp"
  FpToFp i  -> do
    i' <- toZ3 i
    rm <- Z.mkFpRoundToNearestTiesToEven
    s  <- sortToZ3 (sort t)
    Z.mkFpToFp rm i' s
  BvToFp{}     -> nyi "BvToFp"
  DynUbvToFp i -> do
    i' <- toZ3 i
    rm <- Z.mkFpRoundToNearestTiesToEven
    s  <- sortToZ3 (sort t)
    Z.mkUBvToFp rm i' s
  DynSbvToFp i -> do
    i' <- toZ3 i
    rm <- Z.mkFpRoundToNearestTiesToEven
    s  <- sortToZ3 (sort t)
    Z.mkSBvToFp rm i' s

  PfUnExpr{}     -> nyi "Prime fields"
  PfNaryExpr{}   -> nyi "Prime fields"
  IntToPf{}      -> nyi "Prime fields"

  Select a k     -> tyBinZ3Bin Z.mkSelect a k
  Store a k v    -> tyTernZ3Tern Z.mkStore a k v
  ConstArray s v -> do
    s' <- sortToZ3 s
    v' <- toZ3 v
    Z.mkConstArray s' v'
 where
  tyNaryZ3Nary
    :: (SortClass s', MonadZ3 z) => ([Z.AST] -> z Z.AST) -> [Term s'] -> z Z.AST
  tyNaryZ3Nary f a = mapM toZ3 a >>= f
  tyBinZ3Nary
    :: (SortClass s', MonadZ3 z)
    => ([Z.AST] -> z Z.AST)
    -> Term s'
    -> Term s'
    -> z Z.AST
  tyBinZ3Nary f a b = do
    a' <- toZ3 a
    b' <- toZ3 b
    f [a', b']
  tyNaryZ3Bin
    :: (SortClass s', MonadZ3 z)
    => (Z.AST -> Z.AST -> z Z.AST)
    -> [Term s']
    -> z Z.AST
  tyNaryZ3Bin f a = do
    a' <- mapM toZ3 a
    foldM f (head a') (tail a')
  tyBinZ3Bin
    :: (SortClass s', SortClass s'', MonadZ3 z)
    => (Z.AST -> Z.AST -> z Z.AST)
    -> Term s'
    -> Term s''
    -> z Z.AST
  tyBinZ3Bin f a b = do
    a' <- toZ3 a
    b' <- toZ3 b
    f a' b'
  tyTernZ3Tern
    :: (SortClass s', SortClass s'', SortClass s''', MonadZ3 z)
    => (Z.AST -> Z.AST -> Z.AST -> z Z.AST)
    -> Term s'
    -> Term s''
    -> Term s'''
    -> z Z.AST
  tyTernZ3Tern f a b c = do
    a' <- toZ3 a
    tyBinZ3Bin (f a') b c
  nyi x = error $ unwords ["Not yet implemented in toZ3:", show x]
  bvBinOpToZ3 o = case o of
    BvShl  -> Z.mkBvshl
    BvLshr -> Z.mkBvlshr
    BvAshr -> Z.mkBvashr
    BvUrem -> Z.mkBvurem
    BvUdiv -> Z.mkBvudiv
    BvSub  -> Z.mkBvsub
  bvNaryOpToZ3 o = case o of
    BvAdd -> Z.mkBvadd
    BvMul -> Z.mkBvmul
    BvOr  -> Z.mkBvor
    BvAnd -> Z.mkBvand
    BvXor -> Z.mkBvxor
  bvBinPredToZ3 o w = case o of
    BvUgt   -> Z.mkBvugt
    BvUlt   -> Z.mkBvult
    BvUge   -> Z.mkBvuge
    BvUle   -> Z.mkBvule
    BvSgt   -> Z.mkBvsgt
    BvSlt   -> Z.mkBvslt
    BvSge   -> Z.mkBvsge
    BvSle   -> Z.mkBvsle
    -- TODO: underflow?
    BvSaddo -> \a b -> do
      x <- Z.mkBvaddNoOverflow a b True
      y <- Z.mkBvaddNoUnderflow a b
      Z.mkAnd [x, y] >>= Z.mkNot
    BvSsubo -> \a b -> do
      x <- Z.mkBvsubNoOverflow a b
      y <- Z.mkBvsubNoUnderflow a b
      Z.mkAnd [x, y] >>= Z.mkNot
    BvSmulo -> \a b -> do
      let w' = 2 * w
      a'        <- Z.mkSignExt w a
      b'        <- Z.mkSignExt w b
      p         <- Z.mkBvmul a' b'
      maxP      <- Z.mkBvNum w' ((2 :: Integer) ^ (w - 1) - 1)
      minP      <- Z.mkBvNum w' (negate $ (2 :: Integer) ^ (w - 1))
      overflow  <- Z.mkBvsgt p maxP
      underflow <- Z.mkBvslt p minP
      -- NB: This seems like it really should work, but Z3 gives the wrong
      -- result, claiming that (-7) * (-1) overflows in 4 signed bits.
      -- z <- Z.mkBvNum w (0 :: Int)
      -- negA <- Z.mkBvslt a z
      -- negB <- Z.mkBvslt b z
      -- negRes <- Z.mkXor negA negB
      -- posRes <- Z.mkNot negRes
      -- x <- Z.mkBvmulNoOverflow a b True >>= Z.mkNot
      -- overflow <- Z.mkAnd [posRes, x]
      -- y <- Z.mkBvmulNoUnderflow a b >>= Z.mkNot
      -- underflow <- Z.mkAnd[negRes, y]
      Z.mkOr [overflow, underflow]

-- Returns Nothing if UNSAT, or a string description of the model.
evalZ3 :: TermBool -> IO (Maybe String)
evalZ3 term = Z.evalZ3 $ do
  assertion <- toZ3 term
  Z.assert assertion
  m <- Z.getModel
  case snd m of
    Just model -> do
      s <- Z.modelToString model
      return $ Just s
    Nothing -> return Nothing

evalOptimizeZ3 :: Bool -> [CExpr] -> CExpr -> IO (Maybe String)
evalOptimizeZ3 maximize cs obj = Z.evalZ3 $ do
  constraints <- forM cs cToZ3
  objective   <- cToZ3 obj
  m           <- optimizeZ3 maximize constraints objective
  case m of
    Just model -> do
      s <- Z.modelToString model
      return $ Just s
    Nothing -> return Nothing
 where
  optimizeZ3
    :: (MonadOptimize z3) => Bool -> [Z.AST] -> Z.AST -> z3 (Maybe Z.Model)
  optimizeZ3 maximize constraints objective = do
    _ <- Z.getOptimize
    forM_ constraints Z.optimizeAssert
    _ <- if maximize
      then Z.optimizeMaximize objective
      else Z.optimizeMinimize objective
    res   <- Z.optimizeCheck []
    model <- Z.optimizeGetModel
    case res of
      Z.Sat   -> return $ Just model
      Z.Unsat -> return Nothing
      Z.Undef -> error "Optimizer returned undef."

-- For generating a numerical description of the model
data Val = IVal Int
         | BVal Bool
         | DVal Double
         | NegZ
         | NaN
         deriving (Eq, Ord)

i_ :: Int -> Val
i_ = IVal

b_ :: Bool -> Val
b_ = BVal

d_ :: Double -> Val
d_ = DVal

nz :: Val
nz = NegZ

nan :: Val
nan = NaN

instance Show Val where
  show (IVal i) = show i
  show (BVal b) = show b
  show (DVal d) = show d
  show NegZ     = "-0"
  show NaN      = "NaN"

data Z3Result = Z3Result
  { time  :: Double
  , sat   :: Bool
  , model :: Map String Val
  } deriving (Eq, Show)

readBin :: String -> Int
readBin = foldr
  (\d a -> if d `elem` "01"
    then digitToInt d + 2 * a
    else error $ "invalid binary character: " ++ [d]
  )
  0

toDec :: String -> Integer
toDec = foldl' (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0

-- | Returns Nothing if UNSAT, or an association between variables and string if SAT
parseZ3Model :: String -> Double -> IO Z3Result
parseZ3Model str time = do
  let modelLines = splitOn "\n" . filterExtraNewlines $ str
  vs <- forM (init modelLines) $ \line -> return $ case splitOn " -> " line of
    [var, "true" ] -> Just (var, BVal True)
    [var, "false"] -> Just (var, BVal False)
    [var, strVal ] -> parseVal var strVal
    _              -> Nothing
  return Z3Result { time  = time
                  , sat   = True
                  , model = Map.fromList $ catMaybes vs
                  }
 where
  filterExtraNewlines str =
    Text.unpack
      . Text.replace (Text.pack "\n ") (Text.pack "")
      . Text.replace (Text.pack "\n\t") (Text.pack "")
      . Text.pack
      $ str

  parseVal var strVal = case strVal of
    _ : '_' : ' ' : '-' : 'z' : 'e' : 'r' : 'o' : _ -> Just (var, NegZ)
    _ : '_' : ' ' : '+' : 'z' : 'e' : 'r' : 'o' : _ -> Just (var, DVal 0)
    _ : '_' : ' ' : 'N' : 'a' : 'N' : _ -> Just (var, NaN)
     -- Binary
    _ : 'b' : n -> Just (var, IVal $ readBin n)
     -- Hex
    _ : 'x' : _ -> Just (var, IVal (read ('0' : drop 1 strVal) :: Int))
    -- Non-special floating point
    _ : 'f' : 'p' : ' ' : rest ->
      let components = words rest
          sign       = read (drop 2 $ components !! 0) :: Integer
          exp        = toDec $ drop 2 $ components !! 1
          sig        = read ('0' : drop 1 (init $ components !! 2)) :: Integer
          result =
              (sig .&. 0xfffffffffffff)
                .|. ((exp .&. 0x7ff) `shiftL` 52)
                .|. ((sign .&. 0x1) `shiftL` 63)
      in  Just (var, DVal $ IEEE754.wordToDouble $ fromIntegral result)
    -- Real
    _ : '-' : ' ' : rest -> case parseVal var (init rest) of
      (Just (_, DVal d)) -> Just (var, DVal $ (-1 * d))
      (Just (_, IVal i)) -> Just (var, IVal $ (-1 * i))
      (_               ) -> Nothing
    _ : '/' : ' ' : rest -> case words . init $ rest of
      ([a, b]) -> Just (var, DVal $ (read a :: Double) / (read b :: Double))
      _        -> error $ unwords ["Bad division", show strVal]
    _ | '.' == (last . init $ strVal) ->
      Just (var, DVal (read strVal :: Double))
    -- Array, skip.
    _ | "as const" `isInfixOf` (drop 1 strVal) -> Nothing
    -- Did not recognize the pattern
    _ -> error $ unwords ["Bad strVal in model", show strVal]

-- | Returns Nothing if UNSAT, or an association between variables and string if SAT
evalZ3Model :: TermBool -> Log Z3Result
evalZ3Model term = do
  -- We have to do this because the bindings are broken.
  -- Eventually we will just fix the bindings
  -- liftIO $ putStrLn $ "Term: " ++ show (length $ show term)
  start    <- liftIO getSystemTime
  modelstr <- liftIO $ ((length $ show term) `seq` evalZ3 term)
  end      <- liftIO getSystemTime
  let seconds = (fromInteger (tDiffNanos end start) :: Double) / 1.0e9
  (m, sat') <- case modelstr of
    Nothing  -> return (Map.empty, False)
    Just str -> (, True) <$> do
      logIf "z3::model" $ "Model: " ++ str
      liftIO . fmap model $ parseZ3Model str seconds
  return Z3Result { time = seconds, sat = sat', model = m }

tDiffNanos :: SystemTime -> SystemTime -> Integer
tDiffNanos a b =
  let sDiff = toInteger (systemSeconds a) - toInteger (systemSeconds b)
      nDiff = toInteger (systemNanoseconds a) - toInteger (systemNanoseconds b)
  in  sDiff * ((10 :: Integer) ^ (9 :: Integer)) + nDiff

instance BackEnd Z3Result where
  target assertState = do
    doOpt <- Cfg.liftCfg $ asks (Cfg._optForZ3 . Cfg._smtOptCfg)
    a'    <- if doOpt
      then OptAssert.listAssertions <$> Opt.opt assertState
      else return $ Fold.toList $ Assert.asserted assertState
    evalZ3Model $ BoolNaryExpr And a'

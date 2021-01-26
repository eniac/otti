{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module IR.SMT.ToPf
  ( toPf
  )
where

import           IR.SMT.TySmt
import           IR.SMT.Util
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Control.Monad                  ( join )
import           Control.Applicative
import           GHC.TypeNats
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LC
                                                , QEQ
                                                )
import qualified Codegen.Circom.CompTypes.LowDeg
                                               as LD
import qualified Targets.R1cs.Main             as R1cs
import           Targets.R1cs.Main              ( R1CS
                                                , emptyR1cs
                                                , r1csAddConstraint
                                                , r1csStats
                                                , r1csSetSignalVal
                                                , r1csInitSigVals
                                                , r1csEnsureSignal
                                                , r1csAddSignals
                                                , r1csPublicizeSignal
                                                , r1csIsPublicSignal
                                                , qeqShow
                                                , primeShow
                                                )
import qualified Util.AliasMap                 as AMap
import           Util.AliasMap                  ( AliasMap )
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import qualified Data.Bits                     as Bits
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , fromDyn
                                                , fromDynamic
                                                )
import           Data.Field.Galois              ( Prime
                                                , toP
                                                , fromP
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromMaybe
                                                , fromJust
                                                )
import qualified Data.Map.Strict               as Map
import qualified Data.HashSet                  as HSet
import           Data.List                      ( foldl'
                                                , transpose
                                                )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Sequence                 as Seq
import qualified Data.Set                      as Set
import           Data.Typeable                  ( cast )
import           Util.Cfg                       ( MonadCfg(..)
                                                , _toPfCfg
                                                , _assumeNoOverflow
                                                , _assumeInputsInRange
                                                , _optEqs
                                                )
import           Util.Log
import           Util.Show

type PfVar = String
type SmtVals = Map.Map String Dynamic

type LSig n = (LC PfVar (Prime n), Maybe (Prime n))
type ArraySizes = ShowMap (TermArray DynBvSort DynBvSort) Int

data ToPfConfig = ToPfConfig
  { assumeNoBvOverflow  :: Bool
  , optEq               :: Bool
  , assumeInputsInRange :: Bool
  }
  deriving Show

data ToPfState n = ToPfState
  { r1cs       :: R1CS PfVar n
  , bools      :: AliasMap TermBool (LSig n)
  , ints       :: AliasMap TermDynBv (BvEntry n)
  , pfs        :: AliasMap (TermPf n) (LSig n)
  , next       :: Int
  , cfg        :: ToPfConfig
  , arraySizes :: ArraySizes
  -- Variables that are assert to be equal to terms.
  -- These need to be checked to be in range, ever.
  , aliasVars  :: Set.Set String
  }


newtype ToPf n a = ToPf (StateT (ToPfState n) Log a)
    deriving (Functor, Applicative, Monad, MonadState (ToPfState n), MonadIO, MonadLog, MonadCfg)

emptyState :: ToPfState n
emptyState = ToPfState
  { r1cs       = emptyR1cs
  , bools      = AMap.empty
  , ints       = AMap.empty
  , pfs        = AMap.empty
  , next       = 0
  , cfg        = ToPfConfig { assumeNoBvOverflow  = False
                            , optEq               = False
                            , assumeInputsInRange = True
                            }
  , arraySizes = SMap.empty
  , aliasVars  = Set.empty
  }

configureFromEnv :: ToPf n ()
configureFromEnv = do
  a <- liftCfg $ asks (_assumeNoOverflow . _toPfCfg)
  b <- liftCfg $ asks (_optEqs . _toPfCfg)
  c <- liftCfg $ asks (_assumeInputsInRange . _toPfCfg)
  modify $ \s -> s
    { cfg = (cfg s) { assumeNoBvOverflow  = a
                    , optEq               = b
                    , assumeInputsInRange = c
                    }
    }
  logIfM "toPf::cfg" $ do
    c <- gets cfg
    return $ "Cfg: " ++ pShow c

-- # Constraints

enforce :: KnownNat n => QEQ PfVar (Prime n) -> ToPf n ()
enforce qeq = do
  ensureVarsQeq qeq
  modify (\s -> s { r1cs = r1csAddConstraint qeq $ r1cs s })
  logIf "toPfCon" $ qeqShow qeq

enforceCheck :: KnownNat n => (LSig n, LSig n, LSig n) -> ToPf n ()
enforceCheck ((a, av), (b, bv), (c, cv)) = do
  case (av, bv, cv) of
    (Just x, Just y, Just z) -> if x * y == z
      then return ()
      else
        error
        $  unwords
        $  ["The QEQ", qeqShow (a, b, c), "is not satisfied"]
        ++ map primeShow [x, y, z]
    _ -> return ()
  enforce (a, b, c)

enforceTrue :: KnownNat n => LSig n -> ToPf n ()
enforceTrue s = enforceCheck (lcZero, lcZero, lcSub s lcOne)

-- # Variables

nextVar :: KnownNat n => String -> Maybe (Prime n) -> ToPf n (LSig n)
nextVar name value = do
  i <- gets next
  let var = name ++ "_v" ++ show i
  modify $ \s -> s { next = 1 + next s }
  asVar var value

asVar :: KnownNat n => String -> Maybe (Prime n) -> ToPf n (LSig n)
asVar var value = do
  modify $ \s -> s { r1cs = r1csEnsureSignal var $ r1cs s }
  case value of
    Just v -> do
      logIf "toPfVal" $ var ++ " -> " ++ primeShow v
      modify $ \s -> s { r1cs = r1csSetSignalVal var v $ r1cs s }
    _ -> return ()
  return (LD.lcSig var, value)

ensureVarsQeq :: KnownNat n => QEQ PfVar (Prime n) -> ToPf n ()
ensureVarsQeq (a, b, c) = forM_ [a, b, c] ensureVarsLc
 where
  ensureVarsLc :: KnownNat n => LC PfVar (Prime n) -> ToPf n ()
  ensureVarsLc (m, _) =
    modify $ \s -> s { r1cs = foldr r1csEnsureSignal (r1cs s) (Map.keys m) }

-- # Bit constraints and storage

enforceBit :: KnownNat n => LSig n -> ToPf n ()
enforceBit x = enforceCheck (x, lcNot x, lcZero)

asBit :: KnownNat n => LSig n -> ToPf n (LSig n)
asBit l = enforceBit l >> return l

nextBit :: KnownNat n => String -> Maybe Bool -> ToPf n (LSig n)
nextBit name value = nextVar name (bToPf <$> value) >>= asBit

unhandled :: Show a => String -> a -> b
unhandled description thing =
  error $ unwords ["Unhandled", description, ":", show thing]

saveBool :: KnownNat n => TermBool -> LSig n -> ToPf n ()
saveBool b x = modify (\s -> s { bools = AMap.insert b x $ bools s })

lookupBool :: KnownNat n => TermBool -> ToPf n (Maybe (LSig n))
lookupBool b = gets $ AMap.lookup b . bools

lcConst :: KnownNat n => Integer -> LSig n
lcConst c = lcShift (toP c) lcZero

lcOne :: KnownNat n => LSig n
lcOne = lcConst 1

lcAdd :: KnownNat n => LSig n -> LSig n -> LSig n
lcAdd (a, av) (b, bv) = (LD.lcAdd a b, liftA2 (+) av bv)

lcSum :: KnownNat n => [LSig n] -> LSig n
lcSum = foldl' lcAdd lcZero

lcMul :: KnownNat n => String -> LSig n -> LSig n -> ToPf n (LSig n)
lcMul name (a, av) (b, bv) = do
  prod <- nextVar name $ liftA2 (*) av bv
  enforceCheck ((a, av), (b, bv), prod)
  return prod

lcRecip :: KnownNat n => String -> LSig n -> ToPf n (LSig n)
lcRecip name (a, av) = do
  inv <- nextVar name $ fmap recip av
  enforceCheck ((a, av), inv, lcOne)
  return inv

lcZero :: KnownNat n => LSig n
lcZero = ((Map.empty, toP 0), Just $ toP 0)

lcScale :: KnownNat n => Prime n -> LSig n -> LSig n
lcScale c (x, v) = (R1cs.lcScale c x, (* c) <$> v)

lcShift :: KnownNat n => Prime n -> LSig n -> LSig n
lcShift c (x, v) = (R1cs.lcShift c x, (+ c) <$> v)

lcNeg :: KnownNat n => LSig n -> LSig n
lcNeg = lcScale (toP $ negate 1)

lcSub :: KnownNat n => LSig n -> LSig n -> LSig n
lcSub x y = lcAdd x $ lcNeg y

lcNot :: KnownNat n => LSig n -> LSig n
lcNot = lcSub lcOne


pfToPf :: forall n . KnownNat n => Maybe SmtVals -> TermPf n -> ToPf n (LSig n)
pfToPf env term = do
  entry <- gets (AMap.lookup term . pfs)
  case entry of
    Just s  -> return s
    Nothing -> do
      p <- pfToPfUncached term
      modify $ \s -> s { pfs = AMap.insert term p $ pfs s }
      return p
 where
  -- recurse
  rec = pfToPf env
  lookupPfVal :: KnownNat n => String -> Maybe (Prime n)
  lookupPfVal name =
    toP
      .   valAsPf @n
      .   flip fromDyn (error $ name ++ " has wrong type")
      <$> (env >>= (Map.!? name))
  -- Uncached
  pfToPfUncached :: KnownNat n => TermPf n -> ToPf n (LSig n)
  pfToPfUncached t = do
    case t of
      Var        name    _  -> asVar name (lookupPfVal name)
      PfUnExpr   PfNeg   i  -> lcNeg <$> rec i
      PfUnExpr   PfRecip i  -> rec i >>= lcRecip "recip"
      PfNaryExpr PfAdd   is -> lcSum <$> mapM rec is
      IntToPf (IntLit i)    -> return $ lcConst i
      Ite c a b             -> do
        c' <- boolToPf env c
        a' <- rec a
        b' <- rec b
        ite c' a' b'
      PfNaryExpr PfMul is -> do
        is' <- mapM rec is
        if null is
          then return lcOne
          else foldM (lcMul "pfMul") (head is') (tail is')
      _ -> error $ "Unlowerable prime-field term: " ++ show t

boolToPf
  :: forall n . KnownNat n => Maybe SmtVals -> TermBool -> ToPf n (LSig n)
boolToPf env term = do
  entry <- lookupBool term
  case entry of
    Just s  -> return s
    Nothing -> do
      s <- boolToPfUncached term
      saveBool term s
      return s
 where
  lookupBitVal :: String -> Maybe (Prime n)
  lookupBitVal name =
    bToPf
      .   valAsBool
      .   flip fromDyn (error $ name ++ " has wrong type")
      <$> (env >>= (Map.!? name))
  -- Uncached
  boolToPfUncached :: KnownNat n => TermBool -> ToPf n (LSig n)
  boolToPfUncached t = do
    r1cs'                <- gets r1cs
    assumeInputsInRange' <- gets (assumeInputsInRange . cfg)
    aliasVars'           <- gets aliasVars
    let omitRangeCheck input =
          (assumeInputsInRange' && R1cs.r1csIsPublicSignal input r1cs')
            || Set.member input aliasVars'
    case t of
      Eq a b -> case cast a of
        -- Bool
        Just abool -> do
          b' <- boolToPf env $ fromJust $ cast b
          a' <- boolToPf env abool
          bitEq a' b'
        -- Bv
        Nothing -> case cast a of
          Just abv -> do
            let bbv = fromJust $ cast b
            b' <- bvToPf env bbv >> getInt bbv
            a' <- bvToPf env abv >> getInt abv
            binEq a' b'
          Nothing -> case cast a of
            Just apf -> do
              let bpf = fromJust $ cast b
              b' <- pfToPf env bpf
              a' <- pfToPf env apf
              binEq a' b'
            Nothing -> error $ "Cannot lower " ++ show a
      BoolLit b  -> return $ lcShift (bToPf b) lcZero
      Not     a  -> lcNot <$> boolToPf env a
      Var name _ -> do
        v <- asVar name (lookupBitVal name)
        if omitRangeCheck name then return v else asBit v
      BoolNaryExpr o xs -> do
        xs' <- traverse (boolToPf env) xs
        case xs' of
          []  -> pure $ lcShift (bToPf $ opId o) lcZero
          [a] -> pure a
          _   -> case o of
            Or  -> naryOr xs'
            And -> naryAnd xs'
            Xor -> naryXor xs'
      BoolBinExpr Implies a b -> do
        a' <- boolToPf env a
        b' <- boolToPf env b
        impl a' b'
      Ite c t_ f -> do
        c' <- boolToPf env c
        t' <- boolToPf env t_
        f' <- boolToPf env f
        ite c' t' f'
      DynBvExtractBit i b -> do
        bvToPf env b
        head . drop i <$> getIntBits b
      DynBvBinPred p w l r -> bvPredToPf env p w l r
      _                    -> unhandled "in boolToPf" t

opId :: BoolNaryOp -> Bool
opId o = case o of
  And -> True
  Or  -> False
  Xor -> False

naryAnd :: KnownNat n => [LSig n] -> ToPf n (LSig n)
naryAnd xs | null xs        = error "naryAnd of no bits"
           | length xs <= 3 = foldM binAnd (head xs) (tail xs)
           | otherwise      = lcNot <$> naryOr (map lcNot xs)

binAnd :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binAnd = lcMul "and"

naryOr :: KnownNat n => [LSig n] -> ToPf n (LSig n)
naryOr xs = if length xs <= 3
  then lcNot <$> naryAnd (map lcNot xs)
  else let s = foldl1 lcAdd xs in lcNot <$> eqZero s

impl :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
impl a b = do
  -- Could implement this with lcMul, but then the new variable would be the
  -- negation of the result.
  v <- nextVar
    "impl"
    (liftA2 (\hyp conc -> toP 1 - hyp * (toP 1 - conc)) (snd a) (snd b))
  enforceCheck (a, lcNot b, lcNot v)
  return v

bitEq :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
bitEq a b = lcNot <$> binXor a b

eqZero :: KnownNat n => LSig n -> ToPf n (LSig n)
eqZero x = do
  -- m (a - b) - (1 - e) = 0
  -- e (a - b) = 0
  e <- nextVar "eqZero" $ fmap (bToPf . (== 0)) (snd x)
  m <- nextVar "eqZeroInv" $ fmap (\x -> if x == 0 then 1 else recip x) (snd x)
  enforceCheck (m, x, lcSub lcOne e)
  enforceCheck (e, x, lcZero)
  return e

binEq :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binEq a b = eqZero (lcSub a b)

-- Strategy: we add the bits, and decompose the sum. The LSB is the answer.
naryXor :: KnownNat n => [LSig n] -> ToPf n (LSig n)
naryXor xs = if length xs > 3
  then do
    let n = bitsize $ length xs
    let s = foldr lcAdd lcZero xs
    bs <- bitify "xorSum" s n
    -- Could trim a constraint here?
    case bs of
      h : _ -> return h
      []    -> error "naryXor of no bits"
  else foldM binXor (head xs) (tail xs)

binXor :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binXor a b = do
  logIf "toPf" "xor"
  lcSub (lcAdd a b) <$> lcMul "binxor" a b

bitsize :: Int -> Int
bitsize x = if x == 0 then 0 else 1 + bitsize (x `div` 2)

-- # Arith constraints and storage

-- The integer entry holds a (signal, width) pair
-- The bits list has low order bits at low indices
data BvEntry n = BvEntry
  { int  :: Maybe (LSig n, Int)
  , bits :: Maybe [LSig n]
  }
  deriving Show

bvEntryEmpty :: BvEntry n
bvEntryEmpty = BvEntry { int = Nothing, bits = Nothing }


-- Initialize an empty entry
initIntEntry :: TermDynBv -> ToPf n ()
initIntEntry t =
  modify $ \s -> s { ints = AMap.insertWith (const id) t bvEntryEmpty $ ints s }

-- Convert Bool to 0 or 1
bToPf :: KnownNat n => Bool -> Prime n
bToPf = toP . toInteger . fromEnum

-- Saving transalations
saveConstBv :: KnownNat n => TermDynBv -> Bv.BV -> ToPf n ()
saveConstBv term bv = do
  initIntEntry term
  modify $ \s -> s
    { ints =
      AMap.adjust
          (\e -> e
            { int  = Just (lcConst $ Bv.uint bv, Bv.size bv)
            , bits =
              Just $ map (lcConst . toInteger . fromEnum) $ reverse $ Bv.toBits
                bv
            }
          )
          term
        $ ints s
    }

saveInt :: KnownNat n => TermDynBv -> (LSig n, Int) -> ToPf n ()
saveInt term sig = do
  logIf "toPf::saveInt" $ "Saving int: " ++ show term ++ " -> " ++ show sig
  initIntEntry term
  modify
    (\s -> s { ints = AMap.adjust (\e -> e { int = Just sig }) term $ ints s })

saveIntBits :: KnownNat n => TermDynBv -> [LSig n] -> ToPf n ()
saveIntBits term bits_ = do
  logIf "toPf::saveInt" $ "Saving bits: " ++ show term ++ " -> " ++ show bits_
  initIntEntry term
  unless (length bits_ == dynBvWidth term)
    $ error "width mismatch in saveIntBits"
  modify
    (\s ->
      s { ints = AMap.adjust (\e -> e { bits = Just bits_ }) term $ ints s }
    )

-- Fetching transalations
getInt :: KnownNat n => TermDynBv -> ToPf n (LSig n)
getInt term = fromMaybe (error $ "No int for " ++ show term) <$> getIntM term

getSignedInt :: KnownNat n => TermDynBv -> ToPf n (LSig n)
getSignedInt term = deBitify True <$> getIntBits term

getIntM :: KnownNat n => TermDynBv -> ToPf n (Maybe (LSig n))
getIntM term = do
  e <- gets (AMap.lookup term . ints)
  r <- case e >>= int of
    Just i  -> return $ Just $ fst i
    Nothing -> case e >>= bits of
      Just bs -> do
        let i = deBitify False bs
        saveInt term (i, length bs)
        return $ Just i
      Nothing -> return Nothing
  logIf "toPf::getInt" $ unwords ["Int lookup:", show term, "->", show r]
  return r

getIntBits :: KnownNat n => TermDynBv -> ToPf n [LSig n]
getIntBits term = fromMaybe (error $ "No bits for " ++ show term) <$> do
  e <- gets (AMap.lookup term . ints)
  r <- case e >>= bits of
    Just bs -> return $ Just bs
    Nothing -> case e >>= int of
      Just (i, width) -> do
        bs <- bitify ("getBits " ++ show term) i width
        saveIntBits term bs
        return $ Just bs
      Nothing -> return Nothing
  logIf "toPf::getBits" $ unwords ["Bit lookup:", show term, "->", show r]
  return r

twoPow :: KnownNat n => Integer -> Prime n
twoPow = toP . (2 ^)

nbits :: KnownNat n => String -> [Maybe Bool] -> ToPf n [LSig n]
nbits ctx vs =
  forM (zip [0 :: Int ..] vs) (\(i, b) -> nextBit (ctx ++ "_bit" ++ show i) b)

-- A variant of fromP which returns negative integers for field elements in the
-- upper half of the field
fromPNeg :: forall n . KnownNat n => Prime n -> Integer
fromPNeg i =
  let i' = fromP i
      o  = toInteger $ natVal $ Proxy @n
      ho = o `div` 2
  in  if i' < ho then i' else i' - o

decomp :: KnownNat n => Int -> Maybe (Prime n) -> [Maybe Bool]
decomp width i = case i of
  Just i' ->
    let bv = Bv.bitVec width (2 ^ width + fromPNeg i')
    in  map (Just . Bits.testBit bv) [0 .. (width - 1)]
  Nothing -> replicate width Nothing

lazyInt :: KnownNat n => ToPf n Bool
lazyInt = gets (assumeNoBvOverflow . cfg)

-- Require `x` to fit in `width` unsigned bits
-- The 0th index contains the LSB
bitify :: KnownNat n => String -> LSig n -> Int -> ToPf n [LSig n]
bitify ctx x width = do
  logIf "toPf" $ "bitify " ++ show width ++ ": " ++ ctx
  sigs <- nbits ctx $ decomp width $ snd x
  let sum' = foldr lcAdd lcZero $ zipWith lcScale (map twoPow [0 ..]) sigs
  enforceCheck (lcZero, lcZero, lcSub sum' x)
  return sigs

-- Does `number` fit in `w` `signed` bits?
inBits :: KnownNat n => Bool -> Int -> LSig n -> ToPf n (LSig n)
inBits signed w number = do
  logIf "toPf" "inBits"
  bs <- nbits "inBits" $ decomp w $ snd number
  binEq number $ deBitify signed bs

-- | Return a signal equivalent to c ? t : f.
-- Implemented by introducing fresh variables:
--  * i = c * t
--  * j = (1 - c) * f
--  return i + j
ite :: KnownNat n => LSig n -> LSig n -> LSig n -> ToPf n (LSig n)
ite c t f = do
  r <- nextVar "ite" $ liftA3 (?) (snd c) (snd t) (snd f)
  enforceCheck (lcSub t f, c, lcSub r f)
  return r
  where (?) x y z = if (x /= 0) then y else z

deBitify :: KnownNat n => Bool -> [LSig n] -> LSig n
deBitify signed bs =
  let lowBits =
          foldr lcAdd lcZero $ zipWith lcScale (map twoPow [0 ..]) (init bs)
      highBitPos = lcScale (toP $ 2 ^ (length bs - 1)) $ last bs
      highBit    = if signed then lcNeg highBitPos else highBitPos
  in  lcAdd lowBits highBit

data BvOpKind = Division | Arith | Shift

bvToPf :: forall n . KnownNat n => Maybe SmtVals -> TermDynBv -> ToPf n ()
bvToPf env term = do
  entry <- getIntM term
  s     <- get
  when (isNothing entry) $ do
    logIf "toPf::cache" $ "Cache " ++ show (ints s)
    logIf "toPf" $ "Cache miss " ++ show term
    bvToPfUncached term
  unless (isNothing entry) $ logIf "toPf" $ "Cache hit " ++ show term
 where
  unhandledOp :: Show a => a -> b
  unhandledOp = unhandled "bv operator in bvToPf"
  bvOpKind :: BvBinOp -> BvOpKind
  bvOpKind o = case o of
    BvSub  -> Arith
    BvUrem -> Division
    BvUdiv -> Division
    BvShl  -> Shift
    BvLshr -> Shift
    BvAshr -> Shift
  bvNaryNeedsBits :: BvNaryOp -> Bool
  bvNaryNeedsBits o = case o of
    BvAdd -> False
    BvMul -> False
    BvOr  -> True
    BvAnd -> True
    BvXor -> True

  lookupIntVal :: String -> Maybe (Prime n)
  lookupIntVal name =
    bvValToPf
      .   flip fromDyn (error $ name ++ " has wrong type")
      .   fromMaybe (error $ "No value for " ++ name)
      .   (Map.!? name)
      <$> env
  bvValToPf = toP . Bv.nat . valAsDynBv

  -- Uncached
  bvToPfUncached :: KnownNat n => TermDynBv -> ToPf n ()
  bvToPfUncached bv = do
    r1cs'                <- gets r1cs
    assumeInputsInRange' <- gets (assumeInputsInRange . cfg)
    aliasVars'           <- gets aliasVars
    let omitRangeCheck input =
          (assumeInputsInRange' && R1cs.r1csIsPublicSignal input r1cs')
            || Set.member input aliasVars'
    case bv of
      IntToDynBv w (IntLit i) -> saveConstBv bv (Bv.bitVec w i)
      DynBvLit l              -> saveConstBv bv l
      Var name (SortBv w)     -> do
        i <- asVar name $ lookupIntVal name
        saveInt bv (i, w)
        unless (omitRangeCheck name) $ do
          bs <- bitify name i w
          saveIntBits bv bs
      Ite c t_ f -> do
        c' <- boolToPf env c
        bvToPf env t_
        t' <- getInt t_
        bvToPf env f
        f' <- getInt f
        v  <- ite c' t' f'
        saveInt bv (v, dynBvWidth t_)
      DynBvUnExpr BvNeg w x -> do
        bvToPf env x
        x' <- getInt x
        saveInt bv (lcSub (lcConst $ 2 ^ w) x', w)
      DynBvUnExpr BvNot _ x -> do
        bvToPf env x
        x' <- getIntBits x
        saveIntBits bv $ map lcNot x'
      DynBvSext _ deltaW i -> do
        bvToPf env i
        i' <- getIntBits i
        unless (length i' > 0) $ error $ "Sext of no bits " ++ show bv
        saveIntBits bv $ i' ++ replicate deltaW (last i')
      DynBvUext w _ i -> do
        bvToPf env i
        i' <- getInt i
        saveInt bv (i', w)
      DynBvExtract start w i -> do
        bvToPf env i
        i' <- getIntBits i
        saveIntBits bv $ take w (drop start i')
      DynBvConcat _ high low -> do
        bvToPf env high
        bvToPf env low
        high' <- getIntBits high
        low'  <- getIntBits low
        saveIntBits bv $ low' ++ high'
      PfToDynBv w p -> do
        case cast p of
          Just pn -> do
            p' <- pfToPf @n env pn
            saveInt bv (p', w)
          Nothing -> error $ "Bad modulus PfToDynBv: " ++ show bv
      BoolToDynBv b -> do
        b' <- boolToPf env b
        saveIntBits bv [b']
      DynBvBinExpr op w l r -> do
        bvToPf env l
        bvToPf env r
        case bvOpKind op of
          Division -> do
            d <- getInt l
            m <- getInt r
            isZero <- binEq m lcZero
            let dv = Bv.bitVec w . fromP <$> snd d
                mv = Bv.bitVec w . fromP <$> snd m
            q  <- nextVar "div_q" $ toP . Bv.nat <$> liftA2 smtDiv dv mv
            r' <- nextVar "div_r" $ toP . Bv.nat <$> liftA2 smtRem dv mv
            prod <- lcMul "div_qr" m q
            divEqHolds <- binEq prod (lcSub d r')
            enforceCheck (lcNot divEqHolds, lcNot isZero, lcZero)
            _ <- bitify "quotient" q w
            _ <- bitify "remainder" r' w
            isGt <- lcGt w m r'
            enforceCheck (lcNot isGt, lcNot isZero, lcZero)
            resIntVal <- case op of
              BvUdiv -> ite isZero (lcConst $ (2 :: Integer) ^ w - 1) q
              BvUrem -> ite isZero d r'
              _      -> unhandledOp op
            saveInt  bv (resIntVal, w)
          Arith -> do
            l'        <- getInt l
            r'        <- getInt r
            (res, w') <- case op of
              BvSub ->
                pure (lcShift (twoPow $ fromIntegral w) $ lcSub l' r', w + 1)
              _ -> unhandledOp op
            lazy <- lazyInt
            if lazy
              then saveInt bv (res, w)
              else do
                bs <- bitify ("arith" ++ show op) res w'
                saveIntBits bv (take w bs)
          Shift -> do
            rightInt  <- getInt r
            rightBits <- getIntBits r
            let b = bitsize $ w - 1
            unless (2 ^ b == w) $ error $ unwords
              ["width", show w, "is not a power of 2: bitsize is", show b]
            let rightBits' = take b rightBits
            -- Fits in log w bits
            enforceCheck
              (lcZero, lcZero, lcSub (deBitify False rightBits') rightInt)
            -- Shift `x` left by (2 ^ `n`) if bit `b` is true
            -- Done by computing
            --   s = (2 ^ (2 ^ n) - 1) b + 1
            --   output = s * x
            let optPowerShift x (n :: Integer, bit) =
                  let label m = m ++ show n
                      s = lcAdd lcOne $ lcScale (twoPow (2 ^ n) - toP 1) bit
                  in  lcMul (label "shift") s x
            -- Shift `leftInt` left by `rightInt`, above.
            -- If `lowBit` is not Nothing, extend it over the shift
            let shiftInt lowBit leftInt = do
                  let shiftByR v =
                        foldM optPowerShift v (zip [0 ..] rightBits')
                  shifted        <- shiftByR leftInt
                  shiftedWithExt <- case lowBit of
                    Just lowBit' -> do
                      -- The ones to add in
                      extensionPart <- lcAdd (lcNeg lcOne) <$> shiftByR lowBit'
                      -- Adding them in, if appropriate
                      lcAdd shifted
                        <$> lcMul "shiftExtMask" extensionPart lowBit'
                    Nothing -> return shifted
                  resultBits <- bitify "shift" shiftedWithExt (2 * w - 1)
                  return $ take w resultBits
            bs <- case op of
              BvShl  -> getInt l >>= shiftInt Nothing
              BvLshr -> do
                l' <- getIntBits l
                reverse <$> shiftInt Nothing (deBitify False $ reverse l')
              BvAshr -> do
                l' <- getIntBits l
                reverse
                  <$> shiftInt (Just $ last l') (deBitify False $ reverse l')
              _ -> unhandledOp op
            saveIntBits bv bs
      -- Bit-vector ITE, used in SHA
      DynBvNaryExpr o _ [DynBvNaryExpr BvAnd _ [a, b], DynBvNaryExpr BvAnd _ [DynBvUnExpr BvNot _ c, d]]
        | (o == BvOr || o == BvXor) && a == c
        -> do
          logIf "toPf::sha" $ "bit-vector ite: " ++ show bv
          bvToPf env a
          bvToPf env b
          bvToPf env d
          a'      <- getIntBits a
          b'      <- getIntBits b
          d'      <- getIntBits d
          iteBits <- sequence $ zipWith3 ite a' b' d'
          saveIntBits bv iteBits
      DynBvNaryExpr o _ [DynBvNaryExpr BvAnd _ l1@[_, _], DynBvNaryExpr BvAnd _ l2@[_, _], DynBvNaryExpr BvAnd _ l3@[_, _]]
        | let s1 = HSet.fromList l1
              s2 = HSet.fromList l2
              s3 = HSet.fromList l3
          in  (o == BvXor || o == BvOr)
                && (HSet.size s1 == 2)
                && (HSet.size s2 == 2)
                && (HSet.size s3 == 2)
                && (s1 /= s2)
                && (s2 /= s3)
                && (s1 /= s3)
                && (HSet.size (HSet.fromList $ l1 ++ l2 ++ l3) == 3)
        -> do
          logIf "toPf::sha" $ "bit-vector majority: " ++ show bv
          let es = HSet.toList $ HSet.fromList $ l1 ++ l2 ++ l3
          mapM_ (bvToPf env) es
          l <- mapM getIntBits es
          case l of
            [a, b, c] -> do
              bs <- sequence $ zipWith3 maj a b c
              saveIntBits bv bs
             where
              majF x y z = bToPf $ fromP (x + y + z) >= 2
              maj x y z = do
                 -- Derivation:
                 -- r = xy | xz | yz
                 -- r = xy + xz + yz - 2xyz
                 -- r = x(y + z - 2yz) + yz
                 -- r = x(y + z - 2m) + m    // define m = yz
                 -- Two constraints:
                 -- yz = r
                 -- x(y + z - 2m) = r - m
                r <- nextVar "maj" $ liftA3 majF (snd x) (snd y) (snd z)
                m <- lcMul "maj_mul" y z
                enforceCheck (x, lcSub (lcAdd y z) (lcScale 2 m), lcSub r m)
                return r
            _ -> error
              "Unreachable error in bvToPf: pattern guard should guarantee"
      DynBvNaryExpr op w ls -> do
        forM_ ls $ bvToPf env
        if bvNaryNeedsBits op
          then do
            ls' <- mapM getIntBits ls
            let ls'' = transpose ls'
            bs <- case op of
              BvOr  -> mapM naryOr ls''
              BvAnd -> mapM naryAnd ls''
              BvXor -> mapM naryXor ls''
              _     -> unhandledOp op
            saveIntBits bv bs
          else do
            let l2 :: Int -> Int =
                  ceiling . logBase (2.0 :: Double) . fromIntegral
            ls'       <- mapM getInt ls
            (res, w') <- case op of
              BvAdd -> pure (foldl' lcAdd lcZero ls', w + l2 (length ls'))
              BvMul -> if length ls' * w < 240
                then (, length ls' * w)
                  <$> foldM (lcMul "mul") (head ls') (tail ls')
                else (, w) <$> foldM (binMul w) (head ls') (tail ls')
              _ -> unhandledOp op
            lazy <- lazyInt
            if lazy
              then saveInt bv (res, w)
              else do
                bs <- bitify ("arith" ++ show op) res w'
                saveIntBits bv (take w bs)
      _ -> error $ unwords ["Cannot translate", show bv]

binMul :: KnownNat n => Int -> LSig n -> LSig n -> ToPf n (LSig n)
binMul w a b = do
  p <- lcMul "mul" a b
  deBitify False . take w <$> bitify "binMul" p (2 * w)

-- Embed this (dynamic) bit-vector predicate in the constraint system,
-- returning a signal which is 1 if the predicate is satisfied, and 0
-- otherwise.
bvPredToPf
  :: KnownNat n
  => Maybe SmtVals
  -> BvBinPred
  -> Int
  -> TermDynBv
  -> TermDynBv
  -> ToPf n (LSig n)
bvPredToPf env predicate width l r = do
  bvToPf env l
  bvToPf env r
  case predicate of
    BvUgt -> greater width False True l r
    BvUlt -> greater width False True r l
    BvUge -> greater width False False l r
    BvUle -> greater width False False r l
    BvSgt -> greater width True True l r
    BvSlt -> greater width True True r l
    BvSge -> greater width True False l r
    BvSle -> greater width True False r l
    _     -> do
      l' <- getSignedInt l
      r' <- getSignedInt r
      case predicate of
        -- TODO: Better way? E.g. check overflow bits or something?
        BvSaddo -> lcNot <$> inBits True width (lcAdd l' r')
        BvSsubo -> lcNot <$> inBits True width (lcSub l' r')
        BvSmulo -> lcNot <$> (inBits True width =<< lcMul "BvSmulo" l' r')
        _       -> error "unreachable"

-- Is `a` greater than `b`, for varying width, signedness, and strictness.
greater
  :: KnownNat n
  => Int
  -> Bool
  -> Bool
  -> TermDynBv
  -> TermDynBv
  -> ToPf n (LSig n)
greater width signed strict a b =
  let valueGetter = if signed then getSignedInt else getInt
      shifter     = if strict then flip lcSub lcOne else id
  in  join $ liftM2 (lcGt width) (shifter <$> valueGetter a) (valueGetter b)

-- Is `x` greater than `y`, assuming both fit in `w` bits?
lcGt :: KnownNat n => Int -> LSig n -> LSig n -> ToPf n (LSig n)
lcGt width x y = inBits False width (lcSub x y)

-- If this term is an alias, translate it, returning true.
-- o.w. do not translate it, return false
handleAlias :: KnownNat n => Maybe SmtVals -> TermBool -> ToPf n Bool
handleAlias env a = case a of
  Eq v@(Var name _s) t -> do
    modify $ \s -> s { aliasVars = Set.insert name $ aliasVars s }
    s <- get
    if r1csIsPublicSignal name (r1cs s)
      then return False
      else case cast v of
        Just boolV -> if AMap.memberOrAlias boolV (bools s)
          then return False
          else do
            let rBool = fromMaybe (error $ "Not bool: " ++ show t) $ cast t
            _ <- boolToPf env rBool
            logIf "toPf" $ "Alias " ++ show boolV ++ " to " ++ show rBool
            modify $ \st -> st { bools = AMap.alias boolV rBool $ bools st }
            return True
        Nothing -> case cast v of
          Just intV -> if AMap.memberOrAlias intV (ints s)
            then return False
            else do
              let rInt = fromMaybe (error $ "Not int: " ++ show t) $ cast t
              bvToPf env rInt
              logIf "toPf" $ "Alias " ++ show intV ++ " to " ++ show rInt
              modify $ \st -> st { ints = AMap.alias intV rInt $ ints st }
              return True
          Nothing -> case cast v of
            Just pfV -> if AMap.memberOrAlias pfV (pfs s)
              then return False
              else do
                let rPf = fromMaybe (error $ "Not pf: " ++ show t) $ cast t
                _ <- pfToPf env rPf
                logIf "toPf" $ "Alias " ++ show pfV ++ " to " ++ show rPf
                modify $ \st -> st { pfs = AMap.alias pfV rPf $ pfs st }
                return True
            Nothing -> error "Bad alias type"
  _ -> return False

-- # Top Level

enforceAsPf :: KnownNat n => Maybe SmtVals -> TermBool -> ToPf n ()
enforceAsPf env b = do
  n <- gets $ Seq.length . R1cs.constraints . r1cs
  logIf "toPf" $ "enforce: " ++ show b
  doOpt    <- gets (optEq . cfg)
  wasAlias <- if doOpt then handleAlias env b else return False
  logIf "toPf" $ "wasAlias: " ++ show wasAlias
  unless wasAlias $ boolToPf env b >>= enforceTrue
  n' <- gets $ Seq.length . R1cs.constraints . r1cs
  r  <- gets $ r1csStats . r1cs
  logIf "toPf" $ "New constraints: " ++ show (n' - n)
  logIf "toPf" $ "Net: " ++ r

publicizeInputs :: Set.Set PfVar -> ToPf n ()
publicizeInputs is = do
  modify $ \s -> s { r1cs = r1csAddSignals (Set.toList is) $ r1cs s }
  forM_ is $ \i -> modify $ \s -> s { r1cs = r1csPublicizeSignal i $ r1cs s }

-- | Ensure that the provided @inputs@ have an R1CS value assigned to
-- them, consistent with the SMT value found in @values@. A no-op if
-- @values@ is [Nothing].
ensureInputValues
  :: forall n . KnownNat n => Maybe SmtVals -> Set.Set PfVar -> ToPf n ()
ensureInputValues values inputs = forM_ values $ \values ->
  forM_ inputs $ \input -> do
    let value = case Map.lookup input values of
          Nothing   -> error $ "Missing value for input: " ++ show input
          Just dval -> case fromDynamic dval of
            Just b  -> toInteger $ fromEnum $ valAsBool b
            Nothing -> case fromDynamic dval of
              Just b  -> Bv.uint $ valAsDynBv b
              Nothing -> case fromDynamic dval of
                Just b  -> valAsPf @n b
                Nothing -> error $ "Bad input type: " ++ show dval
    let v = toP value
    logIf "toPfVal" $ input ++ " -> " ++ primeShow v
    modify $ \s -> s { r1cs = r1csSetSignalVal input v $ r1cs s }

toPf
  :: KnownNat n
  => Maybe SmtVals
  -> Set.Set PfVar
  -> ArraySizes
  -> [TermBool]
  -> Log (R1CS PfVar n)
toPf env inputs arraySizes' bs = do
  let ToPf a = do
        forM_ env $ \_ -> modify $ \s -> s { r1cs = r1csInitSigVals $ r1cs s }
        configureFromEnv
        publicizeInputs inputs
        forM_ bs (enforceAsPf env)
        -- After enforcement, we ensure the inputs have values
        -- because if an input is missing from the constraints
        -- it will not yet have an assigned value.
        ensureInputValues env inputs
  r <- r1cs <$> execStateT a emptyState { arraySizes = arraySizes' }
  logIf "toPf" "Done with toPf"
  return r

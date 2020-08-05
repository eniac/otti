{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
module IR.SMT.ToPf
  ( toPf
  , toPfWithWit
  )
where

import           IR.SMT.TySmt
import           Control.Monad.State.Strict
import           Control.Monad                  ( )
import           Control.Applicative
import           GHC.TypeNats
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LC
                                                , QEQ
                                                )
import qualified Codegen.Circom.CompTypes.LowDeg
                                               as LD
import           IR.R1cs                        ( R1CS
                                                , emptyR1cs
                                                , r1csAddConstraint
                                                , r1csEnsureSignal
                                                , r1csAddSignals
                                                , r1csPublicizeSignal
                                                , qeqShow
                                                , primeShow
                                                )
import qualified IR.R1cs                       as R1cs
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import qualified Data.Bits                     as Bits
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , fromDyn
                                                )
import           Data.Field.Galois              ( Prime
                                                , toP
                                                , fromP
                                                )
import           Data.Maybe                     ( isNothing
                                                , fromJust
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Set                      as Set
import           Data.Typeable                  ( cast )
import           Debug.Trace

type PfVar = String
type SmtVals = Map.Map String Dynamic
type PfVals n = Map.Map PfVar (Prime n)

type LSig n = (LC PfVar (Prime n), Maybe (Prime n))


data ToPfState n = ToPfState { r1cs :: R1CS PfVar n
                             , bools :: ShowMap TermBool (LSig n)
                             , ints :: ShowMap TermDynBv (BvEntry n)
                             , vals :: PfVals n
                             , next :: Int
                             }

newtype ToPf n a = ToPf (StateT (ToPfState n) IO a)
    deriving (Functor, Applicative, Monad, MonadState (ToPfState n), MonadIO)

emptyState :: ToPfState n
emptyState = ToPfState { r1cs  = emptyR1cs
                       , bools = SMap.empty
                       , ints  = SMap.empty
                       , vals  = Map.empty
                       , next  = 0
                       }

-- # Constraints

enforce :: KnownNat n => QEQ PfVar (Prime n) -> ToPf n ()
enforce qeq = ensureVarsQeq qeq
  >> modify (\s -> s { r1cs = r1csAddConstraint qeq $ r1cs s })

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

enforceNonzero :: KnownNat n => LSig n -> ToPf n ()
enforceNonzero l = do
  inv <- nextVar "inv" (recip <$> snd l)
  enforceCheck (l, inv, lcOne)

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
  case value of
    Just v -> modify $ \s -> s { vals = Map.insert var v $ vals s }
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
enforceBit (x, _) = enforce (x, LD.lcShift (negate 1) x, LD.lcZero)

asBit :: KnownNat n => LSig n -> ToPf n (LSig n)
asBit l = enforceBit l >> return l

nextBit :: KnownNat n => String -> Maybe Bool -> ToPf n (LSig n)
nextBit name value =
  nextVar name (toP . toInteger . fromEnum <$> value) >>= asBit

unhandled :: Show a => String -> a -> b
unhandled description thing =
  error $ unwords ["Unhandled", description, ":", show thing]

saveBool :: KnownNat n => TermBool -> LSig n -> ToPf n ()
saveBool b x = modify (\s -> s { bools = SMap.insert b x $ bools s })

lookupBool :: KnownNat n => TermBool -> ToPf n (Maybe (LSig n))
lookupBool b = gets (SMap.lookup b . bools)

lcConst :: KnownNat n => Integer -> LSig n
lcConst c = lcShift (toP c) lcZero

lcOne :: KnownNat n => LSig n
lcOne = lcConst 1

lcAdd :: KnownNat n => LSig n -> LSig n -> LSig n
lcAdd (a, av) (b, bv) = (LD.lcAdd a b, liftA2 (+) av bv)

lcMul :: KnownNat n => String -> LSig n -> LSig n -> ToPf n (LSig n)
lcMul name (a, av) (b, bv) = do
  prod <- nextVar name $ liftA2 (*) av bv
  enforceCheck ((a, av), (b, bv), prod)
  return prod

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
    toP
      .   toInteger
      .   fromEnum
      .   valAsBool
      .   flip fromDyn (error $ name ++ " has wrong type")
      <$> (env >>= (Map.!? name))
  -- Uncached
  boolToPfUncached :: KnownNat n => TermBool -> ToPf n (LSig n)
  boolToPfUncached t = case t of
    Eq a b -> case cast a of
      -- Bool
      Just abool -> do
        a' <- boolToPf env abool
        b' <- boolToPf env $ fromJust $ cast b
        bitEq a' b'
      -- Bv
      Nothing -> do
        let abv = fromJust $ cast a
            bbv = fromJust $ cast b
        a' <- bvToPf env abv >> (fst . fromJust <$> getInt abv)
        b' <- bvToPf env bbv >> (fst . fromJust <$> getInt bbv)
        binEq a' b'
    BoolLit b -> return $ lcShift (toP $ fromIntegral $ fromEnum b) lcZero
    Not     a            -> lcNot <$> boolToPf env a
    Var          name _  -> asBit =<< asVar name (lookupBitVal name)
    BoolNaryExpr o    xs -> do
      xs' <- traverse (boolToPf env) xs
      case xs' of
        []  -> pure $ lcShift (toP $ fromIntegral $ fromEnum $ opId o) lcZero
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
      v  <- nextVar "ite" $ liftA3 (?) ((/= toP 1) <$> snd c') (snd t') (snd f')
      enforceCheck (c', lcSub v t', lcZero)
      enforceCheck (lcNot c', lcSub v f', lcZero)
      return v
    DynBvBinPred p w l r -> bvPredToPf env p w l r
    _                    -> unhandled "in boolToPf" t

(?) :: Bool -> a -> a -> a
(?) c t f = if c then t else f

opId :: BoolNaryOp -> Bool
opId o = case o of
  And -> True
  Or  -> False
  Xor -> False

naryAnd :: KnownNat n => [LSig n] -> ToPf n (LSig n)
naryAnd xs = if length xs <= 3
  then foldM binAnd (head xs) (tail xs)
  else lcNot <$> naryOr (map lcNot xs)

binAnd :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binAnd = lcMul "and"

naryOr :: KnownNat n => [LSig n] -> ToPf n (LSig n)
naryOr xs = if length xs <= 3
  then lcNot <$> naryAnd (map lcNot xs)
  else
    let s = foldl1 lcAdd xs
    in  do
          or' <- nextBit "or" ((/= toP 0) <$> snd s)
          enforceCheck (s, lcSub lcOne or', lcZero)
          enforceNonzero $ lcSub (lcAdd lcOne s) or'
          return or'

binOr :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binOr a b = naryOr [a, b]

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
bitEq a b = do
  eq <- nextBit "bitEq" $ liftA2 (==) (snd a) (snd b)
  let onesPlace = lcNot eq
  let twosPlace = lcSub (lcAdd a b) onesPlace
  enforceCheck (twosPlace, lcSub twosPlace (lcConst 2), lcZero)
  return eq

binEq :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binEq a b = do
  v <- nextBit "binEq" $ liftA2 (==) (snd a) (snd b)
  enforceCheck (lcSub a b, v, lcZero)
  enforceNonzero $ lcAdd (lcSub a b) v
  return v

-- Strategy: we add the bits, and decompose the sum. The LSB is the answer.
naryXor :: KnownNat n => [LSig n] -> ToPf n (LSig n)
naryXor xs = do
  let n = bitsize $ length xs
  let s = foldr1 lcAdd xs
  bs <- bitify "xorSum" s n
  -- Could trim a constraint here?
  return (head bs)

binXor :: KnownNat n => LSig n -> LSig n -> ToPf n (LSig n)
binXor a b = naryXor [a, b]

bitsize :: Int -> Int
bitsize x = if x == 0 then 0 else 1 + bitsize (x `div` 2)

-- # Arith constraints and storage

-- The integer entry holds a (signal, width) pair
data BvEntry n = BvEntry { int :: Maybe (LSig n, Int)
                         , bits :: Maybe [LSig n]
                         }

bvEntryEmpty :: BvEntry n
bvEntryEmpty = BvEntry { int = Nothing, bits = Nothing }


-- Initialize an empty entry
initIntEntry :: TermDynBv -> ToPf n ()
initIntEntry t =
  modify $ \s -> s { ints = SMap.insertWith (const id) t bvEntryEmpty $ ints s }


saveInt :: TermDynBv -> (LSig n, Int) -> ToPf n ()
saveInt term sig = initIntEntry term >> modify
  (\s -> s { ints = SMap.adjust (\e -> e { int = Just sig }) term $ ints s })

saveIntBits :: TermDynBv -> [LSig n] -> ToPf n ()
saveIntBits term bits_ = initIntEntry term >> modify
  (\s -> s { ints = SMap.adjust (\e -> e { bits = Just bits_ }) term $ ints s })

getInt :: KnownNat n => TermDynBv -> ToPf n (Maybe (LSig n, Int))
getInt term = do
  e <- gets (SMap.lookup term . ints)
  case e >>= int of
    Just i  -> return $ Just i
    Nothing -> case e >>= bits of
      Just bs -> do
        let i = deBitify bs
        saveInt term (i, length bs)
        return $ Just (i, length bs)
      Nothing -> return Nothing

getIntBits :: KnownNat n => TermDynBv -> ToPf n (Maybe [LSig n])
getIntBits term = do
  e <- gets (SMap.lookup term . ints)
  case e >>= bits of
    Just bs -> return $ Just bs
    Nothing -> case e >>= int of
      Just (i, width) -> do
        bs <- bitify "get" i width
        saveIntBits term bs
        return $ Just bs
      Nothing -> return Nothing

twoPow :: KnownNat n => Integer -> Prime n
twoPow = toP . (2 ^)

nbits :: KnownNat n => String -> [Maybe Bool] -> ToPf n [LSig n]
nbits ctx vs =
  forM (zip [0 :: Int ..] vs) (\(i, b) -> nextBit (ctx ++ "_bit" ++ show i) b)

fitsInBits :: forall n . KnownNat n => Bool -> Int -> Prime n -> Bool
fitsInBits signed w' p =
  let i       = fromPNeg p
      w       = toInteger w'
      shifted = if signed then i + 2 ^ (w - 1) else i
  in  0 <= shifted && shifted < 2 ^ w

-- A variant of fromP which returns negative integers for field elements in the
-- upper half of the field
fromPNeg :: forall n . KnownNat n => Prime n -> Integer
fromPNeg i =
  let i' = fromP i
      o  = toInteger $ natVal $ Proxy @n
      ho = o `div` 2
  in  if i' < ho then i' else i' - o

asBits :: KnownNat n => Int -> Maybe (Prime n) -> [Maybe Bool]
asBits width i = case i of
  Just i' ->
    let bv = Bv.bitVec width (2 ^ width + fromPNeg i')
    in  map (Just . Bits.testBit bv) [0 .. (width - 1)]
  Nothing -> replicate width Nothing


bitify :: KnownNat n => String -> LSig n -> Int -> ToPf n [LSig n]
bitify ctx x width = do
  sigs <- nbits ctx $ asBits width $ snd x
  let sum' = foldr1 lcAdd $ zipWith lcScale (map twoPow [0 ..]) sigs
  enforceCheck (lcZero, lcZero, lcSub sum' x)
  return sigs

deBitify :: KnownNat n => [LSig n] -> LSig n
deBitify = foldr1 lcAdd . zipWith lcScale (map twoPow [0 ..])

deBitifySigned :: KnownNat n => [LSig n] -> LSig n
deBitifySigned bs =
  lcAdd (lcScale (negate $ twoPow $ fromIntegral $ length bs - 1) (last bs))
    $ foldr1 lcAdd
    $ zipWith lcScale (map twoPow [0 ..]) (init bs)

data BvOpKind = Arith | Bit | Shift

bvToPf :: forall n . KnownNat n => Maybe SmtVals -> TermDynBv -> ToPf n ()
bvToPf env term = do
  entry <- getInt term
  when (isNothing entry) $ bvToPfUncached term
 where
  unhandledOp = unhandled "bv operator in bvToPf"
  bvOpKind :: BvBinOp -> BvOpKind
  bvOpKind o = case o of
    BvAdd  -> Arith
    BvMul  -> Arith
    BvSub  -> Arith
    BvOr   -> Bit
    BvAnd  -> Bit
    BvXor  -> Bit
    BvShl  -> Shift
    BvLshr -> Shift
    BvAshr -> Shift
    _      -> unhandledOp o

  lookupIntVal :: String -> Maybe (Prime n)
  lookupIntVal name =
    toP
      .   toInteger
      .   fromEnum
      .   Bv.nat
      .   valAsDynBv
      .   flip fromDyn (error $ name ++ " has wrong type")
      <$> (env >>= (Map.!? name))

  -- Uncached
  bvToPfUncached :: KnownNat n => TermDynBv -> ToPf n ()
  bvToPfUncached bv = case bv of
    IntToDynBv w    (IntLit i) -> saveInt bv (lcShift (toP i) lcZero, w)
    Var        name (SortBv w) -> do
      i  <- asVar name $ lookupIntVal name
      bs <- bitify name i w
      saveIntBits bv bs
      saveInt bv (i, w)
    DynBvBinExpr op w l r -> do
      bvToPf env l
      bvToPf env r
      case bvOpKind op of
        Arith -> do
          l'           <- fst . fromJust <$> getInt l
          r'           <- fst . fromJust <$> getInt r
          (resSig, w') <- case op of
            BvAdd -> return (lcAdd l' r', w + 1)
            BvSub ->
              return (lcShift (twoPow $ fromIntegral w) $ lcSub l' r', w + 1)
            BvMul -> (, 2 * w) <$> lcMul "mul" l' r'
            _     -> unhandledOp op
          bs <- bitify "arith" resSig w'
          saveIntBits bv (take w bs)
        Bit -> do
          l' <- fromJust <$> getIntBits l
          r' <- fromJust <$> getIntBits r
          bs <- case op of
            BvOr  -> traverse id $ zipWith binOr l' r'
            BvAnd -> traverse id $ zipWith binAnd l' r'
            BvXor -> traverse id $ zipWith binXor l' r'
            _     -> unhandledOp op
          saveIntBits bv bs
        Shift -> do
          rightInt  <- fst . fromJust <$> getInt r
          rightBits <- fromJust <$> getIntBits r
          let b = bitsize $ w - 1
          unless (2 ^ b == w) $ error $ unwords
            ["width", show w, "is not a power of 2: bitsize is", show b]
          let rightBits' = take b rightBits
          -- Fits in log w bits
          enforceCheck (lcZero, lcZero, lcSub (deBitify rightBits') rightInt)
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
          let shiftInt leftInt lowBit = do
                let shiftByR v = foldM optPowerShift v (zip [0 ..] rightBits')
                shifted        <- shiftByR leftInt
                shiftedWithExt <- case lowBit of
                  Just lowBit' -> do
                    -- The ones to add in
                    extensionPart <- lcAdd (lcNeg lcOne) <$> shiftByR lowBit'
                    -- Adding them in, if appropriate
                    lcAdd shifted <$> lcMul "shiftExtMask" extensionPart lowBit'
                  Nothing -> return shifted
                resultBits <- bitify "shift" shiftedWithExt (2 * w - 1)
                return $ take w resultBits
          bs <- case op of
            BvShl -> do
              li' <- fst . fromJust <$> getInt l
              shiftInt li' Nothing
            BvLshr -> do
              l' <- fromJust <$> getIntBits l
              reverse <$> shiftInt (deBitify $ reverse l') Nothing
            BvAshr -> do
              l' <- fromJust <$> getIntBits l
              reverse <$> shiftInt (deBitify $ reverse l') (Just $ last l')
            _ -> unhandledOp op
          saveIntBits bv bs
    _ -> error $ unwords ["Cannot translate", show bv]

data Signedness = Signed | Unsigned deriving(Show)
isSigned :: Signedness -> Bool
isSigned = \case
  Signed   -> True
  Unsigned -> False

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
    BvUgt -> unsignedGreater width True l r
    BvUlt -> unsignedGreater width True r l
    BvUge -> unsignedGreater width False l r
    BvUle -> unsignedGreater width False r l
    BvSgt -> signedGreater width True l r
    BvSlt -> signedGreater width True r l
    BvSge -> signedGreater width False l r
    BvSle -> signedGreater width False r l
    _     -> do
      l' <- deBitifySigned . fromJust <$> getIntBits l
      r' <- deBitifySigned . fromJust <$> getIntBits r
      case predicate of
        BvSaddo -> lcNot <$> inBits Signed width (lcAdd l' r')
        BvSsubo -> lcNot <$> inBits Signed width (lcSub l' r')
        BvSmulo ->
          lcNot <$> (inBits Signed width =<< lcMul "muloverflow" l' r')
        _ -> error "unreachable"
 where
  signedGreater
    :: KnownNat n => Int -> Bool -> TermDynBv -> TermDynBv -> ToPf n (LSig n)
  signedGreater w strict a b = do
    -- Strategy: To set r = a > b, make r indicate whether a - b - 1 is a small
    -- positive value.
    a' <- deBitifySigned . fromJust <$> getIntBits a
    b' <- deBitifySigned . fromJust <$> getIntBits b
    lcGt w (if strict then lcSub a' lcOne else a') b'
  unsignedGreater
    :: KnownNat n => Int -> Bool -> TermDynBv -> TermDynBv -> ToPf n (LSig n)
  unsignedGreater w strict a b = do
    -- Strategy: To set r = a > b, make r indicate whether a - b - 1 is a small
    -- positive value.
    a' <- fst . fromJust <$> getInt a
    b' <- fst . fromJust <$> getInt b
    lcGt w (if strict then lcSub a' lcOne else a') b'
  -- Does `number` fit in `w` `signed` bits?
  inBits :: KnownNat n => Signedness -> Int -> LSig n -> ToPf n (LSig n)
  inBits signed w number = do
    -- let s (bitSum) be a sum of n bits...
    -- To enforce r <-> s = x, we reduce this constraint as follows...
    --
    -- 1. r(1-r) = 0    // done (emit this constraint as R1CS)
    --    r <-> s = x
    --
    -- 2. (s - x) = 0 -> r = 1
    --    (s - x) /= 0 -> r = 0
    --
    -- 3. (s - x) /= 0 or r = 1
    --    (s - x) = 0 or r = 0 // done, as (s - x)r = 0
    --
    -- 4. (s - x + r) /= 0 // done as exists i. (s - x + r)i = 1
    --
    -- There is some stuff going on with the last step that is a bit tricky.
    -- It takes advantage of the fact that when r = 1, the second constraint
    -- forces (s - x) to be 0
    bs <- nbits "inBits" $ asBits w $ snd number
    let bitSum = (if isSigned signed then deBitifySigned else deBitify) bs
    result <-
      nextBit "inBits"
      $   fitsInBits (isSigned signed) w
      <$> snd number
    let zeroIffTrue = lcSub bitSum number
    enforceCheck (zeroIffTrue, result, lcZero)
    enforceNonzero $ lcAdd zeroIffTrue result
    return result
  lcGt :: KnownNat n => Int -> LSig n -> LSig n -> ToPf n (LSig n)
  lcGt w a b = inBits Unsigned w (lcSub a b)


-- # Top Level

enforceAsPf :: KnownNat n => Maybe SmtVals -> TermBool -> ToPf n ()
enforceAsPf env b = boolToPf env b >>= enforceTrue

runToPf :: KnownNat n => ToPf n a -> ToPfState n -> IO (a, ToPfState n)
runToPf (ToPf f) = runStateT f

publicizeInputs :: Set.Set PfVar -> ToPf n ()
publicizeInputs is = do
  modify $ \s -> s { r1cs = r1csAddSignals (Set.toList is) $ r1cs s }
  forM_ is $ \i -> modify $ \s -> s { r1cs = r1csPublicizeSignal i $ r1cs s }

toPf :: KnownNat n => Set.Set PfVar -> [TermBool] -> IO (R1CS PfVar n)
toPf inputs bs = do
  s <- runToPf (publicizeInputs inputs >> forM_ bs (enforceAsPf Nothing))
               emptyState
  return $ r1cs $ snd s

toPfWithWit
  :: forall n
   . KnownNat n
  => SmtVals
  -> Set.Set PfVar
  -> [TermBool]
  -> IO (R1CS PfVar n, PfVals n)
toPfWithWit env inputs bs = do
  s <- runToPf (publicizeInputs inputs >> forM_ bs (enforceAsPf $ Just env))
               emptyState
  return (r1cs $ snd s, vals $ snd s)

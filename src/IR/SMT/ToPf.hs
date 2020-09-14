{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
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
import           Control.Monad                  ( join )
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
                                                , fromMaybe
                                                , fromJust
                                                )
import qualified Data.Map.Strict               as Map
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Set                      as Set
import           Data.Typeable                  ( cast )
import           Util.Cfg ( readCfgDefault )

type PfVar = String
type SmtVals = Map.Map String Dynamic
type PfVals n = Map.Map PfVar (Prime n)

type LSig n = (LC PfVar (Prime n), Maybe (Prime n))

data ToPfConfig = ToPfConfig { assumeNoBvOverflow :: Bool
                             }

data ToPfState n = ToPfState { r1cs :: R1CS PfVar n
                             , bools :: ShowMap TermBool (LSig n)
                             , ints :: ShowMap TermDynBv (BvEntry n)
                             , vals :: PfVals n
                             , next :: Int
                             , cfg  :: ToPfConfig
                             }

newtype ToPf n a = ToPf (StateT (ToPfState n) IO a)
    deriving (Functor, Applicative, Monad, MonadState (ToPfState n), MonadIO)

emptyState :: ToPfState n
emptyState = ToPfState { r1cs  = emptyR1cs
                       , bools = SMap.empty
                       , ints  = SMap.empty
                       , vals  = Map.empty
                       , next  = 0
                       , cfg   = ToPfConfig { assumeNoBvOverflow = False }
                       }

configureFromEnv :: ToPf n ()
configureFromEnv = do
  a <- liftIO $ readCfgDefault "noOverflow" False
  modify $ \s -> s { cfg = (cfg s) { assumeNoBvOverflow = a } }

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
    Just v -> do
      -- Uncomment to see all variable assignments in ToPf
      --liftIO $ putStrLn $ var ++ " -> " ++ primeShow v
      modify $ \s -> s { vals = Map.insert var v $ vals s }
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
        a' <- bvToPf env abv >> getInt abv
        b' <- bvToPf env bbv >> getInt bbv
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
      v  <- nextVar "ite" $ liftA3 (?) ((/= toP 0) <$> snd c') (snd t') (snd f')
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
-- The bits list has low order bits at low indices
data BvEntry n = BvEntry { int :: Maybe (LSig n, Int)
                         , bits :: Maybe [LSig n]
                         }

bvEntryEmpty :: BvEntry n
bvEntryEmpty = BvEntry { int = Nothing, bits = Nothing }


-- Initialize an empty entry
initIntEntry :: TermDynBv -> ToPf n ()
initIntEntry t =
  modify $ \s -> s { ints = SMap.insertWith (const id) t bvEntryEmpty $ ints s }

-- Saving transalations
saveConstBv :: KnownNat n => TermDynBv -> Bv.BV -> ToPf n ()
saveConstBv term bv = do
  initIntEntry term
  modify $ \s -> s
    { ints =
      SMap.adjust
          (\e -> e
            { int  = Just (lcConst $ Bv.uint bv, Bv.size bv)
            , bits = Just $ map (lcConst . toInteger . fromEnum) $ reverse $ Bv.toBits bv
            }
          )
          term
        $ ints s
    }

saveInt :: TermDynBv -> (LSig n, Int) -> ToPf n ()
saveInt term sig = initIntEntry term >> modify
  (\s -> s { ints = SMap.adjust (\e -> e { int = Just sig }) term $ ints s })

saveIntBits :: TermDynBv -> [LSig n] -> ToPf n ()
saveIntBits term bits_ = initIntEntry term >> modify
  (\s -> s { ints = SMap.adjust (\e -> e { bits = Just bits_ }) term $ ints s })

-- Fetching transalations
getInt :: KnownNat n => TermDynBv -> ToPf n (LSig n)
getInt term = fromMaybe (error $ "No int for " ++ show term) <$> getIntM term

getSignedInt :: KnownNat n => TermDynBv -> ToPf n (LSig n)
getSignedInt term = deBitify True <$> getIntBits term

getIntM :: KnownNat n => TermDynBv -> ToPf n (Maybe (LSig n))
getIntM term = do
  e <- gets (SMap.lookup term . ints)
  case e >>= int of
    Just i  -> return $ Just $ fst i
    Nothing -> case e >>= bits of
      Just bs -> do
        let i = deBitify False bs
        saveInt term (i, length bs)
        return $ Just i
      Nothing -> return Nothing

getIntBits :: KnownNat n => TermDynBv -> ToPf n [LSig n]
getIntBits term = fromMaybe (error $ "No bits for " ++ show term) <$> do
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

lazyInt :: KnownNat n => ToPf n Bool
lazyInt = gets (assumeNoBvOverflow . cfg)

-- Require `x` to fit in `width` unsigned bits
bitify :: KnownNat n => String -> LSig n -> Int -> ToPf n [LSig n]
bitify ctx x width = do
  sigs <- nbits ctx $ asBits width $ snd x
  let sum' = foldr1 lcAdd $ zipWith lcScale (map twoPow [0 ..]) sigs
  enforceCheck (lcZero, lcZero, lcSub sum' x)
  return sigs

-- Does `number` fit in `w` `signed` bits?
inBits :: KnownNat n => Bool -> Int -> LSig n -> ToPf n (LSig n)
inBits signed w number = do
  bs <- nbits "inBits" $ asBits w $ snd number
  binEq number $ deBitify signed bs

deBitify :: KnownNat n => Bool -> [LSig n] -> LSig n
deBitify signed bs =
  let lowBits    = foldr1 lcAdd $ zipWith lcScale (map twoPow [0 ..]) (init bs)
      highBitPos = lcScale (toP $ 2 ^ (length bs - 1)) $ last bs
      highBit    = if signed then lcNeg highBitPos else highBitPos
  in  lcAdd lowBits highBit

data BvOpKind = Division | Arith | Bit | Shift

bvToPf :: forall n . KnownNat n => Maybe SmtVals -> TermDynBv -> ToPf n ()
bvToPf env term = do
  entry <- getIntM term
  when (isNothing entry) $ bvToPfUncached term
 where
  unhandledOp = unhandled "bv operator in bvToPf"
  bvOpKind :: BvBinOp -> BvOpKind
  bvOpKind o = case o of
    BvAdd  -> Arith
    BvMul  -> Arith
    BvSub  -> Arith
    BvUrem -> Division
    BvUdiv -> Division
    BvOr   -> Bit
    BvAnd  -> Bit
    BvXor  -> Bit
    BvShl  -> Shift
    BvLshr -> Shift
    BvAshr -> Shift

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
  bvToPfUncached bv = case bv of
    IntToDynBv w (IntLit i) -> saveConstBv bv (Bv.bitVec w i)
    DynBvLit l              -> saveConstBv bv l
    Var name (SortBv w)     -> do
      i  <- asVar name $ lookupIntVal name
      bs <- bitify name i w
      saveIntBits bv bs
      saveInt bv (i, w)
    Ite c t_ f -> do
      c' <- boolToPf env c
      bvToPf env t_
      t' <- getInt t_
      bvToPf env f
      f' <- getInt f
      v  <- nextVar "ite" $ liftA3 (?) ((/= toP 0) <$> snd c') (snd t') (snd f')
      enforceCheck (c', lcSub v t', lcZero)
      enforceCheck (lcNot c', lcSub v f', lcZero)
      saveInt bv (v, dynBvWidth t_)
    DynBvUnExpr BvNeg w x -> do
      bvToPf env x
      x' <- getInt x
      saveInt bv (lcSub (lcConst $ 2 ^ w) x', w)
    DynBvUnExpr BvNot _ x -> do
      bvToPf env x
      x' <- getIntBits x
      saveIntBits bv $ map lcNeg x'
    DynBvSext _ deltaW i -> do
      bvToPf env i
      i' <- getIntBits i
      saveIntBits bv $ replicate deltaW (head i')
    DynBvUext w _ i -> do
      bvToPf env i
      i' <- getInt i
      saveInt bv (i', w)
    DynBvExtract start w i -> do
      bvToPf env i
      i' <- getIntBits i
      saveIntBits bv $ take w (drop start i')
    DynBvBinExpr op w l r -> do
      bvToPf env l
      bvToPf env r
      case bvOpKind op of
        Division -> do
          d <- getInt l
          m <- getInt r
          let dv = fromP <$> snd d
          let mv = fromP <$> snd m
          q  <- nextVar "div_q" $ toP <$> liftA2 div dv mv
          r' <- nextVar "div_r" $ toP <$> liftA2 rem dv mv
          enforceCheck (m, q, lcSub d r')
          qb <- bitify "quotient" q w
          rb <- bitify "remainder" r' w
          enforceTrue =<< lcGt w (lcSub m lcOne) r'
          saveIntBits bv $ case op of
            BvUdiv -> qb
            BvUrem -> rb
            _      -> unhandledOp op
        Arith -> do
          l'        <- getInt l
          r'        <- getInt r
          (res, w') <- case op of
            BvAdd -> pure (lcAdd l' r', w + 1)
            BvSub ->
              pure (lcShift (twoPow $ fromIntegral w) $ lcSub l' r', w + 1)
            BvMul -> (, 2 * w) <$> lcMul "mul" l' r'
            _     -> unhandledOp op
          lazy <- lazyInt
          if lazy
            then saveInt bv (res, w)
            else do
              bs <- bitify "arith" res w'
              saveIntBits bv (take w bs)
        Bit -> do
          l' <- getIntBits l
          r' <- getIntBits r
          bs <- case op of
            BvOr  -> traverse id $ zipWith binOr l' r'
            BvAnd -> traverse id $ zipWith binAnd l' r'
            BvXor -> traverse id $ zipWith binXor l' r'
            _     -> unhandledOp op
          saveIntBits bv bs
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
    _ -> error $ unwords ["Cannot translate", show bv]

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
  s <- runToPf
    (configureFromEnv >> publicizeInputs inputs >> forM_ bs
                                                         (enforceAsPf Nothing)
    )
    emptyState
  return $ r1cs $ snd s

toPfWithWit
  :: KnownNat n
  => SmtVals
  -> Set.Set PfVar
  -> [TermBool]
  -> IO (R1CS PfVar n, PfVals n)
toPfWithWit env inputs bs = do
  s <- runToPf
    (configureFromEnv >> publicizeInputs inputs >> forM_
      bs
      (enforceAsPf $ Just env)
    )
    emptyState
  return (r1cs $ snd s, vals $ snd s)

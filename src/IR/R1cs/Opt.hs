{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.R1cs.Opt
  ( opt
  )
where

import           Control.Applicative
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Control.Monad.Trans.UnionFind as UnionFind
import           Codegen.Circom.Signal
import           GHC.TypeLits                   ( KnownNat )
import           Data.Bifunctor
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import           Data.Functor.Identity
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Maybe                    as Maybe
import qualified Data.Foldable                 as Fold
import qualified Data.List                     as List
import qualified Data.Sequence                 as Seq
import           IR.R1cs
import           Util.Cfg                       ( Cfg
                                                , MonadCfg(..)
                                                , _optR1cs
                                                )
import           Util.Log
import           Debug.Trace

-- TODO: this would all be a lot fast if the constraints used IntMaps...

-- If this QEQ implies that some signal is an affine function of another,
-- return that.
-- `protected` is a set of variables we should not eliminate
asLinearSub
  :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Maybe (Int, LC Int k)
asLinearSub protected (a, b, (m, c)) = if a == lcZero && b == lcZero && True
  then
         -- TODO: First or last?
    let here     = IntSet.fromDistinctAscList $ Map.keys m
        disjoint = here IntSet.\\ protected
    in  case IntSet.toList disjoint of
          [] -> Nothing
          k : _ ->
            let v  = m Map.! k
                m' = Map.delete k m
            in  Just (k, lcScale (negate $ recip v) (m', c))
  else Nothing

type Subs n = Map.Map Int (LC Int (Prime n))
type Assertion n = QEQ Int (Prime n)

-- subs: contains substitutions that we're applying.
-- uses: contains a map from signal to substitutions that contain them.
-- pub: public signals
data SubState n = SubState { subs :: !(Subs n)
                           , uses :: !(IntMap.IntMap IntSet.IntSet)
                           , pub :: !IntSet.IntSet
                           }

emptySub :: SubState n
emptySub =
  SubState { subs = Map.empty, uses = IntMap.empty, pub = IntSet.empty }

newtype Sub n a = Sub (StateT (SubState n) Log a)
    deriving (Functor, Applicative, Monad, MonadState (SubState n), MonadIO, MonadLog)

instance (KnownNat n) => Show (SubState n) where
  show s =
    "Subs: \n"
      ++ unlines
           (map (\(k, v) -> "  " ++ show k ++ " : " ++ show (lcSigs v))
                (Map.toList $ subs s)
           )
      ++ "\n"
      ++ "Uses: \n"
      ++ unlines
           (map (\(k, v) -> "  " ++ show k ++ " : " ++ show (IntSet.toList v))
                (IntMap.toList $ uses s)
           )

checkSubs :: KnownNat n => Sub n ()
checkSubs = do
  s <- get
  forM_ (Map.toList $ subs s) $ \(v, l) -> do
    let vs = lcSigs l
    forM_ vs $ \u -> unless (IntSet.member v (uses s IntMap.! u)) $ do
      liftIO $ print s
      error $ "Use of " ++ show u ++ " in " ++ show v ++ " not recorded"
  forM_ (IntMap.toList $ uses s) $ \(x, y) -> forM_ (IntSet.toList y) $ \u ->
    unless (x `elem` lcSigs (subs s Map.! u)) $ do
      liftIO $ print s
      error $ "Use of " ++ show u ++ " in " ++ show x ++ " not real"

accumulateSubs :: (KnownNat n, Show s) => R1CS s n -> Sub n ()
accumulateSubs r1cs = do
  modify $ \s -> s { pub = publicInputs r1cs }
  forM_ (constraints r1cs) process
  s <- get
  liftLog $ logIf "r1csOpt::subs" $ "Subs: " ++ show s
 where
  process :: KnownNat n => QEQ Int (Prime n) -> Sub n ()
  process qeq' = do
    --checkSubs
    qeq       <- applyStoredSubs qeq'
    protected <- gets pub
    case asLinearSub protected qeq of
      Just (v, t) -> do
        --liftLog $ logIf "r1csOpt::subs" $ "QEQ: " ++ qeqShow qeq
        liftLog $ logIf "r1csOpt::subs" $ "Sub: " ++ show v ++ " to " ++ show
          (lcSigs t)
        addSub v t
      Nothing -> return ()

  -- Record that x uses ys
  addUses :: KnownNat n => Int -> IntSet.IntSet -> Sub n ()
  addUses x ys = modify $ \s -> s
    { uses = IntSet.foldr
               (IntMap.alter
                 (Just . maybe (IntSet.singleton x) (IntSet.insert x))
               )
               (uses s)
               ys
    }

  addSub :: forall n . KnownNat n => Int -> LC Int (Prime n) -> Sub n ()
  addSub v t = do
    uses' <- gets (maybe [] IntSet.toList . IntMap.lookup v . uses)
    let vars = IntSet.fromDistinctAscList $ map fst $ Map.toAscList $ fst t
    liftLog $ logIf "r1csOpt::subs" $ "vars: " ++ show vars
    liftLog $ logIf "r1csOpt::subs" $ "uses: " ++ show uses'
    let subIn :: Int -> Sub n ()
        subIn x = do
          modify $ \s -> s { subs = Map.adjust (subLcInLc v t) x $ subs s }
          addUses x vars
    forM_ uses' subIn
    modify $ \s ->
      s { uses = IntMap.delete v $ uses s, subs = Map.insert v t $ subs s }
    addUses v vars

  applyStoredSubs :: KnownNat n => Assertion n -> Sub n (Assertion n)
  applyStoredSubs qeq = flip subLcsInQeq qeq <$> gets subs

constantLc :: LC s n -> Maybe n
constantLc (map, constant) = if Map.null map then Just constant else Nothing

-- Is this QEQ constraint true, regardless of variable values.
constantlyTrue :: (Eq n, Num n) => QEQ s n -> Bool
constantlyTrue (a, b, c) = case (constantLc a, constantLc b, constantLc c) of
  (Just a', _, Just c') | isZero a' && isZero c' -> True
  (_, Just b', Just c') | isZero b' && isZero c' -> True
  (Just a', Just b', Just c') | a' * b' == c' -> True
  _ -> False
  where isZero = (0 ==)

applyLinearSubs :: KnownNat n => Subs n -> R1CS s n -> R1CS s n
applyLinearSubs subs r1cs =
  let removed = IntSet.fromAscList $ Map.keys subs
  in  r1cs
        { constraints = Seq.filter (not . constantlyTrue)
                        $   subLcsInQeq subs
                        <$> constraints r1cs
        , numSigs = numSigs r1cs IntMap.\\ IntMap.fromDistinctAscList
                      (map (, SigLocal ("", [])) $ IntSet.toAscList removed)
        , sigNums = Map.filter (not . (`IntSet.member` removed)) $ sigNums r1cs
        }

subLcInLc
  :: forall s k . (Ord s, GaloisField k) => s -> LC s k -> LC s k -> LC s k
subLcInLc x t l@(m, c) = case Map.lookup x m of
  Just v  -> lcAdd (lcScale v t) (Map.delete x m, c)
  Nothing -> l

subLcsInLc
  :: forall s k
   . (Ord s, GaloisField k)
  => Map.Map s (LC s k)
  -> LC s k
  -> LC s k
subLcsInLc subs (m, c) =
  let additional :: [LC s k] =
          Fold.toList $ Map.intersectionWith lcScale m subs
      unmodified = (m Map.\\ subs, c)
  in  Fold.foldl' lcAdd unmodified additional

subLcsInQeq
  :: (Ord s, GaloisField k) => Map.Map s (LC s k) -> QEQ s k -> QEQ s k
subLcsInQeq subs (a, b, c) =
  (subLcsInLc subs a, subLcsInLc subs b, subLcsInLc subs c)

reduceLinearities :: (KnownNat n, Show s) => R1CS s n -> Log (R1CS s n)
reduceLinearities r1cs = do
  let Sub s = accumulateSubs r1cs
  m <- subs <$> execStateT s emptySub
  return $ applyLinearSubs m r1cs

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM pred xs = case xs of
  []      -> return ([], [])
  x : xs' -> do
    c      <- pred x
    (a, b) <- spanM pred xs'
    return $ if c then (x : a, b) else (a, x : b)

groupByM :: Monad m => (a -> a -> m Bool) -> [a] -> m [[a]]
groupByM = go []
 where
  go acc eq xs = case xs of
    []      -> return acc
    x : xs' -> do
      (y, n) <- spanM (eq x) xs'
      go ((x : y) : acc) eq n

slidingPairs :: [a] -> [(a, a)]
slidingPairs xs = case xs of
  x : y : xs' -> (x, y) : slidingPairs (y : xs')
  _           -> []

computePublicReachability :: (Show s, KnownNat n) => R1CS s n -> Log ()
computePublicReachability r1cs = do
  logIf "r1csOpt::reach" $ "Cs: " ++ show (length $ constraints r1cs)
  logIf "r1csOpt::reach" $ "Sigs: " ++ show (IntMap.size $ numSigs r1cs)
  groups <- UnionFind.runUnionFind $ do
    pts <- IntMap.fromList
      <$> forM (IntMap.keys $ numSigs r1cs) (\n -> (n, ) <$> UnionFind.fresh n)
    forM_ (constraints r1cs) $ \qeq ->
      forM_ (slidingPairs $ IntSet.toAscList $ IntSet.fromList $ qeqSigs qeq)
        $ \(a, b) -> UnionFind.union (pts IntMap.! a) (pts IntMap.! b)

    let e a b = UnionFind.equivalent (pts IntMap.! a) (pts IntMap.! b)

    groupByM e $ IntMap.keys pts
  logIf "r1csOpt::reach" $ "Groups: " ++ show (length groups)
  logIf "r1csOpt::reach" $ "net: " ++ show
    (length $ Set.toList $ Set.fromList $ Fold.toList $ constraints r1cs)


opt :: (KnownNat n, Ord s, Show s) => R1CS s n -> Log (R1CS s n)
opt r1cs = do
  doOpt <- liftCfg $ asks _optR1cs
  if doOpt
    then do
      logIf "r1csOpt" $ "Constraints before r1csOpt: " ++ show
        (Seq.length $ constraints r1cs)
      logIf "r1csOpt" $ "public inputs: " ++ show (publicInputs r1cs)
      logIf "r1csOpt" $ "r1cs: " ++ r1csShow r1cs
      r1cs' <- compactifySigNums . removeDeadSignals <$> reduceLinearities r1cs
      --computePublicReachability r1cs'
      logIf "r1csOpt" $ "Constraints  after r1csOpt: " ++ show
        (Seq.length $ constraints r1cs')
      logIf "r1csOpt" $ "r1cs: " ++ r1csShow r1cs'
      return r1cs'
    else return r1cs


-- Remove signals not involved in constraints
removeDeadSignals :: R1CS s n -> R1CS s n
removeDeadSignals r1cs =
  let liveSigs = liveSignalIntsR1cs r1cs `IntSet.union` publicInputs r1cs
  in  r1cs
        { numSigs    = IntMap.filterWithKey (\k v -> IntSet.member k liveSigs)
                         $ numSigs r1cs
        , sigNums    = Map.filter (`IntSet.member` liveSigs) $ sigNums r1cs
        , nextSigNum = 2 + IntSet.size liveSigs
        }
 where
  liveSignalIntsLc (m, c) = Map.keys m
  liveSignalIntsQeq (a, b, c) =
    IntSet.fromList (concatMap liveSignalIntsLc [a, b, c])
  liveSignalIntsR1cs =
    Fold.foldr IntSet.union IntSet.empty . fmap liveSignalIntsQeq . constraints

-- Given a set of constraints, ensures that the signal numbers are in the range
-- [2..(1+n)], where n is the number of signals
compactifySigNums :: R1CS s n -> R1CS s n
compactifySigNums r1cs =
  let usedNums = IntMap.keys $ numSigs r1cs
      numMap   = IntMap.fromDistinctAscList $ zip usedNums [2 ..]
      remap s i =
          Maybe.fromMaybe
              (  error
              $  "Could not find signal number "
              ++ show i
              ++ " when remapping signal numbers.\n\nContext: "
              ++ s
              )
            $         numMap
            IntMap.!? i
  in  R1CS
        { sigNums      = Map.map (remap "sigNums") $ sigNums r1cs
        , numSigs = IntMap.mapKeysMonotonic (remap "numSigs") $ numSigs r1cs
        , constraints  = sigMapQeq (remap "constraints") <$> constraints r1cs
        , publicInputs = IntSet.map (remap "publicInputs") $ publicInputs r1cs
        , nextSigNum   = 2 + IntMap.size numMap
        }

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codegen.Circom.Opt
  ( opt
  )
where

import           Codegen.Circom.Signal
import           Codegen.Circom.Linking         ( R1CS(..)
                                                , r1csStats
                                                , sigMapQeq
                                                )
import           Codegen.Circom.CompTypes.LowDeg
                                                ( QEQ
                                                , LC
                                                , lcZero
                                                , lcAdd
                                                , lcScale
                                                )
import           Control.Applicative
import           GHC.TypeLits                   ( KnownNat )
import           Data.Bifunctor
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Foldable                 as Fold
import qualified Data.List                     as List
import qualified Data.Sequence                 as Seq
import           Debug.Trace

-- TODO: this would all be a lot fast if the constraints used IntMaps...

-- If this QEQ implies that some signal is an affine function of another,
-- return that.
asLinearSub
  :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Maybe (Int, LC Int k)
asLinearSub public (a, b, (m, c)) = if a == lcZero && b == lcZero
  then case Map.toAscList m of
    (y, yk) : (x, xk) : [] -> if yk /= 0 && IntSet.notMember y public
      then Just (y, (Map.singleton x (-xk / yk), -c / yk))
      else if xk /= 0 && IntSet.notMember x public
        then Just (x, (Map.singleton y (-yk / xk), -c / xk))
        else Nothing
    (y, yk) : [] -> if yk /= 0 then Just (y, (Map.empty, -c / yk)) else Nothing
    []           -> if c == 0 then Nothing else (error "Inconsistent!")
    _            -> Nothing
  else Nothing

data Subs n = Subs { fwd :: !(Map.Map Int (LC Int (Prime n)))
                   , used :: !IntSet.IntSet
                   }

lcSigs :: LC Int n -> IntSet.IntSet
lcSigs = IntSet.fromDistinctAscList . Map.keys . fst

emptySubs :: Subs n
emptySubs = Subs Map.empty IntSet.empty

singletonSubs :: Int -> LC Int (Prime n) -> Subs n
singletonSubs sig lc = Subs (Map.singleton sig lc) (lcSigs lc)


-- Assumes that adding this substitution will not produce cycles.
addSubUnsafe :: Int -> LC Int (Prime n) -> Subs n -> Subs n
addSubUnsafe sig lc subs = Subs { fwd  = Map.insert sig lc $ fwd subs
                                , used = IntSet.union (lcSigs lc) $ used subs
                                }


addSub :: KnownNat n => Int -> LC Int (Prime n) -> Subs n -> Maybe (Subs n)
addSub sig lc subs = if IntSet.notMember sig (used subs)
                       -- This variable does not occur in the subs...
  then Just $ addSubUnsafe sig (subLcsInLc (fwd subs) lc) subs
  else Nothing
    -- TODO: opposite?
    --else if Map.null (Map.intersection (fst lc) (fwd subs))
        --then Just $ addSubUnsafe

-- check that the domain and range are disjoint
checkSubs :: Subs n -> ()
checkSubs subs =
  let d = IntSet.fromAscList $ Map.keys (fwd subs)
      r = Map.foldr' (IntSet.union . (IntSet.fromAscList . Map.keys . fst))
                     IntSet.empty
                     (fwd subs)
      i = IntSet.intersection d r
  in  if IntSet.null i
        then ()
        else
          error
          $  "The substitution set "
          ++ plines (Map.toAscList (fwd subs))
          ++ "\nis circular on\n"
          ++ show i

plines :: (Fold.Foldable f, Show a) => f a -> String
plines = Fold.foldMap (\a -> "\n - " ++ show a)

extractLinearSubs :: forall n . KnownNat n => R1CS n -> (Subs n, R1CS n)
extractLinearSubs r1cs = (constants, r1cs { constraints = newConstraints })
 where
  safeAdd sig lc subs = (\s -> checkSubs s `seq` s) <$> addSub sig lc subs
  partition = Fold.foldl'
    (\(subs, constraints') c -> case asLinearSub (publicInputs r1cs) c of
      Just (x, lc) -> case addSub x lc subs of
        Just subs' -> (subs', constraints')
        Nothing    -> (subs, c Seq.<| constraints')
      Nothing -> (subs, c Seq.<| constraints')
    )
    (emptySubs, Seq.empty)
  (constants, newConstraints) = partition $ constraints r1cs

lcRemove :: (Ord s) => s -> LC s k -> LC s k
lcRemove sig (m, c) = (Map.delete sig m, c)

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

applyLinearSubs :: KnownNat n => Subs n -> R1CS n -> R1CS n
applyLinearSubs subs r1cs =
  let removed = IntSet.fromAscList $ Map.keys (fwd subs)
  in  r1cs
        { constraints = fmap (subLcsInQeq (fwd subs)) $ constraints r1cs
        , numSigs = numSigs r1cs IntMap.\\ IntMap.fromDistinctAscList
                      (map (, SigLocal ("", [])) $ IntSet.toAscList removed)
        , sigNums = Map.filter (not . (`IntSet.member` removed)) $ sigNums r1cs
        }

reduceLinearities :: KnownNat n => R1CS n -> R1CS n
reduceLinearities r1cs =
  let (subs, r1cs') = extractLinearSubs r1cs
  in  if not $ Map.null (fwd subs)
        then
          trace (r1csStats r1cs)
          $  reduceLinearities
          $! applyLinearSubs subs r1cs'
        else r1cs

opt :: KnownNat n => R1CS n -> R1CS n
opt = compactifySigNums . reduceLinearities

-- Given a set of constraints, ensures that the signal numbers are in the range
-- [2..(1+n)], where n is the number of signals
compactifySigNums :: R1CS n -> R1CS n
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
        , constraints  = fmap (sigMapQeq (remap "constraints"))
                           $ constraints r1cs
        , publicInputs = IntSet.map (remap "publicInputs") $ publicInputs r1cs
        , nextSigNum   = 2 + IntMap.size numMap
        }

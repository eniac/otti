{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codegen.Circom.Opt
  ( reduceLinearities
  )
where

import           Codegen.Circom.Signal
import           Codegen.Circom.Linking         ( R1CS(..)
                                                , r1csStats
                                                )
import           Codegen.Circom.CompTypes.LowDeg
                                                ( QEQ
                                                , LC
                                                , lcZero
                                                , lcAdd
                                                , lcScale
                                                )
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

-- If this QEQ implies that some signal has some constant value, return the
-- signal and the value.
asConstant :: GaloisField k => QEQ Int k -> Maybe (Int, k)
asConstant (a, b, (m, c)) = if (a == lcZero) && (b == lcZero) && (Map.size m == 1)
  then
    let (x, c') = head $ Map.toAscList m
    in  if c' /= 0 then Just (x, c / c') else Nothing
  else Nothing

-- If this QEQ implies that some signal is an affine function of another,
-- return that.
-- Returns (y, m, x, b), where y, x are signals, m, b are constants, such that
-- y = mx + b
asLinear :: GaloisField k => QEQ Int k -> Maybe (Int, k, Int, k)
asLinear (a, b, (m, c)) = if a == lcZero && b == lcZero
  then case Map.toAscList m of
    (y, yk) : (x, xk) : [] ->
      if yk /= 0 then Just (y, -xk / yk, x, -c / yk) else Nothing
    _ -> Nothing
  else Nothing

asLinearSub :: GaloisField k => QEQ Int k -> Maybe(Int, LC Int k)
asLinearSub (a, b, (m, c)) = if a == lcZero && b == lcZero
  then case Map.toAscList m of
    (y, yk) : (x, xk) : [] ->
      if yk /= 0 then Just (y, (Map.singleton x (-xk / yk), -c / yk)) else Nothing
    (y, yk) : [] ->
      if yk /= 0 then Just (y, (Map.empty, -c / yk)) else Nothing
    _ -> Nothing
  else Nothing

findLinearSub :: KnownNat n => R1CS n -> (Maybe (Int, LC Int (Prime n)), R1CS n)
findLinearSub r1cs = (constants, r1cs { constraints = Seq.fromList newConstraints })
 where
  f [] = (Nothing, [])
  f (c:cs) = case asLinearSub c of
    Just x -> (Just x, cs)
    Nothing -> let (a, b) = f cs in (a, c: b)
  (constants, newConstraints) = f $ Fold.toList $ constraints r1cs

extractLinearSubs :: KnownNat n => R1CS n -> (Map.Map Int (LC Int (Prime n)), R1CS n)
extractLinearSubs r1cs = (constants, r1cs { constraints = newConstraints })
 where
  partition = Fold.foldl' (\(subs, constraints') c -> case asLinearSub c of
          Just (x, lc)  -> (Map.insert x (subLcsInLc subs lc) subs , constraints')
          Nothing -> (subs, c Seq.<| constraints')) (Map.empty, Seq.empty)
  (constants, newConstraints) = partition $ constraints r1cs

lcRemove :: (Ord s) => s -> LC s k -> LC s k
lcRemove sig (m, c) = (Map.delete sig m, c)

subLcsInLc :: forall s k. (Ord s, GaloisField k) => Map.Map s (LC s k) -> LC s k -> LC s k
subLcsInLc subs (m, c) =
  let additional :: [LC s k] = Fold.toList $ Map.intersectionWith lcScale m subs
      unmodified = (m Map.\\ subs, c)
  in Fold.foldl' lcAdd unmodified additional

subLcsInQeq :: (Ord s, GaloisField k) => Map.Map s (LC s k) -> QEQ s k -> QEQ s k
subLcsInQeq subs (a, b, c) =
  (subLcsInLc subs a, subLcsInLc subs b, subLcsInLc subs c)

applyLinearSubs :: KnownNat n => Map.Map Int (LC Int (Prime n)) -> R1CS n -> R1CS n
applyLinearSubs subs r1cs =
  let cset = IntSet.fromAscList $ List.sort $ Map.keys subs
  in  r1cs
        { nums        = nums r1cs IntSet.\\ cset
        , constraints = fmap (subLcsInQeq subs) $ constraints r1cs
        , numSigs     = numSigs r1cs IntMap.\\ IntMap.fromAscList
                          (map (, SigLocal ("", [])) $ IntSet.toAscList cset)
        , sigNums     = Map.filter (not . (`IntSet.member` cset)) $ sigNums r1cs
        }

reduceLinearities :: KnownNat n => R1CS n -> R1CS n
reduceLinearities r1cs =
  let (subs, r1cs') = extractLinearSubs r1cs
  in
    if not $ Map.null subs
    then
      reduceLinearities $! applyLinearSubs subs r1cs'
    else
      r1cs

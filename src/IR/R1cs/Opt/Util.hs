{-# LANGUAGE ScopedTypeVariables #-}
module IR.R1cs.Opt.Util
  ( asConst
  , asEqOrConst
  , Eliminatable(..)
  , normalize
  , asLinearSub
  , constantlyTrue
  , subLcInLc
  , subLcInQeq
  )
where
import           IR.R1cs

import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Control.Monad.State.Strict

data Eliminatable s k = Const s k
                      | Eq    s s
                      | Neither

normalize :: (Ord s, GaloisField k) => QEQ s k -> QEQ s k
normalize (a, b, c) = case (asConst a, asConst b) of
  (Just a', _      ) -> (lcZero, lcZero, lcAdd c $ lcScale a' b)
  (_      , Just b') -> (lcZero, lcZero, lcAdd c $ lcScale b' a)
  (_      , _      ) -> (a, b, c)
  where asConst (m, c) = if Map.null m then Just c else Nothing

asConst :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Maybe (Int, k)
asConst protected (a, b, (m, c)) = if a == lcZero || b == lcZero
  then case Map.toList m of
    [(x, v)] | not (IntSet.member x protected) -> Just (x, negate c / v)
    _ -> Nothing
  else Nothing


asEqOrConst :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Eliminatable Int k
asEqOrConst protected (a, b, (m, c)) = if a == lcZero || b == lcZero
  then case Map.toList m of
    [(a, ac), (b, bc)] | ac == negate bc && c == 0 ->
      if not (IntSet.member a protected)
        then Eq a b
        else if not (IntSet.member b protected) then Eq b a else Neither
    [(a, ac)] | not (IntSet.member a protected) -> Const a (negate c / ac)
    _ -> Neither
  else Neither

asLinearSub
  :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Maybe (Int, LC Int k)
asLinearSub protected (a, b, (m, c)) = if a == lcZero || b == lcZero
  then
    let here     = IntSet.fromDistinctAscList $ Map.keys m
        disjoint = here IntSet.\\ protected
    in  case IntSet.toList disjoint of
          [] -> Nothing
          k : _ ->
            let v  = m Map.! k
                m' = Map.delete k m
            in  Just (k, lcScale (negate $ recip v) (m', c))
  else Nothing

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

subLcInLc
  :: forall s k . (Ord s, GaloisField k) => s -> LC s k -> LC s k -> LC s k
subLcInLc x t l@(m, c) = case Map.lookup x m of
  Just v  -> lcAdd (lcScale v t) (Map.delete x m, c)
  Nothing -> l

subLcInQeq
  :: forall s k . (Ord s, GaloisField k) => s -> LC s k -> QEQ s k -> QEQ s k
subLcInQeq x t q@(a, b, c) =
  (subLcInLc x t a, subLcInLc x t b, subLcInLc x t c)

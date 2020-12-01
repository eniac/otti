{-# LANGUAGE ScopedTypeVariables #-}
module Targets.R1cs.Opt.Util
  ( asEqOrConst
  , Eliminatable(..)
  , normalize
  , asLinearSub
  , constantlyTrue
  , subLcInLc
  , subLcInQeq
  )
where
import           Targets.R1cs.Main

import           Data.Field.Galois              ( GaloisField )
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map

data Eliminatable s k = Const s k
                      | Eq    s s
                      | Neither

normalize :: (Ord s, GaloisField k) => QEQ s k -> QEQ s k
normalize (a, b, c) = case (constantLc a, constantLc b) of
  (Just a', _      ) -> (lcZero, lcZero, lcAdd c $ lcScale (negate a') b)
  (_      , Just b') -> (lcZero, lcZero, lcAdd c $ lcScale (negate b') a)
  (_      , _      ) -> (a, b, c)


asEqOrConst :: GaloisField k => IntSet.IntSet -> QEQ Int k -> Eliminatable Int k
asEqOrConst protected (a, b, (m, c)) = if a == lcZero || b == lcZero
  then case Map.toList m of
    [(a', ac), (b', bc)] | ac == negate bc && c == 0 ->
      if not (IntSet.member a' protected)
        then Eq a' b'
        else if not (IntSet.member b' protected) then Eq b' a' else Neither
    [(a', ac)] | not (IntSet.member a' protected) -> Const a' (negate c / ac)
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
constantLc (m, constant) = if Map.null m then Just constant else Nothing

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
subLcInQeq x t (a, b, c) = (subLcInLc x t a, subLcInLc x t b, subLcInLc x t c)

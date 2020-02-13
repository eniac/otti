{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Codegen.Circom.Term ( lcZero
                           , Term(..)
                           , LTerm(..)
                           , Signal(..)
                           , WireBundle(..)
                           , Constraint
                           , BaseTerm
                           , LC
                           , mapSignalsInTerm
                           , termSignals
                           , sigAsTerm
                           ) where

import qualified IR.TySmt                   as Smt
import qualified Codegen.Circom.Constraints as CS
import           Codegen.Circom.Constraints (Constraints, Constraint, LC, Signal)
import           Data.Field.Galois          (Prime, PrimeField)
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.Set                   (Set)
import           Data.Proxy                 (Proxy (Proxy))
import           GHC.TypeLits               (KnownNat, natVal)

data WireBundle k = Scalar k
                  | Linear (LC k)
                  | Quadratic (LC k) (LC k) (LC k)
                  | Other
                  deriving (Show, Ord, Eq)

type BaseTerm k = (WireBundle k, Smt.PfTerm)

data Term k = Sig Signal
            | Base (BaseTerm k)
            | Array [Term k]                                    -- An array of terms
            | Struct (Map.Map String (Term k)) (Constraints k)  -- A structure, environment and constraints
            deriving (Show,Ord,Eq)

-- An evaluated l-value
data LTerm = LTermIdent String
           | LTermPin LTerm String
           | LTermIdx LTerm Int
           deriving (Show,Ord,Eq)

instance Num Smt.PfTerm where
  a + b = Smt.PfNaryExpr Smt.PfAdd [a, b]
  a * b = Smt.PfNaryExpr Smt.PfMul [a, b]
  fromInteger n = error "NYI: Need modulus"
  signum s = error "NYI: Need modulus"
  abs s = s
  negate = Smt.PfUnExpr Smt.PfNeg

instance Fractional Smt.PfTerm where
  fromRational n = error "NYI: Need modulus"
  recip = Smt.PfUnExpr Smt.PfRecip

instance PrimeField k => Num (WireBundle k) where
  s + t = case (s, t) of
    (Other, _) -> Other
    (Linear (m1, c1), Linear (m2, c2)) -> Linear (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Quadratic a b (m2, c2)) -> Quadratic a b (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Scalar c) -> Linear (m1, c1 + c)
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic a b (m, c1), Scalar c2) -> Quadratic a b (m, c1 + c2)
    (Scalar c1, Scalar c2) -> Scalar $ c1 + c2
    (l, r) -> r + l
  s * t = case (s, t) of
    (Other, _) -> Other
    (Linear l1, Linear l2) -> Quadratic l1 l2 (Map.empty, 0)
    (Linear _, Quadratic {}) -> Other
    (Linear l, Scalar c) -> Linear $ lcScale l c
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic l1 l2 l3, Scalar c) -> Quadratic (lcScale l1 c) (lcScale l2 c) (lcScale l3 c)
    (Scalar c1 , Scalar c2) -> Scalar $ c1 * c2
    (l, r) -> r * l
  fromInteger n = Scalar $ fromInteger n
  signum s = case s of
    Other        -> Scalar 1
    Linear {}    -> Scalar 1
    Quadratic {} -> Scalar 1
    Scalar n     -> Scalar $ signum n
  abs s = case s of
    Other          -> Other
    l@Linear {}    -> l
    q@Quadratic {} -> q
    Scalar n       -> Scalar $ abs n
  negate s = fromInteger (-1) * s

instance PrimeField k => Fractional (WireBundle k) where
  fromRational = Scalar . fromRational
  recip t = case t of
    Scalar c1    -> Scalar (recip c1)
    Other        -> Other
    Linear _     -> Other
    Quadratic {} -> Other

instance forall k. KnownNat k => Num (BaseTerm (Prime k)) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) * (c, d) = (a * c, b * d)
    fromInteger n = (fromInteger n, Smt.PfLit n $ natVal (Proxy :: Proxy k))
    signum (a, b) = (signum a, signum b)
    abs (a, b) = (abs a, abs b)
    negate (a, b) = (negate a, negate b)

instance KnownNat k => Fractional (BaseTerm (Prime k)) where
    fromRational n = (fromRational n, fromRational n)
    recip (a, b) = (recip a, recip b)

instance KnownNat k => Num (Term (Prime k)) where
  s + t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Struct {}, _) -> error $ "Cannot add struct term " ++ show a ++ " to anything"
    (Sig s, r) -> sigAsTerm s + r
    (Base a, Base b) -> Base $ a + b
    (l, r) -> r + l
  s * t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Struct {}, _) -> error $ "Cannot multiply struct term " ++ show a ++ " with anything"
    (Sig s, r) -> sigAsTerm s * r
    (Base a, Base b) -> Base $ a * b
    (l, r) -> r * l
  fromInteger n = Base $ fromInteger n
  signum s = case s of
    Array {}     -> error $ "Cannot get sign of array term " ++ show s
    Struct {}    -> error $ "Cannot get sign of struct term " ++ show s
    Base a       -> Base $ signum a
    Sig {}       -> Base $ fromInteger 1
  abs s = case s of
    Array a        -> Base $ fromIntegral $ length a
    Struct s _     -> Base $ fromIntegral $ Map.size s
    Base a         -> Base $ abs a
    s@Sig {}       -> s
  negate s = fromInteger (-1) * s

instance KnownNat k => Fractional (Term (Prime k)) where
  fromRational = Base . fromRational
  recip t = case t of
    a@Array {}   -> error $ "Cannot invert array term " ++ show a
    a@Struct {}  -> error $ "Cannot invert struct term " ++ show a
    Base a       -> Base $ recip a
    Sig s        -> Base (Other, Smt.PfUnExpr Smt.PfRecip (sigAsSmt s))

lcScale :: PrimeField k => LC k -> k -> LC k
lcScale (m, c) a = (Map.map (*a) m, a * c)

lcZero :: PrimeField k => LC k
lcZero = (Map.empty, 0)

sigAsTerm :: PrimeField k => Signal -> Term k
sigAsTerm s = Base (Linear (Map.fromList [(s, 1)], 0), sigAsSmt s)

sigAsSmt :: Signal -> Smt.PfTerm
sigAsSmt = Smt.PfVar . show

mapSignalsInWires :: (Signal -> Signal) -> WireBundle k -> WireBundle k
mapSignalsInWires f t = case t of
    Linear (m, c) -> Linear (Map.mapKeys f m, c)
    Quadratic (m1, c1) (m2, c2) (m3, c3) -> Quadratic (Map.mapKeys f m1, c1) (Map.mapKeys f m2, c2) (Map.mapKeys f m3, c3)
    Scalar c -> Scalar c
    Other -> Other

mapSignalsInTerm :: (Signal -> Signal) -> (String -> String) -> Term k -> Term k
mapSignalsInTerm f fs t = case t of
    Sig s -> Sig $ f s
    Array ts -> Array $ map (mapSignalsInTerm f fs) ts
    Struct ts cs -> Struct (Map.map (mapSignalsInTerm f fs) ts) (CS.mapSignals f cs)
    Base (a, b) -> Base (mapSignalsInWires f a, Smt.mapVar fs b)

wireSignals :: WireBundle k -> Set Signal
wireSignals t = case t of
    Other -> Set.empty
    Linear (m, _) -> Set.fromList (Map.keys m)
    Quadratic (a, _) (b, _) (c, _) -> foldr Set.union Set.empty (map (Set.fromList . Map.keys) [a, b, c])
    Scalar s -> Set.empty

termSignals :: Term k -> Set Signal
termSignals t = case t of
    Sig s -> Set.insert s Set.empty
    Array ts -> foldr Set.union Set.empty $ map termSignals ts
    Struct m cs -> foldr Set.union Set.empty $ map termSignals $ Map.elems m
    Base (a, _) -> wireSignals a


{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
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
                           , sigAsLinearTerm
                           , sigAsSigTerm
                           , sigAsWire
                           , sigAsLC
                           , sigLocation
                           ) where

import           Codegen.Circom.Constraints (Constraint, Constraints, LC,
                                             Signal)
import qualified Codegen.Circom.Constraints as CS
import           Data.Field.Galois          (Prime, PrimeField)
import qualified Data.Map.Strict            as Map
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           GHC.TypeLits
import qualified IR.TySmt                   as Smt

data WireBundle k = Sig Signal
                  | Scalar k
                  | Linear (LC k)
                  | Quadratic (LC k) (LC k) (LC k)
                  | Other
                  deriving (Show, Ord, Eq)

type BaseTerm k = (WireBundle (Prime k), Smt.Term (Smt.PfSort k))

data Term k = Base (BaseTerm k)
            | Array [Term k]                                    -- An array of terms
            | Struct (Map.Map String (Term k)) (Constraints (Prime k))  -- A structure, environment and constraints
            deriving (Show)

-- An evaluated l-value
data LTerm = LTermIdent String
           | LTermPin LTerm String
           | LTermIdx LTerm Int
           deriving (Show,Ord,Eq)

instance forall n. KnownNat n => Num (Smt.Term (Smt.PfSort n)) where
  a + b = Smt.PfNaryExpr Smt.PfAdd [a, b]
  a * b = Smt.PfNaryExpr Smt.PfMul [a, b]
  fromInteger i = Smt.IntToPf $ Smt.IntLit $ i `rem` natVal (Proxy :: Proxy n)
  signum s = Smt.IntToPf $ Smt.IntLit 1
  abs s = s
  negate = Smt.PfUnExpr Smt.PfNeg

instance forall k. KnownNat k => Fractional (Smt.Term (Smt.PfSort k)) where
  fromRational n = error "NYI"
  recip = Smt.PfUnExpr Smt.PfRecip

instance PrimeField k => Num (WireBundle k) where
  s + t = case (s, t) of
    (Other, _) -> Other
    (Sig s, r) -> sigAsWire s + r
    (Linear (m1, c1), Linear (m2, c2)) -> Linear (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Quadratic a b (m2, c2)) -> Quadratic a b (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Scalar c) -> Linear (m1, c1 + c)
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic a b (m, c1), Scalar c2) -> Quadratic a b (m, c1 + c2)
    (Scalar c1, Scalar c2) -> Scalar $ c1 + c2
    (l, r) -> r + l
  s * t = case (s, t) of
    (Other, _) -> Other
    (Sig s, r) -> sigAsWire s * r
    (Linear l1, Linear l2) -> Quadratic l1 l2 (Map.empty, 0)
    (Linear _, Quadratic {}) -> Other
    (Linear l, Scalar c) -> Linear $ lcScale l c
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic l1 l2 l3, Scalar c) -> Quadratic (lcScale l1 c) (lcScale l2 c) (lcScale l3 c)
    (Scalar c1 , Scalar c2) -> Scalar $ c1 * c2
    (l, r) -> r * l
  fromInteger n = Scalar $ fromInteger n
  signum s = case s of
    Sig {}       -> Scalar 1
    Other        -> Scalar 1
    Linear {}    -> Scalar 1
    Quadratic {} -> Scalar 1
    Scalar n     -> Scalar $ signum n
  abs s = case s of
    Other    -> Other
    Scalar n -> Scalar $ abs n
    _        -> s
  negate s = fromInteger (-1) * s

instance PrimeField k => Fractional (WireBundle k) where
  fromRational = Scalar . fromRational
  recip t = case t of
    Scalar c1    -> Scalar (recip c1)
    Other        -> Other
    Sig {}       -> Other
    Linear _     -> Other
    Quadratic {} -> Other

instance forall k. KnownNat k => Num (BaseTerm k) where
    (a, b) + (c, d) = (a + c, b + d)
    (a, b) * (c, d) = (a * c, b * d)
    fromInteger n = (fromInteger n, Smt.IntToPf $ Smt.IntLit n)
    signum (a, b) = (signum a, signum b)
    abs (a, b) = (abs a, abs b)
    negate (a, b) = (negate a, negate b)

instance forall k. KnownNat k => Fractional (BaseTerm k) where
    fromRational n = (fromRational n, fromRational n)
    recip (a, b) = (recip a, recip b)

instance forall k. KnownNat k => Num (Term k) where
  s + t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Struct {}, _) -> error $ "Cannot add struct term " ++ show a ++ " to anything"
    (Base a, Base b) -> Base $ a + b
    (l, r) -> r + l
  s * t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Struct {}, _) -> error $ "Cannot multiply struct term " ++ show a ++ " with anything"
    (Base a, Base b) -> Base $ a * b
    (l, r) -> r * l
  fromInteger n = Base $ fromInteger n
  signum s = case s of
    Array {}  -> error $ "Cannot get sign of array term " ++ show s
    Struct {} -> error $ "Cannot get sign of struct term " ++ show s
    Base a    -> Base $ signum a
  abs s = case s of
    Array a    -> Base $ fromIntegral $ length a
    Struct s _ -> Base $ fromIntegral $ Map.size s
    Base a     -> Base $ abs a
  negate s = fromInteger (-1) * s

instance forall k. KnownNat k => Fractional (Term k) where
  fromRational = Base . fromRational
  recip t = case t of
    a@Array {}  -> error $ "Cannot invert array term " ++ show a
    a@Struct {} -> error $ "Cannot invert struct term " ++ show a
    Base a      -> Base $ recip a

lcScale :: PrimeField k => LC k -> k -> LC k
lcScale (m, c) a = (Map.map (*a) m, a * c)

lcZero :: PrimeField k => LC k
lcZero = (Map.empty, 0)

sigAsLC :: PrimeField k => Signal -> LC k
sigAsLC s = (Map.fromList [(s, 1)], 0)

sigAsWire :: PrimeField k => Signal -> WireBundle k
sigAsWire = Linear . sigAsLC

sigAsSigTerm :: KnownNat k => Signal -> Term k
sigAsSigTerm s = Base (Sig s, sigAsSmt s)

sigAsLinearTerm :: KnownNat k => Signal -> Term k
sigAsLinearTerm s = Base (Linear (Map.fromList [(s, 1)], 0), sigAsSmt s)

sigAsSmt :: KnownNat n => Signal -> Smt.Term (Smt.PfSort n)
sigAsSmt = Smt.Var . show

mapSignalsInWires :: (Signal -> Signal) -> WireBundle k -> WireBundle k
mapSignalsInWires f t = case t of
    Sig s -> Sig $ f s
    Linear (m, c) -> Linear (Map.mapKeys f m, c)
    Quadratic (m1, c1) (m2, c2) (m3, c3) -> Quadratic (Map.mapKeys f m1, c1) (Map.mapKeys f m2, c2) (Map.mapKeys f m3, c3)
    Scalar c -> Scalar c
    Other -> Other

mapVarsInSmtTerms :: (String -> String) -> Smt.Term s -> Smt.Term s
mapVarsInSmtTerms f = Smt.mapTerm visit
  where
    visit :: Smt.Term t -> Maybe (Smt.Term t)
    visit t = case t of
      Smt.Var v ->
        Just $ Smt.Var (f v)
      Smt.Exists v s tt ->
        Just $ Smt.Exists (f v) s (Smt.mapTerm visit tt)
      Smt.Let v s tt ->
        Just $ Smt.Let (f v) (Smt.mapTerm visit s) (Smt.mapTerm visit tt)
      _ -> Nothing


mapSignalsInTerm :: (Signal -> Signal) -> (String -> String) -> Term k -> Term k
mapSignalsInTerm f fs t = case t of
    Array ts -> Array $ map (mapSignalsInTerm f fs) ts
    Struct ts cs -> Struct (Map.map (mapSignalsInTerm f fs) ts) (CS.mapSignals f cs)
    Base (a, b) -> Base (mapSignalsInWires f a, mapVarsInSmtTerms fs b)

wireSignals :: WireBundle k -> Set Signal
wireSignals t = case t of
    Other -> Set.empty
    Sig s       -> Set.insert s Set.empty
    Linear (m, _) -> Set.fromList (Map.keys m)
    Quadratic (a, _) (b, _) (c, _) -> foldr Set.union Set.empty (map (Set.fromList . Map.keys) [a, b, c])
    Scalar s -> Set.empty

termSignals :: Term k -> Set Signal
termSignals t = case t of
    Array ts    -> foldr Set.union Set.empty $ map termSignals ts
    Struct m cs -> foldr Set.union Set.empty $ map termSignals $ Map.elems m
    Base (a, _) -> wireSignals a

sigLocation :: Signal -> LTerm
sigLocation s =
    foldl (flip $ either (flip LTermPin) (flip LTermIdx))
          (LTermIdent (CS.signalLeadingName s))
          (drop 1 $ CS.signalAccesses s)

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codegen.Circom.CompTypes.LowDeg
  ( LowDegTerm
  , LowDeg(..)
  , LowDegCtx(..)
  , LC
  , QEQ
  , LowDegCompCtx
  , lcZero
  , lcScale
  , lcAdd
  )
where

import           AST.Circom                     ( BinOp(..)
                                                , UnOp(..)
                                                )
import           Codegen.Circom.CompTypes       ( BaseTerm(..)
                                                , BaseCtx(..)
                                                , Term
                                                , CompCtx
                                                , primeUnOp
                                                , primeBinOp
                                                )
import qualified Codegen.Circom.Signal         as Sig

import           Data.Field.Galois              ( Prime
                                                , GaloisField
                                                )
import qualified Data.Map.Strict               as Map
import           GHC.TypeNats

type LC s n = (Map.Map s n, n) -- A linear combination of signals and gen-time constants
type QEQ s n = (LC s n, LC s n, LC s n)

data LowDeg n = Scalar !n
              | Linear !(LC Sig.Signal n)
              | Quadratic !(QEQ Sig.Signal n)
              | HighDegree
              deriving (Show,Eq,Ord)

lcZero :: GaloisField k => LC s k
lcZero = (Map.empty, 0)

lcAdd :: (Ord s, GaloisField k) => LC s k -> LC s k -> LC s k
lcAdd (sm, sc) (tm, tc) = (Map.unionWith (+) sm tm, sc + tc)

lcSig :: (Ord s, GaloisField k) => s -> LC s k
lcSig s = (Map.fromList [(s, 1)], 0)

lcScale :: GaloisField k => k -> LC s k -> LC s k
lcScale c (sm, sc) = (Map.map (* c) sm, c * sc)

lcShift :: GaloisField k => k -> LC s k -> LC s k
lcShift c (sm, sc) = (sm, c + sc)

qeqLcAdd :: (Ord s, GaloisField k) => QEQ s k -> LC s k -> QEQ s k
qeqLcAdd (a1, b1, c1) l = (a1, b1, lcAdd c1 l)

qeqScale :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqScale k (a2, b2, c2) = (lcScale k a2, lcScale k b2, lcScale k c2)

qeqShift :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqShift k (a2, b2, c2) = (lcShift k a2, lcShift k b2, lcShift k c2)

newtype LowDegCtx k = LowDegCtx { constraints :: [QEQ Sig.Signal k] } deriving (Show)

instance KnownNat k => BaseTerm (LowDeg (Prime k)) (Prime k) where
  fromConst  = Scalar
  fromSignal = Linear . lcSig
  unOp o t = case (o, t) of
    (_    , Scalar f) -> Scalar $ primeUnOp o f
    (UnPos, _       ) -> t
    (UnNeg, _       ) -> binOp Mul (fromConst (-1)) t
    _                 -> HighDegree
  binOp o s t = case (o, s, t) of
    (_  , Scalar a   , Scalar b   ) -> Scalar $ primeBinOp o a b
    (Add, Linear a   , Linear b   ) -> Linear $ lcAdd a b
    (Add, Linear l   , Quadratic q) -> Quadratic $ qeqLcAdd q l
    (Add, Linear l   , Scalar c   ) -> Linear $ lcShift c l
    (Add, Quadratic{}, Quadratic{}) -> HighDegree
    (Add, Quadratic q, Scalar k   ) -> Quadratic $ qeqShift k q
    (Add, l          , r          ) -> binOp Add r l
    (Sub, l          , r          ) -> binOp Add l $ unOp UnNeg r
    (Mul, Linear l1  , Linear l2  ) -> Quadratic (l1, l2, lcZero)
    (Mul, Linear _   , Quadratic{}) -> HighDegree
    (Mul, Linear l   , Scalar c   ) -> Linear $ lcScale c l
    (Mul, Quadratic{}, Quadratic{}) -> HighDegree
    (Mul, Quadratic q, Scalar c   ) -> Quadratic $ qeqScale c q
    (Mul, l          , r          ) -> binOp Mul r l
    _                               -> HighDegree
  ite c t f = case c of
    Scalar n -> if n /= 0 then t else f
    _        -> HighDegree

instance KnownNat k => BaseCtx (LowDegCtx (Prime k)) (LowDeg (Prime k)) (Prime k) where
  assert t (LowDegCtx cs) = case t of
    Scalar    0  -> LowDegCtx cs
    Linear    lc -> LowDegCtx ((lcZero, lcZero, lc) : cs)
    Quadratic q  -> LowDegCtx (q : cs)
    _            -> error $ "Cannot constain " ++ show t ++ " to zero"
  emptyCtx = LowDegCtx []
  storeCtx _span _sigKind _signalLoc _term = id
  getCtx _kind _loc = id
  ignoreCompBlock = const True
  finalize        = id

type LowDegTerm k = Term (LowDeg k) k
type LowDegCompCtx n = CompCtx (LowDegCtx n) (LowDeg n) n

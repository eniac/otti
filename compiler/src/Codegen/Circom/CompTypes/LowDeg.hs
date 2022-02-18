{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , lcSig
  , lcShift
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
import           Data.Aeson
import qualified Data.Text                     as Text
import qualified Data.ByteString.Lazy.Char8    as Char8
import qualified Data.Map.Strict               as Map
import           Data.Field.Galois              ( Prime )
import           GHC.TypeNats
import           Targets.R1cs.Main

data LowDeg n = Scalar !n
              | Linear !(LC Sig.Signal n)
              | Quadratic !(QEQ Sig.Signal n)
              | HighDegree
              deriving (Eq,Ord)

instance Show n => ToJSON (LowDeg n) where
  toJSON x = case x of
    Scalar    i -> toJSON $ Text.pack (show i)
    Linear    l -> toJSON $ lcToJson l
    Quadratic q -> qeqToJson q
    HighDegree  -> toJSON $ Text.pack "HighDegree"
   where
    qeqToJson (a, b, c) = toJSON $ map lcToJson [a, b, c]
    lcToJson (m, c) =
      let back = map (\(k, v) -> Text.pack (show k) .= show v) (Map.toList m)
      in  object $ ("1" .= show c) : back

instance Show n => Show (LowDeg n) where
  show s = Char8.unpack $ encode s

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

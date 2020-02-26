{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# OPTIONS_GHC -Wall #-}

module IR.TySmt ( IntSort(..)
                , BoolSort(..)
                , BvSort(..)
                , PfSort(..)
                , FpSort(..)
                , ArraySort(..)
                , F32
                , F64
                , Sort(..)
                , BoolBinOp(..)
                , BvBinOp(..)
                , BvBinPred(..)
                , PfNaryOp(..)
                , PfUnOp(..)
                , PfBinPred(..)
                , FpBinOp(..)
                , FpUnOp(..)
                , FpBinPred(..)
                , FpUnPred(..)
                , Term(..)
                , mapVar
                ) where

import           GHC.TypeNats

data IntSort = IntSort deriving (Show,Ord,Eq)
data BoolSort = BoolSort deriving (Show,Ord,Eq)
data BvSort n = BvSort Int deriving (Show,Ord,Eq)
data PfSort n = PfSort Integer deriving (Show,Ord,Eq)
data FpSort n m = FpSort Int Int deriving (Show,Ord,Eq)
data ArraySort k v = ArraySort deriving (Show,Ord,Eq)

type F32 = FpSort 8 24
type F64 = FpSort 11 53

data Sort = SortInt
          | SortBool
          | SortBv Int
          | SortPf Integer
          | SortFp Int Int
          | SortArray Sort Sort
          deriving (Show,Ord,Eq)

data BoolBinOp = And | Or | Xor | Implies deriving (Show,Ord,Eq)

data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow | IntAdd | IntMul
              deriving (Show, Ord, Eq)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Ord, Eq)
data IntBinPred = IntLt | IntLe | IntGt | IntGe | IntEq | IntNe
                deriving (Show, Ord, Eq)

data BvBinOp = BvShl
             | BvLshr
             | BvAshr
             | BvUrem
             | BvUdiv
             | BvAdd
             | BvMul
             | BvSub
             | BvOr
             | BvAnd
             | BvXor
             deriving (Show, Ord, Eq)

data BvBinPred = BvEq
               | BvNe
               | BvUgt
               | BvUlt
               | BvUge
               | BvUle
               | BvSgt
               | BvSlt
               | BvSge
               | BvSle
               deriving (Show, Ord, Eq)

data PfNaryOp = PfAdd | PfMul deriving (Show, Ord, Eq)
data PfUnOp = PfNeg | PfRecip deriving (Show, Ord, Eq)
data PfBinPred = PfEq | PfNe deriving (Show, Ord, Eq)

data FpBinOp = FpAdd
             | FpSub
             | FpMul
             | FpDiv
             | FpRem
             | FpMax
             | FpMin
             deriving (Show, Ord, Eq)

data FpUnOp = FpNeg
            | FpAbs
            | FpSqrt
            | FpRound
            deriving (Show, Ord, Eq)

data FpBinPred = FpLe
               | FpLt
               | FpGe
               | FpGt
               | FpEq
               deriving (Show, Ord, Eq)

data FpUnPred = FpIsNormal
              | FpIsSubnormal
              | FpIsZero
              | FpIsInfinite
              | FpIsNaN
              | FpIsNegative
              | FpIsPositive
              deriving (Show, Ord, Eq)


data Term s where
    -- Boolean terms
    BoolLit     :: Bool -> Term BoolSort
    BoolBinExpr :: BoolBinOp -> Term BoolSort -> Term BoolSort -> Term BoolSort
    Not         :: Term BoolSort -> Term BoolSort

    -- Core terms
    Ite    :: Term BoolSort -> Term s -> Term s -> Term s
    Var    :: String -> Term s
    Exists :: String -> Sort -> Term s -> Term s

    -- Bit-vector terms
    BvConcat  :: (KnownNat n, KnownNat m) => Term (BvSort n) -> Term (BvSort m) -> Term (BvSort (n + m))
    BvExtract :: (KnownNat n, KnownNat i, KnownNat j) => Term (BvSort n) -> Term (BvSort (j - i))
    BvBinExpr :: KnownNat n => BvBinOp -> Term (BvSort n) -> Term (BvSort n) -> Term BoolSort
    BvBinPred :: KnownNat n => BvBinPred -> Term (BvSort n) -> Term (BvSort n) -> Term BoolSort
    IntToBv   :: KnownNat n => Term IntSort -> Term (BvSort n)
    FpToBv    :: (KnownNat e, KnownNat s) => Term (FpSort e s) -> Term (BvSort (e + s))

    -- Integer terms
    IntLit        :: Integer -> Term IntSort
    IntUnExpr     :: IntUnOp -> Term IntSort -> Term BoolSort
    IntBinExpr    :: IntBinOp -> Term IntSort -> Term IntSort -> Term BoolSort
    IntBinPred    :: IntBinPred -> Term IntSort -> Term IntSort -> Term BoolSort
    PfToInt       :: (KnownNat n) => Term (PfSort n) -> Term IntSort
    BvToInt       :: (KnownNat n) => Term (BvSort n) -> Term IntSort
    SignedBvToInt :: (KnownNat n) => Term (BvSort n) -> Term IntSort
    BoolToInt     :: Term BoolSort -> Term IntSort

    -- Floating point terms
    Fp64Lit   :: Double -> Term F64
    Fp32Lit   :: Float -> Term F32
    FpUnExpr  :: (KnownNat e, KnownNat s) => FpUnOp -> Term (FpSort e s) -> Term BoolSort
    FpBinExpr :: (KnownNat e, KnownNat s) => FpBinOp -> Term (FpSort e s) -> Term (FpSort e s) -> Term BoolSort
    FpFma     :: (KnownNat e, KnownNat s) => Term (FpSort e s) -> Term (FpSort e s) -> Term (FpSort e s) -> Term (FpSort e s)
    FpBinPred :: (KnownNat e, KnownNat s) => FpBinPred -> Term (FpSort e s) -> Term (FpSort e s) -> Term BoolSort
    FpUnPred  :: (KnownNat e, KnownNat s) => FpUnPred -> Term (FpSort e s) -> Term BoolSort
    IntToFp   :: (KnownNat e, KnownNat s) => Term IntSort -> Term (FpSort e s)
    BvToFp    :: (KnownNat e, KnownNat s) => Term (BvSort (e + s)) -> Term (FpSort e s)
    FpToFp    :: (KnownNat e1, KnownNat s1, KnownNat e2, KnownNat s2) => Term (FpSort e1 s1) -> Term (FpSort e2 s2)

    -- Prime field terms
    PfUnExpr   :: KnownNat n => PfUnOp -> Term (PfSort n) -> Term (PfSort n)
    PfNaryExpr :: KnownNat n => PfNaryOp -> [Term (PfSort n)] -> Term (PfSort n)
    PfBinPred  :: KnownNat n => PfBinPred -> Term (PfSort n) -> Term (PfSort n) -> Term (PfSort n)
    IntToPf    :: KnownNat n => Term IntSort -> Term (PfSort n)

    -- Array terms
    Select   :: Term (ArraySort k v) -> Term k -> Term v
    Store    :: Term (ArraySort k v) -> Term k -> Term v -> Term (ArraySort k v)
    NewArray :: Term (ArraySort k v)


deriving instance Show (Term s)

mapVar :: (String -> String) -> Term s -> Term s
mapVar f t = case t of
    BoolLit {} -> t
    BoolBinExpr o l r -> BoolBinExpr o (mapVar f l) (mapVar f r)
    Not s -> Not (mapVar f s)

    Ite c tt ff -> Ite c (mapVar f tt) (mapVar f ff)
    Var v -> Var (f v)
    Exists v s tt -> Exists (f v) s (mapVar f tt)

    BvConcat a b -> BvConcat (mapVar f a) (mapVar f b)
    BvExtract {} -> error "NYI: Ambiguous!"
    -- Not handled!! BvExtract a -> BvExtract (mapVar f a
    BvBinExpr o l r -> BvBinExpr o (mapVar f l) (mapVar f r)
    BvBinPred o l r -> BvBinPred o (mapVar f l) (mapVar f r)
    IntToBv tt -> IntToBv (mapVar f tt)
    FpToBv tt -> FpToBv (mapVar f tt)


    IntLit {} -> t
    IntBinExpr o l r -> IntBinExpr o (mapVar f l) (mapVar f r)
    IntUnExpr o l -> IntUnExpr o (mapVar f l)
    IntBinPred o l r -> IntBinPred o (mapVar f l) (mapVar f r)
    BvToInt tt -> BvToInt (mapVar f tt)
    SignedBvToInt tt -> SignedBvToInt (mapVar f tt)
    BoolToInt tt -> BoolToInt (mapVar f tt)
    PfToInt tt -> PfToInt (mapVar f tt)

    Fp64Lit {} -> t
    Fp32Lit {} -> t
    FpBinExpr o l r -> FpBinExpr o (mapVar f l) (mapVar f r)
    FpUnExpr o l -> FpUnExpr o (mapVar f l)
    FpBinPred o l r -> FpBinPred o (mapVar f l) (mapVar f r)
    FpUnPred o l -> FpUnPred o (mapVar f l)
    FpFma a b c -> FpFma (mapVar f a) (mapVar f b) (mapVar f c)
    IntToFp tt -> IntToFp (mapVar f tt)
    FpToFp tt -> FpToFp (mapVar f tt)
    BvToFp tt -> BvToFp (mapVar f tt)

    PfNaryExpr o as -> PfNaryExpr o (map (mapVar f) as)
    PfUnExpr o l -> PfUnExpr o (mapVar f l)
    PfBinPred o l r -> PfBinPred o (mapVar f l) (mapVar f r)
    IntToPf tt -> IntToPf (mapVar f tt)

    Select a k -> Select (mapVar f a) (mapVar f k)
    Store a k v -> Store (mapVar f a) (mapVar f k) (mapVar f v)
    NewArray -> t



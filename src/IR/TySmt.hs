{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

import GHC.TypeNats

main :: IO ()
main = do
    print "Hi"
    print $ show $ IntLit 5
    print $ show $ IntBinExpr IntAdd (IntLit 5) (IntLit 6)
    -- print $ show $ IntBinExpr IntAdd (IntLit 5) (BoolLit True) // ERROR!
    print $ show $ BoolBinExpr And (BoolLit True) (BoolLit False)
    print $ show $ NewArray
    print $ show $ Store NewArray (IntLit 0) (BoolLit True)
    print $ show $ Select (Store NewArray (IntLit 0) (BoolLit True)) (IntLit 0)
    -- print $ show $ Select (Store NewArray (IntLit 0) (BoolLit True)) (BoolLit 0) // ERROR!
    print $ show $ FpBinExpr FpAdd (Fp64Lit 3.4) (Var "a")
    -- print $ show $ FpBinExpr FpAdd (Fp64Lit 3.4) (Var @F32 "a")

data IntSort = IntSort deriving (Show)
data BoolSort = BoolSort deriving (Show)
data BvSort n = BvSort Int deriving (Show)
data PfSort n = PfSort Integer deriving (Show)
data FpSort n m = FpSort Int Int deriving (Show)
data ArraySort k v = ArraySort deriving (Show)

type F32 = FpSort 8 24
type F64 = FpSort 11 53

data Sort = SortInt
          | SortBool
          | SortBv Int
          | SortPf Integer
          | SortFp Int Int
          | SortArray Sort Sort
          deriving (Show)

data BoolBinOp = And | Or | Xor | Implies deriving (Show)

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

data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow | IntAdd | IntMul
              deriving (Show, Ord, Eq)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Ord, Eq)
data IntBinPred = IntLt | IntLe | IntGt | IntGe | IntEq | IntNe
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
    Ite    :: Term BoolSort -> Term a -> Term a
    Var    :: String -> Term s
    Exists :: String -> Sort -> Term t -> Term t

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
    FpFma     :: (KnownNat e, KnownNat s) => FpBinOp -> Term (FpSort e s) -> Term (FpSort e s) -> Term (FpSort e s) -> Term (FpSort e s)
    FpBinPred :: (KnownNat e, KnownNat s) => FpBinPred -> Term (FpSort e s) -> Term (FpSort e s) -> Term BoolSort
    FpUnPred  :: (KnownNat e, KnownNat s) => FpUnPred -> Term (FpSort e s) -> Term BoolSort
    IntToFp   :: (KnownNat e, KnownNat s) => Term IntSort -> Term (FpSort e s)
    BvToFp    :: (KnownNat e, KnownNat s) => Term (BvSort (e + s)) -> Term (FpSort e s)
    FpToFp    :: (KnownNat e1, KnownNat s1, KnownNat e2, KnownNat s2) => Term (FpSort e1 s1) -> Term (FpSort e2 s2)

    -- Prime field terms
    PfUnExpr   :: KnownNat n => PfUnOp -> Term (PfSort n) -> Term BoolSort
    PfNaryExpr :: KnownNat n => PfNaryOp -> [Term (PfSort n)] -> Term BoolSort
    PfBinPred  :: KnownNat n => PfBinPred -> Term (PfSort n) -> Term (PfSort n) -> Term BoolSort
    IntToPf    :: KnownNat n => Term IntSort -> Term (PfSort n)

    -- Array terms
    Select   :: (Show k, Show v) => Term (ArraySort k v) -> Term k -> Term v
    Store    :: (Show k, Show v) => Term (ArraySort k v) -> Term k -> Term v -> Term (ArraySort k v)
    NewArray :: Term (ArraySort k v)


deriving instance Show s => Show (Term s)

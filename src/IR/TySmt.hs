{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE Rank2Types          #-}
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
                , BoolNaryOp(..)
                , BvBinOp(..)
                , BvBinPred(..)
                , IntBinOp(..)
                , IntUnOp(..)
                , IntBinPred(..)
                , PfNaryOp(..)
                , PfUnOp(..)
                , PfBinPred(..)
                , FpBinOp(..)
                , FpUnOp(..)
                , FpBinPred(..)
                , FpUnPred(..)
                , Term(..)
                , mapTerm
                , reduceTerm
                , depth
                ) where

import           GHC.TypeNats
import           Data.Dynamic (Typeable)

data IntSort = IntSort deriving (Show,Ord,Eq,Typeable)
data BoolSort = BoolSort deriving (Show,Ord,Eq,Typeable)
data BvSort n = BvSort Int deriving (Show,Ord,Eq,Typeable)
data PfSort n = PfSort Integer deriving (Show,Ord,Eq,Typeable)
data FpSort n m = FpSort Int Int deriving (Show,Ord,Eq,Typeable)
data ArraySort k v = ArraySort deriving (Show,Ord,Eq,Typeable)

type F32 = FpSort 8 24
type F64 = FpSort 11 53

data Sort = SortInt
          | SortBool
          | SortBv Int
          | SortPf Integer
          | SortFp Int Int
          | SortArray Sort Sort
          deriving (Show,Ord,Eq,Typeable,Typeable)

data BoolNaryOp = And | Or | Xor deriving (Show,Ord,Eq,Typeable)

data BoolBinOp = Implies deriving (Show,Ord,Eq,Typeable)

data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow | IntAdd | IntMul
              deriving (Show, Ord, Eq, Typeable)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Ord, Eq, Typeable)
data IntBinPred = IntLt | IntLe | IntGt | IntGe | IntEq | IntNe
                deriving (Show, Ord, Eq, Typeable)

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
             deriving (Show, Ord, Eq, Typeable)

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
               deriving (Show, Ord, Eq, Typeable)

data PfNaryOp = PfAdd | PfMul deriving (Show, Ord, Eq, Typeable)
data PfUnOp = PfNeg | PfRecip deriving (Show, Ord, Eq, Typeable)
data PfBinPred = PfEq | PfNe deriving (Show, Ord, Eq, Typeable)

data FpBinOp = FpAdd
             | FpSub
             | FpMul
             | FpDiv
             | FpRem
             | FpMax
             | FpMin
             deriving (Show, Ord, Eq, Typeable)

data FpUnOp = FpNeg
            | FpAbs
            | FpSqrt
            | FpRound
            deriving (Show, Ord, Eq, Typeable)

data FpBinPred = FpLe
               | FpLt
               | FpGe
               | FpGt
               | FpEq
               deriving (Show, Ord, Eq, Typeable)

data FpUnPred = FpIsNormal
              | FpIsSubnormal
              | FpIsZero
              | FpIsInfinite
              | FpIsNaN
              | FpIsNegative
              | FpIsPositive
              deriving (Show, Ord, Eq, Typeable)


data Term s where
    -- Boolean terms
    BoolLit     :: Bool -> Term BoolSort
    BoolBinExpr :: BoolBinOp -> Term BoolSort -> Term BoolSort -> Term BoolSort
    BoolNaryExpr :: BoolNaryOp -> [Term BoolSort] -> Term BoolSort
    Not         :: Term BoolSort -> Term BoolSort

    -- Core terms
    Ite    :: Term BoolSort -> Term s -> Term s -> Term s
    Var    :: String -> Term s
    Let    :: String -> Term s -> Term t -> Term t
    Exists :: String -> Sort -> Term t -> Term t

    -- Bit-vector terms
    BvConcat  :: (KnownNat n, KnownNat m) => Term (BvSort n) -> Term (BvSort m) -> Term (BvSort (n + m))
    BvExtract :: (KnownNat n, KnownNat i, i <= n) => Int -> Term (BvSort n) -> Term (BvSort i)
    BvBinExpr :: KnownNat n => BvBinOp -> Term (BvSort n) -> Term (BvSort n) -> Term (BvSort n)
    BvBinPred :: KnownNat n => BvBinPred -> Term (BvSort n) -> Term (BvSort n) -> Term BoolSort
    IntToBv   :: KnownNat n => Term IntSort -> Term (BvSort n)
    FpToBv    :: (KnownNat e, KnownNat s) => Term (FpSort e s) -> Term (BvSort (e + s))

    -- Integer terms
    IntLit        :: Integer -> Term IntSort
    IntUnExpr     :: IntUnOp -> Term IntSort -> Term BoolSort
    IntBinExpr    :: IntBinOp -> Term IntSort -> Term IntSort -> Term IntSort
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
    PfBinPred  :: KnownNat n => PfBinPred -> Term (PfSort n) -> Term (PfSort n) -> Term BoolSort
    IntToPf    :: KnownNat n => Term IntSort -> Term (PfSort n)

    -- Array terms
    Select   :: Term (ArraySort k v) -> Term k -> Term v
    Store    :: Term (ArraySort k v) -> Term k -> Term v -> Term (ArraySort k v)
    NewArray :: Term (ArraySort k v)


deriving instance Show (Term s)
deriving instance Typeable (Term s)


-- Given a function that optionally transforms a term, traverses the term
-- applying that function at every stage. When the function returns something,
-- this is the transformation. When the function does not, the transformation
-- recurses.
mapTerm :: (forall t. Term t -> Maybe (Term t)) -> Term s -> Term s
mapTerm f t = case f t of
  Nothing -> case t of
    BoolLit {} -> t
    BoolBinExpr o l r -> BoolBinExpr o (mapTerm f l) (mapTerm f r)
    BoolNaryExpr o as -> BoolNaryExpr o (map (mapTerm f) as)
    Not s -> Not (mapTerm f s)

    Ite c tt ff -> Ite (mapTerm f c) (mapTerm f tt) (mapTerm f ff)
    Var {} -> t
    Exists v s tt -> Exists v s (mapTerm f tt)
    Let v s e -> Let v (mapTerm f s) (mapTerm f e)

    BvConcat a b -> BvConcat (mapTerm f a) (mapTerm f b)
    BvExtract i s -> BvExtract i (mapTerm f s)
    BvBinExpr o l r -> BvBinExpr o (mapTerm f l) (mapTerm f r)
    BvBinPred o l r -> BvBinPred o (mapTerm f l) (mapTerm f r)
    IntToBv tt -> IntToBv (mapTerm f tt)
    FpToBv tt -> FpToBv (mapTerm f tt)


    IntLit {} -> t
    IntBinExpr o l r -> IntBinExpr o (mapTerm f l) (mapTerm f r)
    IntUnExpr o l -> IntUnExpr o (mapTerm f l)
    IntBinPred o l r -> IntBinPred o (mapTerm f l) (mapTerm f r)
    BvToInt tt -> BvToInt (mapTerm f tt)
    SignedBvToInt tt -> SignedBvToInt (mapTerm f tt)
    BoolToInt tt -> BoolToInt (mapTerm f tt)
    PfToInt tt -> PfToInt (mapTerm f tt)

    Fp64Lit {} -> t
    Fp32Lit {} -> t
    FpBinExpr o l r -> FpBinExpr o (mapTerm f l) (mapTerm f r)
    FpUnExpr o l -> FpUnExpr o (mapTerm f l)
    FpBinPred o l r -> FpBinPred o (mapTerm f l) (mapTerm f r)
    FpUnPred o l -> FpUnPred o (mapTerm f l)
    FpFma a b c -> FpFma (mapTerm f a) (mapTerm f b) (mapTerm f c)
    IntToFp tt -> IntToFp (mapTerm f tt)
    FpToFp tt -> FpToFp (mapTerm f tt)
    BvToFp tt -> BvToFp (mapTerm f tt)

    PfNaryExpr o as -> PfNaryExpr o (map (mapTerm f) as)
    PfUnExpr o l -> PfUnExpr o (mapTerm f l)
    PfBinPred o l r -> PfBinPred o (mapTerm f l) (mapTerm f r)
    IntToPf tt -> IntToPf (mapTerm f tt)

    Select a k -> Select (mapTerm f a) (mapTerm f k)
    Store a k v -> Store (mapTerm f a) (mapTerm f k) (mapTerm f v)
    NewArray -> t
  Just s -> s


reduceTerm :: (forall t. Term t -> Maybe k) -> k-> (k -> k -> k) -> Term s -> k
reduceTerm mapF i foldF t = case mapF t of
  Nothing -> case t of
    BoolLit {} -> i
    BoolBinExpr _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BoolNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    Not s -> reduceTerm mapF i foldF s

    Ite c tt ff -> foldF (foldF (reduceTerm mapF i foldF c)
                                (reduceTerm mapF i foldF tt))
                         (reduceTerm mapF i foldF ff)
    Var {} -> i
    Exists _ _ tt -> reduceTerm mapF i foldF tt
    Let _ s e -> foldF (reduceTerm mapF i foldF s) (reduceTerm mapF i foldF e)

    BvConcat a b -> foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    BvExtract _ s -> reduceTerm mapF i foldF s
    BvBinExpr _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvBinPred _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntToBv tt -> reduceTerm mapF i foldF tt
    FpToBv tt -> reduceTerm mapF i foldF tt


    IntLit {} -> i
    IntBinExpr _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntUnExpr _ l -> reduceTerm mapF i foldF l
    IntBinPred _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvToInt tt -> reduceTerm mapF i foldF tt
    SignedBvToInt tt -> reduceTerm mapF i foldF tt
    BoolToInt tt -> reduceTerm mapF i foldF tt
    PfToInt tt -> reduceTerm mapF i foldF tt

    Fp64Lit {} -> i
    Fp32Lit {} -> i
    FpBinExpr _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    FpUnExpr _ l -> reduceTerm mapF i foldF l
    FpBinPred _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    FpUnPred _ l -> reduceTerm mapF i foldF l
    FpFma a b c -> foldF (foldF (reduceTerm mapF i foldF a)
                                (reduceTerm mapF i foldF b))
                         (reduceTerm mapF i foldF c)
    IntToFp tt -> reduceTerm mapF i foldF tt
    FpToFp tt -> reduceTerm mapF i foldF tt
    BvToFp tt -> reduceTerm mapF i foldF tt

    PfNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    PfUnExpr _ l -> reduceTerm mapF i foldF l
    PfBinPred _ l r -> foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntToPf tt -> reduceTerm mapF i foldF tt

    Select a k -> foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k)
    Store a k v -> foldF (foldF (reduceTerm mapF i foldF a)
                                (reduceTerm mapF i foldF k))
                         (reduceTerm mapF i foldF v)
    NewArray -> i
  Just s -> s

depth :: Term s -> Int
depth = reduceTerm visit 0 (\a b -> 1 + max a b)
    where
        visit :: Term s -> Maybe Int
        visit t = case t of
            NewArray -> Just 0
            Fp64Lit _ -> Just 0
            Fp32Lit _ -> Just 0
            IntLit _ -> Just 0
            BoolLit _ -> Just 0
            Var _ -> Just 0
            _ -> Nothing


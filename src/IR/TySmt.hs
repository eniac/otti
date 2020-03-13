{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

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
                , Value(..)
                , mapTerm
                , reduceTerm
                , depth
                , eval
                , nNodes
                , nChars
                ) where

import           GHC.TypeLits
import           Control.Monad  (liftM2)
import           Data.Dynamic   (Typeable, Dynamic, fromDyn, toDyn)
import qualified Data.BitVector as Bv
import qualified Data.Map       as Map
import           Data.Map       (Map)
import qualified Data.Maybe     as Maybe
import           Data.Bits      as Bits
import           Data.Proxy     (Proxy(..))
import           Data.Fixed     (mod')
import qualified Data.Binary.IEEE754 as IEEE754

data IntSort = IntSort deriving (Show,Ord,Eq,Typeable)
data BoolSort = BoolSort deriving (Show,Ord,Eq,Typeable)
data BvSort (n :: Nat) = BvSort Int deriving (Show,Ord,Eq,Typeable)
data PfSort (n :: Nat) = PfSort Integer deriving (Show,Ord,Eq,Typeable)
data RealFloat f => FpSort f = FpSort Int Int deriving (Show,Ord,Eq,Typeable)
data ArraySort k v = ArraySort deriving (Show,Ord,Eq,Typeable)

type F32 = FpSort Float
type F64 = FpSort Double

class (RealFloat fp, Typeable fp, KnownNat (Size fp)) => ComputableFp fp where
    type Size fp :: Nat
    asRepr :: Value (FpSort fp) -> fp
    fromRepr :: fp -> Value (FpSort fp)
    asBits :: fp -> Bv.BV
    fromBits :: Bv.BV -> fp

    asOther :: ComputableFp fp2 => fp -> fp2
    asOther = fromRational . toRational

    evalBin :: FpBinOp -> (Value (FpSort fp)) -> (Value (FpSort fp)) -> (Value (FpSort fp))
    evalBin op a b = fromRepr $ asRepr a `f` asRepr b
      where
        f = case op of
          FpAdd -> (+)
          FpSub -> (-)
          FpMul -> (*)
          FpDiv -> (/)
          FpRem -> mod'
          FpMax -> max
          FpMin -> min

    evalUn :: FpUnOp -> (Value (FpSort fp)) -> (Value (FpSort fp))
    evalUn op = fromRepr . f . asRepr
      where
        f = case op of
          FpNeg -> negate
          FpAbs -> abs
          FpSqrt -> sqrt
          FpRound -> fromIntegral @Integer . round

    evalUnPred :: FpUnPred -> (Value (FpSort fp)) -> (Value BoolSort)
    evalUnPred op = ValBool . f . asRepr
      where
        f :: fp -> Bool
        f = case op of
          FpIsNormal -> not . isDenormalized
          FpIsSubnormal -> isDenormalized
          FpIsZero -> (==0.0)
          FpIsInfinite -> isInfinite
          FpIsNaN -> isNaN
          FpIsNegative -> liftM2 (||) (<0.0) isNegativeZero
          FpIsPositive -> liftM2 (||) (>0.0) (isNegativeZero . negate)

    evalBinPred :: FpBinPred -> Value (FpSort fp) -> Value (FpSort fp) -> Value BoolSort
    evalBinPred op a b = ValBool $ f (asRepr a) (asRepr b)
      where
        f = case op of
          FpLe -> (<)
          FpLt -> (>)
          FpGe -> (>=)
          FpGt -> (<=)
          FpEq -> (==)
          FpNe -> (/=)

    evalFromInt :: Value IntSort -> Value (FpSort fp)
    evalFromInt = fromRepr . fromIntegral . valAsInt

instance ComputableFp Double where
    type Size Double = 64
    asRepr (ValDouble x) = x
    fromRepr = ValDouble
    asBits = Bv.bitVec 64 . IEEE754.doubleToWord
    fromBits = IEEE754.wordToDouble . fromIntegral . Bv.nat

instance ComputableFp Float where
    type Size Float = 32
    asRepr (ValFloat x) = x
    fromRepr = ValFloat
    asBits = Bv.bitVec 32 . IEEE754.floatToWord
    fromBits = IEEE754.wordToFloat . fromIntegral . Bv.nat

data Value s where
    ValBool :: Bool -> Value BoolSort
    ValInt :: Integer -> Value IntSort
    ValBv :: KnownNat n => Bv.BV -> Value (BvSort n)
    ValPf :: KnownNat n => Integer -> Value (PfSort n)
    ValFloat :: Float -> Value F32
    ValDouble :: Double -> Value F64
    ValArray :: Map (Value a) (Value b) -> Value (ArraySort a b)

deriving instance Typeable (Value s)
deriving instance Show (Value s)
deriving instance Eq (Value s)
deriving instance Ord (Value s)

data Sort = SortInt
          | SortBool
          | SortBv Int
          | SortPf Integer
          | SortFp Int Int
          | SortArray Sort Sort
          deriving (Show,Ord,Eq,Typeable,Typeable)

data BoolNaryOp = And | Or | Xor deriving (Show,Ord,Eq,Typeable)

data BoolBinOp = Implies deriving (Show,Ord,Eq,Typeable)

data IntNaryOp = IntAdd | IntMul deriving (Show, Ord, Eq, Typeable)
data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow
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
               | FpNe
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
    Let    :: (Typeable s) => String -> Term s -> Term t -> Term t
    Exists :: String -> Sort -> Term t -> Term t

    -- Bit-vector terms
    BvConcat  :: (KnownNat n, KnownNat m) => Term (BvSort n) -> Term (BvSort m) -> Term (BvSort (n + m))
    BvExtract :: (KnownNat n, KnownNat i, i <= n) => Int -> Term (BvSort n) -> Term (BvSort i)
    BvBinExpr :: KnownNat n => BvBinOp -> Term (BvSort n) -> Term (BvSort n) -> Term (BvSort n)
    BvBinPred :: KnownNat n => BvBinPred -> Term (BvSort n) -> Term (BvSort n) -> Term BoolSort
    IntToBv   :: KnownNat n => Term IntSort -> Term (BvSort n)
    FpToBv    :: (ComputableFp f) => Term (FpSort f) -> Term (BvSort (Size f))

    -- Integer terms
    IntLit        :: Integer -> Term IntSort
    IntUnExpr     :: IntUnOp -> Term IntSort -> Term IntSort
    IntBinExpr    :: IntBinOp -> Term IntSort -> Term IntSort -> Term IntSort
    IntNaryExpr   :: IntNaryOp -> [Term IntSort] -> Term IntSort
    IntBinPred    :: IntBinPred -> Term IntSort -> Term IntSort -> Term BoolSort
    PfToInt       :: (KnownNat n) => Term (PfSort n) -> Term IntSort
    BvToInt       :: (KnownNat n) => Term (BvSort n) -> Term IntSort
    SignedBvToInt :: (KnownNat n) => Term (BvSort n) -> Term IntSort
    BoolToInt     :: Term BoolSort -> Term IntSort

    -- Floating point terms
    Fp64Lit   :: Double -> Term F64
    Fp32Lit   :: Float -> Term F32
    FpUnExpr  :: (ComputableFp f) => FpUnOp -> Term (FpSort f) -> Term (FpSort f)
    FpBinExpr :: (ComputableFp f) => FpBinOp -> Term (FpSort f) -> Term (FpSort f) -> Term (FpSort f)
    FpFma     :: (ComputableFp f) => Term (FpSort f) -> Term (FpSort f) -> Term (FpSort f) -> Term (FpSort f)
    FpBinPred :: (ComputableFp f) => FpBinPred -> Term (FpSort f) -> Term (FpSort f) -> Term BoolSort
    FpUnPred  :: (ComputableFp f) => FpUnPred -> Term (FpSort f) -> Term BoolSort
    IntToFp   :: (ComputableFp f) => Term IntSort -> Term (FpSort f)
    BvToFp    :: (ComputableFp f) => Term (BvSort (Size f)) -> Term (FpSort f)
    FpToFp    :: (ComputableFp f1, ComputableFp f2) => Term (FpSort f1) -> Term (FpSort f2)

    -- Prime field terms
    PfUnExpr   :: KnownNat n => PfUnOp -> Term (PfSort n) -> Term (PfSort n)
    PfNaryExpr :: KnownNat n => PfNaryOp -> [Term (PfSort n)] -> Term (PfSort n)
    PfBinPred  :: KnownNat n => PfBinPred -> Term (PfSort n) -> Term (PfSort n) -> Term BoolSort
    IntToPf    :: KnownNat n => Term IntSort -> Term (PfSort n)

    -- Array terms
    Select   :: (Typeable k, Typeable v) => Term (ArraySort k v) -> Term k -> Term v
    Store    :: (Typeable k, Typeable v) => Term (ArraySort k v) -> Term k -> Term v -> Term (ArraySort k v)
    NewArray :: (Typeable k, Typeable v) => Term (ArraySort k v)


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
    IntNaryExpr o as -> IntNaryExpr o (map (mapTerm f) as)
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
    IntNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
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
depth = reduceTerm (const Nothing) 0 (\a b -> 1 + max a b)

nNodes :: Term s -> Int
nNodes = reduceTerm (const Nothing) 1 ((+) . (1+))

nChars :: Term s -> Int
nChars = reduceTerm visit 0 (+)
  where
    visit t = case t of
      Var s -> Just $ length s
      _ -> Nothing

type Env = Map String Dynamic

boolBinFn :: BoolBinOp -> Bool -> Bool -> Bool
boolBinFn op = case op of
    Implies -> \a b -> not a || b

boolNaryFn :: BoolNaryOp -> [Bool] -> Bool
boolNaryFn op = case op of
    And -> and
    Or -> or
    Xor -> foldr Bits.xor False

intUnFn :: IntUnOp -> Integer -> Integer
intUnFn op = case op of
    IntNeg -> (0 -)
    IntAbs -> abs

intBinFn :: IntBinOp -> Integer -> Integer -> Integer
intBinFn op = case op of
    IntSub -> (-)
    IntDiv -> div
    IntMod -> rem
    IntShl -> \a b -> Bits.shiftL a (fromIntegral b)
    IntShr -> \a b -> Bits.shiftR a (fromIntegral b)
    IntPow -> (^)

intBinPredFn :: IntBinPred -> Integer -> Integer -> Bool
intBinPredFn op = case op of
    IntNe -> (/=)
    IntEq -> (==)
    IntGe -> (>=)
    IntLe -> (<=)
    IntGt -> (>)
    IntLt -> (<)

intNaryFn :: IntNaryOp -> [Integer] -> Integer
intNaryFn op = case op of
    IntAdd -> sum
    IntMul -> product

extGCD :: Integer -> Integer -> (Integer,Integer,Integer)
extGCD a 0 = (1,0,a)     -- Base case
extGCD a b = (x, c - q*x, y)
           where q = a `div` b
                 r = a `rem` b
                 (c,x,y) = extGCD b r

invMod :: Integer -> Integer -> Integer
invMod x m = x'
    where (x', _, _) = extGCD x m

pfUnFn :: PfUnOp -> Integer -> Integer -> Integer
pfUnFn op m = case op of
    PfNeg -> (m -)
    PfRecip -> flip invMod m


pfBinPredFn :: PfBinPred -> Integer -> Integer -> Bool
pfBinPredFn op = case op of
    PfNe -> (/=)
    PfEq -> (==)

pfNaryFn :: PfNaryOp -> Integer -> [Integer] -> Integer
pfNaryFn op m = case op of
    PfAdd -> foldr (\a b -> (a + b) `rem` m) 0
    PfMul -> foldr (\a b -> (a * b) `rem` m) 1

bvBinFn :: BvBinOp -> Bv.BV -> Bv.BV -> Bv.BV
bvBinFn op = case op of
    BvShl -> Bv.shl
    BvLshr -> Bv.shr
    BvAshr -> Bv.ashr
    BvUrem -> rem
    BvUdiv -> div
    BvAdd -> (+)
    BvMul -> (*)
    BvSub -> (-)
    BvOr -> (Bv..|.)
    BvAnd -> (Bv..&.)
    BvXor -> Bv.xor

bvBinPredFn :: BvBinPred -> Bv.BV -> Bv.BV -> Bool
bvBinPredFn op = case op of
    BvEq -> (==)
    BvNe -> (/=)
    BvUgt -> (Bv.>.)
    BvUge -> (Bv.>=.)
    BvUlt -> (Bv.<.)
    BvUle -> (Bv.<=.)
    BvSgt -> Bv.sgt
    BvSge -> Bv.sge
    BvSlt -> Bv.slt
    BvSle -> Bv.sle

valAsBool :: Value BoolSort -> Bool
valAsBool (ValBool b) = b

valAsInt :: Value IntSort -> Integer
valAsInt (ValInt b) = b

valAsPf :: KnownNat n => Value (PfSort n) -> Integer
valAsPf (ValPf b) = b

valAsBv :: KnownNat n => Value (BvSort n) -> Bv.BV
valAsBv (ValBv b) = b

valAsArray :: Value (ArraySort k v) -> Map (Value k) (Value v)
valAsArray (ValArray b) = b

modulus :: forall n. KnownNat n => Term (PfSort n) -> Integer
modulus _ = natVal (Proxy :: Proxy n)

size :: forall n. KnownNat n => Term (BvSort n) -> Int
size _ = fromIntegral $ natVal $ Proxy @n

bvExtract :: forall n i. (KnownNat n, KnownNat i, i <= n) => Env -> Int -> Term (BvSort n) -> Value (BvSort i)
bvExtract env start term = ValBv $ Bv.extract start (min (oldSize - 1) (start + newSize - 1)) (valAsBv $ eval env term)
    where oldSize = fromInteger $ natVal (Proxy :: Proxy n)
          newSize = fromInteger $ natVal (Proxy :: Proxy i)

newArray :: forall k v. (Typeable k, Typeable v) => Term (ArraySort k v) -> Value (ArraySort k v)
newArray t = case t of
    NewArray -> ValArray $ (Map.empty :: (Map.Map (Value k) (Value v)))

eval :: forall s. Typeable s => Env -> Term s -> Value s
eval e t = case t of
    BoolLit b -> ValBool b
    BoolBinExpr o l r -> ValBool $ boolBinFn o (valAsBool $ eval e l) (valAsBool $ eval e r)
    BoolNaryExpr o as -> ValBool $ boolNaryFn o (map (valAsBool . eval e) as)
    Not s -> (ValBool . not . valAsBool . eval e) s

    Ite c tt ff -> if valAsBool $ eval e c then eval e tt else eval e ff
    Var s -> typed
      where
        entry = Map.findWithDefault (error $ "Unknown identifier '" ++ show s ++ "'") s e
        typed = fromDyn entry (error $ "Indentifier '" ++ show s ++ "' of wrong sort.\nValue: " ++ show entry)
    Exists {} -> error "Cannot evaluate existential quantifiers!"
    Let x s t' -> eval e' t'
        where v  = eval e s
              e' = Map.insert x (toDyn v) e

    BvConcat a b -> ValBv $ valAsBv (eval e a) `mappend` valAsBv (eval e b)
    BvExtract start t' -> bvExtract e start t'
    BvBinExpr o l r -> ValBv $ bvBinFn o (valAsBv $ eval e l) (valAsBv $ eval e r)
    BvBinPred o l r -> ValBool $ bvBinPredFn o (valAsBv $ eval e l) (valAsBv $ eval e r)
    IntToBv i -> ValBv $ Bv.bitVec (size t) $ valAsInt $ eval e i
    FpToBv tt -> ValBv $ asBits $ asRepr (eval e tt)

    IntLit i -> ValInt i
    IntUnExpr o t' -> ValInt $ intUnFn o (valAsInt $ eval e t')
    IntBinExpr o l r -> ValInt $ intBinFn o (valAsInt $ eval e l) (valAsInt $ eval e r)
    IntNaryExpr o as -> ValInt $ intNaryFn o (map (valAsInt . eval e) as)
    IntBinPred o l r -> ValBool $ intBinPredFn o (valAsInt $ eval e l) (valAsInt $ eval e r)
    BvToInt tt -> ValInt $ Bv.nat $ valAsBv $ eval e tt
    SignedBvToInt tt -> ValInt $ Bv.nat $ valAsBv $ eval e tt
    BoolToInt t' -> ValInt $ if valAsBool (eval e t') then 1 else 0
    PfToInt t' -> ValInt $ valAsPf $ eval e t'

    Fp64Lit d -> ValDouble d
    Fp32Lit f -> ValFloat f
    FpBinExpr o l r -> evalBin o (eval e l) (eval e r)
    FpUnExpr o l -> evalUn o (eval e l)
    FpBinPred o l r -> evalBinPred o (eval e l) (eval e r)
    FpUnPred o l -> evalUnPred o (eval e l)
    FpFma a b c -> eval e (FpBinExpr FpAdd (FpBinExpr FpMul a b) c)
    IntToFp tt -> evalFromInt (eval e tt)
    FpToFp tt -> (fromRepr . asOther . asRepr) (eval e tt)
    BvToFp tt -> fromRepr $ fromBits $ valAsBv (eval e tt)

    PfUnExpr o t' -> ValPf $ pfUnFn o (modulus t') (valAsPf $ eval e t')
    PfNaryExpr o as -> ValPf $ pfNaryFn o m (map (valAsPf . eval e) as)
        where m = modulus (PfNaryExpr o as)
    PfBinPred o l r -> ValBool $ pfBinPredFn o (valAsPf $ eval e l) (valAsPf $ eval e r)
    IntToPf t' -> ValPf $ valAsInt $ eval e t'

    Select a k -> Maybe.fromMaybe (error $ "Array has no entry for " ++ show k') (a' Map.!? k')
        where
            a' = valAsArray $ eval e a
            k' = eval e k
    Store a k v -> ValArray $ Map.insert (eval e k) (eval e v) (valAsArray $ eval e a)
    NewArray -> newArray t



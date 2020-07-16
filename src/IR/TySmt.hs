{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE AllowAmbiguousTypes  #-}
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

module IR.TySmt
  ( IntSort(..)
  , BoolSort(..)
  , BvSort(..)
  , PfSort(..)
  , FpSort(..)
  , ArraySort(..)
  , DynBvSort(..)
  , F32
  , F64
  , Sort(..)
  , BoolBinOp(..)
  , BoolNaryOp(..)
  , BvBinOp(..)
  , BvBinPred(..)
  , BvUnOp(..)
  , IntBinOp(..)
  , IntUnOp(..)
  , IntBinPred(..)
  , PfNaryOp(..)
  , PfUnOp(..)
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
  , toZ3
  , sortToZ3
  , evalZ3
  , mkDynBvExtract
  , mkDynBvBinExpr
  , mkDynBvConcat
  , mkDynBvBinPred
  , mkDynBvEq
  , mkDynamizeBv
  , mkStatifyBv
  , mkDynBvUnExpr
  , mkDynBvUext
  , mkDynBvSext
  , SortClass(..)
  , TermBv
  , TermDynBv
  , TermBool
  , TermInt
  , TermPf
  , TermDouble
  , sort
  , dynBvWidth
  , mkVar
  , sortDouble
  , valAsPf
  )
where

import           GHC.TypeLits
import           Control.Monad                  ( liftM2
                                                , foldM
                                                )
import           Data.Dynamic                   ( Typeable
                                                , Dynamic
                                                , fromDyn
                                                , toDyn
                                                )
import qualified Data.BitVector                as Bv
import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import qualified Data.Maybe                    as Maybe
import           Data.Bits                     as Bits
import           Data.Proxy                     ( Proxy(..) )
import           Data.Fixed                     ( mod' )
import qualified Data.Binary.IEEE754           as IEEE754
import           Z3.Monad                       ( MonadZ3 )
import qualified Z3.Monad                      as Z

data IntSort = IntSort deriving (Show,Ord,Eq,Typeable)
data BoolSort = BoolSort deriving (Show,Ord,Eq,Typeable)
data DynBvSort = DynBvSort Int deriving (Show,Ord,Eq,Typeable)
data BvSort (n :: Nat) = BvSort Int deriving (Show,Ord,Eq,Typeable)
data PfSort (n :: Nat) = PfSort Integer deriving (Show,Ord,Eq,Typeable)
data ComputableFp f => FpSort f = FpSort Int Int deriving (Show,Ord,Eq,Typeable)
data ArraySort k v = ArraySort deriving (Show,Ord,Eq,Typeable)

class (Show n, Typeable n, Ord n, Eq n) => SortClass n where
  sorted :: Maybe Sort

instance SortClass IntSort where
  sorted = Just SortInt

instance SortClass BoolSort where
  sorted = Just SortBool

instance SortClass DynBvSort where
  sorted = Nothing

instance forall n. KnownNat n => SortClass (BvSort n) where
  sorted = Just $ SortBv $ fromIntegral $ natVal $ Proxy @n

instance forall n. KnownNat n => SortClass (PfSort n) where
  sorted = Just $ SortPf $ natVal $ Proxy @n

instance ComputableFp f => SortClass (FpSort f) where
  sorted = Just $ SortFp (fromIntegral $ natVal $ Proxy @(ExpSize f))
                         (fromIntegral $ natVal $ Proxy @(SigSize f))

instance forall k v. (SortClass k, SortClass v) => SortClass (ArraySort k v) where
  sorted = do
    k' <- sorted @k
    v' <- sorted @v
    return $ SortArray k' v'

type F32 = FpSort Float
type F64 = FpSort Double

class (RealFloat fp, Typeable fp, KnownNat (Size fp), KnownNat (SigSize fp), KnownNat (ExpSize fp)) => ComputableFp fp where
    type Size fp :: Nat
    type SigSize fp :: Nat
    type ExpSize fp :: Nat
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
          FpEq -> (==)
          FpGe -> (>=)
          FpGt -> (<=)

    evalFromInt :: Value IntSort -> Value (FpSort fp)
    evalFromInt = fromRepr . fromIntegral . valAsInt

instance ComputableFp Double where
  type Size Double = 64
  type SigSize Double = 53
  type ExpSize Double = 11
  asRepr (ValDouble x) = x
  fromRepr = ValDouble
  asBits   = Bv.bitVec 64 . IEEE754.doubleToWord
  fromBits = IEEE754.wordToDouble . fromIntegral . Bv.nat

instance ComputableFp Float where
  type Size Float = 32
  type SigSize Float = 24
  type ExpSize Float = 8
  asRepr (ValFloat x) = x
  fromRepr = ValFloat
  asBits   = Bv.bitVec 32 . IEEE754.floatToWord
  fromBits = IEEE754.wordToFloat . fromIntegral . Bv.nat

data Value s where
    ValBool ::Bool -> Value BoolSort
    ValInt ::Integer -> Value IntSort
    ValBv ::KnownNat n => Bv.BV -> Value (BvSort n)
    ValDynBv ::Bv.BV -> Value DynBvSort
    ValPf ::KnownNat n => Integer -> Value (PfSort n)
    ValFloat ::Float -> Value F32
    ValDouble ::Double -> Value F64
    ValArray ::Map (Value a) (Value b) -> Value (ArraySort a b)

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

sortDouble :: Sort
sortDouble = SortFp 11 53

data BoolNaryOp = And | Or | Xor deriving (Show,Ord,Eq,Typeable)

data BoolBinOp = Implies deriving (Show,Ord,Eq,Typeable)

data IntNaryOp = IntAdd | IntMul deriving (Show, Ord, Eq, Typeable)
data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow
              deriving (Show, Ord, Eq, Typeable)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Ord, Eq, Typeable)
data IntBinPred = IntLt | IntLe | IntGt | IntGe
                deriving (Show, Ord, Eq, Typeable)

data BvUnOp = BvNeg
            | BvNot
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

data BvBinPred = BvUgt
               | BvUlt
               | BvUge
               | BvUle
               | BvSgt
               | BvSlt
               | BvSge
               | BvSle
               | BvSaddo
               | BvSsubo
               | BvSmulo
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
               | FpEq
               | FpGe
               | FpGt
               deriving (Show, Ord, Eq, Typeable)

data FpUnPred = FpIsNormal
              | FpIsSubnormal
              | FpIsZero
              | FpIsInfinite
              | FpIsNaN
              | FpIsNegative
              | FpIsPositive
              deriving (Show, Ord, Eq, Typeable)


type TermBv n = Term (BvSort n)
type TermDynBv = Term DynBvSort
type TermBool = Term BoolSort
type TermInt = Term IntSort
type TermPf n = Term (PfSort n)
type TermDouble = Term F64

data Term s where
    -- Boolean terms
    BoolLit     ::Bool -> TermBool
    BoolBinExpr ::BoolBinOp -> TermBool -> TermBool -> TermBool
    BoolNaryExpr ::BoolNaryOp -> [TermBool] -> TermBool
    Not         ::TermBool -> TermBool

    -- Core terms
    Ite    ::TermBool -> Term s -> Term s -> Term s
    Var    ::String -> Sort -> Term s
    Let    ::(Typeable s) => String -> Term s -> Term t -> Term t
    Exists ::String -> Sort -> Term t -> Term t
    Eq     :: SortClass s => Term s -> Term s -> TermBool

    -- Bit-vector terms
    BvConcat  ::(KnownNat n, KnownNat m) => TermBv n -> TermBv m -> TermBv (n + m)
    BvExtract :: forall n i. (KnownNat n, KnownNat i, i <= n) => Int -> TermBv n -> TermBv i
    BvBinExpr ::KnownNat n => BvBinOp -> TermBv n -> TermBv n -> TermBv n
    BvUnExpr  ::KnownNat n => BvUnOp -> TermBv n -> TermBv n
    BvBinPred ::KnownNat n => BvBinPred -> TermBv n -> TermBv n -> TermBool
    IntToBv   ::KnownNat n => TermInt -> TermBv n
    FpToBv    ::(ComputableFp f) => Term (FpSort f) -> Term (BvSort (Size f))

    DynamizeBv ::KnownNat n => Int -> TermBv n -> TermDynBv
    DynBvExtract ::Int -> Int -> TermDynBv -> TermDynBv
    DynBvConcat  ::Int -> TermDynBv -> TermDynBv -> TermDynBv
    DynBvBinExpr ::BvBinOp -> Int -> TermDynBv -> TermDynBv -> TermDynBv
    DynBvBinPred ::BvBinPred -> Int -> TermDynBv -> TermDynBv -> TermBool
    DynBvUnExpr  ::BvUnOp -> Int -> TermDynBv -> TermDynBv
    DynBvUext    :: Int -> TermDynBv -> TermDynBv
    DynBvSext    :: Int -> TermDynBv -> TermDynBv
    IntToDynBv   :: Int -> TermInt -> TermDynBv
    StatifyBv ::KnownNat n => TermDynBv -> TermBv n
    -- width, signedness, floating point number
    RoundFpToDynBv :: (ComputableFp f) => Int -> Bool -> Term (FpSort f) -> TermDynBv

    -- Integer terms
    IntLit        ::Integer -> TermInt
    IntUnExpr     ::IntUnOp -> TermInt -> TermInt
    IntBinExpr    ::IntBinOp -> TermInt -> TermInt -> TermInt
    IntNaryExpr   ::IntNaryOp -> [TermInt] -> TermInt
    IntBinPred    ::IntBinPred -> TermInt -> TermInt -> TermBool
    PfToInt       ::(KnownNat n) => TermPf n -> TermInt
    BvToInt       ::(KnownNat n) => TermBv n -> TermInt
    SignedBvToInt ::(KnownNat n) => TermBv n -> TermInt
    BoolToInt     ::TermBool -> TermInt

    -- Floating point terms
    Fp64Lit   ::Double -> Term F64
    Fp32Lit   ::Float -> Term F32
    FpUnExpr  ::(ComputableFp f) => FpUnOp -> Term (FpSort f) -> Term (FpSort f)
    FpBinExpr ::(ComputableFp f) => FpBinOp -> Term (FpSort f) -> Term (FpSort f) -> Term (FpSort f)
    FpFma     ::(ComputableFp f) => Term (FpSort f) -> Term (FpSort f) -> Term (FpSort f) -> Term (FpSort f)
    FpBinPred ::(ComputableFp f) => FpBinPred -> Term (FpSort f) -> Term (FpSort f) -> TermBool
    FpUnPred  ::(ComputableFp f) => FpUnPred -> Term (FpSort f) -> TermBool
    IntToFp   ::(ComputableFp f) => TermInt -> Term (FpSort f)
    BvToFp    ::(ComputableFp f) => Term (BvSort (Size f)) -> Term (FpSort f)
    FpToFp    ::(ComputableFp f1, ComputableFp f2) => Term (FpSort f1) -> Term (FpSort f2)
    DynUbvToFp    ::(ComputableFp f) => Term DynBvSort -> Term (FpSort f)
    DynSbvToFp    ::(ComputableFp f) => Term DynBvSort -> Term (FpSort f)

    -- Prime field terms
    PfUnExpr   ::KnownNat n => PfUnOp -> TermPf n -> TermPf n
    PfNaryExpr ::KnownNat n => PfNaryOp -> [TermPf n] -> TermPf n
    IntToPf    ::KnownNat n => TermInt -> TermPf n

    -- Array terms
    Select   ::(SortClass k, SortClass v) => Term (ArraySort k v) -> Term k -> Term v
    Store    ::(SortClass k, SortClass v) => Term (ArraySort k v) -> Term k -> Term v -> Term (ArraySort k v)
    NewArray ::(SortClass k, SortClass v) => Term (ArraySort k v)


deriving instance Show (Term s)
deriving instance Typeable (Term s)

mkVar :: forall s. SortClass s => String -> Term s
mkVar name = Var name (Maybe.fromJust (sorted @s))


widthErr :: Term s -> Maybe String -> Int -> Int -> a
widthErr term width expected actual =
  error $ unwords [ "In", show term, "a width", Maybe.maybe "" (\s -> "(" ++ s ++ ")") width, "should have been", show expected, "but was", show actual]

mkDynamizeBv :: forall n. KnownNat n => TermBv n -> TermDynBv
mkDynamizeBv t = DynamizeBv (fromIntegral $ natVal $ Proxy @n) t

mkStatifyBv :: forall n. KnownNat n => TermDynBv -> TermBv n
mkStatifyBv t =
  let width = dynBvWidth t
      w = fromIntegral $ natVal $ Proxy @n
  in  if width == w then StatifyBv t else widthErr (StatifyBv @n t) Nothing width w

mkDynBvExtract :: Int -> Int -> TermDynBv -> TermDynBv
mkDynBvExtract start width t =
  let w = dynBvWidth t
  in  if start + width <= w then DynBvExtract start width t else error $ unwords ["DynBvExtract too long!", "start =", show start, "width =", show width, "acutal width =", show w]

mkDynBvBinExpr :: BvBinOp -> TermDynBv -> TermDynBv -> TermDynBv
mkDynBvBinExpr o a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  if aw == bw then DynBvBinExpr o aw a b else widthErr (DynBvBinExpr o aw a b) (Just "the second width") aw bw

mkDynBvConcat :: TermDynBv -> TermDynBv -> TermDynBv
mkDynBvConcat a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  DynBvConcat (aw + bw) a b

mkDynBvBinPred :: BvBinPred -> TermDynBv -> TermDynBv -> TermBool
mkDynBvBinPred o a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  if aw == bw then DynBvBinPred o aw a b else widthErr (DynBvBinPred o aw a b) (Just "the second width") aw bw

mkDynBvUnExpr :: BvUnOp -> TermDynBv -> TermDynBv
mkDynBvUnExpr o a = DynBvUnExpr o (dynBvWidth a) a

mkDynBvSext :: Int -> TermDynBv -> TermDynBv
mkDynBvSext w a = if w >= (dynBvWidth a) then DynBvSext w a else error "sext shrink!"

mkDynBvUext :: Int -> TermDynBv -> TermDynBv
mkDynBvUext w a = if w >= (dynBvWidth a) then DynBvUext w a else error "uext shrink!"

mkDynBvEq :: TermDynBv -> TermDynBv -> TermBool
mkDynBvEq a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  if aw == bw then Eq a b else widthErr (Eq a b) (Just "the second width") aw bw

dynBvWidth :: TermDynBv -> Int
dynBvWidth t = case t of
  DynBvConcat w _ _ -> w
  DynBvExtract _ w _ -> w
  DynBvBinExpr _ w _ _ -> w
  DynBvUnExpr _ w _ -> w
  DynBvSext w _ -> w
  DynBvUext w _ -> w
  DynamizeBv w _ -> w
  RoundFpToDynBv w _ _ -> w
  IntToDynBv w _ -> w
  Ite _ tt _ -> dynBvWidth tt
  Var _ s -> case s of
    SortBv w -> w
    _ -> error "Can't deduce vare bitwidth"
  Exists _ _ tt -> dynBvWidth tt
  Let _ _ tt -> dynBvWidth tt
  Select a _ -> case sort a of
    SortArray _ (SortBv w) -> w
    _ -> error "Invalid array sort"

sort :: forall s . SortClass s => Term s -> Sort
sort t = case sorted @s of
  Just s' -> s'
  Nothing -> case t of
    Ite _ tt _           -> sort tt
    Var _ s              -> s
    Exists _ _ tt        -> sort tt
    Eq _ tt              -> sort tt
    Let    _ _ e         -> sort e

    RoundFpToDynBv w _ _ -> SortBv w
    DynBvUnExpr _ w _ -> SortBv w
    DynBvSext w _ -> SortBv w
    DynBvUext w _ -> SortBv w
    DynBvBinExpr _ w _ _ -> SortBv w
    DynBvConcat w _ _    -> SortBv w
    DynBvExtract _ w _   -> SortBv w
    DynamizeBv w _       -> SortBv w
    IntToDynBv w _ -> SortBv w
    Select a _ -> case sort a of
      SortArray _ s' -> s'
      _ -> error "Invalid array sort"

    _ -> error $ "Unreachable: " ++ show t ++ " should have static sort"

-- Given a function that optionally transforms a term, traverses the term
-- applying that function at every stage. When the function returns something,
-- this is the transformation. When the function does not, the transformation
-- recurses.
mapTerm :: (forall t . Term t -> Maybe (Term t)) -> Term s -> Term s
mapTerm f t = case f t of
  Nothing -> case t of
    BoolLit{}            -> t
    BoolBinExpr o l r    -> BoolBinExpr o (mapTerm f l) (mapTerm f r)
    BoolNaryExpr o as    -> BoolNaryExpr o (map (mapTerm f) as)
    Not s                -> Not (mapTerm f s)

    Ite c tt ff          -> Ite (mapTerm f c) (mapTerm f tt) (mapTerm f ff)
    Var{}                -> t
    Exists v s tt        -> Exists v s (mapTerm f tt)
    Let    v s e         -> Let v (mapTerm f s) (mapTerm f e)
    Eq a b               -> Eq (mapTerm f a) (mapTerm f b)

    BvConcat  a b        -> BvConcat (mapTerm f a) (mapTerm f b)
    BvExtract i s        -> BvExtract i (mapTerm f s)
    BvBinExpr o l r      -> BvBinExpr o (mapTerm f l) (mapTerm f r)
    BvBinPred o l r      -> BvBinPred o (mapTerm f l) (mapTerm f r)
    BvUnExpr o r         -> BvUnExpr o (mapTerm f r)
    IntToBv   tt         -> IntToBv (mapTerm f tt)
    FpToBv    tt         -> FpToBv (mapTerm f tt)

    StatifyBv t'         -> StatifyBv (mapTerm f t')
    RoundFpToDynBv w s t'-> RoundFpToDynBv w s (mapTerm f t')
    DynBvBinExpr o w a b -> DynBvBinExpr o w (mapTerm f a) (mapTerm f b)
    DynBvConcat w a b    -> DynBvConcat w (mapTerm f a) (mapTerm f b)
    DynBvBinPred o w a b -> DynBvBinPred o w (mapTerm f a) (mapTerm f b)
    DynBvExtract s l b   -> DynBvExtract s l (mapTerm f b)
    DynBvUnExpr o w r    -> DynBvUnExpr o w (mapTerm f r)
    DynBvSext w r        -> DynBvSext w (mapTerm f r)
    DynBvUext w r        -> DynBvUext w (mapTerm f r)
    DynamizeBv w b       -> DynamizeBv w (mapTerm f b)
    IntToDynBv w i       -> IntToDynBv w (mapTerm f i)


    IntLit{}             -> t
    IntBinExpr o l r     -> IntBinExpr o (mapTerm f l) (mapTerm f r)
    IntNaryExpr o as     -> IntNaryExpr o (map (mapTerm f) as)
    IntUnExpr   o l      -> IntUnExpr o (mapTerm f l)
    IntBinPred o l r     -> IntBinPred o (mapTerm f l) (mapTerm f r)
    BvToInt       tt     -> BvToInt (mapTerm f tt)
    SignedBvToInt tt     -> SignedBvToInt (mapTerm f tt)
    BoolToInt     tt     -> BoolToInt (mapTerm f tt)
    PfToInt       tt     -> PfToInt (mapTerm f tt)

    Fp64Lit{}            -> t
    Fp32Lit{}            -> t
    FpBinExpr o l r      -> FpBinExpr o (mapTerm f l) (mapTerm f r)
    FpUnExpr o l         -> FpUnExpr o (mapTerm f l)
    FpBinPred o l r      -> FpBinPred o (mapTerm f l) (mapTerm f r)
    FpUnPred o l         -> FpUnPred o (mapTerm f l)
    FpFma a b c          -> FpFma (mapTerm f a) (mapTerm f b) (mapTerm f c)
    IntToFp tt           -> IntToFp (mapTerm f tt)
    FpToFp  tt           -> FpToFp (mapTerm f tt)
    BvToFp  tt           -> BvToFp (mapTerm f tt)
    DynUbvToFp tt        -> DynUbvToFp (mapTerm f tt)
    DynSbvToFp tt        -> DynSbvToFp (mapTerm f tt)

    PfNaryExpr o as      -> PfNaryExpr o (map (mapTerm f) as)
    PfUnExpr   o l       -> PfUnExpr o (mapTerm f l)
    IntToPf tt           -> IntToPf (mapTerm f tt)

    Select a k           -> Select (mapTerm f a) (mapTerm f k)
    Store a k v          -> Store (mapTerm f a) (mapTerm f k) (mapTerm f v)
    NewArray             -> t
  Just s -> s


reduceTerm
  :: (forall t . Term t -> Maybe k) -> k -> (k -> k -> k) -> Term s -> k
reduceTerm mapF i foldF t = case mapF t of
  Nothing -> case t of
    BoolLit{} -> i
    BoolBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BoolNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    Not s             -> reduceTerm mapF i foldF s

    Ite c tt ff       -> foldF
      (foldF (reduceTerm mapF i foldF c) (reduceTerm mapF i foldF tt))
      (reduceTerm mapF i foldF ff)
    Var{} -> i
    Exists _ _ tt -> reduceTerm mapF i foldF tt
    Let _ s e -> foldF (reduceTerm mapF i foldF s) (reduceTerm mapF i foldF e)
    Eq tt ff       -> foldF
      (reduceTerm mapF i foldF tt)
      (reduceTerm mapF i foldF ff)

    BvConcat a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    BvUnExpr _ s -> reduceTerm mapF i foldF s
    BvExtract _ s -> reduceTerm mapF i foldF s
    BvBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntToBv   tt -> reduceTerm mapF i foldF tt
    FpToBv    tt -> reduceTerm mapF i foldF tt

    StatifyBv t' -> reduceTerm mapF i foldF t'
    RoundFpToDynBv _ _ t' -> reduceTerm mapF i foldF t'
    DynBvBinExpr _ _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvConcat _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvBinPred _ _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvExtract _ _ b -> reduceTerm mapF i foldF b
    DynBvUnExpr _ _ b -> reduceTerm mapF i foldF b
    DynBvSext _ b -> reduceTerm mapF i foldF b
    DynBvUext _ b -> reduceTerm mapF i foldF b
    DynamizeBv _ b     -> reduceTerm mapF i foldF b
    IntToDynBv _ i'      -> reduceTerm mapF i foldF i'


    IntLit{}           -> i
    IntBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    IntUnExpr   _ l  -> reduceTerm mapF i foldF l
    IntBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvToInt       tt -> reduceTerm mapF i foldF tt
    SignedBvToInt tt -> reduceTerm mapF i foldF tt
    BoolToInt     tt -> reduceTerm mapF i foldF tt
    PfToInt       tt -> reduceTerm mapF i foldF tt

    Fp64Lit{}        -> i
    Fp32Lit{}        -> i
    FpBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    FpUnExpr _ l -> reduceTerm mapF i foldF l
    FpBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    FpUnPred _ l -> reduceTerm mapF i foldF l
    FpFma a b c  -> foldF
      (foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b))
      (reduceTerm mapF i foldF c)
    IntToFp tt      -> reduceTerm mapF i foldF tt
    FpToFp  tt      -> reduceTerm mapF i foldF tt
    BvToFp  tt      -> reduceTerm mapF i foldF tt
    DynUbvToFp tt   -> reduceTerm mapF i foldF tt
    DynSbvToFp tt   -> reduceTerm mapF i foldF tt

    PfNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    PfUnExpr   _ l  -> reduceTerm mapF i foldF l
    IntToPf tt  -> reduceTerm mapF i foldF tt

    Select a k  -> foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k)
    Store a k v -> foldF
      (foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k))
      (reduceTerm mapF i foldF v)
    NewArray -> i
  Just s -> s

depth :: Term s -> Int
depth = reduceTerm (const Nothing) 0 (\a b -> 1 + max a b)

nNodes :: Term s -> Int
nNodes = reduceTerm (const Nothing) 1 ((+) . (1 +))

nChars :: Term s -> Int
nChars = reduceTerm visit 0 (+)
 where
  visit t = case t of
    Var s _ -> Just $ length s
    _       -> Nothing

type Env = Map String Dynamic

boolBinFn :: BoolBinOp -> Bool -> Bool -> Bool
boolBinFn op = case op of
  Implies -> \a b -> not a || b

boolNaryFn :: BoolNaryOp -> [Bool] -> Bool
boolNaryFn op = case op of
  And -> and
  Or  -> or
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
  IntGe -> (>=)
  IntLe -> (<=)
  IntGt -> (>)
  IntLt -> (<)

intNaryFn :: IntNaryOp -> [Integer] -> Integer
intNaryFn op = case op of
  IntAdd -> sum
  IntMul -> product

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (1, 0, a)     -- Base case
extGCD a b = (x, c - q * x, y)
 where
  q         = a `div` b
  r         = a `rem` b
  (c, x, y) = extGCD b r

invMod :: Integer -> Integer -> Integer
invMod x m = x' where (x', _, _) = extGCD x m

pfUnFn :: PfUnOp -> Integer -> Integer -> Integer
pfUnFn op m = case op of
  PfNeg   -> (m -)
  PfRecip -> flip invMod m


pfNaryFn :: PfNaryOp -> Integer -> [Integer] -> Integer
pfNaryFn op m = case op of
  PfAdd -> foldr (\a b -> (a + b) `rem` m) 0
  PfMul -> foldr (\a b -> (a * b) `rem` m) 1

bvUnFn :: BvUnOp -> Bv.BV -> Bv.BV
bvUnFn op = case op of
  BvNot  -> Bv.not
  BvNeg -> negate

bvBinFn :: BvBinOp -> Bv.BV -> Bv.BV -> Bv.BV
bvBinFn op = case op of
  BvShl  -> Bv.shl
  BvLshr -> Bv.shr
  BvAshr -> Bv.ashr
  BvUrem -> rem
  BvUdiv -> div
  BvAdd  -> (+)
  BvMul  -> (*)
  BvSub  -> (-)
  BvOr   -> (Bv..|.)
  BvAnd  -> (Bv..&.)
  BvXor  -> Bv.xor

bvBinPredFn :: BvBinPred -> Bv.BV -> Bv.BV -> Bool
bvBinPredFn op = case op of
  BvUgt -> (Bv.>.)
  BvUge -> (Bv.>=.)
  BvUlt -> (Bv.<.)
  BvUle -> (Bv.<=.)
  BvSgt -> Bv.sgt
  BvSge -> Bv.sge
  BvSlt -> Bv.slt
  BvSle -> Bv.sle
  BvSaddo -> \a b -> let
    s = Bv.size a
    v = Bv.int a + Bv.int b
    in v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v
  BvSsubo -> \a b -> let
    s = Bv.size a
    v = Bv.int a - Bv.int b
    in v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v
  BvSmulo -> \a b -> let
    s = Bv.size a
    v = Bv.int a * Bv.int b
    in v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v

valAsBool :: Value BoolSort -> Bool
valAsBool (ValBool b) = b

valAsInt :: Value IntSort -> Integer
valAsInt (ValInt b) = b

valAsPf :: KnownNat n => Value (PfSort n) -> Integer
valAsPf (ValPf b) = b

valAsBv :: KnownNat n => Value (BvSort n) -> Bv.BV
valAsBv (ValBv b) = b

valAsDynBv :: Value DynBvSort -> Bv.BV
valAsDynBv (ValDynBv b) = b

valAsArray :: Value (ArraySort k v) -> Map (Value k) (Value v)
valAsArray (ValArray b) = b

modulus :: forall n . KnownNat n => TermPf n -> Integer
modulus _ = natVal (Proxy :: Proxy n)

size :: forall n . KnownNat n => TermBv n -> Int
size _ = fromIntegral $ natVal $ Proxy @n


newArray
  :: forall k v
   . (Typeable k, Typeable v)
  => Term (ArraySort k v)
  -> Value (ArraySort k v)
newArray t = case t of
  NewArray -> ValArray $ (Map.empty :: (Map.Map (Value k) (Value v)))
  _        -> error "should be unreachable"

eval :: forall s . Typeable s => Env -> Term s -> Value s
eval e t = case t of
  BoolLit b -> ValBool b
  BoolBinExpr o l r ->
    ValBool $ boolBinFn o (valAsBool $ eval e l) (valAsBool $ eval e r)
  BoolNaryExpr o as -> ValBool $ boolNaryFn o (map (valAsBool . eval e) as)
  Not s             -> (ValBool . not . valAsBool . eval e) s

  Ite c tt ff       -> if valAsBool $ eval e c then eval e tt else eval e ff
  Eq tt ff       -> ValBool $ eval e tt == eval e ff
  Var s _           -> typed
   where
    entry =
      Map.findWithDefault (error $ "Unknown identifier '" ++ show s ++ "'") s e
    typed = fromDyn
      entry
      (  error
      $  "Indentifier '"
      ++ show s
      ++ "' of wrong sort.\nValue: "
      ++ show entry
      )
  Exists{}   -> error "Cannot evaluate existential quantifiers!"
  Let x s t' -> eval e' t'
   where
    v  = eval e s
    e' = Map.insert x (toDyn v) e

  BvConcat  a      b  -> ValBv $ valAsBv (eval e a) `mappend` valAsBv (eval e b)
  BvUnExpr o t' -> ValBv $ bvUnFn o $ valAsBv (eval e t')
  BvExtract start' t' -> bvExtract e start' t'
   where
    bvExtract
      :: forall n i
       . (KnownNat n, KnownNat i, i <= n)
      => Env
      -> Int
      -> TermBv n
      -> Value (BvSort i)
    bvExtract env start term = ValBv $ Bv.extract
      (min (oldSize - 1) (start + newSize - 1))
      start
      (valAsBv $ eval env term)
     where
      oldSize = fromInteger $ natVal (Proxy :: Proxy n)
      newSize = fromInteger $ natVal (Proxy :: Proxy i)
  BvBinExpr o l r ->
    ValBv $ bvBinFn o (valAsBv $ eval e l) (valAsBv $ eval e r)
  BvBinPred o l r ->
    ValBool $ bvBinPredFn o (valAsBv $ eval e l) (valAsBv $ eval e r)
  IntToBv i  -> ValBv $ Bv.bitVec (size t) $ valAsInt $ eval e i
  FpToBv  tt -> ValBv $ asBits $ asRepr (eval e tt)

  StatifyBv t' ->
    let inner = valAsDynBv $ eval e t'
        width = dynBvWidth t'
    in  if Bv.width inner == width
          then ValBv inner
          else error $ "bitwidth mis-match while evaluating " ++ show t
  -- TODO: check this
  RoundFpToDynBv w _ t' ->
    let i :: Integer = round $ asRepr $ eval e t'
    in  ValDynBv $ Bv.bitVec w i
  DynBvUnExpr o w t' ->
    let inner = valAsDynBv $ eval e t'
    in  if Bv.width inner == w
          then ValDynBv $ bvUnFn o inner
          else error $ "bitwidth mis-match while evaluating " ++ show t
  DynBvUext w t' -> ValDynBv $ Bv.zeroExtend w $ valAsDynBv $ eval e t'
  DynBvSext w t' -> ValDynBv $ Bv.signExtend w $ valAsDynBv $ eval e t'
  DynBvExtract s w a ->
    let a' = valAsDynBv $ eval e a
    in  if s + w <= Bv.width a'
          then ValDynBv $ Bv.extract (s + w - 1) s a'
          else error $ "bitwidth mis-match while evaluating " ++ show t
  DynBvBinExpr o w a b ->
    let a' = valAsDynBv $ eval e a
        b' = valAsDynBv $ eval e b
    in  if w == Bv.width a' && w == Bv.width b'
          then ValDynBv $ bvBinFn o a' b'
          else error $ "bitwidth mis-match while evaluating " ++ show t
  DynBvConcat _ a b -> ValDynBv $ Bv.concat [valAsDynBv $ eval e a, valAsDynBv $ eval e b]
  DynBvBinPred o w a b ->
    let a' = valAsDynBv $ eval e a
        b' = valAsDynBv $ eval e b
    in  if w == Bv.width a' && w == Bv.width b'
          then ValBool $ bvBinPredFn o a' b'
          else error $ "bitwidth mis-match while evaluating " ++ show t
  DynamizeBv w a ->
    let a' = valAsBv $ eval e a
    in  if w == Bv.width a'
          then ValDynBv a'
          else error $ "bitwidth mis-match while evaluating " ++ show t
  IntToDynBv w i  -> ValDynBv $ Bv.bitVec w $ valAsInt $ eval e i

  IntLit i       -> ValInt i
  IntUnExpr o t' -> ValInt $ intUnFn o (valAsInt $ eval e t')
  IntBinExpr o l r ->
    ValInt $ intBinFn o (valAsInt $ eval e l) (valAsInt $ eval e r)
  IntNaryExpr o as -> ValInt $ intNaryFn o (map (valAsInt . eval e) as)
  IntBinPred o l r ->
    ValBool $ intBinPredFn o (valAsInt $ eval e l) (valAsInt $ eval e r)
  BvToInt       tt -> ValInt $ Bv.nat $ valAsBv $ eval e tt
  SignedBvToInt tt -> ValInt $ Bv.nat $ valAsBv $ eval e tt
  BoolToInt     t' -> ValInt $ if valAsBool (eval e t') then 1 else 0
  PfToInt       t' -> ValInt $ valAsPf $ eval e t'

  Fp64Lit       d  -> ValDouble d
  Fp32Lit       f  -> ValFloat f
  FpBinExpr o l r  -> evalBin o (eval e l) (eval e r)
  FpUnExpr o l     -> evalUn o (eval e l)
  FpBinPred o l r  -> evalBinPred o (eval e l) (eval e r)
  FpUnPred o l     -> evalUnPred o (eval e l)
  FpFma a b c      -> eval e (FpBinExpr FpAdd (FpBinExpr FpMul a b) c)
  IntToFp tt       -> evalFromInt (eval e tt)
  FpToFp  tt       -> (fromRepr . asOther . asRepr) (eval e tt)
  BvToFp  tt       -> fromRepr $ fromBits $ valAsBv (eval e tt)
  DynUbvToFp  tt   -> fromRepr $ fromIntegral $ Bv.nat $ valAsDynBv (eval e tt)
  DynSbvToFp  tt   -> fromRepr $ fromIntegral $ Bv.nat $ valAsDynBv (eval e tt)

  PfUnExpr   o t'  -> ValPf $ pfUnFn o (modulus t') (valAsPf $ eval e t')
  PfNaryExpr o as  -> ValPf $ pfNaryFn o m (map (valAsPf . eval e) as)
    where m = modulus (PfNaryExpr o as)
  IntToPf t' -> ValPf $ valAsInt $ eval e t'

  Select a k -> Maybe.fromMaybe
    (error $ "Array has no entry for " ++ show k')
    (a' Map.!? k')
   where
    a' = valAsArray $ eval e a
    k' = eval e k
  Store a k v ->
    ValArray $ Map.insert (eval e k) (eval e v) (valAsArray $ eval e a)
  NewArray -> newArray t


sortToZ3 :: forall z . MonadZ3 z => Sort -> z Z.Sort
sortToZ3 s = case s of
  SortBool -> Z.mkBoolSort
  SortInt -> Z.mkIntSort
  SortBv w -> Z.mkBvSort w
  SortPf _ -> error "Prime fields"
  SortFp 11 53 -> Z.mkDoubleSort
  SortFp e si -> error $ unwords ["Fp" , show e, show si, "unsupported"]
  SortArray k v -> do
    k' <- sortToZ3 k
    v' <- sortToZ3 v
    Z.mkArraySort k' v'

toZ3 :: forall s z . (SortClass s, MonadZ3 z) => Term s -> z Z.AST
toZ3 t = case t of
  BoolLit b               -> Z.mkBool b
  BoolBinExpr Implies l r -> tyBinZ3Bin Z.mkImplies l r
  BoolNaryExpr o a        -> case o of
    Xor -> tyNaryZ3Bin Z.mkXor a
    And -> tyNaryZ3Nary Z.mkAnd a
    Or  -> tyNaryZ3Nary Z.mkOr a
  Not s               -> toZ3 s >>= Z.mkNot

  Ite c tt ff         -> tyTernZ3Tern Z.mkIte c tt ff
  Var name s'         -> do
    s'' <- sortToZ3 s'
    name' <- Z.mkStringSymbol name
    Z.mkVar name' s''
  Exists{}            -> error "NYI"
  Let _x _s _t'       -> error "NYI"
  Eq tt ff         -> tyBinZ3Bin Z.mkEq tt ff

  BvConcat  a      b  -> tyBinZ3Bin Z.mkConcat a b
  BvUnExpr o b  -> toZ3 b >>= case o of
    BvNeg -> Z.mkBvneg
    BvNot -> Z.mkBvnot
  BvExtract start' t' -> bvExtract start' t t'
   where
    -- We pass the original term in to get its width, encoded in its type.
    bvExtract
      :: forall n i
       . (KnownNat n, KnownNat i, i <= n)
      => Int
      -> TermBv i
      -> TermBv n
      -> z Z.AST
    bvExtract start _term innerTerm = toZ3 innerTerm >>= Z.mkExtract start end
     where
      end     = start + newSize - 1
      newSize = fromInteger $ natVal (Proxy :: Proxy i)
  BvBinExpr o l r -> tyBinZ3Bin (bvBinOpToZ3 o) l r
  BvBinPred o l r -> tyBinZ3Bin (bvBinPredToZ3 o) l r
  IntToBv i -> toZ3 i >>= Z.mkInt2bv (width t)
   where
    width :: forall n . KnownNat n => TermBv n -> Int
    width _ = fromInteger $ natVal (Proxy @n)
  FpToBv tt      -> toZ3 tt >>= Z.mkFpIEEEBv

  DynamizeBv _ i -> toZ3 i
  StatifyBv i -> toZ3 i
  RoundFpToDynBv _w _s _i -> nyi "RoundFpToDynBv"
  DynBvBinExpr o _ l r -> tyBinZ3Bin (bvBinOpToZ3 o) l r
  DynBvUnExpr o _ b  -> toZ3 b >>= case o of
    BvNeg -> Z.mkBvneg
    BvNot -> Z.mkBvnot
  DynBvSext w b  -> toZ3 b >>= Z.mkSignExt w
  DynBvUext w b  -> toZ3 b >>= Z.mkZeroExt w
  DynBvConcat _ l r -> tyBinZ3Bin Z.mkConcat l r
  DynBvBinPred o _ l r -> tyBinZ3Bin (bvBinPredToZ3 o) l r
  DynBvExtract s w i -> toZ3 i >>= Z.mkExtract (w + s - 1) s
  IntToDynBv w i -> toZ3 i >>= Z.mkInt2bv w

  IntLit i       -> Z.mkInteger i
  IntUnExpr o t' -> case o of
    IntNeg -> toZ3 t' >>= Z.mkUnaryMinus
    IntAbs -> nyi o
  IntBinExpr o l r -> case o of
    IntDiv -> tyBinZ3Bin Z.mkDiv l r
    IntMod -> tyBinZ3Bin Z.mkMod l r
    IntSub -> tyBinZ3Nary Z.mkSub l r
    _      -> nyi o
  IntNaryExpr o as -> tyNaryZ3Nary
    (case o of
      IntAdd -> Z.mkAdd
      IntMul -> Z.mkMul
    )
    as
  IntBinPred o l r ->
    let f = case o of
          IntLt -> Z.mkLt
          IntLe -> Z.mkLe
          IntGt -> Z.mkGt
          IntGe -> Z.mkGe
    in  tyBinZ3Bin f l r
  BvToInt       tt -> toZ3 tt >>= flip Z.mkBv2int False
  SignedBvToInt tt -> toZ3 tt >>= flip Z.mkBv2int True
  BoolToInt     t' -> toZ3 $ Ite t' (IntLit 1) (IntLit 0)
  PfToInt{}        -> nyi "Prime fields"

  Fp64Lit d        -> Z.mkDoubleSort >>= Z.mkFpFromDouble d
  Fp32Lit{}        -> nyi "floats"
  FpBinExpr o l r ->

    let wrapRound g a b = do 
           m <- Z.mkFpRoundToNearestTiesToEven
           g m a b
        f :: Z.AST -> Z.AST -> z Z.AST = case o of
          FpAdd -> wrapRound Z.mkFpAdd
          FpSub -> wrapRound Z.mkFpSub
          FpMul -> wrapRound Z.mkFpMul
          FpDiv -> wrapRound Z.mkFpDiv
          FpRem -> Z.mkFpRem
          FpMax -> Z.mkFpMax
          FpMin -> Z.mkFpMin
    in  tyBinZ3Bin f l r
  FpUnExpr o l ->
    let f = case o of
          FpNeg   -> Z.mkFpNeg
          FpAbs   -> Z.mkFpAbs
          FpSqrt  -> nyi o
          FpRound -> nyi "Fp rounding"
    in  toZ3 l >>= f
  FpBinPred o l r ->
    let f = case o of
          FpLe -> Z.mkFpLeq
          FpLt -> Z.mkFpLt
          FpEq -> Z.mkFpEq
          FpGe -> Z.mkFpGeq
          FpGt -> Z.mkFpGt
    in  tyBinZ3Bin f l r

  FpUnPred o l ->
    let f = case o of
          FpIsNormal    -> nyi o
          FpIsSubnormal -> nyi o
          FpIsZero      -> Z.mkFpIsZero
          FpIsInfinite  -> Z.mkFpIsInf
          FpIsNaN       -> Z.mkFpIsNan
          FpIsNegative  -> Z.mkFpIsNeg
          FpIsPositive  -> Z.mkFpIsPos
    in  toZ3 l >>= f

  FpFma{}      -> nyi "fused multiply-add"
  IntToFp{}    -> nyi "IntToFp"
  FpToFp{}     -> nyi "FpToFp"
  BvToFp{}     -> nyi "BvToFp"
  DynUbvToFp{} -> nyi "DynUbvToFp"
  DynSbvToFp{} -> nyi "DynSbvToFp"

  PfUnExpr{}   -> nyi "Prime fields"
  PfNaryExpr{} -> nyi "Prime fields"
  IntToPf{}    -> nyi "Prime fields"

  Select a k   -> tyBinZ3Bin Z.mkSelect a k
  Store a k v  -> tyTernZ3Tern Z.mkStore a k v
  NewArray     -> nyi "Need to define default values..."
 where
  tyNaryZ3Nary :: (SortClass s', MonadZ3 z) => ([Z.AST] -> z Z.AST) -> [Term s'] -> z Z.AST
  tyNaryZ3Nary f a = mapM toZ3 a >>= f
  tyBinZ3Nary
    :: (SortClass s', MonadZ3 z) => ([Z.AST] -> z Z.AST) -> Term s' -> Term s' -> z Z.AST
  tyBinZ3Nary f a b = do
    a' <- toZ3 a
    b' <- toZ3 b
    f [a', b']
  tyNaryZ3Bin
    :: (SortClass s', MonadZ3 z) => (Z.AST -> Z.AST -> z Z.AST) -> [Term s'] -> z Z.AST
  tyNaryZ3Bin f a = do
    a' <- mapM toZ3 a
    foldM f (head a') (tail a')
  tyBinZ3Bin
    :: (SortClass s', SortClass s'', MonadZ3 z)
    => (Z.AST -> Z.AST -> z Z.AST)
    -> Term s'
    -> Term s''
    -> z Z.AST
  tyBinZ3Bin f a b = do
    a' <- toZ3 a
    b' <- toZ3 b
    f a' b'
  tyTernZ3Tern
    :: (SortClass s', SortClass s'', SortClass s''', MonadZ3 z)
    => (Z.AST -> Z.AST -> Z.AST -> z Z.AST)
    -> Term s'
    -> Term s''
    -> Term s'''
    -> z Z.AST
  tyTernZ3Tern f a b c = do
    a' <- toZ3 a
    tyBinZ3Bin (f a') b c
  nyi x = error $ unwords ["Not yet implemented in toZ3:", show x]
  bvBinOpToZ3 o = case o of
          BvShl  -> Z.mkBvshl
          BvLshr -> Z.mkBvlshr
          BvAshr -> Z.mkBvashr
          BvUrem -> Z.mkBvurem
          BvUdiv -> Z.mkBvudiv
          BvAdd  -> Z.mkBvadd
          BvMul  -> Z.mkBvmul
          BvSub  -> Z.mkBvsub
          BvOr   -> Z.mkBvor
          BvAnd  -> Z.mkBvand
          BvXor  -> Z.mkBvxor
  bvBinPredToZ3 o = case o of
          BvUgt -> Z.mkBvugt
          BvUlt -> Z.mkBvult
          BvUge -> Z.mkBvuge
          BvUle -> Z.mkBvule
          BvSgt -> Z.mkBvsgt
          BvSlt -> Z.mkBvslt
          BvSge -> Z.mkBvsge
          BvSle -> Z.mkBvsle
          -- TODO: underflow?
          BvSaddo -> \a b -> do
            x <- Z.mkBvaddNoOverflow a b True
            y <- Z.mkBvaddNoUnderflow a b
            Z.mkAnd [x, y] >>= Z.mkNot
          BvSsubo -> \a b -> do
            x <- Z.mkBvsubNoOverflow a b
            y <- Z.mkBvsubNoUnderflow a b
            Z.mkAnd [x, y] >>= Z.mkNot
          BvSmulo -> \a b -> do
            x <- Z.mkBvmulNoOverflow a b True
            y <- Z.mkBvmulNoUnderflow a b
            Z.mkAnd [x, y] >>= Z.mkNot

-- Returns Nothing if UNSAT, or a string description of the model.
evalZ3 :: TermBool -> IO (Maybe String)
evalZ3 term =
  Z.evalZ3 $ do
    assertion <- toZ3 term
    Z.assert assertion
    m <- Z.getModel
    case snd m of
      Just model -> do
        s <- Z.modelToString model
        return $ Just s
      Nothing -> return Nothing

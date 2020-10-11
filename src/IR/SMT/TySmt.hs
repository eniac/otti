{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}

module IR.SMT.TySmt
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
  , boolNaryId
  , BvBinOp(..)
  , BvBinPred(..)
  , BvUnOp(..)
  , IntBinOp(..)
  , IntNaryOp(..)
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
  , mapTermM
  , reduceTerm
  , depth
  , eval
  , nNodes
  , vars
  , nChars
  , mkDynBvExtract
  , mkDynBvBinExpr
  , mkDynBvConcat
  , mkDynBvBinPred
  , mkEq
  , mkIte
  , mkDynamizeBv
  , mkStatifyBv
  , mkDynBvUnExpr
  , mkDynBvUext
  , mkDynBvSext
  , mkSelect
  , mkStore
  , SortClass(..)
  , TermBv
  , TermDynBv
  , TermBool
  , TermInt
  , TermPf
  , TermDouble
  , TermFloat
  , TermArray
  , sort
  , ComputableFp
  , dynBvWidth
  , bvWidth
  , asVarName
  , mkVar
  , sortDouble
  , sortFloat
  , valAsPf
  , valAsBool
  , valAsDynBv
  , SortError(..)
  )
where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( liftM
                                                , liftM2
                                                , liftM3
                                                )
import qualified Data.Binary.IEEE754           as IEEE754
import           Data.Bits                     as Bits
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , Typeable
                                                , fromDyn
                                                , toDyn
                                                )
import           Data.Fixed                     ( mod' )
import           Data.Hashable  ( Hashable(hashWithSalt))
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Typeable                  ( cast )
import           GHC.TypeLits
import           GHC.Generics (Generic)
import           Prelude                 hiding ( exp )
import qualified IR.SMT.TySmt.DefaultMap       as DMap
import           IR.SMT.TySmt.DefaultMap        ( DefaultMap )

data IntSort = IntSort deriving (Show,Ord,Eq,Typeable,Generic,Hashable)
data BoolSort = BoolSort deriving (Show,Ord,Eq,Typeable,Generic,Hashable)
data DynBvSort = DynBvSort !Int deriving (Show,Ord,Eq,Typeable,Generic,Hashable)
data BvSort (n :: Nat) = BvSort !Int deriving (Show,Ord,Eq,Typeable,Generic,Hashable)
data PfSort (n :: Nat) = PfSort !Integer deriving (Show,Ord,Eq,Typeable,Generic,Hashable)
data FpSort f = FpSort !Int !Int deriving (Show,Ord,Eq,Typeable,Generic,Hashable)
data ArraySort k v = ArraySort deriving (Show,Ord,Eq,Typeable,Generic,Hashable)

class (Show n, Typeable n, Ord n, Eq n, Hashable n) => SortClass n where
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
          FpNeg   -> negate
          FpAbs   -> abs
          FpSqrt  -> sqrt
          FpRound -> fromIntegral @Integer . round

    evalUnPred :: FpUnPred -> (Value (FpSort fp)) -> (Value BoolSort)
    evalUnPred op = ValBool . f . asRepr
      where
        f :: fp -> Bool
        f = case op of
          FpIsNormal    -> not . isDenormalized
          FpIsSubnormal -> isDenormalized
          FpIsZero      -> (==0.0)
          FpIsInfinite  -> isInfinite
          FpIsNaN       -> isNaN
          FpIsNegative  -> liftM2 (||) (<0.0) isNegativeZero
          FpIsPositive  -> liftM2 (||) (>0.0) (isNegativeZero . negate)

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
    ValBool   :: !Bool -> Value BoolSort
    ValInt    :: !Integer -> Value IntSort
    ValBv     :: KnownNat n => !Bv.BV -> Value (BvSort n)
    ValDynBv  :: !Bv.BV -> Value DynBvSort
    ValPf     :: KnownNat n => !Integer -> Value (PfSort n)
    ValFloat  :: !Float -> Value F32
    ValDouble :: !Double -> Value F64
    ValArray  :: !(DefaultMap (Value a) (Value b)) -> Value (ArraySort a b)

deriving instance Typeable (Value s)
deriving instance Show (Value s)
deriving instance Eq (Value s)
deriving instance Ord (Value s)
instance Hashable (Value s) where
  hashWithSalt s v = case v of
    ValBool b -> s # t 0 # b
    ValInt b -> s # t 0 # b
    ValBv b -> s # t 0 # (HashBv b)
    ValDynBv b -> s # t 0 # (HashBv b)
    ValPf i -> s # t 0 # i
    ValFloat i -> s # t 0 # i
    ValDouble i -> s # t 0 # i
    ValArray m -> s # t 0 # m
   where
    -- Help type inference
    t :: Int -> Int
    t = id

newtype HashBv = HashBv Bv.BV
instance Hashable HashBv where
  hashWithSalt s (HashBv b) = s # Bv.size b # Bv.nat b

data Sort = SortInt
          | SortBool
          | SortBv !Int
          | SortPf !Integer
          | SortFp !Int !Int
          | SortArray !Sort !Sort
          deriving (Show,Ord,Eq,Typeable,Generic,Hashable)

sortDouble :: Sort
sortDouble = SortFp 11 53

sortFloat :: Sort
sortFloat = SortFp 8 24

data BoolNaryOp = And | Or | Xor deriving (Show,Ord,Eq,Typeable,Generic, Hashable)

-- Return the identity for the associative operator
boolNaryId :: BoolNaryOp -> TermBool
boolNaryId o = case o of
  And -> BoolLit True
  Or  -> BoolLit False
  Xor -> BoolLit False

data BoolBinOp = Implies deriving (Show,Ord,Eq,Typeable,Generic,Hashable)

data IntNaryOp = IntAdd | IntMul deriving (Show, Ord, Eq, Typeable, Generic, Hashable)
data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow
              deriving (Show, Ord, Eq, Typeable, Generic, Hashable)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Ord, Eq, Typeable, Generic, Hashable)
data IntBinPred = IntLt | IntLe | IntGt | IntGe
                deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

data BvUnOp = BvNeg
            | BvNot
            deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

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
             deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

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
               deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

data PfNaryOp = PfAdd | PfMul deriving (Show, Ord, Eq, Typeable, Generic, Hashable)
data PfUnOp = PfNeg | PfRecip deriving (Show, Ord, Eq, Typeable, Generic, Hashable)
data PfBinPred = PfEq | PfNe deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

data FpBinOp = FpAdd
             | FpSub
             | FpMul
             | FpDiv
             | FpRem
             | FpMax
             | FpMin
             deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

data FpUnOp = FpNeg
            | FpAbs
            | FpSqrt
            | FpRound
            deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

data FpBinPred = FpLe
               | FpLt
               | FpEq
               | FpGe
               | FpGt
               deriving (Show, Ord, Eq, Typeable, Generic, Hashable)

data FpUnPred = FpIsNormal
              | FpIsSubnormal
              | FpIsZero
              | FpIsInfinite
              | FpIsNaN
              | FpIsNegative
              | FpIsPositive
              deriving (Show, Ord, Eq, Typeable, Generic, Hashable)


type TermBv n = Term (BvSort n)
type TermFp f = Term (FpSort f)
type TermDynBv = Term DynBvSort
type TermBool = Term BoolSort
type TermInt = Term IntSort
type TermPf n = Term (PfSort n)
type TermDouble = Term F64
type TermFloat = Term F32
type TermArray k v = Term (ArraySort k v)

data Term s where
    -- Boolean terms
    BoolLit      :: !Bool -> TermBool
    BoolBinExpr  :: !BoolBinOp -> !TermBool -> !TermBool -> TermBool
    BoolNaryExpr :: !BoolNaryOp -> ![TermBool] -> TermBool
    Not          :: !TermBool -> TermBool

    -- Core terms
    Ite    :: !TermBool -> !(Term s) -> !(Term s) -> Term s
    Var    :: !String -> !Sort -> Term s
    Let    ::SortClass s => !String -> !(Term s) -> !(Term t) -> Term t
    Exists :: !String -> !Sort -> !(Term t) -> Term t
    Eq     ::SortClass s => !(Term s) -> !(Term s) -> TermBool

    -- Bit-vector terms
    BvConcat  ::(KnownNat n, KnownNat m) => !(TermBv n) -> !(TermBv m) -> TermBv (n + m)
    BvExtract ::(KnownNat n, KnownNat i, i <= n) => !Int -> !(TermBv n) -> TermBv i
    BvBinExpr ::KnownNat n => !BvBinOp -> !(TermBv n) -> !(TermBv n) -> TermBv n
    BvUnExpr  ::KnownNat n => !BvUnOp -> !(TermBv n) -> TermBv n
    BvBinPred ::KnownNat n => !BvBinPred -> !(TermBv n) -> TermBv n -> TermBool
    IntToBv   ::KnownNat n => !TermInt -> TermBv n
    FpToBv    ::(ComputableFp f) => !(Term (FpSort f)) -> Term (BvSort (Size f))

    DynamizeBv ::KnownNat n => !Int -> !(TermBv n) -> TermDynBv
    DynBvExtract :: !Int -> !Int -> !TermDynBv -> TermDynBv
    DynBvConcat  :: !Int -> !TermDynBv -> !TermDynBv -> TermDynBv
    DynBvBinExpr :: !BvBinOp -> !Int -> !TermDynBv -> !TermDynBv -> TermDynBv
    DynBvBinPred :: !BvBinPred -> !Int -> !TermDynBv -> !TermDynBv -> TermBool
    DynBvUnExpr  :: !BvUnOp -> !Int -> !TermDynBv -> TermDynBv
    DynBvLit     :: !Bv.BV -> TermDynBv
    -- width, extension amount, inner
    DynBvUext    :: !Int -> !Int -> !TermDynBv -> TermDynBv
    -- width, extension amount, inner
    DynBvSext    :: !Int -> !Int -> !TermDynBv -> TermDynBv
    IntToDynBv   :: !Int -> !TermInt -> TermDynBv
    StatifyBv ::KnownNat n => !TermDynBv -> TermBv n
    -- width, signedness, floating point number
    RoundFpToDynBv ::(ComputableFp f) => !Int -> !Bool -> !(Term (FpSort f)) -> TermDynBv

    -- Integer terms
    IntLit        :: !Integer -> TermInt
    IntUnExpr     :: !IntUnOp -> !TermInt -> TermInt
    IntBinExpr    :: !IntBinOp -> !TermInt -> !TermInt -> TermInt
    IntNaryExpr   :: !IntNaryOp -> ![TermInt] -> TermInt
    IntBinPred    :: !IntBinPred -> !TermInt -> !TermInt -> TermBool
    PfToInt       ::(KnownNat n) => !(TermPf n) -> TermInt
    BvToInt       ::(KnownNat n) => !(TermBv n) -> TermInt
    SignedBvToInt ::(KnownNat n) => !(TermBv n) -> TermInt
    BoolToInt     :: !TermBool -> TermInt

    -- Floating point terms
    Fp64Lit   :: !Double -> Term F64
    Fp32Lit   :: !Float -> Term F32
    FpUnExpr  ::(ComputableFp f) => !FpUnOp -> TermFp f -> TermFp f
    FpBinExpr ::(ComputableFp f) => !FpBinOp -> TermFp f -> TermFp f -> TermFp f
    FpFma     ::(ComputableFp f) => !(TermFp f) -> TermFp f -> TermFp f -> TermFp f
    FpBinPred ::(ComputableFp f) => !FpBinPred -> !(TermFp f) -> TermFp f -> TermBool
    FpUnPred  ::(ComputableFp f) => !FpUnPred -> !(TermFp f) -> TermBool
    IntToFp   ::(ComputableFp f) => !TermInt -> TermFp f
    BvToFp    ::(ComputableFp f) => !(TermBv (Size f)) -> TermFp f
    FpToFp    ::(ComputableFp f1, ComputableFp f2) => !(TermFp f1) -> TermFp f2
    DynUbvToFp    ::(ComputableFp f) => !(Term DynBvSort) -> TermFp f
    DynSbvToFp    ::(ComputableFp f) => !(Term DynBvSort) -> TermFp f

    -- Prime field terms
    PfUnExpr   ::KnownNat n => !PfUnOp -> !(TermPf n) -> TermPf n
    PfNaryExpr ::KnownNat n => !PfNaryOp -> !([TermPf n]) -> TermPf n
    IntToPf    ::KnownNat n => !TermInt -> TermPf n

    -- Array terms
    Select   ::(SortClass k, SortClass v) => !(TermArray k v) -> !(Term k) -> Term v
    Store    ::(SortClass k, SortClass v) => !(TermArray k v) -> !(Term k) -> !(Term v) -> Term (ArraySort k v)
    -- domain, value
    ConstArray ::(SortClass k, SortClass v) => !Sort -> !(Term v) -> TermArray k v


deriving instance Show (Term s)
deriving instance Typeable (Term s)

widthErr :: Term s -> Maybe String -> Int -> Int -> a
widthErr term width expected actual = throw $ SortError $ unwords
  [ "In"
  , show term
  , "a width"
  , Maybe.maybe "" (\s -> "(" ++ s ++ ")") width
  , "should have been"
  , show expected
  , "but was"
  , show actual
  ]

mkDynamizeBv :: forall n . KnownNat n => TermBv n -> TermDynBv
mkDynamizeBv t = DynamizeBv (fromIntegral $ natVal $ Proxy @n) t

mkStatifyBv :: forall n . KnownNat n => TermDynBv -> TermBv n
mkStatifyBv t =
  let width = dynBvWidth t
      w     = fromIntegral $ natVal $ Proxy @n
  in  if width == w
        then StatifyBv t
        else widthErr (StatifyBv @n t) Nothing width w

mkDynBvExtract :: Int -> Int -> TermDynBv -> TermDynBv
mkDynBvExtract start width t =
  let w = dynBvWidth t
  in  if start + width <= w
        then DynBvExtract start width t
        else throw $ SortError $ unwords
          [ "DynBvExtract too long!"
          , "start ="
          , show start
          , "width ="
          , show width
          , "acutal width ="
          , show w
          ]

mkDynBvBinExpr :: BvBinOp -> TermDynBv -> TermDynBv -> TermDynBv
mkDynBvBinExpr o a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  if aw == bw
        then DynBvBinExpr o aw a b
        else widthErr (DynBvBinExpr o aw a b) (Just "the second width") aw bw

mkDynBvConcat :: TermDynBv -> TermDynBv -> TermDynBv
mkDynBvConcat a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  DynBvConcat (aw + bw) a b

mkDynBvBinPred :: BvBinPred -> TermDynBv -> TermDynBv -> TermBool
mkDynBvBinPred o a b =
  let aw = dynBvWidth a
      bw = dynBvWidth b
  in  if aw == bw
        then DynBvBinPred o aw a b
        else widthErr (DynBvBinPred o aw a b) (Just "the second width") aw bw

mkDynBvUnExpr :: BvUnOp -> TermDynBv -> TermDynBv
mkDynBvUnExpr o a = DynBvUnExpr o (dynBvWidth a) a

mkDynBvSext :: Int -> TermDynBv -> TermDynBv
mkDynBvSext newW a =
  let oldW = dynBvWidth a
  in  if newW >= oldW
        then DynBvSext newW (newW - oldW) a
        else throw $ SortError "bv_sext shrink"

mkDynBvUext :: Int -> TermDynBv -> TermDynBv
mkDynBvUext newW a =
  let oldW = dynBvWidth a
  in  if newW >= oldW
        then DynBvUext newW (newW - oldW) a
        else throw $ SortError "bv_uext shrink"

mkEq :: forall s . SortClass s => Term s -> Term s -> TermBool
mkEq a b =
  let aS = sort a
      bS = sort b
  in  if aS == bS
        then Eq a b
        else
          throw
          $  SortError
          $  "Different sorts in eq: "
          ++ show aS
          ++ " and "
          ++ show bS

mkIte :: forall s . SortClass s => TermBool -> Term s -> Term s -> Term s
mkIte c a b =
  let aS = checkSort @s (show a) $ sort a
      bS = checkSort @s (show b) $ sort b
  in  if aS == bS
        then Ite c a b
        else
          throw
          $  SortError
          $  "Different sorts in Ite: "
          ++ show aS
          ++ " and "
          ++ show bS

dynBvWidth :: TermDynBv -> Int
dynBvWidth t = case t of
  DynBvConcat  w _ _   -> w
  DynBvExtract _ w _   -> w
  DynBvBinExpr _ w _ _ -> w
  DynBvUnExpr _ w _    -> w
  DynBvLit l           -> Bv.size l
  DynBvSext w _ _      -> w
  DynBvUext w _ _      -> w
  DynamizeBv w _       -> w
  RoundFpToDynBv w _ _ -> w
  IntToDynBv w _       -> w
  Ite _ tt _           -> dynBvWidth tt
  Var _ s              -> case s of
    SortBv w -> w
    _        -> throw $ SortError "Can't deduce vare bitwidth"
  Exists _ _ tt -> dynBvWidth tt
  Let    _ _ tt -> dynBvWidth tt
  Select a _    -> case sort a of
    SortArray _ (SortBv w) -> w
    _                      -> throw $ SortError "Invalid array sort"

bvWidth :: forall n . KnownNat n => Term (BvSort n) -> Int
bvWidth _ = fromIntegral $ natVal $ Proxy @n

sort :: forall s . SortClass s => Term s -> Sort
sort t = case sorted @s of
  Just s' -> s'
  Nothing -> case t of
    Ite _ tt _           -> sort tt
    Var _ s              -> s
    Exists _ _ tt        -> sort tt
    Eq _ tt              -> sort tt
    Let            _ _ e -> sort e

    RoundFpToDynBv w _ _ -> SortBv w
    DynBvUnExpr    _ w _ -> SortBv w
    DynBvLit bv          -> SortBv (Bv.size bv)
    DynBvSext w _ _      -> SortBv w
    DynBvUext w _ _      -> SortBv w
    DynBvBinExpr _ w _ _ -> SortBv w
    DynBvConcat  w _ _   -> SortBv w
    DynBvExtract _ w _   -> SortBv w
    DynamizeBv w _       -> SortBv w
    IntToDynBv w _       -> SortBv w
    Select     a _       -> case sort a of
      SortArray _ s' -> s'
      _              -> throw $ SortError "Invalid array sort"
    Store _ k v     -> SortArray (sort k) (sort v)
    ConstArray ks v -> SortArray ks (sort v)

    _ ->
      throw
        $  SortError
        $  "Unreachable: "
        ++ show t
        ++ " should have static sort"

mkVar :: forall s . SortClass s => String -> Sort -> Term s
mkVar name sort' = Var name (checkSort @s ("Variable " ++ show name) sort')

newtype SortError = SortError String
     deriving (Show, Typeable)

instance Exception SortError


checkSort :: forall s . SortClass s => String -> Sort -> Sort
checkSort thing sort' = case sorted @s of
  Just sort'' -> if sort' == sort''
    then sort'
    else
      throw
      $  SortError
      $  thing
      ++ " constructed as a "
      ++ show sort'
      ++ " but typed as a "
      ++ show sort''
  Nothing -> sort'

mkStore
  :: forall k v
   . (SortClass k, SortClass v)
  => Term (ArraySort k v)
  -> Term k
  -> Term v
  -> Term (ArraySort k v)
mkStore a k' v' =
  let aS = checkSort @(ArraySort k v) (show a) $ sort a
      kS = checkSort @k (show k') $ sort k'
      vS = checkSort @v (show v') $ sort v'
  in  if SortArray kS vS == aS
        then Store a k' v'
        else throw $ SortError $ unwords
          [ "Cannot build store"
          , show a
          , show k'
          , show v'
          , "since the array has sort"
          , show aS
          , "and the key-value pair are for an array of sort"
          , show (SortArray kS vS)
          ]

mkSelect
  :: forall k v
   . (SortClass k, SortClass v)
  => Term (ArraySort k v)
  -> Term k
  -> Term v
mkSelect a k' =
  let aS = checkSort @(ArraySort k v) (show a) $ sort a
      kS = checkSort @k (show k') $ sort k'
  in  case aS of
        SortArray kS' vS | checkSort @v "value" vS `seq` kS' == kS ->
          Select a k'
        _ -> throw $ SortError $ unwords
          [ "Cannot build select"
          , show a
          , show k'
          , "since the array has sort"
          , show aS
          , "and the key has sort"
          , show kS
          ]

-- |
-- Given a function that optionally transforms a term, traverses the term
-- applying that function at every stage. When the function returns something,
-- this is the transformation. When the function does not, the transformation
-- recurses.
-- TODO: Define this in terms of mapTermM, and measure perf.
mapTerm
  :: SortClass s
  => (forall t . SortClass t => Term t -> Maybe (Term t))
  -> Term s
  -> Term s
mapTerm f t = case f t of
  Nothing -> case t of
    BoolLit{}             -> t
    BoolBinExpr o l r     -> BoolBinExpr o (mapTerm f l) (mapTerm f r)
    BoolNaryExpr o as     -> BoolNaryExpr o (map (mapTerm f) as)
    Not s                 -> Not (mapTerm f s)

    Ite c tt ff           -> Ite (mapTerm f c) (mapTerm f tt) (mapTerm f ff)
    Var{}                 -> t
    Exists v s tt         -> Exists v s (mapTerm f tt)
    Let    v s e          -> Let v (mapTerm f s) (mapTerm f e)
    Eq        a b         -> Eq (mapTerm f a) (mapTerm f b)

    BvConcat  a b         -> BvConcat (mapTerm f a) (mapTerm f b)
    BvExtract i s         -> BvExtract i (mapTerm f s)
    BvBinExpr o l r       -> BvBinExpr o (mapTerm f l) (mapTerm f r)
    BvBinPred o l r       -> BvBinPred o (mapTerm f l) (mapTerm f r)
    BvUnExpr o r          -> BvUnExpr o (mapTerm f r)
    IntToBv   tt          -> IntToBv (mapTerm f tt)
    FpToBv    tt          -> FpToBv (mapTerm f tt)

    StatifyBv t'          -> StatifyBv (mapTerm f t')
    RoundFpToDynBv w s t' -> RoundFpToDynBv w s (mapTerm f t')
    DynBvBinExpr o w a b  -> DynBvBinExpr o w (mapTerm f a) (mapTerm f b)
    DynBvConcat w a b     -> DynBvConcat w (mapTerm f a) (mapTerm f b)
    DynBvBinPred o w a b  -> DynBvBinPred o w (mapTerm f a) (mapTerm f b)
    DynBvExtract s l b    -> DynBvExtract s l (mapTerm f b)
    DynBvUnExpr  o w r    -> DynBvUnExpr o w (mapTerm f r)
    DynBvLit bv           -> DynBvLit bv
    DynBvSext w w' r      -> DynBvSext w w' (mapTerm f r)
    DynBvUext w w' r      -> DynBvUext w w' (mapTerm f r)
    DynamizeBv w b        -> DynamizeBv w (mapTerm f b)
    IntToDynBv w i        -> IntToDynBv w (mapTerm f i)


    IntLit{}              -> t
    IntBinExpr o l r      -> IntBinExpr o (mapTerm f l) (mapTerm f r)
    IntNaryExpr o as      -> IntNaryExpr o (map (mapTerm f) as)
    IntUnExpr   o l       -> IntUnExpr o (mapTerm f l)
    IntBinPred o l r      -> IntBinPred o (mapTerm f l) (mapTerm f r)
    BvToInt       tt      -> BvToInt (mapTerm f tt)
    SignedBvToInt tt      -> SignedBvToInt (mapTerm f tt)
    BoolToInt     tt      -> BoolToInt (mapTerm f tt)
    PfToInt       tt      -> PfToInt (mapTerm f tt)

    Fp64Lit{}             -> t
    Fp32Lit{}             -> t
    FpBinExpr o l r       -> FpBinExpr o (mapTerm f l) (mapTerm f r)
    FpUnExpr o l          -> FpUnExpr o (mapTerm f l)
    FpBinPred o l r       -> FpBinPred o (mapTerm f l) (mapTerm f r)
    FpUnPred o l          -> FpUnPred o (mapTerm f l)
    FpFma a b c           -> FpFma (mapTerm f a) (mapTerm f b) (mapTerm f c)
    IntToFp    tt         -> IntToFp (mapTerm f tt)
    FpToFp     tt         -> FpToFp (mapTerm f tt)
    BvToFp     tt         -> BvToFp (mapTerm f tt)
    DynUbvToFp tt         -> DynUbvToFp (mapTerm f tt)
    DynSbvToFp tt         -> DynSbvToFp (mapTerm f tt)

    PfNaryExpr o as       -> PfNaryExpr o (map (mapTerm f) as)
    PfUnExpr   o l        -> PfUnExpr o (mapTerm f l)
    IntToPf tt            -> IntToPf (mapTerm f tt)

    Select a k            -> Select (mapTerm f a) (mapTerm f k)
    Store a k v           -> Store (mapTerm f a) (mapTerm f k) (mapTerm f v)
    ConstArray s i        -> ConstArray s (mapTerm f i)
  Just s -> s

-- |
-- Given a function that monadically maps a term to an optional term, traverses
-- the term (left-to-right), applying the action when appropriate.
--
-- If no term is returned, does a default recursion.
mapTermM
  :: forall m s
   . (Monad m, SortClass s)
  => (forall t . SortClass t => Term t -> m (Maybe (Term t)))
  -> Term s
  -> m (Term s)
mapTermM f t = do
  a <- f t
  case a of
    Nothing -> case t of
      BoolLit{}             -> return t
      BoolBinExpr o l r     -> liftM2 (BoolBinExpr o) (rec l) (rec r)
      BoolNaryExpr o as     -> fmap (BoolNaryExpr o) (mapM rec as)
      Not s                 -> fmap Not (rec s)

      Ite c tt ff           -> liftM3 Ite (rec c) (rec tt) (rec ff)
      Var{}                 -> return t
      Exists v s tt         -> fmap (Exists v s) (rec tt)
      Let    v s e          -> liftM2 (Let v) (rec s) (rec e)
      Eq        a b         -> liftM2 Eq (rec a) (rec b)

      BvConcat  a b         -> liftM2 BvConcat (rec a) (rec b)
      BvExtract i s         -> liftM (BvExtract i) (rec s)
      BvBinExpr o l r       -> liftM2 (BvBinExpr o) (rec l) (rec r)
      BvBinPred o l r       -> liftM2 (BvBinPred o) (rec l) (rec r)
      BvUnExpr o r          -> liftM (BvUnExpr o) (rec r)
      IntToBv   tt          -> liftM IntToBv (rec tt)
      FpToBv    tt          -> liftM FpToBv (rec tt)

      StatifyBv t'          -> liftM StatifyBv (rec t')
      RoundFpToDynBv w s t' -> liftM (RoundFpToDynBv w s) (rec t')
      DynBvBinExpr o w a b  -> liftM2 (DynBvBinExpr o w) (rec a) (rec b)
      DynBvConcat w a b     -> liftM2 (DynBvConcat w) (rec a) (rec b)
      DynBvBinPred o w a b  -> liftM2 (DynBvBinPred o w) (rec a) (rec b)
      DynBvExtract s l b    -> liftM (DynBvExtract s l) (rec b)
      DynBvUnExpr  o w r    -> liftM (DynBvUnExpr o w) (rec r)
      DynBvLit bv           -> return $ DynBvLit bv
      DynBvSext w w' r      -> liftM (DynBvSext w w') (rec r)
      DynBvUext w w' r      -> liftM (DynBvUext w w') (rec r)
      DynamizeBv w b        -> liftM (DynamizeBv w) (rec b)
      IntToDynBv w i        -> liftM (IntToDynBv w) (rec i)


      IntLit{}              -> return t
      IntBinExpr o l r      -> liftM2 (IntBinExpr o) (rec l) (rec r)
      IntNaryExpr o as      -> liftM (IntNaryExpr o) (mapM (rec) as)
      IntUnExpr   o l       -> liftM (IntUnExpr o) (rec l)
      IntBinPred o l r      -> liftM2 (IntBinPred o) (rec l) (rec r)
      BvToInt       tt      -> liftM BvToInt (rec tt)
      SignedBvToInt tt      -> liftM SignedBvToInt (rec tt)
      BoolToInt     tt      -> liftM BoolToInt (rec tt)
      PfToInt       tt      -> liftM PfToInt (rec tt)

      Fp64Lit{}             -> return t
      Fp32Lit{}             -> return t
      FpBinExpr o l r       -> liftM2 (FpBinExpr o) (rec l) (rec r)
      FpUnExpr o l          -> liftM (FpUnExpr o) (rec l)
      FpBinPred o l r       -> liftM2 (FpBinPred o) (rec l) (rec r)
      FpUnPred o l          -> liftM (FpUnPred o) (rec l)
      FpFma a b c           -> liftM3 FpFma (rec a) (rec b) (rec c)
      IntToFp    tt         -> liftM IntToFp (rec tt)
      FpToFp     tt         -> liftM FpToFp (rec tt)
      BvToFp     tt         -> liftM BvToFp (rec tt)
      DynUbvToFp tt         -> liftM DynUbvToFp (rec tt)
      DynSbvToFp tt         -> liftM DynSbvToFp (rec tt)

      PfNaryExpr o as       -> liftM (PfNaryExpr o) (mapM (rec) as)
      PfUnExpr   o l        -> liftM (PfUnExpr o) (rec l)
      IntToPf tt            -> liftM IntToPf (rec tt)

      Select a k            -> liftM2 Select (rec a) (rec k)
      Store a k v           -> liftM3 Store (rec a) (rec k) (rec v)
      ConstArray s i        -> liftM (ConstArray s) (rec i)
    Just s -> return $ s
 where
  rec :: SortClass s' => Term s' -> m (Term s')
  rec = mapTermM f

reduceTerm
  :: SortClass s
  => (forall t . SortClass t => Term t -> Maybe k)
  -> k
  -> (k -> k -> k)
  -> Term s
  -> k
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
    Eq tt ff -> foldF (reduceTerm mapF i foldF tt) (reduceTerm mapF i foldF ff)

    BvConcat a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    BvUnExpr  _ s -> reduceTerm mapF i foldF s
    BvExtract _ s -> reduceTerm mapF i foldF s
    BvBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntToBv   tt          -> reduceTerm mapF i foldF tt
    FpToBv    tt          -> reduceTerm mapF i foldF tt

    StatifyBv t'          -> reduceTerm mapF i foldF t'
    RoundFpToDynBv _ _ t' -> reduceTerm mapF i foldF t'
    DynBvBinExpr _ _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvConcat _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvBinPred _ _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvExtract _ _ b -> reduceTerm mapF i foldF b
    DynBvUnExpr  _ _ b -> reduceTerm mapF i foldF b
    DynBvLit{}         -> i
    DynBvSext _ _ b    -> reduceTerm mapF i foldF b
    DynBvUext _ _ b    -> reduceTerm mapF i foldF b
    DynamizeBv _ b     -> reduceTerm mapF i foldF b
    IntToDynBv _ i'    -> reduceTerm mapF i foldF i'


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
    IntToFp    tt   -> reduceTerm mapF i foldF tt
    FpToFp     tt   -> reduceTerm mapF i foldF tt
    BvToFp     tt   -> reduceTerm mapF i foldF tt
    DynUbvToFp tt   -> reduceTerm mapF i foldF tt
    DynSbvToFp tt   -> reduceTerm mapF i foldF tt

    PfNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    PfUnExpr   _ l  -> reduceTerm mapF i foldF l
    IntToPf tt      -> reduceTerm mapF i foldF tt

    Select a k -> foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k)
    Store a k v     -> foldF
      (foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k))
      (reduceTerm mapF i foldF v)
    ConstArray _s v -> reduceTerm mapF i foldF v
  Just s -> s

asVarName :: Term s -> Maybe String
asVarName (Var n _) = Just n
asVarName _         = Nothing

depth :: SortClass s => Term s -> Int
depth = reduceTerm (const Nothing) 0 (\a b -> 1 + max a b)

nNodes :: SortClass s => Term s -> Int
nNodes = reduceTerm (const Nothing) 1 ((+) . (1 +))

vars :: SortClass s => Term s -> Set String
vars = reduceTerm visit Set.empty Set.union
 where
  visit t = case t of
    Var s _ -> Just $ Set.singleton s
    _       -> Nothing

nChars :: SortClass s => Term s -> Int
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
  BvNot -> Bv.not
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
  BvUgt   -> (Bv.>.)
  BvUge   -> (Bv.>=.)
  BvUlt   -> (Bv.<.)
  BvUle   -> (Bv.<=.)
  BvSgt   -> Bv.sgt
  BvSge   -> Bv.sge
  BvSlt   -> Bv.slt
  BvSle   -> Bv.sle
  BvSaddo -> \a b ->
    let s = Bv.size a
        v = Bv.int a + Bv.int b
    in  v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v
  BvSsubo -> \a b ->
    let s = Bv.size a
        v = Bv.int a - Bv.int b
    in  v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v
  BvSmulo -> \a b ->
    let s = Bv.size a
        v = Bv.int a * Bv.int b
    in  v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v

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

valAsArray :: Value (ArraySort k v) -> DefaultMap (Value k) (Value v)
valAsArray (ValArray b) = b

modulus :: forall n . KnownNat n => TermPf n -> Integer
modulus _ = natVal (Proxy :: Proxy n)

size :: forall n . KnownNat n => TermBv n -> Int
size _ = fromIntegral $ natVal $ Proxy @n

eval :: forall s . Typeable s => Env -> Term s -> Value s
eval e t = case t of
  BoolLit b -> ValBool b
  BoolBinExpr o l r ->
    ValBool $ boolBinFn o (valAsBool $ eval e l) (valAsBool $ eval e r)
  BoolNaryExpr o as -> ValBool $ boolNaryFn o (map (valAsBool . eval e) as)
  Not s             -> (ValBool . not . valAsBool . eval e) s

  Ite c tt ff       -> if valAsBool $ eval e c then eval e tt else eval e ff
  Eq  tt ff         -> ValBool $ eval e tt == eval e ff
  Var s  _          -> typed
   where
    entry = Map.findWithDefault
      (error $ "Unknown identifier '" ++ s ++ "'" ++ show e)
      s
      e
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
  BvUnExpr  o      t' -> ValBv $ bvUnFn o $ valAsBv (eval e t')
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
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  -- TODO: check this
  RoundFpToDynBv w _ t' ->
    let i :: Integer = round $ asRepr $ eval e t' in ValDynBv $ Bv.bitVec w i
  DynBvUnExpr o w t' ->
    let inner = valAsDynBv $ eval e t'
    in  if Bv.width inner == w
          then ValDynBv $ bvUnFn o inner
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynBvLit bv -> ValDynBv bv
  DynBvUext _ wDelta t' ->
    ValDynBv $ Bv.zeroExtend wDelta $ valAsDynBv $ eval e t'
  DynBvSext _ wDelta t' ->
    ValDynBv $ Bv.signExtend wDelta $ valAsDynBv $ eval e t'
  DynBvExtract s w a ->
    let a' = valAsDynBv $ eval e a
    in  if s + w <= Bv.width a'
          then ValDynBv $ Bv.extract (s + w - 1) s a'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynBvBinExpr o w a b ->
    let a' = valAsDynBv $ eval e a
        b' = valAsDynBv $ eval e b
    in  if w == Bv.width a' && w == Bv.width b'
          then ValDynBv $ bvBinFn o a' b'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynBvConcat _ a b ->
    ValDynBv $ Bv.concat [valAsDynBv $ eval e a, valAsDynBv $ eval e b]
  DynBvBinPred o w a b ->
    let a' = valAsDynBv $ eval e a
        b' = valAsDynBv $ eval e b
    in  if w == Bv.width a' && w == Bv.width b'
          then ValBool $ bvBinPredFn o a' b'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynamizeBv w a ->
    let a' = valAsBv $ eval e a
    in  if w == Bv.width a'
          then ValDynBv a'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  IntToDynBv w i -> ValDynBv $ Bv.bitVec w $ valAsInt $ eval e i

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
  IntToFp    tt    -> evalFromInt (eval e tt)
  FpToFp     tt    -> (fromRepr . asOther . asRepr) (eval e tt)
  BvToFp     tt    -> fromRepr $ fromBits $ valAsBv (eval e tt)
  DynUbvToFp tt    -> fromRepr $ fromIntegral $ Bv.nat $ valAsDynBv (eval e tt)
  DynSbvToFp tt    -> fromRepr $ fromIntegral $ Bv.nat $ valAsDynBv (eval e tt)

  PfUnExpr   o t'  -> ValPf $ pfUnFn o (modulus t') (valAsPf $ eval e t')
  PfNaryExpr o as  -> ValPf $ pfNaryFn o m (map (valAsPf . eval e) as)
    where m = modulus (PfNaryExpr o as)
  IntToPf t' -> ValPf $ valAsInt $ eval e t'

  Select a k -> Maybe.fromMaybe
    (error $ "Array has no entry for " ++ show k')
    (a' DMap.!? k')
   where
    a' = valAsArray $ eval e a
    k' = eval e k
  Store a k v ->
    ValArray $ DMap.insert (eval e k) (eval e v) (valAsArray $ eval e a)
  -- Not quite right
  ConstArray _ v -> newArray t $ Just $ eval e v

newArray
  :: forall k v
   . (Typeable k, Typeable v)
  => Term (ArraySort k v)
  -> Maybe (Value v)
  -> Value (ArraySort k v)
newArray _t v = case v of
  Just v' ->
    ValArray $ (DMap.emptyWithDefault v' :: (DefaultMap (Value k) (Value v)))
  Nothing -> ValArray $ (DMap.empty :: (DefaultMap (Value k) (Value v)))

(#) :: Hashable a => Int -> a -> Int
(#) = hashWithSalt

instance SortClass sort => Hashable (Term sort) where
  hashWithSalt s term =
    case term of
      BoolLit b -> s # t 0 # b
      BoolBinExpr o a b -> s # t 1 # o # a # b
      BoolNaryExpr o bs -> s # t 2 # o # bs
      Not b -> s # t 3 # b
      Ite c a b -> s # t 4 # c # a # b
      Var n sort -> s # t 5 # n # sort
      Let n v b -> s # t 6 # n # v # b
      Exists v sort b -> s # t 7 # v # sort # b
      Eq a b -> s # t 8 # a # b
      BvConcat a b -> s # t 9 # a # b
      BvExtract a b -> s # t 10 # a # b
      BvBinExpr o a b -> s # t 12 # o # a # b
      BvUnExpr o b -> s # t 11 # o # b
      BvBinPred o a b -> s # t 12 # o # a # b
      IntToBv b -> s # t 13 # b
      FpToBv b -> s # t 13 # b
      DynamizeBv i b -> s # t 14 # i # b
      DynBvConcat w a b -> s # t 15 # w # a # b
      DynBvExtract w a b -> s # t 16 # w # a # b
      DynBvBinExpr o w a b -> s # t 17 # o # w # a # b
      DynBvUnExpr o w b -> s # t 18 # o # w # b
      DynBvBinPred o w a b -> s # t 19 # o # w # a # b
      DynBvLit b -> s # t 20 # (HashBv b)
      DynBvUext w e b -> s # t 21 # w # e # b
      DynBvSext w e b -> s # t 22 # w # e # b
      IntToDynBv w b -> s # t 23 # w # b
      StatifyBv b -> s # t 24 # b
      RoundFpToDynBv w si fp -> s # t 25 # w # si # fp
      IntLit i -> s # t 26 # i
      IntUnExpr o i -> s # t 27 # o # i
      IntBinExpr o a b -> s # t 28 # o # a # b
      IntNaryExpr o is -> s # t 29 # o # is
      IntBinPred o a b -> s # t 30 # o # a # b
      PfToInt p -> s # t 31 # p
      BvToInt b -> s # t 32 # b
      SignedBvToInt b -> s # t 33 # b
      BoolToInt b -> s # t 34 # b
      Fp64Lit b -> s # t 35 # b
      Fp32Lit b -> s # t 36 # b
      FpUnExpr o b -> s # t 37 # o # b
      FpBinExpr o a b -> s # t 38 # o # a # b
      FpFma a b c -> s # t 39 # a # b # c
      FpBinPred o a b -> s # t 40 # o # a # b
      FpUnPred o b -> s # t 41 # o # b
      IntToFp i -> s # t 42 # i
      BvToFp b -> s # t 43 # b
      FpToFp b -> s # t 44 # b
      DynUbvToFp b -> s # t 45 # b
      DynSbvToFp b -> s # t 46 # b
      PfUnExpr o p -> s # t 47 # o # p
      PfNaryExpr o ps -> s # t 48 # o # ps
      IntToPf i -> s # t 49 # i
      Select a i -> s # t 50 # a # i
      Store a i v -> s # t 51 # a # i # v
      ConstArray ks v -> s # t 52 # ks # v
   where
    -- Hash tags
    t :: Int -> Int
    t = id


-- Tries to case the values to the same type
dynEq :: (Typeable a, Typeable b) => Term a -> Term b -> Bool
dynEq a b = Just a == cast b

-- BEGIN (MOSTLY) AUTOGENERATED
-- Most of this comes from GHC's derive implementation of Eq
-- I've replaced == with `dynEq` in strategic places.
instance Eq (Term s) where
  (==) (BoolLit a1_abUE) (BoolLit b1_abUF) = ((a1_abUE == b1_abUF))
  (==) (BoolBinExpr a1_abUG a2_abUH a3_abUI) (BoolBinExpr b1_abUJ b2_abUK b3_abUL)
    = (  ((a1_abUG == b1_abUJ))
      && (((a2_abUH == b2_abUK)) && ((a3_abUI == b3_abUL)))
      )
  (==) (BoolNaryExpr a1_abUM a2_abUN) (BoolNaryExpr b1_abUO b2_abUP) =
    (((a1_abUM == b1_abUO)) && ((a2_abUN == b2_abUP)))
  (==) (Not a1_abUQ) (Not b1_abUR) = ((a1_abUQ == b1_abUR))
  (==) (Ite a1_abUS a2_abUT a3_abUU) (Ite b1_abUV b2_abUW b3_abUX) =
    (  ((a1_abUS == b1_abUV))
    && (((a2_abUT == b2_abUW)) && ((a3_abUU == b3_abUX)))
    )
  (==) (Var a1_abUY a2_abUZ) (Var b1_abV0 b2_abV1) =
    (((a1_abUY == b1_abV0)) && ((a2_abUZ == b2_abV1)))
  (==) (Let a1_abV2 a2_abV3 a3_abV4) (Let b1_abV5 b2_abV6 b3_abV7) =
    (  ((a1_abV2 == b1_abV5))
    && (((dynEq a2_abV3 b2_abV6)) && ((a3_abV4 == b3_abV7)))
    )
  (==) (Exists a1_abV8 a2_abV9 a3_abVa) (Exists b1_abVb b2_abVc b3_abVd) =
    (  ((a1_abV8 == b1_abVb))
    && (((a2_abV9 == b2_abVc)) && ((a3_abVa == b3_abVd)))
    )
  (==) (Eq a1_abVe a2_abVf) (Eq b1_abVg b2_abVh) =
    (((a1_abVe `dynEq` b1_abVg)) && ((a2_abVf `dynEq` b2_abVh)))
  (==) (BvConcat a1_abVi a2_abVj) (BvConcat b1_abVk b2_abVl) =
    (((a1_abVi `dynEq` b1_abVk)) && ((a2_abVj `dynEq` b2_abVl)))
  (==) (BvExtract a1_abVm a2_abVn) (BvExtract b1_abVo b2_abVp) =
    (((a1_abVm == b1_abVo)) && ((a2_abVn `dynEq` b2_abVp)))
  (==) (BvBinExpr a1_abVq a2_abVr a3_abVs) (BvBinExpr b1_abVt b2_abVu b3_abVv)
    = (  ((a1_abVq == b1_abVt))
      && (((a2_abVr == b2_abVu)) && ((a3_abVs == b3_abVv)))
      )
  (==) (BvUnExpr a1_abVw a2_abVx) (BvUnExpr b1_abVy b2_abVz) =
    (((a1_abVw == b1_abVy)) && ((a2_abVx == b2_abVz)))
  (==) (BvBinPred a1_abVA a2_abVB a3_abVC) (BvBinPred b1_abVD b2_abVE b3_abVF)
    = (  ((a1_abVA == b1_abVD))
      && (((a2_abVB `dynEq` b2_abVE)) && ((a3_abVC `dynEq` b3_abVF)))
      )
  (==) (IntToBv a1_abVG) (IntToBv b1_abVH) = ((a1_abVG == b1_abVH))
  (==) (FpToBv  a1_abVI) (FpToBv  b1_abVJ) = ((a1_abVI `dynEq` b1_abVJ))
  (==) (DynamizeBv a1_abVK a2_abVL) (DynamizeBv b1_abVM b2_abVN) =
    (((a1_abVK == b1_abVM)) && ((a2_abVL `dynEq` b2_abVN)))
  (==) (DynBvExtract a1_abVO a2_abVP a3_abVQ) (DynBvExtract b1_abVR b2_abVS b3_abVT)
    = (  ((a1_abVO == b1_abVR))
      && (((a2_abVP == b2_abVS)) && ((a3_abVQ == b3_abVT)))
      )
  (==) (DynBvConcat a1_abVU a2_abVV a3_abVW) (DynBvConcat b1_abVX b2_abVY b3_abVZ)
    = (  ((a1_abVU == b1_abVX))
      && (((a2_abVV == b2_abVY)) && ((a3_abVW == b3_abVZ)))
      )
  (==) (DynBvBinExpr a1_abW0 a2_abW1 a3_abW2 a4_abW3) (DynBvBinExpr b1_abW4 b2_abW5 b3_abW6 b4_abW7)
    = (  ((a1_abW0 == b1_abW4))
      && (  ((a2_abW1 == b2_abW5))
         && (((a3_abW2 == b3_abW6)) && ((a4_abW3 == b4_abW7)))
         )
      )
  (==) (DynBvBinPred a1_abW8 a2_abW9 a3_abWa a4_abWb) (DynBvBinPred b1_abWc b2_abWd b3_abWe b4_abWf)
    = (  ((a1_abW8 == b1_abWc))
      && (  ((a2_abW9 == b2_abWd))
         && (((a3_abWa == b3_abWe)) && ((a4_abWb == b4_abWf)))
         )
      )
  (==) (DynBvUnExpr a1_abWg a2_abWh a3_abWi) (DynBvUnExpr b1_abWj b2_abWk b3_abWl)
    = (  ((a1_abWg == b1_abWj))
      && (((a2_abWh == b2_abWk)) && ((a3_abWi == b3_abWl)))
      )
  (==) (DynBvLit a1_abWg) (DynBvLit b1_abWj) = (a1_abWg == b1_abWj)
  (==) (DynBvUext a1_abWm a2_abWn a) (DynBvUext b1_abWo b2_abWp b) =
    (((a1_abWm == b1_abWo)) && ((a2_abWn == b2_abWp)) && (a == b))
  (==) (DynBvSext a1_abWq a2_abWr a) (DynBvSext b1_abWs b2_abWt b) =
    (((a1_abWq == b1_abWs)) && ((a2_abWr == b2_abWt)) && (a == b))
  (==) (IntToDynBv a1_abWu a2_abWv) (IntToDynBv b1_abWw b2_abWx) =
    (((a1_abWu == b1_abWw)) && ((a2_abWv == b2_abWx)))
  (==) (StatifyBv a1_abWy) (StatifyBv b1_abWz) = ((a1_abWy == b1_abWz))
  (==) (RoundFpToDynBv a1_abWA a2_abWB a3_abWC) (RoundFpToDynBv b1_abWD b2_abWE b3_abWF)
    = (  ((a1_abWA == b1_abWD))
      && (((a2_abWB == b2_abWE)) && ((a3_abWC `dynEq` b3_abWF)))
      )
  (==) (IntLit a1_abWG) (IntLit b1_abWH) = ((a1_abWG == b1_abWH))
  (==) (IntUnExpr a1_abWI a2_abWJ) (IntUnExpr b1_abWK b2_abWL) =
    (((a1_abWI == b1_abWK)) && ((a2_abWJ == b2_abWL)))
  (==) (IntBinExpr a1_abWM a2_abWN a3_abWO) (IntBinExpr b1_abWP b2_abWQ b3_abWR)
    = (  ((a1_abWM == b1_abWP))
      && (((a2_abWN == b2_abWQ)) && ((a3_abWO == b3_abWR)))
      )
  (==) (IntNaryExpr a1_abWS a2_abWT) (IntNaryExpr b1_abWU b2_abWV) =
    (((a1_abWS == b1_abWU)) && ((a2_abWT == b2_abWV)))
  (==) (IntBinPred a1_abWW a2_abWX a3_abWY) (IntBinPred b1_abWZ b2_abX0 b3_abX1)
    = (  ((a1_abWW == b1_abWZ))
      && (((a2_abWX == b2_abX0)) && ((a3_abWY == b3_abX1)))
      )
  (==) (PfToInt a1_abX2) (PfToInt b1_abX3) = ((a1_abX2 `dynEq` b1_abX3))
  (==) (BvToInt a1_abX4) (BvToInt b1_abX5) = ((a1_abX4 `dynEq` b1_abX5))
  (==) (SignedBvToInt a1_abX6) (SignedBvToInt b1_abX7) =
    ((a1_abX6 `dynEq` b1_abX7))
  (==) (BoolToInt a1_abX8) (BoolToInt b1_abX9) = ((a1_abX8 == b1_abX9))
  (==) (Fp64Lit   a1_abXa) (Fp64Lit   b1_abXb) = ((a1_abXa == b1_abXb))
  (==) (Fp32Lit   a1_abXc) (Fp32Lit   b1_abXd) = ((a1_abXc == b1_abXd))
  (==) (FpUnExpr a1_abXe a2_abXf) (FpUnExpr b1_abXg b2_abXh) =
    (((a1_abXe == b1_abXg)) && ((a2_abXf == b2_abXh)))
  (==) (FpBinExpr a1_abXi a2_abXj a3_abXk) (FpBinExpr b1_abXl b2_abXm b3_abXn)
    = (  ((a1_abXi == b1_abXl))
      && (((a2_abXj == b2_abXm)) && ((a3_abXk == b3_abXn)))
      )
  (==) (FpFma a1_abXo a2_abXp a3_abXq) (FpFma b1_abXr b2_abXs b3_abXt) =
    (  ((a1_abXo == b1_abXr))
    && (((a2_abXp == b2_abXs)) && ((a3_abXq == b3_abXt)))
    )
  (==) (FpBinPred a1_abXu a2_abXv a3_abXw) (FpBinPred b1_abXx b2_abXy b3_abXz)
    = (  ((a1_abXu == b1_abXx))
      && (((a2_abXv `dynEq` b2_abXy)) && ((a3_abXw `dynEq` b3_abXz)))
      )
  (==) (FpUnPred a1_abXA a2_abXB) (FpUnPred b1_abXC b2_abXD) =
    (((a1_abXA == b1_abXC)) && ((a2_abXB `dynEq` b2_abXD)))
  (==) (IntToFp    a1_abXE) (IntToFp    b1_abXF) = ((a1_abXE == b1_abXF))
  (==) (BvToFp     a1_abXG) (BvToFp     b1_abXH) = ((a1_abXG == b1_abXH))
  (==) (FpToFp     a1_abXI) (FpToFp     b1_abXJ) = ((a1_abXI `dynEq` b1_abXJ))
  (==) (DynUbvToFp a1_abXK) (DynUbvToFp b1_abXL) = ((a1_abXK == b1_abXL))
  (==) (DynSbvToFp a1_abXM) (DynSbvToFp b1_abXN) = ((a1_abXM == b1_abXN))
  (==) (PfUnExpr a1_abXO a2_abXP) (PfUnExpr b1_abXQ b2_abXR) =
    (((a1_abXO == b1_abXQ)) && ((a2_abXP == b2_abXR)))
  (==) (PfNaryExpr a1_abXS a2_abXT) (PfNaryExpr b1_abXU b2_abXV) =
    (((a1_abXS == b1_abXU)) && ((a2_abXT == b2_abXV)))
  (==) (IntToPf a1_abXW) (IntToPf b1_abXX) = ((a1_abXW == b1_abXX))
  (==) (Select a1_abXY a2_abXZ) (Select b1_abY0 b2_abY1) =
    (((a1_abXY `dynEq` b1_abY0)) && ((a2_abXZ `dynEq` b2_abY1)))
  (==) (Store a1_abY2 a2_abY3 a3_abY4) (Store b1_abY5 b2_abY6 b3_abY7) =
    (  ((a1_abY2 == b1_abY5))
    && (((a2_abY3 == b2_abY6)) && ((a3_abY4 == b3_abY7)))
    )
  (==) (ConstArray s1 t1) (ConstArray s2 t2) = (s1 == s2) && (t1 == t2)
  (==) _                  _                  = False
-- END (MOSTLY) AUTOGENERATED

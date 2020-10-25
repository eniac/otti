{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
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
  , BvNaryOp(..)
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
  , mkDynBvExtract
  , mkDynBvExtractBit
  , mkDynBvBinExpr
  , mkDynBvNaryExpr
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
  , dynBvWidth
  , bvWidth
  , asVarName
  , mkVar
  , sortDouble
  , sortFloat
  , valAsArray
  , valAsBool
  , valAsBv
  , valAsDynBv
  , valAsInt
  , valAsPf
  , SortError(..)
  , ComputableFp(..)
  )
where

import           Control.Exception              ( Exception
                                                , throw
                                                )
import           Control.Monad                  ( liftM2
                                                )
import qualified Data.Binary.IEEE754           as IEEE754
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Typeable )
import           Data.Fixed                     ( mod' )
import           Data.Hashable  ( Hashable(hashWithSalt))
import           Data.List                      ( group )
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy(..) )
import           Data.Typeable                  ( cast )
import           GHC.TypeLits
import           GHC.Generics (Generic)
import           Prelude                 hiding ( exp )
import           IR.SMT.TySmt.DefaultMap        ( DefaultMap )
import           Text.Read

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
    ValBv b -> s # t 0 # HashBv b
    ValDynBv b -> s # t 0 # HashBv b
    ValPf i -> s # t 0 # i
    ValFloat i -> s # t 0 # i
    ValDouble i -> s # t 0 # i
    ValArray m -> s # t 0 # m
   where
    -- Help type inference
    t :: Int -> Int
    t = id

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

asVarName :: Term s -> Maybe String
asVarName (Var n _) = Just n
asVarName _         = Nothing

newtype HashBv = HashBv Bv.BV
instance Hashable HashBv where
  hashWithSalt s (HashBv b) = s # Bv.size b # Bv.nat b

data Sort = SortInt
          | SortBool
          | SortBv !Int
          | SortPf !Integer
          | SortFp !Int !Int
          | SortArray !Sort !Sort
          deriving (Show,Read,Ord,Eq,Typeable,Generic,Hashable)

--instance Show Sort where
--  -- Omit the order for SortPf. It's just ugly.
--  show s = case s of
--    SortInt -> "SortInt"
--    SortBool -> "SortBool"
--    SortBv i -> "(SortBv " ++ show i ++ ")"
--    SortFp i j -> "(SortFp " ++ show i ++ " " ++ show j ++ ")"
--    SortArray i j -> "(SortArray " ++ show i ++ " " ++ show j ++ ")"
--    SortPf _ -> "SortPf"

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

data IntNaryOp = IntAdd | IntMul deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)
data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow
              deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)
data IntBinPred = IntLt | IntLe | IntGt | IntGe
                deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data BvUnOp = BvNeg
            | BvNot
            deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data BvNaryOp = BvAdd
              | BvMul
              | BvOr
              | BvAnd
              | BvXor
              deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data BvBinOp = BvShl
             | BvLshr
             | BvAshr
             | BvUrem
             | BvUdiv
             | BvSub
             deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

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
               deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data PfNaryOp = PfAdd | PfMul deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)
data PfUnOp = PfNeg | PfRecip deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)
data PfBinPred = PfEq | PfNe deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data FpBinOp = FpAdd
             | FpSub
             | FpMul
             | FpDiv
             | FpRem
             | FpMax
             | FpMin
             deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data FpUnOp = FpNeg
            | FpAbs
            | FpSqrt
            | FpRound
            deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data FpBinPred = FpLe
               | FpLt
               | FpEq
               | FpGe
               | FpGt
               deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)

data FpUnPred = FpIsNormal
              | FpIsSubnormal
              | FpIsZero
              | FpIsInfinite
              | FpIsNaN
              | FpIsNegative
              | FpIsPositive
              deriving (Show, Read, Ord, Eq, Typeable, Generic, Hashable)


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
    BvNaryExpr::KnownNat n => !BvNaryOp -> ![TermBv n] -> TermBv n
    BvUnExpr  ::KnownNat n => !BvUnOp -> !(TermBv n) -> TermBv n
    BvBinPred ::KnownNat n => !BvBinPred -> !(TermBv n) -> TermBv n -> TermBool
    IntToBv   ::KnownNat n => !TermInt -> TermBv n
    FpToBv    ::(ComputableFp f) => !(Term (FpSort f)) -> Term (BvSort (Size f))

    DynamizeBv ::KnownNat n => !Int -> !(TermBv n) -> TermDynBv
    -- extract start, extraction width
    DynBvExtract :: !Int -> !Int -> !TermDynBv -> TermDynBv
    DynBvExtractBit :: !Int -> !TermDynBv -> TermBool
    DynBvConcat  :: !Int -> !TermDynBv -> !TermDynBv -> TermDynBv
    DynBvBinExpr :: !BvBinOp -> !Int -> !TermDynBv -> !TermDynBv -> TermDynBv
    DynBvNaryExpr :: !BvNaryOp -> !Int -> ![TermDynBv] -> TermDynBv
    DynBvBinPred :: !BvBinPred -> !Int -> !TermDynBv -> !TermDynBv -> TermBool
    DynBvUnExpr  :: !BvUnOp -> !Int -> !TermDynBv -> TermDynBv
    DynBvLit     :: !Bv.BV -> TermDynBv
    -- width, extension amount, inner
    DynBvUext    :: !Int -> !Int -> !TermDynBv -> TermDynBv
    -- width, extension amount, inner
    DynBvSext    :: !Int -> !Int -> !TermDynBv -> TermDynBv
    IntToDynBv   :: !Int -> !TermInt -> TermDynBv
    PfToDynBv    :: KnownNat n => !Int -> !(TermPf n) -> TermDynBv
    BoolToDynBv  :: !TermBool -> TermDynBv
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

instance Read TermDynBv where
  readPrec = parens $ (appPrec $ do
                         Ident "Var" <- lexP
                         n :: String <- readPrec
                         s :: Sort <- readPrec
                         return $ Var n s) +++
                      (appPrec $ do
                         Ident "DynBvConcat" <- lexP
                         w :: Int <- readPrec
                         a :: TermDynBv <- readPrec
                         b :: TermDynBv <- readPrec
                         return $ DynBvConcat w a b) +++
                      (appPrec $ do
                         Ident "DynBvLit" <- lexP
                         b :: Bv.BV <- readPrec
                         return $ DynBvLit b) +++
                      (appPrec $ do
                         Ident "DynBvUext" <- lexP
                         w :: Int <- readPrec
                         dw :: Int <- readPrec
                         a :: TermDynBv <- readPrec
                         return $ DynBvUext w dw a) +++
                      (appPrec $ do
                         Ident "DynBvNaryExpr" <- lexP
                         o :: BvNaryOp <- readPrec
                         w :: Int <- readPrec
                         a :: [TermDynBv] <- readPrec
                         return $ DynBvNaryExpr o w a)

   where
    appPrec = prec 10

instance Read TermBool where
  readPrec = parens $ (appPrec $ do
                         Ident "Eq" <- lexP
                         a :: TermDynBv <- readPrec
                         b :: TermDynBv <- readPrec
                         return $ Eq a b)
   where
    appPrec = prec 10

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

mkDynBvExtractBit :: Int -> TermDynBv -> TermBool
mkDynBvExtractBit bit t = 
  let w = dynBvWidth t
  in  if bit < w
        then DynBvExtractBit bit t
        else throw $ SortError $ unwords
          [ "DynBvExtractBit OOB"
          , "bit ="
          , show bit
          , "width ="
          , show w
          ]

mkDynBvNaryExpr :: BvNaryOp -> [TermDynBv] -> TermDynBv
mkDynBvNaryExpr o bs =
  let ws = map dynBvWidth bs
  in  if length (group ws) == 1
        then DynBvNaryExpr o (head ws) bs
        else widthErr (DynBvNaryExpr o (head ws) bs) Nothing (head ws) (head $ tail ws)

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
          $  unwords ["Different sorts in Ite:" , show aS, "and", show bS]

dynBvWidth :: TermDynBv -> Int
dynBvWidth t = case t of
  DynBvConcat  w _ _   -> w
  DynBvExtract _ w _   -> w
  DynBvBinExpr _ w _ _ -> w
  DynBvNaryExpr _ w _  -> w
  DynBvUnExpr _ w _    -> w
  DynBvLit l           -> Bv.size l
  DynBvSext w _ _      -> w
  DynBvUext w _ _      -> w
  DynamizeBv w _       -> w
  RoundFpToDynBv w _ _ -> w
  IntToDynBv w _       -> w
  PfToDynBv w _        -> w
  BoolToDynBv _        -> 1
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
    DynBvNaryExpr _ w _  -> SortBv w
    DynBvConcat  w _ _   -> SortBv w
    DynBvExtract _ w _   -> SortBv w
    DynBvExtractBit {} -> SortBool
    DynamizeBv w _       -> SortBv w
    IntToDynBv w _       -> SortBv w
    PfToDynBv  w _       -> SortBv w
    BoolToDynBv  _       -> SortBv 1
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
      DynBvExtractBit i b -> s # t 53 # i # b
      PfToDynBv w b -> s # t 54 # w # b
      BoolToDynBv b -> s # t 55 # b
      BvNaryExpr o b -> s # t 55 # o # b
      DynBvNaryExpr o w b -> s # t 55 # o # w # b
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
  BvNaryExpr o1 x1 == BvNaryExpr o2 x2 = (o1 == o2) && (x1 == x2)
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
  (==) (DynBvExtractBit a0 a1) (DynBvExtractBit b0 b1) = (a0 == b0) && (a1 == b1)
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
  DynBvNaryExpr o1 w1 x1 == DynBvNaryExpr o2 w2 x2 = (o1 == o2) && (w1 == w2) && (x1 == x2)
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
  (==) (PfToDynBv a1_abWu a2_abWv) (PfToDynBv b1_abWw b2_abWx) =
    (((a1_abWu == b1_abWw)) && ((a2_abWv `dynEq` b2_abWx)))
  (==) (BoolToDynBv a1_abWy) (BoolToDynBv b1_abWz) = ((a1_abWy == b1_abWz))
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

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Codegen.Zokrates.Term
  ( Term(..)
  -- Binary
  , zAdd
  , zSub
  , zMul
  , zDiv
  , zShl
  , zShr
  , zPow
  , zBitAnd
  , zBitOr
  , zBitXor
  , zGe
  , zGt
  , zLt
  , zLe
  , zNe
  , zEq
  , zAnd
  , zOr
  -- Unary
  , zNeg
  , zNot
  -- Ternary
  , zCond
  -- Typing & casting
  , zBool
  , zConstInt
  , zType
  -- Array
  , zSpread
  , zSlice
  , zArray
  , zArrayGet
  , zArraySet
  -- Struct
  , zFieldGet
  , zFieldSet
  -- Infra
  , zTermVars
  -- Built-ins
  , zU32fromBits
  , zU32toBits
  , zFieldtoBits
  )
where

import           Control.Monad
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify                ( Embeddable(..) )
import qualified Codegen.Zokrates.Type         as T
import           Codegen.LangVal                ( InMap
                                                , setInputFromMap
                                                )
import qualified Data.BitVector                as Bv
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map.Strict               as Map
import           Data.List                      ( group )
import           Data.Proxy                     ( Proxy(..) )
import qualified Data.Set                      as Set
import qualified IR.SMT.TySmt                  as S
import qualified IR.SMT.TySmt.Alg              as SAlg
import qualified IR.SMT.Assert                 as Assert
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Util.Log

data Term n = BitInt Int S.TermDynBv
            | Field  (S.TermPf n)
            | Bool   S.TermBool
            | Array Int [Term n]
            | Struct String (Map.Map String (Term n))
            deriving (Show)

wrapBin
  :: forall n
   . String
  -> Maybe (S.TermDynBv -> S.TermDynBv -> S.TermDynBv)
  -> Maybe (S.TermPf n -> S.TermPf n -> S.TermPf n)
  -> Maybe (S.TermBool -> S.TermBool -> S.TermBool)
  -> Term n
  -> Term n
  -> Either String (Term n)
wrapBin name fI fF fB a b = case (a, b, fI, fF, fB) of
  (BitInt w0 a', BitInt w1 b', Just f, _, _) | w0 == w1 ->
    Right $ BitInt w0 $ f a' b'
  (Field a', Field b', _, Just f, _) -> Right $ Field $ f a' b'
  (Bool a', Bool b', _, _, Just f) -> Right $ Bool $ f a' b'
  _ -> Left $ unwords
    ["Cannot perform operation", show name, "on\n", show a, "and\n", show b]

wrapBinPred
  :: forall n
   . String
  -> Maybe (S.TermDynBv -> S.TermDynBv -> S.TermBool)
  -> Maybe (S.TermPf n -> S.TermPf n -> S.TermBool)
  -> Maybe (S.TermBool -> S.TermBool -> S.TermBool)
  -> Term n
  -> Term n
  -> Either String (Term n)
wrapBinPred name fI fF fB a b = case (a, b, fI, fF, fB) of
  (BitInt w0 a', BitInt w1 b', Just f, _, _) | w0 == w1 ->
    Right $ Bool $ f a' b'
  (Field a', Field b', _, Just f, _) -> Right $ Bool $ f a' b'
  (Bool a', Bool b', _, _, Just f) -> Right $ Bool $ f a' b'
  _ -> Left $ unwords
    ["Cannot perform operation", show name, "on", show a, "and", show b]

zAdd, zSub, zMul, zDiv, zShl, zShr, zPow, zBitAnd, zBitOr, zBitXor, zGe, zGt, zLt, zLe, zNe, zEq, zAnd, zOr
  :: KnownNat n => Term n -> Term n -> Either String (Term n)


ne :: S.SortClass s => S.Term s -> S.Term s -> S.TermBool
ne x y = S.Not $ S.mkEq x y

-- Binarize an nary fn
bin :: ([a] -> t) -> a -> a -> t
bin f x y = f [x, y]

zAdd = wrapBin "+"
               (Just $ bin $ S.mkDynBvNaryExpr S.BvAdd)
               (Just $ bin (S.PfNaryExpr S.PfAdd))
               Nothing
zSub = wrapBin
  "-"
  (Just $ S.mkDynBvBinExpr S.BvSub)
  (Just $ \a b -> S.PfNaryExpr S.PfAdd [a, S.PfUnExpr S.PfNeg b])
  Nothing
zMul = wrapBin "*"
               (Just $ bin $ S.mkDynBvNaryExpr S.BvMul)
               (Just $ bin (S.PfNaryExpr S.PfMul))
               Nothing
zDiv = wrapBin
  "/"
  (Just $ S.mkDynBvBinExpr S.BvUdiv)
  (Just $ \a b -> S.PfNaryExpr S.PfMul [a, S.PfUnExpr S.PfRecip b])
  Nothing
zPow = wrapBin "**" Nothing (Just undefined) Nothing
zBitAnd = wrapBin "&" (Just $ bin $ S.mkDynBvNaryExpr S.BvAnd) Nothing Nothing
zBitOr = wrapBin "|" (Just $ bin $ S.mkDynBvNaryExpr S.BvOr) Nothing Nothing
zBitXor = wrapBin "^" (Just $ bin $ S.mkDynBvNaryExpr S.BvXor) Nothing Nothing
zAnd = wrapBin "&&" Nothing Nothing (Just $ bin (S.BoolNaryExpr S.And))
zOr = wrapBin "||" Nothing Nothing (Just $ bin (S.BoolNaryExpr S.Or))
zEq = wrapBinPred "==" (Just S.mkEq) (Just S.mkEq) (Just S.mkEq)
zNe = wrapBinPred "!=" (Just ne) (Just ne) (Just ne)
zGe = wrapBinPred ">=" (Just $ S.mkDynBvBinPred S.BvUge) Nothing Nothing
zGt = wrapBinPred ">" (Just $ S.mkDynBvBinPred S.BvUgt) Nothing Nothing
zLe = wrapBinPred "<=" (Just $ S.mkDynBvBinPred S.BvUle) Nothing Nothing
zLt = wrapBinPred "<" (Just $ S.mkDynBvBinPred S.BvUlt) Nothing Nothing

wrapShift
  :: KnownNat n
  => String
  -> S.BvBinOp
  -> Term n
  -> Term n
  -> Either String (Term n)
wrapShift name op a b = do
  amt <- toInteger <$> zConstInt b
  case a of
    (BitInt w0 a') ->
      Right $ BitInt w0 $ S.mkDynBvBinExpr op a' $ Mem.bvNum False w0 amt
    _ -> Left $ unwords
      ["Cannot perform operation", show name, "on\n", show a, "and\n", show b]
zShl = wrapShift "<<" S.BvShl
zShr = wrapShift ">>" S.BvLshr

wrapUn
  :: forall n
   . String
  -> Maybe (S.TermDynBv -> S.TermDynBv)
  -> Maybe (S.TermPf n -> S.TermPf n)
  -> Maybe (S.TermBool -> S.TermBool)
  -> Term n
  -> Either String (Term n)
wrapUn name fI fF fB a = case (a, fI, fF, fB) of
  (BitInt w a', Just f, _, _) -> Right $ BitInt w $ f a'
  (Field a', _, Just f, _) -> Right $ Field $ f a'
  (Bool a', _, _, Just f) -> Right $ Bool $ f a'
  _ -> Left $ unwords ["Cannot perform operation", show name, "on", show a]

zNeg, zNot :: KnownNat n => Term n -> Either String (Term n)
zNeg = wrapUn "unary-"
              (Just $ S.mkDynBvUnExpr S.BvNeg)
              (Just $ S.PfUnExpr S.PfNeg)
              Nothing
zNot = wrapUn "!" (Just $ S.mkDynBvUnExpr S.BvNot) Nothing (Just S.Not)

type Field n = (String, Term n)
zIte :: KnownNat n => S.TermBool -> Term n -> Term n -> Either String (Term n)
zIte c a b = case (a, b) of
  (Bool  x, Bool y )                      -> Right $ Bool $ S.mkIte c x y
  (Field x, Field y)                      -> Right $ Field $ S.mkIte c x y
  (BitInt w0 x, BitInt w1 y) | w0 == w1   -> Right $ BitInt w0 $ S.mkIte c x y
  (Array w0 x, Array w1 y) | w0 == w1     -> Array w0 <$> zipWithM (zIte c) x y
  (Struct n0 xs, Struct n1 ys) | n0 == n1 -> do
    ps <- zipWithM zipField (Map.toAscList xs) (Map.toAscList ys)
    return $ Struct n0 $ Map.fromList ps
   where
    zipField :: KnownNat n => Field n -> Field n -> Either String (Field n)
    zipField (f0, x) (f1, y) = if f0 == f1
      then (f0, ) <$> zIte c x y
      else Left $ unwords ["Field mismatch in ite:", show f0, "v", show f1]
  _ -> Left $ unwords ["Cannot perform ITE on", show a, "and", show b]

zBool :: KnownNat n => Term n -> Either String S.TermBool
zBool t = case t of
  Bool b -> Right b
  _      -> Left $ show t ++ " is not bool"

zConstInt :: KnownNat n => Term n -> Either String Int
zConstInt t = case t of
  BitInt _ (S.DynBvLit b) -> Right $ fromIntegral $ Bv.nat b
  Field (S.IntToPf (S.IntLit f)) -> Right $ fromIntegral f
  _ -> Left $ show t ++ " is not a constant integer"

zCond :: KnownNat n => Term n -> Term n -> Term n -> Either String (Term n)
zCond c t f = do
  c' <- zBool c
  zIte c' t f

zSlice
  :: KnownNat n => Term n -> Maybe Int -> Maybe Int -> Either String (Term n)
zSlice t a b = case t of
  Array d items ->
    let a' = fromMaybe 0 a
        w  = fromMaybe d b - a'
    in  Right $ Array w $ take w $ drop a' items
  _ -> Left $ unwords ["Cannot slice", show t]

zSpread :: KnownNat n => Term n -> Either String [Term n]
zSpread t = case t of
  Array _ items -> Right items
  _             -> Left $ unwords ["Cannot spread", show t]

zType :: KnownNat n => Term n -> T.Type
zType t = case t of
  BitInt i _  -> T.Uint i
  Bool  _     -> T.Bool
  Field _     -> T.Field
  Struct n fs -> T.Struct n $ Map.map zType fs
  Array d xs ->
    let tys = map zType xs
    in  case length (group tys) of
          0 -> error "Cannot get type of empty array"
          1 -> T.Array d (head tys)
          _ -> error $ "Different types in " ++ show t

zFieldGet :: KnownNat n => String -> Term n -> Either String (Term n)
zFieldGet f t = case t of
  Struct _ fs | Map.member f fs -> Right $ fs Map.! f
  _ -> Left $ unwords ["Cannot find field", show f, "in", show t]

zFieldSet :: KnownNat n => String -> Term n -> Term n -> Either String (Term n)
zFieldSet f v t = case t of
  Struct n fs | Map.member f fs -> Right $ Struct n $ Map.adjust (const v) f fs
  _ -> Left $ unwords ["Cannot find field", show f, "in", show t]

zArrayGet :: KnownNat n => Term n -> Term n -> Either String (Term n)
zArrayGet i a = case (i, a) of
  -- TODO bounds check index
  (Field i', Array d xs) | d > 0 ->
    foldM (\acc (j, v) -> zIte (S.mkEq i' (S.IntToPf $ S.IntLit j)) v acc)
          (head xs)
      $ zip [1 ..]
      $ tail xs
  _ -> Left $ unwords ["Cannot get index", show i, "from array", show a]

zArraySet :: KnownNat n => Term n -> Term n -> Term n -> Either String (Term n)
zArraySet i v a = case (i, a) of
  -- TODO bounds check index?? Less clear here.
  (Field i', Array d xs) | d > 0 ->
    Array d
      <$> zipWithM (\j -> zIte (S.mkEq i' (S.IntToPf $ S.IntLit j)) v) [0 ..] xs
  _ -> Left $ unwords ["Cannot get index", show i, "from array", show a]

zArray :: KnownNat n => [Term n] -> Either String (Term n)
zArray ts = if length (group $ map zType ts) < 2
  then Right $ Array (length ts) ts
  else Left $ unwords ["Cannot build array from", show ts]

--   The first name is required, and is an SMT name that should be a prefix of all generated SMT variables
--   The second name is optional, and is a user-visible name.
--   If present, this varaiable is user-visible, and if values are being
--   computed, the @declare@ function is responsible for getting its value
--   from the user.
zDeclare
  :: forall n
   . KnownNat n
  => Maybe InMap
  -> T.Type
  -> String
  -> Maybe String
  -> Mem.Mem (Term n)
zDeclare inputs ty name mUserName = case ty of
  T.Uint w -> declBase (S.ValDynBv . Bv.bitVec w)
                       (S.ValDynBv $ Bv.zeros w)
                       (S.SortBv w)
                       (BitInt w)
  T.Field -> declBase S.ValPf (S.ValPf 0) (S.SortPf $ natVal (Proxy @n)) Field
  T.Bool  -> declBase (S.ValBool . (/= 0)) (S.ValBool False) S.SortBool Bool
  T.Array s inner ->
    let rec i = zDeclare inputs inner (aName i name) (aName i <$> mUserName)
    in  Array s <$> forM [0 .. (s - 1)] rec
  T.Struct n fs ->
    let rec (f, t) =
            (f, ) <$> zDeclare inputs t (sName f name) (sName f <$> mUserName)
    in  Struct n . Map.fromList <$> forM (Map.toList fs) rec
 where
  declBase
    :: S.SortClass s
    => (Integer -> S.Value s)
    -> S.Value s
    -> S.Sort
    -> (S.Term s -> Term n)
    -> Mem.Mem (Term n)
  declBase parse default_ sort toTerm = Assert.liftAssert $ do
    logIf "decl" $ "declBase: " ++ name ++ " " ++ show mUserName
    t <- Assert.newVar name sort
    setInputFromMap inputs parse default_ name mUserName
    return $ toTerm t

-- | Create a new set of variables for @term@, which have variable names
-- prefixed by @name@
zAlias :: forall n . KnownNat n => String -> Term n -> Assert.Assert (Term n)
zAlias name term = case term of
  Bool b      -> base S.SortBool b Bool
  BitInt w i  -> base (S.SortBv w) i (BitInt w)
  Field f     -> base (S.SortPf $ natVal (Proxy @n)) f Field
  Struct n fs -> Struct n . Map.fromList <$> forM
    (Map.toList fs)
    (\(f, t) -> (f, ) <$> zAlias (sName f name) t)
  Array n xs -> Array n <$> zipWithM (\i -> zAlias (aName i name)) [0 ..] xs
 where
  base
    :: S.SortClass s
    => S.Sort
    -> S.Term s
    -> (S.Term s -> Term n)
    -> Assert.Assert (Term n)
  base sort term toTerm = do
    v <- Assert.newVar name sort
    Assert.assign v term
    return $ toTerm v

-- | Array index name
aName :: Int -> String -> String
aName idx base = base ++ "_" ++ show idx
-- | Struct field name
sName :: String -> String -> String
sName field base = base ++ "_" ++ field

zAssign :: KnownNat n => T.Type -> String -> Term n -> Mem.Mem (Term n, Term n)
zAssign ty name term = if zType term == ty
  then Assert.liftAssert $ (, term) <$> zAlias name term
  else error $ unwords ["Type mismatch", show ty, "v.", show term]

zSetValues :: KnownNat n => String -> Term n -> Assert.Assert ()
zSetValues name t = do
  logIf "values" $ "Setting " ++ show name ++ " to " ++ show t
  case t of
    Bool b     -> Assert.evalAndSetValue name b
    BitInt _ i -> Assert.evalAndSetValue name i
    Field f    -> Assert.evalAndSetValue name f
    Array _ xs -> zipWithM_ (\i x -> zSetValues (aName i name) x) [0 ..] xs
    Struct _ fs ->
      forM_ (Map.toList fs) $ \(f, t) -> zSetValues (sName f name) t

zTermVars :: KnownNat n => String -> Term n -> Set.Set String
zTermVars name t = case t of
  Bool b     -> SAlg.vars b
  BitInt _ i -> SAlg.vars i
  Field f    -> SAlg.vars f
  Struct _ l -> Set.unions
    $ Map.mapWithKey (\fName fTerm -> zTermVars (sName fName name) fTerm) l
  Array _elemTy items -> Set.unions
    $ zipWith (\i fTerm -> zTermVars (aName i name) fTerm) [0 ..] items

zU32toBits :: KnownNat n => Term n -> Either String (Term n)
zU32toBits u32 = case u32 of
  BitInt 32 bv ->
    Right $ Array 32 $ map (Bool . flip S.mkDynBvExtractBit bv) [0 .. 31]
  _ -> Left $ "Cannot call (u32) to_bits on " ++ show u32

zU32fromBits :: KnownNat n => Term n -> Either String (Term n)
zU32fromBits array = case array of
  Array 32 bits -> do
    bits' <- forM bits $ \case
      Bool b -> Right b
      t      -> Left $ "Non-bit " ++ show t
    return $ BitInt 32 $ foldl1 S.mkDynBvConcat $ map S.BoolToDynBv bits'
  _ -> Left $ "Cannot call (u32) from_bits on " ++ show array

zFieldtoBits :: KnownNat n => Term n -> Either String (Term n)
zFieldtoBits t = case t of
  Field f ->
    let bv = S.PfToDynBv 254 f
    in  Right $ Array 254 $ map (Bool . flip S.mkDynBvExtractBit bv) [0 .. 253]
  _ -> Left $ "Cannot call (field) unpack on " ++ show t

instance KnownNat n => Embeddable T.Type (Term n) (Maybe InMap) where
  declare   = zDeclare
  ite       = const $ ((.) . (.) . (.)) (return . either error id) zIte
  assign    = const zAssign
  setValues = const zSetValues

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections #-}
module Codegen.Circom.CompTypes
  ( BaseTerm(..)
  , BaseCtx(..)
  , LTerm(..)
  , Term(..)
  , termAsNum
  , termAsConst
  , CompCtx(..)
  , IdKind(..)
  , TemplateInvocation
  , ctxOrderedSignals
  , primeUnOp
  , primeBinOp
  )
where


import           AST.Circom                     ( BinOp(..)
                                                , UnOp(..)
                                                , Span
                                                , SignalKind
                                                , SBlock
                                                )
import qualified Codegen.Circom.Signal         as Sig
import           Codegen.Circom.Utils           ( spanE )
import qualified Codegen.Circom.Typing         as Typing

import qualified Data.Array                    as Arr
import qualified Data.Bits                     as Bits
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           GHC.TypeNats

data LTerm = LTermLocal Sig.IndexedIdent
           | LTermForeign Sig.IndexedIdent Sig.IndexedIdent
           deriving (Show,Eq,Ord,Read)

-- A base term type `b` over constant type `k`
class (Show b, Num b, Fractional b) => BaseTerm b k | b -> k where
  fromConst :: k -> b
  fromSignal :: Sig.Signal -> b
  -- will not be called with arithmetic operations
  nonArithBinOp :: BinOp -> b -> b -> b
  -- will not be called with negation
  nonNegUnOp :: UnOp -> b -> b

  binOp :: BinOp -> b -> b -> b
  binOp o = case o of
    Add -> (+)
    Sub -> (-)
    Div -> (/)
    Mul -> (*)
    _ -> nonArithBinOp o

  unOp :: UnOp -> b -> b
  unOp o = case o of
    UnNeg -> negate
    _ -> nonNegUnOp o

class (Show c, BaseTerm b k) => BaseCtx c b k | c -> b where
  assert :: b -> c -> c
  emptyCtx :: c
  storeCtx :: Span -> SignalKind -> LTerm -> b -> c -> c
  -- Notification that a particular signal was gotten.
  getCtx :: SignalKind -> LTerm -> c -> c
  ignoreCompBlock :: c -> Bool
  -- Called after function exit.
  finalize :: c -> c

data Term b n = Base b
                 | Array (Arr.Array Int (Term b n))
                 | Component (TemplateInvocation n)
                 | Const n
                 deriving (Show,Eq,Ord)

termAsNum :: (Show b, Num n, PrimeField k) => Span -> Term b k -> n
termAsNum s t = case t of
  Const n -> fromInteger $ fromP n
  _ -> spanE s $ "term " ++ show t ++ " should be constant integer, but is not"

termAsConst :: (Show b, Show k) => Span -> Term b k -> Term (Void k) k
termAsConst s t = case t of
  Const     n -> Const n
  Array     a -> Array $ fmap (termAsConst s) a
  Component _ -> spanE s "don't pass function calls as arguments"
  Base{} -> spanE s $ "term " ++ show t ++ " should be constant, but is not"

instance (BaseTerm b k, GaloisField k) => Num (Term b k) where
  s + t = case (s, t) of
    (a@Array{}, _) ->
      error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Component{}, _) ->
      error $ "Cannot add component term " ++ show a ++ " to anything"
    (Base  a, Base b ) -> Base $ a + b
    (Const a, Const b) -> Const $ a + b
    (Const a, Base b ) -> Base (b + fromConst a)
    (l      , r      ) -> r + l
  s * t = case (s, t) of
    (a@Array{}, _) ->
      error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Component{}, _) ->
      error $ "Cannot multiply component term " ++ show a ++ " with anything"
    (Base  a, Base b ) -> Base $ a * b
    (Const a, Const b) -> Const $ a * b
    (Const a, Base b ) -> Base (b * fromConst a)
    (l      , r      ) -> r * l
  fromInteger = Const . fromInteger
  signum s = case s of
    Array{}     -> error $ "Cannot get sign of array term " ++ show s
    Component{} -> error $ "Cannot get sign of component term " ++ show s
    Base  a     -> Base $ signum a
    Const k     -> Const $ signum k
  abs s = case s of
    Array a     -> Base $ fromIntegral $ length a
    Component{} -> error $ "Cannot get size of component term " ++ show s
    Base  a     -> Base $ abs a
    Const k     -> Const $ abs k
  negate s = fromInteger (-1) * s

instance (BaseTerm b k, GaloisField k) => Fractional (Term b k) where
  fromRational = Const . fromRational
  recip t = case t of
    a@Array{}     -> error $ "Cannot invert array term " ++ show a
    a@Component{} -> error $ "Cannot invert component term " ++ show a
    Base  a       -> Base $ recip a
    Const a       -> Const $ recip a

instance (BaseTerm b (Prime k), KnownNat k) => BaseTerm (Term b (Prime k)) (Prime k) where
  fromConst  = Const
  fromSignal = Base . fromSignal
  nonNegUnOp o t = case t of
    Const f              -> Const $ primeUnOp o f
    Base  b              -> Base $ nonNegUnOp o b
    Array a | o == UnPos -> Const $ fromInteger $ fromIntegral $ length a
    _ -> error $ "Cannot perform operation " ++ show o ++ " on " ++ show t
  nonArithBinOp o s t = case (s, t) of
    (Const a, Const b) -> Const $ primeBinOp o a b
    (Const a, Base b ) -> Base $ binOp o (fromConst a) b
    (Base  a, Const b) -> Base $ binOp o a (fromConst b)
    (Base  a, Base b ) -> Base $ binOp o a b
    _ ->
      error
        $  "Cannot perform operation "
        ++ show o
        ++ " on "
        ++ show s
        ++ " and "
        ++ show t

type TemplateInvocation n = (String, [Term (Void n) n])

data IdKind = IKVar | IKSig | IKComp deriving (Show,Eq,Ord)


data CompCtx c b n = CompCtx { env :: Map.Map String (Term b n)
                             , baseCtx :: c
                             , signals :: Map.Map String (SignalKind, [Int])
                             , type_ :: Typing.InstanceType
                             , ids :: Map.Map String IdKind
                             , returning :: Maybe (Term b n)
                             --                             isFn, frmlArgs, code
                             , callables :: Map.Map String (Bool, [String], SBlock)
                             , cache :: Map.Map (TemplateInvocation n) (CompCtx c b n)
                             } deriving (Show)

ctxOrderedSignals :: CompCtx c b n -> [Sig.IndexedIdent]
ctxOrderedSignals =
      -- We sort the signals in public-inputs-first order. By starting with
      -- signal number 2, this ensures that signals numbered 2...n will be the
      -- public inputs, which is what our r1cs format requires
  map snd
    . List.sort
    . concatMap (\(n, (k, d)) -> map (k, ) $ expandSig n d)
    . Map.toAscList
    . signals
 where
  expandSig :: String -> [Int] -> [Sig.IndexedIdent]
  expandSig sigName dims = map (sigName, ) $ mapM (\d -> take d [0 ..]) dims

data Void k deriving (Show,Eq,Ord)

instance Num (Void k) where
instance Fractional (Void k) where
instance BaseTerm (Void k) k where

primeBinOp :: (KnownNat k) => BinOp -> Prime k -> Prime k -> Prime k
primeBinOp o = case o of
  IntDiv -> liftIntToPrime div
  Mod    -> liftIntToPrime mod
  Lt     -> liftIntPredToPrime (<)
  Gt     -> liftIntPredToPrime (>)
  Le     -> liftIntPredToPrime (<=)
  Ge     -> liftIntPredToPrime (>=)
  Eq     -> liftIntPredToPrime (==)
  Ne     -> liftIntPredToPrime (/=)
  And    -> liftBoolToPrime (&&)
  Or     -> liftBoolToPrime (||)
  BitAnd -> liftIntToPrime (Bits..&.)
  BitOr  -> liftIntToPrime (Bits..|.)
  BitXor -> liftIntToPrime Bits.xor
  Pow    -> liftIntToPrime (^)
  Shl    -> liftIntToPrime (liftShiftToInt Bits.shiftL)
  Shr    -> liftIntToPrime (liftShiftToInt Bits.shiftR)
  Add    -> (+)
  Sub    -> (-)
  Mul    -> (*)
  Div    -> (/)
 where
  liftIntToPrime f a b = toP $ f (fromP a) (fromP b)
  liftIntPredToPrime f =
    liftIntToPrime (\a b -> fromIntegral $ fromEnum (f a b))
  liftBoolToPrime f = liftIntPredToPrime (\a b -> f (a /= 0) (b /= 0))
  liftShiftToInt :: (Integer -> Int -> Integer) -> Integer -> Integer -> Integer
  liftShiftToInt a b c = a b (fromIntegral c)

primeUnOp :: (KnownNat k) => UnOp -> Prime k -> Prime k
primeUnOp o = case o of
  UnNeg -> negate
  BitNot ->
    error "Bitwise negation has unclear semantics for prime field elements"
  Not   -> \a -> if a == 0 then 1 else 0
  UnPos -> id


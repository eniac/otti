{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FunctionalDependencies #-}
module Codegen.Circom.CompTypes
  ( BaseTerm(..)
  , BaseCtx(..)
  , LTerm(..)
  )
where


import           AST.Circom                     ( BinOp(..)
                                                , UnOp(..)
                                                , Span
                                                , SignalKind
                                                )
import qualified Codegen.Circom.Signal         as Sig

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

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
module IR.Circify.Control where

import           GHC.Generics
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import           Language.C.Data.Node
import           Language.C.Data.Position
import           Language.C.Data.Ident

-- Control flow syntax
data Control term =
    For { iter :: term, start :: term, end :: term, body :: Control term }
    | While { cond :: term, body :: Control term }
    | Call { func :: String, args :: [term] }
    | If { cond :: term, thenBody :: Control term, elseBody :: Control term }
    | Seq { left :: Control term, right :: Control term }
    | Term { inner :: term }
    | Empty {}

deriving instance Functor Control
deriving instance Foldable Control
deriving instance Traversable Control
deriving instance Eq term => Eq (Control term)
deriving instance Show term => Show (Control term)
instance Semigroup (Control term) where
  Empty <> right = right
  left  <> Empty = left
  left  <> right = Seq left right

instance Monoid (Control t) where
  mempty = Empty
deriving instance Generic t => Generic (Control t)

-- In order to do control flow flattening, this is the minimum contract the terms must follow
-- + Compare to integer
-- + Compare equality to integer
-- + Assignment, with re-assignment
-- + Increment by 1
-- + Introduce an integer literal
-- + Introduce an integer variable by name
class ControlTerm a where
    (<:)  :: a -> a -> a
    (==:) :: a -> a -> a
    (*:)  :: a -> a -> a
    (=:)  :: a -> Integer -> Control a
    (++:) :: a -> Control a
    lit   :: Integer -> a
    var   :: String -> a

-- Generated code has no debug symbols
nosym :: NodeInfo
nosym = OnlyPos nopos (nopos, 0)

instance ControlTerm CExpr where
  a <: b = CBinary CLeqOp a b nosym
  a ==: b = CBinary CEqOp a b nosym
  a *: b = CBinary CMulOp a b nosym
  l =: r = Term $ CAssign CAssignOp l (lit r) nosym
  (++:) i = Term $ CUnary CPostIncOp i nosym
  lit v = CConst $ CIntConst (CInteger v DecRepr noFlags) nosym
  var v = CVar (Ident v 0 nosym) nosym

infixl 7 =:

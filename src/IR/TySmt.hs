{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module IR.TySmt ( Sort(..)
                , AnyTerm(..)
                , BoolTerm(..)
                , BoolNaryOp(..)
                , BoolBinOp(..)
                , IntTerm(..)
                , IntNaryOp(..)
                , IntBinOp(..)
                , IntUnOp(..)
                , IntBinPred(..)
                , PfTerm(..)
                , PfNaryOp(..)
                , PfUnOp(..)
                , PfBinPred(..)
                , Term(..)
                ) where

import           Control.Monad.State.Strict (foldM, liftIO, unless)

data Sort = Bv Int
          | Pf Integer
          | B
          | Z
          deriving (Show, Ord, Eq)

data AnyTerm = TermBool BoolTerm
             | TermInt  IntTerm
             | TermPf   PfTerm
             | TermBv   BvTerm
             deriving (Show, Ord, Eq)

-- Booleans
data BoolTerm = BoolLit Bool
              | BoolBinExpr BoolBinOp BoolTerm BoolTerm
              | BoolNaryExpr BoolNaryOp [BoolTerm]
              | BoolNeg BoolTerm
              | IntPred IntBinPred IntTerm IntTerm
              | PfPred PfBinPred PfTerm PfTerm
              | BvBinPred BvBinPred BvTerm BvTerm
              | BoolVar String
              | BoolExists String Sort BoolTerm
              | BoolLet String AnyTerm BoolTerm
              deriving (Show, Ord, Eq)

data BoolNaryOp = BoolAnd | BoolOr | BoolXor deriving (Show, Ord, Eq)
data BoolBinOp = BoolImplies deriving (Show, Ord, Eq)

-- Integers
data IntTerm = IntLit Integer
             | IntNaryExpr IntBinOp [IntTerm]
             | IntBinExpr IntBinOp IntTerm IntTerm
             | IntUnExpr IntUnOp IntTerm
             | PfToNat PfTerm
             | BvToNat BvTerm
             | IntVar String
             | IntExists String Sort IntTerm
             | IntLet String AnyTerm IntTerm
             deriving (Show, Ord, Eq)

data IntNaryOp = IntAdd | IntMul
               deriving (Show, Ord, Eq)
data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr
              deriving (Show, Ord, Eq)
data IntUnOp = IntNeg | IntAbs
             deriving (Show, Ord, Eq)
data IntBinPred = IntLt | IntLe | IntGt | IntGe | IntEq | IntNe
                deriving (Show, Ord, Eq)


-- Prime Fields
data PfTerm = PfLit Integer Integer -- order value
            | PfNaryExpr PfNaryOp [PfTerm]
            | PfUnExpr PfUnOp PfTerm
            | IntToPf Integer IntTerm
            | PfVar String
            | PfExists String Sort PfTerm
            | PfLet String AnyTerm PfTerm
            deriving (Show, Ord, Eq)

data PfNaryOp = PfAdd | PfMul deriving (Show, Ord, Eq)
data PfUnOp = PfNeg | PfRecip deriving (Show, Ord, Eq)
data PfBinPred = PfEq | PfNe deriving (Show, Ord, Eq)

-- Bitvectors
data BvTerm = BvLit Int Integer
            | BvBinExpr BvBinOp BvTerm BvTerm
            | BvExtract Int Int BvTerm
            | IntToBv Int IntTerm
            | BvVar String
            | BvExists String Sort BvTerm
            | BvLet String AnyTerm BvTerm
            deriving (Show, Ord, Eq)

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
             | BvConcat
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

class Sorted a where
    sort :: a -> Sort

class Term a where
    asAnyTerm :: a -> AnyTerm
    children :: a -> [AnyTerm]

    depth :: a -> Int
    depth t = 1 + foldr max 0 (map depth $ children t)

instance Term AnyTerm where
    asAnyTerm = id
    children t = case t of
        TermBv t   -> children t
        TermPf t   -> children t
        TermBool t -> children t
        TermInt t  -> children t

instance Term BvTerm where
    asAnyTerm = TermBv
    children t = case t of
        BvLit {}        -> []
        BvBinExpr _ a b -> [asAnyTerm a, asAnyTerm b]
        BvExtract _ _ a -> [asAnyTerm a]
        IntToBv _ a     -> [asAnyTerm a]
        BvVar {}        -> []
        BvExists _ _ a  -> [asAnyTerm a]
        BvLet _ a b     -> [a, asAnyTerm b]

instance Term IntTerm where
    asAnyTerm = TermInt
    children t = case t of
        IntLit {}        -> []
        IntNaryExpr _ as -> map asAnyTerm as
        IntBinExpr _ a b -> [asAnyTerm a, asAnyTerm b]
        IntUnExpr _ a    -> [asAnyTerm a]
        BvToNat a        -> [asAnyTerm a]
        PfToNat a        -> [asAnyTerm a]
        IntVar {}        -> []
        IntExists _ _ a  -> [asAnyTerm a]
        IntLet _ a b     -> [a, asAnyTerm b]

instance Term PfTerm where
    asAnyTerm = TermPf
    children t = case t of
        PfLit {}        -> []
        PfNaryExpr _ as -> map asAnyTerm as
        PfUnExpr _ a    -> [asAnyTerm a]
        IntToPf _ a     -> [asAnyTerm a]
        PfVar {}        -> []
        PfExists _ _ a  -> [asAnyTerm a]
        PfLet _ a b     -> [a, asAnyTerm b]


instance Term BoolTerm where
    asAnyTerm = TermBool
    children t = case t of
        BoolLit {}        -> []
        BoolNaryExpr _ as -> map asAnyTerm as
        BoolBinExpr _ a b -> [asAnyTerm a, asAnyTerm b]
        BoolNeg a         -> [asAnyTerm a]
        BoolVar {}        -> []
        BoolExists _ _ a  -> [asAnyTerm a]
        BoolLet _ a b     -> [a, asAnyTerm b]
        IntPred _ a b     -> [asAnyTerm a, asAnyTerm b]
        PfPred _ a b      -> [asAnyTerm a, asAnyTerm b]
        BvBinPred _ a b   -> [asAnyTerm a, asAnyTerm b]

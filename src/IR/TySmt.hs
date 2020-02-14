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
                , BvTerm(..)
                , BvBinOp(..)
                , BvBinPred(..)
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
              | BoolIte BoolTerm BoolTerm BoolTerm
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
             | BoolToInt BoolTerm
             | IntVar String
             | IntExists String Sort IntTerm
             | IntLet String AnyTerm IntTerm
             | IntIte BoolTerm IntTerm IntTerm
             deriving (Show, Ord, Eq)

data IntNaryOp = IntAdd | IntMul
               deriving (Show, Ord, Eq)
data IntBinOp = IntSub | IntDiv | IntMod | IntShl | IntShr | IntPow
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
            | PfIte BoolTerm PfTerm PfTerm
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
            | BvIte BoolTerm BvTerm BvTerm
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
    mapVar :: (String -> String) -> a -> a

    depth :: a -> Int
    depth t = 1 + foldr max 0 (map depth $ children t)

    allDescendants :: a -> [AnyTerm]
    allDescendants t = asAnyTerm t : concatMap allDescendants (children t)

    asVariable :: a -> Maybe String

instance Term AnyTerm where
    asAnyTerm = id
    children t = case t of
        TermBv t   -> children t
        TermPf t   -> children t
        TermBool t -> children t
        TermInt t  -> children t
    mapVar f t = case t of
        TermBv t   -> TermBv   $ mapVar f t
        TermPf t   -> TermPf   $ mapVar f t
        TermBool t -> TermBool $ mapVar f t
        TermInt t  -> TermInt  $ mapVar f t
    asVariable t = case t of
        TermBv t   -> asVariable t
        TermPf t   -> asVariable t
        TermBool t -> asVariable t
        TermInt t  -> asVariable t

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
        BvIte c t f     -> [asAnyTerm c, asAnyTerm t, asAnyTerm f]
    mapVar f t = case t of
        l@BvLit {}      -> l
        BvBinExpr o a b -> BvBinExpr o (mapVar f a) (mapVar f b)
        BvExtract i j a -> BvExtract i j (mapVar f a)
        IntToBv i a     -> IntToBv i (mapVar f a)
        BvVar s         -> BvVar (f s)
        BvExists s l a  -> BvExists (f s) l (mapVar f a)
        BvLet s a b     -> BvLet (f s) (mapVar f a) (mapVar f b)
        BvIte c t ff    -> BvIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        BvVar s -> Just s
        _       -> Nothing

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
        BoolToInt a      -> [asAnyTerm a]
        IntIte c t f     -> [asAnyTerm c, asAnyTerm t, asAnyTerm f]
    mapVar f t = case t of
        l@IntLit {}      -> l
        IntNaryExpr o as -> IntNaryExpr o (map (mapVar f) as)
        IntBinExpr o a b -> IntBinExpr o (mapVar f a) (mapVar f b)
        IntUnExpr o a    -> IntUnExpr o (mapVar f a)
        BvToNat a        -> BvToNat (mapVar f a)
        PfToNat a        -> PfToNat (mapVar f a)
        IntVar s         -> IntVar (f s)
        IntExists s l a  -> IntExists (f s) l (mapVar f a)
        IntLet s a b     -> IntLet (f s) (mapVar f a) (mapVar f b)
        BoolToInt a      -> BoolToInt (mapVar f a)
        IntIte c t ff    -> IntIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        IntVar s -> Just s
        _        -> Nothing

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
        PfIte c t f     -> [asAnyTerm c, asAnyTerm t, asAnyTerm f]
    mapVar f t = case t of
        l@PfLit {}      -> l
        PfNaryExpr o as -> PfNaryExpr o (map (mapVar f) as)
        PfUnExpr o a    -> PfUnExpr o (mapVar f a)
        IntToPf i a     -> IntToPf i (mapVar f a)
        PfVar s         -> PfVar (f s)
        PfExists s l a  -> PfExists (f s) l (mapVar f a)
        PfLet s a b     -> PfLet (f s) (mapVar f a) (mapVar f b)
        PfIte c t ff    -> PfIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        PfVar s -> Just s
        _       -> Nothing


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
        BoolIte c t f     -> [asAnyTerm c, asAnyTerm t, asAnyTerm f]
    mapVar f t = case t of
        l@BoolLit {}      -> l
        BoolNaryExpr o as -> BoolNaryExpr o (map (mapVar f) as)
        BoolBinExpr o a b -> BoolBinExpr o (mapVar f a) (mapVar f b)
        BoolNeg a         -> BoolNeg (mapVar f a)
        BoolVar s         -> BoolVar (f s)
        BoolExists s l a  -> BoolExists (f s) l (mapVar f a)
        BoolLet s a b     -> BoolLet (f s) (mapVar f a) (mapVar f b)
        IntPred p a b     -> IntPred p (mapVar f a) (mapVar f b)
        PfPred p a b      -> PfPred p (mapVar f a) (mapVar f b)
        BvBinPred p a b   -> BvBinPred p (mapVar f a) (mapVar f b)
        BoolIte c t ff    -> BoolIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        BoolVar s -> Just s
        _         -> Nothing

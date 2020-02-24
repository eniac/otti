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
          | Array String String
          | Fp Int Int
          deriving (Show, Ord, Eq)

sortFloat32 = Fp 8 24
sortFloat64 = Fp 11 53

data AnyTerm = TermBool BoolTerm
             | TermInt  IntTerm
             | TermPf   PfTerm
             | TermBv   BvTerm
             | TermFp   FpTerm
             -- TODO: TermArray?
             deriving (Show, Ord, Eq)

-- Booleans
data BoolTerm = BoolLit Bool
              | BoolBinExpr BoolBinOp BoolTerm BoolTerm
              | BoolNaryExpr BoolNaryOp [BoolTerm]
              | BoolNeg BoolTerm
              | IntPred IntBinPred IntTerm IntTerm
              | PfPred PfBinPred PfTerm PfTerm
              | BvBinPred BvBinPred BvTerm BvTerm
              | FpBinPred FpBinPred FpTerm FpTerm
              | FpUnPred FpUnPred FpTerm
              | BoolVar String
              | BoolExists String Sort BoolTerm
              | BoolLet String AnyTerm BoolTerm
              | BoolIte BoolTerm BoolTerm BoolTerm
              -- TODO: Select?
              deriving (Show, Ord, Eq)

data BoolNaryOp = BoolAnd | BoolOr | BoolXor deriving (Show, Ord, Eq)
data BoolBinOp = BoolImplies deriving (Show, Ord, Eq)

-- Arrays
data ArrayTerm k v = Store (ArrayTerm k v) k v
                   | ArrayVar String
                   | ArrayExists String Sort (ArrayTerm k v)
                   | ArrayLet String AnyTerm (ArrayTerm k v)

-- Integers
data IntTerm = IntLit Integer
             | IntNaryExpr IntBinOp [IntTerm]
             | IntBinExpr IntBinOp IntTerm IntTerm
             | IntUnExpr IntUnOp IntTerm
             | PfToNat PfTerm
             | BvToNat BvTerm
             | BvToNatSigned BvTerm
             | BoolToInt BoolTerm
             | IntVar String
             | IntExists String Sort IntTerm
             | IntLet String AnyTerm IntTerm
             | IntIte BoolTerm IntTerm IntTerm
              -- TODO: Select?
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
              -- TODO: Select?
            deriving (Show, Ord, Eq)

data PfNaryOp = PfAdd | PfMul deriving (Show, Ord, Eq)
data PfUnOp = PfNeg | PfRecip deriving (Show, Ord, Eq)
data PfBinPred = PfEq | PfNe deriving (Show, Ord, Eq)

-- Bitvectors
data BvTerm = BvLit Int Integer
            | BvBinExpr BvBinOp BvTerm BvTerm
            | BvExtract Int Int BvTerm
            | IntToBv Int IntTerm
            | IntToBvSigned Int IntTerm
            | BvVar String
            | BvExists String Sort BvTerm
            | BvLet String AnyTerm BvTerm
            | BvIte BoolTerm BvTerm BvTerm
            | FpToBv FpTerm
            | FpToBvSigned FpTerm
              -- TODO: Select?
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

-- Fp
data FpTerm = FpFloatLit Float
            | FpDoubleLit Double
            | FpVar String
            | FpFma FpTerm FpTerm FpTerm
            | FpBinExpr FpBinOp FpTerm FpTerm
            | FpUnExpr FpUnOp FpTerm
            | BvToFp BvTerm Int Int
            | BvToFpSigned BvTerm Int Int
            | FpToFp BvTerm Int Int
            | IntToFp IntTerm Int Int
            | FpExists String Sort FpTerm
            | FpLet String AnyTerm FpTerm
            | FpIte BoolTerm FpTerm FpTerm
            deriving (Show, Ord, Eq)


data FpBinOp = FpAdd
             | FpSub
             | FpMul
             | FpDiv
             | FpRem
             | FpMax
             | FpMin
             deriving (Show, Ord, Eq)

data FpUnOp = FpNeg
            | FpAbs
            | FpSqrt
            | FpRound
            deriving (Show, Ord, Eq)

data FpBinPred = FpLe
               | FpLt
               | FpGe
               | FpGt
               | FpEq
               deriving (Show, Ord, Eq)

data FpUnPred = FpIsNormal
              | FpIsSubnormal
              | FpIsZero
              | FpIsInfinite
              | FpIsNaN
              | FpIsNegative
              | FpIsPositive
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
        TermFp t   -> children t
        TermPf t   -> children t
        TermBool t -> children t
        TermInt t  -> children t
    mapVar f t = case t of
        TermBv t   -> TermBv   $ mapVar f t
        TermPf t   -> TermPf   $ mapVar f t
        TermFp t   -> TermFp   $ mapVar f t
        TermBool t -> TermBool $ mapVar f t
        TermInt t  -> TermInt  $ mapVar f t
    asVariable t = case t of
        TermBv t   -> asVariable t
        TermPf t   -> asVariable t
        TermFp t   -> asVariable t
        TermBool t -> asVariable t
        TermInt t  -> asVariable t

instance Term BvTerm where
    asAnyTerm = TermBv
    children t = case t of
        BvLit {}          -> []
        BvBinExpr _ a b   -> [asAnyTerm a, asAnyTerm b]
        BvExtract _ _ a   -> [asAnyTerm a]
        IntToBv _ a       -> [asAnyTerm a]
        IntToBvSigned _ a -> [asAnyTerm a]
        FpToBv a          -> [asAnyTerm a]
        FpToBvSigned a    -> [asAnyTerm a]
        BvVar {}          -> []
        BvExists _ _ a    -> [asAnyTerm a]
        BvLet _ a b       -> [a, asAnyTerm b]
        BvIte c t f       -> [asAnyTerm c, asAnyTerm t, asAnyTerm f]
    mapVar f t = case t of
        l@BvLit {}        -> l
        BvBinExpr o a b   -> BvBinExpr o (mapVar f a) (mapVar f b)
        BvExtract i j a   -> BvExtract i j (mapVar f a)
        IntToBv i a       -> IntToBv i (mapVar f a)
        IntToBvSigned i a -> IntToBvSigned i (mapVar f a)
        FpToBv a          -> FpToBv (mapVar f a)
        FpToBvSigned a    -> FpToBvSigned (mapVar f a)
        BvVar s           -> BvVar (f s)
        BvExists s l a    -> BvExists (f s) l (mapVar f a)
        BvLet s a b       -> BvLet (f s) (mapVar f a) (mapVar f b)
        BvIte c t ff      -> BvIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        BvVar s -> Just s
        _       -> Nothing

instance Term FpTerm where
    asAnyTerm = TermFp
    children t = case t of
        FpFloatLit _       -> []
        FpDoubleLit _      -> []
        FpVar _            -> []
        FpFma a b c        -> map asAnyTerm [a, b, c]
        FpBinExpr _ a b    -> map asAnyTerm [a, b]
        FpUnExpr _ a       -> [asAnyTerm a]
        BvToFp a _ _       -> [asAnyTerm a]
        BvToFpSigned a _ _ -> [asAnyTerm a]
        FpToFp a _ _       -> [asAnyTerm a]
        IntToFp a _ _      -> [asAnyTerm a]
        FpExists _ _ a     -> [asAnyTerm a]
        FpLet _ a b        -> [a, asAnyTerm b]
        FpIte c t f        -> [asAnyTerm c, asAnyTerm t, asAnyTerm f]
    mapVar f t = case t of
        FpFloatLit l       -> t
        FpDoubleLit l      -> t
        FpVar s            -> FpVar $ f s
        FpFma a b c        -> FpFma (mapVar f a) (mapVar f b) (mapVar f c)
        FpBinExpr o a b    -> FpBinExpr o (mapVar f a) (mapVar f b)
        FpUnExpr o a       -> FpUnExpr o (mapVar f a)
        BvToFp a e s       -> BvToFp (mapVar f a) e s
        BvToFpSigned a e s -> BvToFpSigned (mapVar f a) e s
        FpToFp a e s       -> FpToFp (mapVar f a) e s
        IntToFp a e s      -> IntToFp (mapVar f a) e s
        FpExists s l a     -> FpExists (f s) l (mapVar f a)
        FpLet s a b        -> FpLet (f s) (mapVar f a) (mapVar f b)
        FpIte c t ff       -> FpIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        FpVar s -> Just s
        _       -> Nothing

instance Term IntTerm where
    asAnyTerm = TermInt
    children t = case t of
        IntLit {}        -> []
        IntNaryExpr _ as -> map asAnyTerm as
        IntBinExpr _ a b -> [asAnyTerm a, asAnyTerm b]
        IntUnExpr _ a    -> [asAnyTerm a]
        BvToNat a        -> [asAnyTerm a]
        BvToNatSigned a  -> [asAnyTerm a]
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
        BvToNatSigned a  -> BvToNatSigned (mapVar f a)
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
        FpBinPred _ a b   -> [asAnyTerm a, asAnyTerm b]
        FpUnPred _ a      -> [asAnyTerm a]
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
        FpBinPred p a b   -> FpBinPred p (mapVar f a) (mapVar f b)
        FpUnPred p a      -> FpUnPred p (mapVar f a)
        BoolIte c t ff    -> BoolIte (mapVar f c) (mapVar f t) (mapVar f ff)
    asVariable t = case t of
        BoolVar s -> Just s
        _         -> Nothing

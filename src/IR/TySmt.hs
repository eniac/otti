module IR.TySmt where

import           Z3.Monad                   (MonadZ3)
import qualified Z3.Monad                   as Z
import           Control.Monad.State.Strict (foldM, liftIO, unless)

data Sort = BV Int
          | FF Integer
          | B
          | Z

-- Booleans
data BoolTerm = BoolLit Bool
              | BoolBinExpr BoolBinOp BoolTerm BoolTerm
              | BoolNeg BoolTerm
              | IntPred IntBinPred IntTerm IntTerm
              | PfPred PfBinPred PfTerm PfTerm
              | BoolVar String

data BoolBinOp = BoolAnd | BoolOr | BoolXor | BoolImplies

-- Integers
data IntTerm = IntLit Integer
             | IntBinExpr IntBinOp IntTerm IntTerm
             | IntUnExpr IntUnOp IntTerm
             | PfToNat PfTerm
             | BvToNat BvTerm
             | IntVar String

data IntBinOp = IntAdd | IntSub | IntMul | IntDiv | IntMod
data IntUnOp = IntNeg | IntAbs
data IntBinPred = IntLt | IntLe | IntGt | IntGe | IntEq | IntNe


-- Prime Fields
data PfTerm = PfLit Integer Integer -- order value
            | PfBinExpr PfBinOp PfTerm PfTerm
            | PfUnExpr PfUnOp PfTerm
            | IntToPf Integer IntTerm

data PfBinOp = PfAdd | PfSub | PfMul | PfDiv
data PfUnOp = PfNeg | PfRecip
data PfBinPred = PfEq | PfNe

-- Bitvectors
data BvTerm = BvLit Int Integer
            | BvBinTerm BvBinOp BvTerm BvTerm
            | BvBinPred BvBinOp BvTerm BvTerm
            | BvExtract Int Int BvTerm
            | IntToBv Int IntTerm
            | BvVar String

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

class Sorted a where
    sort :: a -> Sort


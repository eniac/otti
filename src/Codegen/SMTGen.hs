module Codegen.SMTGen where
import           AST.Simple
import           Codegen.CompilerMonad
import           IR.SMT
import           Prelude               hiding (Num)
import           Targets.SMT           (SMT)

{-|

Module that generates SMT for the Simple AST defined in AST.Simple.
Codegen from AST to a circuit consists of the following challenges:
1. SSA all variables
2. Inline all function calls, loops, if statements, etc

-}

genVarSMT :: Var -> Compiler SMTNode
genVarSMT var = getNodeFor $ varName var

genNumSMT :: Num -> Compiler SMTNode
genNumSMT num = case num of
                  INum ty _   | isDouble ty -> error "Cannot make int with double val"
                  INum ty val -> liftSMT $ newInt ty val
                  FNum ty _   | not $ isDouble ty -> error "Cannot make double with int val"
                  FNum ty val -> liftSMT $ newDouble ty val

genExprSMT :: Expr -> Compiler SMTNode
genExprSMT expr =
  case expr of
    VarExpr v  -> genVarSMT v
    NumExpr n  -> genNumSMT n
    Neg n      -> genExprSMT n >>= liftSMT . cppNeg
    Not n      -> genExprSMT n >>= liftSMT . cppBitwiseNeg
    Eq a b     -> genBinOpSMT a b cppEq
    NEq a b    -> error ""
    And a b    -> genBinOpSMT a b cppAnd
    Add a b    -> genBinOpSMT a b cppAdd
    Sub a b    -> genBinOpSMT a b cppSub
    Mul a b    -> genBinOpSMT a b cppMul
    Or a b     -> genBinOpSMT a b cppOr
    XOr a b    -> error ":cppXOr a b"
    Min a b    -> genBinOpSMT a b cppMin
    Max a b    -> genBinOpSMT a b cppMax
    Gt a b     -> genBinOpSMT a b cppGt
    Gte a b    -> genBinOpSMT a b cppGte
    Lt a b     -> genBinOpSMT a b cppLt
    Lte a b    -> genBinOpSMT a b cppLte
    -- Shl a b    -> genBinOpSMT a b cppShiftLeft
    -- Shr a b    -> genBinOpSMT a b cppShiftRight
    Tern c t f -> do
      c' <- genExprSMT c
      t' <- genExprSMT t
      f' <- genExprSMT f
      -- liftSMT $ cppCond c' t' f'
      error ""
    Cast v t -> do
      v' <- genExprSMT v
      error ""
      -- liftSMT $ cppCast v' t
    Call name args -> error ""
    _          -> error "Unsupported instruction"

genBinOpSMT :: Expr
            -> Expr
            -> (SMTNode -> SMTNode -> SMT SMTNode)
            -> Compiler SMTNode
genBinOpSMT e1 e2 op = do
  s1 <- genExprSMT e1
  s2 <- genExprSMT e2
  liftSMT $ op s1 s2

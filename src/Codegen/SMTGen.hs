module Codegen.SMTGen where
import           AST.Simple
import           Codegen.CompilerMonad
import           IR.SMT
import           Prelude               hiding (Num)

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


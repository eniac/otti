module Codegen.SMTGen where
import           AST.Simple
import           IR.IR
import           Targets.SMT

{-|

Module that generates SMT for the Simple AST defined in AST.Simple.
Codegen from AST to a circuit consists of the following challenges:
1. SSA all variables
2. Inline all function calls, loops, if statements, etc

-}




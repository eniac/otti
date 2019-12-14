module AST.SimpleBuilder where
import           AST.Simple

{-|

This module presents AST nodes for function, class, and program definitions.
It also includes all functions that create AST nodes.
This way, the functions can be attached to any parser.

AST builder functions build up an AST by saving information in the Compiler monad.
The Compiler monad appears in Compiler.hs

-}

module Compiler.AST.SimpleMonadic where
import           Compiler.AST.SimplePure
import           Compiler.CompilerMonad

{-|

This module presents AST nodes for function, class, and program definitions.

-}

-- | A function definition is a name, a return type, arguments, and a list of
-- statement *actions*---not just statements. This is because saving all AST
-- code statements as actions allows us to do bookkeeping about those statements on
-- demand, each time the function is called. For example, any time a function is
-- called, we automatically inline that function and version all of the variables
-- within it. See SimpleBuilder to see the versioning in action
data FunctionDef = Function { fName :: FunctionName
                            , fTy   :: WrappedType
                            , fArgs :: [(VarName, WrappedType)]
                            , fBody :: [Compiler Stmt]
                            }

-- | A class is fields and types and then any class functions
data ClassDef = ClassDef ClassName [(FieldName, Type)] [FunctionDef]

-- | A program is function definitions and class definitions
data Program = Program [FunctionDef] [ClassDef]

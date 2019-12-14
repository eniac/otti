module AST.Simple where
import           Prelude hiding (Num)

{-|

This module presents the part of the  simple AST that we parse in ourselves using John's
parser (SimpleParser.hs). This file just includes expressions and statements.
SimpleBuilder.hs provides top-level function declarations and class declarations,
as well as the functions that create all AST nodes.

-}

---
--- Types
---

-- | A C type: a signed or unsigned integer of a certain width,
-- a boolean, or a double-precision floating point number
data Type = U8 | S8
          | U16 | S16
          | U32 | S32
          | U64 | S64
          | Bool
          | Double
          deriving (Eq, Ord, Show)

isSignedInt, isUnsignedInt, isDouble :: Type -> Bool
isSignedInt S8  = True
isSignedInt S16 = True
isSignedInt S32 = True
isSignedInt S64 = True
isSignedInt _   = False
isUnsignedInt U8  = True
isUnsignedInt U16 = True
isUnsignedInt U32 = True
isUnsignedInt U64 = True
isDouble Double = True
isDouble _      = False

-- | Class and struct fields, classes, and functions are identified by name
type FieldName = String
type ClassName = String
type FunctionName = String

data WrappedType = PrimType { primType :: Type}
                 | ClassType { classType :: ClassName }
                 | Void
                 deriving (Eq, Ord, Show)

isPrimType, isClassType, isVoidType :: WrappedType -> Bool
isPrimType PrimType{} = True
isPrimType _          = False
isClassType ClassType{} = True
isClassType _           = False
isVoidType Void = True
isVoidType _    = False

---
--- Variables
---

-- | All variables in the AST are associated with versions.
-- This allows us to store an AST that is in SSA form, which we need for
-- any analysis, optimization, and eventual codegen
type Version = Int

-- | Variables have names. Right now, a name is just a string, but we
-- may switch this type out later for better performance (e.g., to word)
type VarName = String

-- | A variable: either a primitive type or a class. We may want to make this more general
-- later (ie get rid of the distinction), especially once we add memory
data Var = PrimVar { varTy      :: Type
                   , varName    :: VarName
                   , varVersion :: Version
                   }
         | ClassVar { varClass :: ClassName
                    , varName  :: VarName
                    }
         deriving (Eq, Ord, Show)

isPrimVar, isClassVar :: Var -> Bool
isPrimVar PrimVar{} = True
isPrimVar _         = False
isClassVar ClassVar{} = True
isClassVar _          = False

setVersion :: Var -> Int -> Var
setVersion (PrimVar ty name _) ver = PrimVar ty name ver
setVersion classVar _              = classVar

---
--- Numbers
---

data Num = INum { numTy  :: Type
                , numVal :: Integer
                }
         | FNum { numTy    :: Type
                , floatVal :: Double
                }
         deriving (Eq, Ord, Show)

---
--- AST definition
---

-- | An AST expression
-- This includes both (1) normal C AST nodes (e.g., subtraction) and
--                    (2) JS nodes for verification, since we were using this system
--                        for verifying properties of a JavaScript JIT
--                    (3) nodes that query internal state for verification
--                        (e.g., Undef returns the undef bit of the given IR node)
-- We will certainly want to have some clearer breakdown of the AST into
-- verification-related nodes, other nodes, etc.
data Expr = VarExpr { varExpr :: Var }
          | NumExpr { numExpr :: Num }
          | Neg Expr
          | Not Expr
          | JSNot Expr
          | Abs Expr
          | JSAbs Expr
          | Eq Expr Expr
          | NEq Expr Expr
          | And Expr Expr
          | JSAnd Expr Expr
          | Add Expr Expr
          | JSAdd Expr Expr
          | Sub Expr Expr
          | JSSub Expr Expr
          | Mul Expr Expr
          | JSMul Expr Expr
          | Or Expr Expr
          | JSOr Expr Expr
          | XOr Expr Expr
          | JSXOr Expr Expr
          | Min Expr Expr
          | JSMin Expr Expr
          | Max Expr Expr
          | JSMax Expr Expr
          | Gt Expr Expr
          | Gte Expr Expr
          | Lt Expr Expr
          | Lte Expr Expr
          | IsNan Expr
          | IsInf Expr
          | IsZero Expr
          | IsNegative Expr
          | IsNegativeZero Expr
          | GetExp Expr
          | Shl Expr Expr
          | JSLsh Expr Expr
          | Shr Expr Expr
          | JSRsh Expr Expr
          | JSUrsh Expr Expr
          | Tern Expr Expr Expr
          | Cast Expr Type
          | Call FunctionName [Expr]
          | FieldExpr FieldName
          | JSCeil Expr
          | JSFloor Expr
          | JSSign Expr
          | JSDiv Expr Expr
          | JSRem Expr Expr
          | Undef Expr
          | TestImplies Expr Expr
            deriving (Eq, Ord, Show)

-- isCallExpr :: Expr -> Bool
-- isCallExpr Call{} = True
-- isCallExpr _      = False

-- isClassExpr :: Expr -> Bool
-- isClassExpr (VarExpr v) = not $ isPrimType v
-- isClassExpr _           = False

-- isPrimVarExpr :: Expr -> Bool
-- isPrimVarExpr (VarExpr v) = isPrimType v
-- isPrimVarExpr _           = False

data Stmt = Decl Var
          | Assign Expr Expr
          | If Expr [Stmt] [Stmt]
          | VoidCall FunctionName [Expr]
          | Return Expr
          | Assert Expr
          | Implies Expr Expr
          | NotIff Expr Expr
          | Iff Expr Expr
          | Versioned Expr
--          | Expect (SMTResult -> Bool) (SMTResult -> IO ())
          | Push
          | Pop




module AST.Simple where
import           Prelude hiding (Num)

{-|

This module presents a simple AST. You can parse it in using whatever parser you want

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

---
--- Variables
---

-- | Variables have names. Right now, a name is just a string, but we
-- may switch this type out later for better performance (e.g., to word)
type VarName = String

-- | A variable: either a primitive type or a class. We may want to make this more general
-- later (ie get rid of the distinction), especially once we add memory
data Var = Var { varTy   :: Type
               , varName :: VarName
               }
           deriving (Eq, Ord, Show)

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
          | Abs Expr
          | Eq Expr Expr
          | NEq Expr Expr
          | And Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Or Expr Expr
          | XOr Expr Expr
          | Min Expr Expr
          | Max Expr Expr
          | Gt Expr Expr
          | Gte Expr Expr
          | Lt Expr Expr
          | Lte Expr Expr
          | Shl Expr Expr
          | Shr Expr Expr
          | Tern Expr Expr Expr
          | Cast Expr Type
          | Call FunctionName [Expr]
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

---
--- Functions, classes, and programs. NOTE: for now I have gotten rid of classes
---

type FunctionName = String

data Function = Function { fName :: FunctionName
                         , fTy   :: Type
                         , fArgs :: [(VarName, Type)]
                         }

-- | A program is function definitions and class definitions
data Program = Program [Function]



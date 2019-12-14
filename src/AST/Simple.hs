module AST.Simple where

{-|

This module presents the simple AST that we parse in ourselves using John's
parser (SimpleParser.hs).

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



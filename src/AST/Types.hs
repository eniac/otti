module AST.Types where

-- | General types: for now, signed or unsigned integers of
-- a certain width, bools, or double-precision floats
data Type = U8 | S8
          | U16 | S16
          | U32 | S32
          | U64 | S64
          | Bool
          | Double
          | Ptr64 Type
          | Ptr32 Type
          | Struct [Type]
          | Array Int Type
          deriving (Eq, Ord, Show)

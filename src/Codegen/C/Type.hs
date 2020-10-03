module Codegen.C.Type where

-- Types

-- | General types: for now, signed or unsigned integers of
-- a certain width, bools, or double-precision floats
data Type = U8 | S8
          | U16 | S16
          | U32 | S32
          | U64 | S64
          | Bool
          | Float
          | Double
          | Ptr64 Type
          | Ptr32 Type
          | Struct [(String, Type)]
          | Array (Maybe Int) Type
          | Void
          | Char
          deriving (Eq, Ord, Show)

isSimple :: Type -> Bool
isSimple ty = isIntegerType ty || ty == Bool || isDouble ty || isFloat ty

isIntegerType :: Type -> Bool
isIntegerType ty = isSignedInt ty || isUnsignedInt ty

makeIntTy :: Int -> Bool -> Type
makeIntTy numBits isSigned = case numBits of
  8 | isSigned  -> S8
  8             -> U8
  16 | isSigned -> S16
  16            -> U16
  32 | isSigned -> S32
  32            -> U32
  64 | isSigned -> S64
  64            -> U64
  _             -> error $ "Unexpected width to makeType " ++ show numBits

numBits :: Type -> Int
numBits U8                    = 8
numBits S8                    = 8
numBits U16                   = 16
numBits S16                   = 16
numBits U32                   = 32
numBits S32                   = 32
numBits U64                   = 64
numBits S64                   = 64
numBits Bool                  = 1
numBits Double                = 64
numBits Ptr64{}               = 64
numBits Ptr32{}               = 32
numBits (Struct tys         ) = sum $ map (numBits . snd) tys
numBits (Array (Just num) ty) = num * numBits ty
numBits (Array Nothing    _ ) = 32
numBits _                     = error "nyi"

isSignedInt :: Type -> Bool
isSignedInt S8  = True
isSignedInt S16 = True
isSignedInt S32 = True
isSignedInt S64 = True
isSignedInt _   = False

isUnsignedInt :: Type -> Bool
isUnsignedInt U8  = True
isUnsignedInt U16 = True
isUnsignedInt U32 = True
isUnsignedInt U64 = True
isUnsignedInt _   = False

isDouble :: Type -> Bool
isDouble Double = True
isDouble _      = False

isFloat :: Type -> Bool
isFloat Float = True
isFloat _     = False

isPointer :: Type -> Bool
isPointer Ptr64{} = True
isPointer Ptr32{} = True
isPointer _       = False

isStruct :: Type -> Bool
isStruct Struct{} = True
isStruct _        = False

isArray :: Type -> Bool
isArray Array{} = True
isArray _       = False

pointeeType :: Type -> Type
pointeeType (Ptr64 ty) = ty
pointeeType (Ptr32 ty) = ty
pointeeType v =
  error $ unwords ["Can't get pointee type of non-pointer", show v]

arrayBaseType :: Type -> Type
arrayBaseType (Array _ ty) = ty
arrayBaseType a =
  error $ unwords ["Cannot call arrayBaseType on non-array", show a]

arrayNumElems :: Type -> Int
arrayNumElems (Array (Just n) _) = n
arrayNumElems n =
  error $ unwords ["Cannot call array num elems on non-array type", show n]

structFieldTypes :: Type -> [Type]
structFieldTypes (Struct tys) = map snd tys
structFieldTypes s =
  error $ unwords ["Cannot call structFieldTypes on non-struct", show s]

structFieldList :: Type -> [(String, Type)]
structFieldList (Struct tys) = tys
structFieldList s =
  error $ unwords ["Cannot call structFieldList on non-struct", show s]

type FunctionName = String

module AST.Typed where

class Typed a where
  numBits :: a -> Int
  isSignedInt :: a -> Bool
  isUnsignedInt :: a -> Bool
  isDouble :: a -> Bool
  isPointer :: a -> Bool
  pointeeType :: a -> a
  isStruct :: a -> Bool
  structFieldTypes :: a -> [a]
  structFieldList :: a -> [(String, a)]
  isArray :: a -> Bool
  arrayBaseType :: a -> a
  arrayNumElems :: a -> Int
  newStructType :: [(String, a)] -> a
  newArrayType :: Int -> a -> a

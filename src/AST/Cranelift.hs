module AST.Cranelift where
import           AST.Typed

data CraneliftTy = B1 | B8 | B16 | B32 | B64
                 | I8 | I16 | I32 | I64
                 | F32 | F64
                 deriving (Eq, Ord, Show)


instance Typed CraneliftTy where
  numBits B1  = 1
  numBits B8  = 8
  numBits B16 = 16
  numBits B32 = 32
  numBits B64 = 64
  numBits I8  = 8
  numBits I16 = 16
  numBits I32 = 32
  numBits I64 = 64
  numBits F32 = 32
  numBits F64 = 64

  isSignedInt _ = False
  isUnsignedInt I8  = True
  isUnsignedInt I16 = True
  isUnsignedInt I32 = True
  isUnsignedInt I64 = True
  isUnsignedInt _   = False

  isDouble F32 = True
  isDouble F64 = True

  isPointer I32 = True
  isPointer I64 = True
  isPointer _   = False

  isStruct _ = False
  isArray _ = False



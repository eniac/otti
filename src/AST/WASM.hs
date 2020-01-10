module AST.WASM ( module Language.Wasm.Structure
                , module AST.WASM
                ) where
import           AST.Typed
import           Language.Wasm.Structure

-- | I32, I64, F32, F64
instance Typed ValueType where
  numBits I32 = 32
  numBits I64 = 64
  numBits F32 = 32
  numBits F64 = 64

  isSignedInt _ = False
  isUnsignedInt I32 = True
  isUnsignedInt I64 = True
  isUnsignedInt _   = False

  isDouble F32 = True
  isDouble F64 = True
  isDouble _   = False

  isPointer I32 = True
  isPointer I64 = True
  isPointer _   = False

  pointeeType = error "Not revelevant to WASM"
  isStruct = error "Not relevant to WASM"
  structFieldTypes = error "Not relevant to WASM"
  isArray = error "Not relevant to WASM"
  arrayBaseType = error "Not relevant to WASM"
  arrayNumElems = error "Not relevant to WASM"
  newStructType = error "Not relevant to WASM"
  newArrayType = error "Not relevant to WASM"

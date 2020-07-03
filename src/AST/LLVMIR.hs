module AST.LLVMIR where
import           AST.Typed
import           LLVM.AST.Type

data LLVMType = LLVMType { size :: Int
                         , ty   :: Type
                         }
              deriving (Eq, Ord, Show)

makeLLVMType :: Type -> LLVMType
makeLLVMType = error "Not implemented"

getFieldTypes :: LLVMType -> [LLVMType]
getFieldTypes = error "NYI"

getElemType :: LLVMType -> LLVMType
getElemType = error "NYI"

instance Typed LLVMType where
  numBits = size
  isSignedInt = const False
  isUnsignedInt llvmty = case ty llvmty of
                           IntegerType{} -> True
                           _             -> False
  isDouble llvmty = case ty llvmty of
                      FloatingPointType{} -> True
                      _                   -> False
  isPointer llvmty = case ty llvmty of
                       PointerType{} -> True
                       _             -> False
  pointeeType llvmty = case ty llvmty of
                         PointerType refTy _ -> makeLLVMType refTy
                         _ -> error "Cannot call pointee type on non-pointer type"
  isStruct llvmty = case ty llvmty of
                      StructureType{} -> True
                      _               -> False
  structFieldTypes llvmty = case ty llvmty of
                              StructureType{} -> getFieldTypes llvmty
                              _ -> error "Must call structFieldTypes on struct"
  structFieldList s =
      error $ unwords ["Cannot call structFieldList on LLVM type", show s]

  isArray llvmty = case ty llvmty of
                     ArrayType{} -> True
                     _           -> False
  arrayBaseType llvmty = case ty llvmty of
                           ArrayType{} -> getElemType llvmty
                           _ -> error "Must call arrayBaseType on array"
  arrayNumElems llvmty = case ty llvmty of
                           ArrayType n _ -> fromIntegral n
                           _ -> error "Must call arrayNumElems on array"
  newStructType tys = makeLLVMType $ StructureType False $ map (ty . snd) tys
  newArrayType n e = makeLLVMType $ ArrayType (fromIntegral n) $ ty e







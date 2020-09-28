module AST.C where
import           Language.C.Data.Ident
import           Language.C.Syntax.AST

-- Types

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
          | Struct [(String, Type)]
          | Array (Maybe Int) Type
          | Void
          | Char
          | Float
          deriving (Eq, Ord, Show)

isIntegerType :: Type -> Bool
isIntegerType ty = isSignedInt ty || isUnsignedInt ty

makeType :: Int -> Bool -> Type
makeType numBits isSigned = case numBits of
  8 | isSigned  -> S8
  8             -> U8
  16 | isSigned -> S16
  16            -> U16
  32 | isSigned -> S32
  32            -> U32
  64 | isSigned -> S64
  64            -> U64
  _             -> error "Unexpected width to makeType"

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

newStructType :: [(String, Type)] -> Type
newStructType = Struct

newArrayType :: Int -> Type -> Type
newArrayType = Array . Just

int8, int16, int32, int64 :: Type -> Bool
int8 S8 = True
int8 U8 = True
int8 _  = False
int16 S16 = True
int16 U16 = True
int16 _   = False
int32 S32     = True
int32 U32     = True
int32 Ptr32{} = True
int32 _       = False
int64 S64     = True
int64 U64     = True
int64 Ptr64{} = True
int64 _       = False

type FunctionName = String

-- Other Helpers

nameFromFunc :: CFunctionDef a -> String
nameFromFunc (CFunDef _ decl _ _ _) = nameFromIdent $ identFromDecl decl

baseTypeFromFunc :: CFunctionDef a -> [CDeclarationSpecifier a]
baseTypeFromFunc (CFunDef tys _ _ _ _) = tys

bodyFromFunc :: CFunctionDef a -> CStatement a
bodyFromFunc (CFunDef _ _ _ stmt _) = stmt

ptrsFromFunc :: (Show a) => CFunctionDef a -> [CDerivedDeclarator a]
ptrsFromFunc (CFunDef _ decl _ _ _) = case derivedFromDecl decl of
  _ : ptrs -> ptrs
  f        -> error $ unwords ["Expected function declaration but got", show f]

argsFromFunc :: (Show a) => CFunctionDef a -> [CDeclaration a]
argsFromFunc (CFunDef _ decl _ _ _) = case derivedFromDecl decl of
  (CFunDeclr (Right decls) _ _) : _ -> fst decls
  f -> error $ unwords ["Expected function declaration but got", show f]

derivedFromDecl :: CDeclarator a -> [CDerivedDeclarator a]
derivedFromDecl (CDeclr _ derived _ _ _) = derived

identFromDecl :: CDeclarator a -> Ident
identFromDecl (CDeclr mIdent _ _ _ _) = case mIdent of
  Nothing -> error "Expected identifier in declarator"
  Just i  -> i

identToVarName :: Ident -> String
identToVarName (Ident name _ _) = name

specToStorage :: (Show a) => CDeclarationSpecifier a -> CStorageSpecifier a
specToStorage spec = case spec of
  CStorageSpec s -> s
  s -> error $ unwords ["Expected storage specifier in declaration", show s]

specToType :: CDeclarationSpecifier a -> CTypeSpecifier a
specToType spec = case spec of
  CTypeSpec ts -> ts
  _            -> error "Expected type specificer in declaration"

-- General utilities

-- Declarators

identFromDeclr :: CDeclarator a -> Maybe Ident
identFromDeclr (CDeclr ids _ _ _ _) = ids

derivedFromDeclr :: CDeclarator a -> [CDerivedDeclarator a]
derivedFromDeclr (CDeclr _ derived _ _ _) = derived

---- don't know what the maybe string literal is

attrsFromDeclr :: CDeclarator a -> [CAttribute a]
attrsFromDeclr (CDeclr _ _ _ attrs _) = attrs

-- Declaration specifiers

isStorageSpec :: CDeclarationSpecifier a -> Bool
isStorageSpec CStorageSpec{} = True
isStorageSpec _              = False

storageFromSpec :: CDeclarationSpecifier a -> CStorageSpecifier a
storageFromSpec (CStorageSpec spec) = spec
storageFromSpec _                   = error "Expected storage specifier"

isTypeSpec :: CDeclarationSpecifier a -> Bool
isTypeSpec CTypeSpec{} = True
isTypeSpec _           = False

typeFromSpec :: (Show a) => CDeclarationSpecifier a -> Maybe (CTypeSpecifier a)
typeFromSpec (CTypeSpec spec) = Just spec
typeFromSpec _                = Nothing

isTypeQual :: CDeclarationSpecifier a -> Bool
isTypeQual CTypeQual{} = True
isTypeQual _           = False

qualFromSpec :: CDeclarationSpecifier a -> CTypeQualifier a
qualFromSpec (CTypeQual spec) = spec
qualFromSpec _                = error "Expected type qualifier"

isFuncSpec :: CDeclarationSpecifier a -> Bool
isFuncSpec CFunSpec{} = True
isFuncSpec _          = False

funcFromSpec :: CDeclarationSpecifier a -> CFunctionSpecifier a
funcFromSpec (CFunSpec spec) = spec
funcFromSpec _               = error "Expected function specifier"

isAlignSpec :: CDeclarationSpecifier a -> Bool
isAlignSpec CAlignSpec{} = True
isAlignSpec _            = False

alignFromSpec :: CDeclarationSpecifier a -> CAlignmentSpecifier a
alignFromSpec (CAlignSpec spec) = spec
alignFromSpec _                 = error "Expected alignment specifier"

-- Storage specifiers

isAuto :: CStorageSpecifier a -> Bool
isAuto CAuto{} = True
isAuto _       = False

isRegister :: CStorageSpecifier a -> Bool
isRegister CRegister{} = True
isRegister _           = False

isStatic :: CStorageSpecifier a -> Bool
isStatic CStatic{} = True
isStatic _         = False

isExtern :: CStorageSpecifier a -> Bool
isExtern CExtern{} = True
isExtern _         = False

isTypedef :: CStorageSpecifier a -> Bool
isTypedef CTypedef{} = True
isTypedef _          = False

isThread :: CStorageSpecifier a -> Bool
isThread CThread{} = True
isThread _         = False

isKernelFn :: CStorageSpecifier a -> Bool
isKernelFn CClKernel{} = True
isKernelFn _           = False

isGlobal :: CStorageSpecifier a -> Bool
isGlobal CClGlobal{} = True
isGlobal _           = False

isLocal :: CStorageSpecifier a -> Bool
isLocal CClLocal{} = True
isLocal _          = False

-- Type qualifiers

isConstQualifier :: CTypeQualifier a -> Bool
isConstQualifier CConstQual{} = True
isConstQualifier _            = False

isVolatileQualifier :: CTypeQualifier a -> Bool
isVolatileQualifier CVolatQual{} = True
isVolatileQualifier _            = False

isRestrictQualifier :: CTypeQualifier a -> Bool
isRestrictQualifier CRestrQual{} = True
isRestrictQualifier _            = False

isAtomicQualifier :: CTypeQualifier a -> Bool
isAtomicQualifier CAtomicQual{} = True
isAtomicQualifier _             = False

isAttributeQualifier :: CTypeQualifier a -> Bool
isAttributeQualifier CAttrQual{} = True
isAttributeQualifier _           = False

attrFromQualifier :: CTypeQualifier a -> CAttribute a
attrFromQualifier (CAttrQual attr) = attr
attrFromQualifier _                = error "Expected attribute qualifier"

isNullableQualifier :: CTypeQualifier a -> Bool
isNullableQualifier CNullableQual{} = True
isNullableQualifier _               = False

isNonnullableQualifier :: CTypeQualifier a -> Bool
isNonnullableQualifier CNonnullQual{} = True
isNonnullableQualifier _              = False

isReadOnlyQualifier :: CTypeQualifier a -> Bool
isReadOnlyQualifier CClRdOnlyQual{} = True
isReadOnlyQualifier _               = False

isWriteOnlyQualifier :: CTypeQualifier a -> Bool
isWriteOnlyQualifier CClWrOnlyQual{} = True
isWriteOnlyQualifier _               = False

-- Derived declarators

isPtrDecl :: CDerivedDeclarator a -> Bool
isPtrDecl CPtrDeclr{} = True
isPtrDecl _           = False

getTypesFromPtrDecl :: CDerivedDeclarator a -> [CTypeQualifier a]
getTypesFromPtrDecl (CPtrDeclr quals _) = quals
getTypesFromPtrDecl _ = error "Expected pointer derived declarator"

isArrDecl :: CDerivedDeclarator a -> Bool
isArrDecl CArrDeclr{} = True
isArrDecl _           = False

getInfoFromArrDecl :: CDerivedDeclarator a -> ([CTypeQualifier a], CArraySize a)
getInfoFromArrDecl (CArrDeclr ty size _) = (ty, size)
getInfoFromArrDecl _                     = error "Expected array declarator"

isFunDecl :: CDerivedDeclarator a -> Bool
isFunDecl CFunDeclr{} = True
isFunDecl _           = False

getInfoFromFunDecl
  :: CDerivedDeclarator a
  -> (Either [Ident] ([CDeclaration a], Bool), [CAttribute a])
getInfoFromFunDecl (CFunDeclr a b _) = (a, b)
getInfoFromFunDecl _ = error "nyi"

-- Misc

nameFromIdent :: Ident -> String
nameFromIdent (Ident name _ _) = name



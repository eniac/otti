module AST.C where
import           AST.Simple
import           Data.Maybe            (isJust)
import           Language.C.Data.Ident
import           Language.C.Syntax.AST

bodyFromFunc :: CFunctionDef a -> CStatement a
bodyFromFunc (CFunDef _ _ _ stmt _) = stmt

argsFromFunc :: (Show a) => CFunctionDef a -> [CDeclaration a]
argsFromFunc (CFunDef _ decl _ _ _) =
  case derivedFromDecl decl of
    (CFunDeclr (Right decls) _ _):_ -> fst decls
    f ->
      error $ unwords ["Expected function declaration but got", show f]

derivedFromDecl :: CDeclarator a -> [CDerivedDeclarator a]
derivedFromDecl (CDeclr _ derived _ _ _) = derived

identFromDecl :: CDeclarator a -> Ident
identFromDecl (CDeclr mIdent _ _ _ _) = case mIdent of
                                          Nothing -> error "Expected identifier in declarator"
                                          Just i  -> i

identToVarName :: Ident -> String
identToVarName (Ident name _ _) = name

specToStorage :: (Show a) => CDeclarationSpecifier a -> CStorageSpecifier a
specToStorage spec =
  case spec of
    CStorageSpec s -> s
    s              -> error $ unwords ["Expected storage specifier in declaration", show s]

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

typeFromSpec :: CDeclarationSpecifier a -> CTypeSpecifier a
typeFromSpec (CTypeSpec spec) = spec
typeFromSpec _                = error "Expected type specifier"

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
getTypesFromPtrDecl _                   = error "Expected pointer derived declarator"

isArrDecl :: CDerivedDeclarator a -> Bool
isArrDecl CArrDeclr{} = True
isArrDecl _           = False

getInfoFromArrDecl :: CDerivedDeclarator a -> ([CTypeQualifier a], CArraySize a)
getInfoFromArrDecl (CArrDeclr ty size _) = (ty, size)
getInfoFromArrDecl _                     = error "Expected array declarator"

isFunDecl :: CDerivedDeclarator a -> Bool
isFunDecl CFunDeclr{} = True
isFunDecl _           = False

getInfoFromFunDecl :: CDerivedDeclarator a -> (Either [Ident] ([CDeclaration a], Bool), [CAttribute a])
getInfoFromFunDecl (CFunDeclr a b _) = (a, b)






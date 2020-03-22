module AST.C where
import           AST.Simple
import           Data.Maybe            (isJust)
import           Language.C.Data.Ident
import           Language.C.Syntax.AST

bodyFromFunc :: CFunctionDef a -> CStatement a
bodyFromFunc (CFunDef _ _ _ stmt _) = stmt

argsFromFunc :: CFunctionDef a -> [CDeclaration a]
argsFromFunc (CFunDef _ decl _ _ _) =
  case derivedFromDecl decl of
    [CFunDeclr (Right decls) _ _] -> fst decls
    _                             -> error "Expected function declaration"

derivedFromDecl :: CDeclarator a -> [CDerivedDeclarator a]
derivedFromDecl (CDeclr _ derived _ _ _) = derived

ctypeToType :: (Show a) => [CTypeSpecifier a] -> Type
ctypeToType ty = case ty of
                   [CVoidType{}]   -> Void
                   [CCharType{}]   -> Char
                   [CIntType{}]    -> S32
                   [CFloatType{}]  -> Float
                   [CDoubleType{}] -> Double
                   [ty] -> error $ unwords  ["Unexpected type", show ty]
                   [CLongType{}, CUnsigType{}, CIntType{}] -> U64
                   ty -> error $ unwords ["Unexpected type", show ty]

identFromDecl :: CDeclarator a -> Ident
identFromDecl (CDeclr mIdent _ _ _ _) = case mIdent of
                                          Nothing -> error "Expected identifier in declarator"
                                          Just i  -> i

identToVarName :: Ident -> String
identToVarName (Ident name _ _) = name

specToStorage :: CDeclarationSpecifier a -> CStorageSpecifier a
specToStorage spec = case spec of
                       CStorageSpec s -> s
                       _              -> error "Expected storage specifier in declaration"

specToType :: CDeclarationSpecifier a -> CTypeSpecifier a
specToType spec = case spec of
                    CTypeSpec ts -> ts
                    _            -> error "Expected type specificer in declaration"

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

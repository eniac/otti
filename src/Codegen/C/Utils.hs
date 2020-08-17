module Codegen.C.Utils where
import           AST.C
import           AST.Simple
import           Codegen.C.CompilerMonad
import           Language.C.Data.Ident
import           Language.C.Syntax.AST

-- | When expr appears on the lhs of an assignment, the assignment is actually a store
isStore :: (Show a) => CExpression a -> Bool
isStore expr = case expr of
                 CIndex{}          -> True
                 CUnary CIndOp _ _ -> True
                 CVar{}            -> False
                 _ -> error $ unwords ["Unexpected assignment with", show expr]

--

ctype :: (Show a) => [CDeclarationSpecifier a] -> [CDerivedDeclarator a] -> Compiler Type
ctype tys ptrs = do
  ty <- baseTypeFromSpecs tys
  return $ getTy ty ptrs

refTy :: Type -> Type
refTy (Ptr32 ty) = ty
refTy (Ptr64 ty) = ty
refTy ty         = error $ unwords ["Expected pointer ty in refTy", show ty]

-- helpers to be renamed

ctypeToType :: (Show a) => [CTypeSpecifier a] -> Compiler Type
ctypeToType ty = case ty of
                   [CVoidType{}]   -> return Void
                   [CTypeDef (Ident name _ _) _] -> untypedef name
                   [CCharType{}]   -> return S8
                   [CBoolType{}]   -> return Bool
                   [CUnsigType{}, CCharType{}] -> return U8
                   [CIntType{}]    -> return S32
                   [CUnsigType{}]  -> return U32
                   [CFloatType{}]  -> return Float
                   [CDoubleType{}] -> return Double
                   [ty] -> error $ unwords  ["Unexpected type", show ty]
                   [CLongType{}, CUnsigType{}, CIntType{}] -> return U64
                   [CUnsigType{}, CLongType{}, CIntType{}] -> return U64
                   ty -> error $ unwords ["Unexpected type", show ty]

getTy :: (Show a) => Type -> [CDerivedDeclarator a] -> Type
getTy ty [] = ty
getTy ty (d:ds) = case d of
                    _ | isPtrDecl d -> getTy (Ptr32 ty) ds
                    _ -> error "Do not support"

cDeclToType :: (Show a) => CDeclaration a -> Compiler Type
cDeclToType (CDecl specs _ _) = ctypeToType $ map specToType specs

baseTypeFromSpecs :: (Show a) => [CDeclarationSpecifier a] -> Compiler Type
baseTypeFromSpecs all@(elem:rest) =
  if isTypeQual elem || isAlignSpec elem
  then baseTypeFromSpecs rest
  else ctypeToType $ map typeFromSpec all

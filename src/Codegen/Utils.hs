module Codegen.Utils where
import           AST.C
import           AST.Simple
import           Codegen.CompilerMonad
import           Language.C.Data.Ident
import           Language.C.Syntax.AST

ctypeToType :: (Show a) => [CTypeSpecifier a] -> Compiler Type
ctypeToType ty = case ty of
                   [CVoidType{}]   -> return Void
                   [CTypeDef (Ident name _ _) _] -> untypedef name
                   [CCharType{}]   -> return S8
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
                    _ | isPtrDecl d -> getTy (Ptr64 ty) ds
                    _ -> error "Do not support"

baseTypeFromSpecs :: (Show a) => [CDeclarationSpecifier a] -> Compiler Type
baseTypeFromSpecs all@(elem:rest) =
  if isTypeQual elem || isAlignSpec elem
  then baseTypeFromSpecs rest
  else ctypeToType $ map typeFromSpec all

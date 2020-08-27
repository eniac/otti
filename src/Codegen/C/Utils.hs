module Codegen.C.Utils
  ( ctype
  , cDeclToType
  , baseTypeFromSpecs
  )
where
import           AST.C
import           AST.Simple
import           Codegen.C.CompilerMonad
import           Control.Monad.IO.Class
import           Control.Applicative
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants
import           Data.Maybe                     ( fromJust, isJust, catMaybes )

-- | When expr appears on the lhs of an assignment, the assignment is actually a store
isStore :: (Show a) => CExpression a -> Bool
isStore expr = case expr of
  CIndex{}          -> True
  CUnary CIndOp _ _ -> True
  CVar{}            -> False
  _                 -> error $ unwords ["Unexpected assignment with", show expr]

--

ctype
  :: (Show a)
  => [CDeclarationSpecifier a]
  -> [CDerivedDeclarator a]
  -> Compiler (Either String Type)
ctype tys ptrs = do
  ty <- baseTypeFromSpecs tys
  return $ ty >>= flip getTy ptrs

refTy :: Type -> Type
refTy (Ptr32 ty) = ty
refTy (Ptr64 ty) = ty
refTy ty         = error $ unwords ["Expected pointer ty in refTy", show ty]

-- helpers to be renamed

cParseIntTypeLength :: [CTypeSpecifier a] -> Maybe Int
cParseIntTypeLength l = case l of
  [CCharType{}] -> Just 8
  [CShortType{}] -> Just 16
  [CShortType{},CIntType{}] -> Just 16
  -- Not quite right
  [] -> Just 32
  [CIntType{}] -> Just 32
  [CLongType{}] -> Just 32
  [CLongType{},CIntType{}] -> Just 32
  [CLongType{},CLongType{}] -> Just 64
  [CLongType{},CLongType{},CIntType{}] -> Just 64
  _ -> Nothing

cParseIntType :: [CTypeSpecifier a] -> Maybe Type
cParseIntType l = case l of
  CUnsigType {}: r -> flip makeType False <$> cParseIntTypeLength r
  CSignedType {}: r -> flip makeType True <$> cParseIntTypeLength r
  r -> flip makeType True <$> cParseIntTypeLength r

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

ctypeToType :: (Show a) => [CTypeSpecifier a] -> Compiler (Either String Type)
ctypeToType ty = case ty of
  [CVoidType{}] -> return $ Right Void
  [CTypeDef (Ident name _ _) _] ->
    maybeToEither "Missing typedef" <$> untypedef name
  [CBoolType{}]                -> return $ Right Bool
  [CFloatType{} ]              -> return $ Right Float
  [CDoubleType{}]              -> return $ Right Double
  _ | isJust (cParseIntType ty) -> return $ Right $ fromJust $ cParseIntType ty
  [ty] -> return $ Left $ unwords ["Unexpected type", show ty]
  ty -> return $ Left $ unwords ["Unexpected type", show ty]

getTy :: (Show a) => Type -> [CDerivedDeclarator a] -> (Either String Type)
getTy ty []       = Right ty
getTy ty (d : ds) = case d of
  _ | isPtrDecl d -> getTy (Ptr32 ty) ds
  CArrDeclr _ (CArrSize _ (CConst (CIntConst (CInteger i _ _) _))) _ ->
    getTy (Array (Just $ fromIntegral i) ty) ds
  CArrDeclr _ (CNoArrSize _) _ -> getTy (Array Nothing ty) ds
  _                            -> Left $ "Do not support type " ++ show d

cDeclToType :: (Show a) => CDeclaration a -> Compiler (Either String Type)
cDeclToType (CDecl specs _ _) = ctypeToType $ map specToType specs

baseTypeFromSpecs
  :: (Show a) => [CDeclarationSpecifier a] -> Compiler (Either String Type)
baseTypeFromSpecs all@(elem : rest) = if isTypeQual elem || isAlignSpec elem
  then baseTypeFromSpecs rest
  else ctypeToType $ catMaybes $ map typeFromSpec all

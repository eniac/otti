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


ctypeToType :: CTypeSpecifier a -> Type
ctypeToType ty = case ty of
                   CVoidType{}   -> Void
                   CCharType{}   -> Char
                   CShortType{}  -> U8
                   CIntType{}    -> S32
                   CLongType{}   -> S64
                   CFloatType{}  -> Float
                   CDoubleType{} -> Double
                   _             -> error "Not done"


declToVarName :: CDeclarator a -> String
declToVarName (CDeclr mIdent _ _ _ _) = case mIdent of
                                          Nothing -> error "Expected identifier in declarator"
                                          Just i  -> identToVarName i

identToVarName :: Ident -> String
identToVarName (Ident name _ _) = name

specToType :: CDeclarationSpecifier a -> CTypeSpecifier a
specToType spec = case spec of
                    CTypeSpec ts -> ts
                    _            -> error "Expected type specificer in declaration"

-- toSimpleFunction :: CFunDef -> Function
-- toSimpleFunction = undefined

-- toSimpleStmt :: CStat -> Stmt
-- toSimpleStmt = undefined

-- toSimpleExpr :: CExpr -> Expr
-- toSimpleExpr expr =
--   case expr of
--     CCond cond mt f _ | isJust mt -> error ""
--     CBinary op l r _  ->
--       let left  = toSimpleExpr l
--           right = toSimpleExpr r
--       in case op of
--            CAddOp -> Add left right
--            _      -> error ""
--     _ -> error $ unwords [ "Unsupported expression in C AST"
--                          , show expr
--                          ]

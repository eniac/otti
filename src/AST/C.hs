module AST.C where
import           AST.Simple
import           Data.Maybe            (isJust)
import           Language.C.Syntax.AST

toSimpleFunction :: CFunDef -> Function
toSimpleFunction = undefined

toSimpleStmt :: CStat -> Stmt
toSimpleStmt = undefined

toSimpleExpr :: CExpr -> Expr
toSimpleExpr expr =
  case expr of
    CCond cond mt f _ | isJust mt -> error ""
    CBinary op l r _  ->
      let left  = toSimpleExpr l
          right = toSimpleExpr r
      in case op of
           CAddOp -> Add left right
           _      -> error ""
    _ -> error $ unwords [ "Unsupported expression in C AST"
                         , show expr
                         ]

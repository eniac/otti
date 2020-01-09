module Static.StaticCheck where
import           AST.Simple
import           Static.Checker

checkStmt :: a -> Stmt -> a
checkStmt checker stmt =
  case stmt of
    Return expr      -> error ""
    While expr stmts -> error ""
    If c t f         -> error ""
    Store e1 e2      -> error ""
    Assign v1 e2     -> error ""

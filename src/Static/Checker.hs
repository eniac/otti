{-# LANGUAGE MultiParamTypeClasses #-}
module Static.Checker where
import           AST.Simple

data Bug = Bug

class Checker a where
  checkStmt :: a -> Stmt -> a
  checkExpr :: a -> Expr -> a
  getBugs :: a -> Bug

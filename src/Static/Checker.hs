{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Static.Checker where
import           AST.Simple


class Checker a b | a -> b where
  checkExpr :: a -> Expr -> a
  getBugs :: a -> b



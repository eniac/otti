{-# OPTIONS_GHC -Wall #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.Circom.Typing
  ( InstanceType
  , emptyType
  )
where

import qualified AST.Circom                    as AST

-- import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe

data InstanceType = InstanceType { inputs :: Map.Map String [Int]
                                 , outputs :: Map.Map String [Int] }
                                 deriving (Show,Ord,Eq)

emptyType = InstanceType Map.empty Map.empty

noSigDecls :: AST.Statement -> Bool
noSigDecls s = case s of
  AST.Assign{}          -> True
  AST.OpAssign{}        -> True
  AST.AssignConstrain{} -> True
  AST.Constrain{}       -> True
  AST.VarDeclaration{}  -> True
  AST.SigDeclaration{}  -> False
  AST.SubDeclaration{}  -> True
  AST.If _ true falseOpt ->
    noSigDeclsStatements true && Maybe.maybe True noSigDeclsStatements falseOpt
  AST.For init _ step body ->
    noSigDecls init && noSigDecls step && noSigDeclsStatements body
  AST.While   _    body -> noSigDeclsStatements body
  AST.DoWhile body _    -> noSigDeclsStatements body
  AST.Compute body      -> noSigDeclsStatements body
  AST.Return{}          -> True
  AST.Log{}             -> True
  AST.Ignore{}          -> True

noSigDeclsStatements :: [AST.Statement] -> Bool
noSigDeclsStatements = all noSigDecls

-- Doesn't work because we need to execute.
--
-- newtype TypeCtx a = TypeCtx (State InstanceType a)
--     deriving (Functor, Applicative, Monad, MonadState InstanceType)
--
-- typeStatement :: AST.Statement -> TypeCtx ()
-- typeStatement s = case s of
--     AST.Assign {} -> return ()
--     AST.OpAssign {} -> return ()
--     AST.AssignConstrain {} -> return ()
--     AST.Constrain {} -> return ()
--     AST.VarDeclaration {} -> return ()
--     AST.SigDeclaration name kind dims -> False
--     AST.SubDeclaration {} -> True
--     AST.If _ true falseOpt -> noSigDeclsStatements true && Maybe.maybe True noSigDeclsStatements falseOpt
--     AST.For init _ step body -> noSigDecls init && noSigDecls step && noSigDeclsStatements body
--     AST.While _ body -> noSigDeclsStatements body
--     AST.DoWhile body _ -> noSigDeclsStatements body
--     AST.Compute body -> noSigDeclsStatements body
--     AST.Return {} -> True
--     AST.Log {} -> True
--     AST.Ignore {} -> True

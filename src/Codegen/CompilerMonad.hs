{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.CompilerMonad where
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Targets.SMT
import qualified Z3.Monad                   as Z

-- | Internal state of the compiler for both translation to IR
-- and for code generation.
data CompilerState = CompilerState { vars :: M.Map String Int
                                   }

newtype Compiler a = Compiler (StateT CompilerState SMT a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

instance Z.MonadZ3 Compiler where
    getSolver = Compiler $ lift $ Z.getSolver
    getContext = Compiler $ lift $ Z.getContext

instance MonadFail Compiler where
    fail = error "FAILED"

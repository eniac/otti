{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.CompilerMonad where
import           AST.Simple
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List                  (isInfixOf)
import qualified Data.Map                   as M
import           Targets.SMT
import qualified Z3.Monad                   as Z

type Version = Int

-- | Internal state of the compiler for both translation to IR
-- and for code generation.
data CompilerState = CompilerState { vars :: M.Map VarName Version
                                   , tys  :: M.Map VarName Type
                                   }

newtype Compiler a = Compiler (StateT CompilerState SMT a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

instance Z.MonadZ3 Compiler where
    getSolver = Compiler $ lift $ Z.getSolver
    getContext = Compiler $ lift $ Z.getContext

instance MonadFail Compiler where
    fail = error "FAILED"

---
--- Setup, monad functions, etc
---

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState M.empty M.empty

liftSMT :: SMT a -> Compiler a
liftSMT = Compiler . lift

runCodegen :: Maybe Integer -- ^ Optional timeout
           -> Compiler a       -- ^ Codegen computation
           -> IO (a, CompilerState)
runCodegen mTimeout (Compiler act) = evalSMT mTimeout $ runStateT act emptyCompilerState

evalCodegen :: Maybe Integer -> Compiler a -> IO a
evalCodegen mt act = fst <$> runCodegen mt act

execCodegen :: Maybe Integer -> Compiler a -> IO CompilerState
execCodegen mt act = snd <$> runCodegen mt act

runSolverOnSMT :: Compiler SMTResult
runSolverOnSMT = liftSMT runSolver

---
---
---

declareVar :: VarName -> Type -> Compiler ()
declareVar varName ty = undefined

nextVar :: VarName -> Compiler ()
nextVar = undefined

getVarNode :: VarName -> Compiler ()
getVarNode name = undefined





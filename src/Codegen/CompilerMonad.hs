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
-- | Variable name and de-ambiguating caller context information.
-- This way, you can have two variables name 'foo' in different functions
-- and everything will still work.
-- Right now, we do this via the entire callstack, but we should actually just hash it.
data ASTVar = ASTVar VarName [FunctionName]
            deriving (Eq, Ord, Show)

-- | Internal state of the compiler for code generation
data CompilerState = CompilerState { -- Mapping AST variables to information
                                     vars             :: M.Map ASTVar Version
                                   , tys              :: M.Map ASTVar Type
                                     -- Codegen context information
                                   , callStack        :: [FunctionName]
                                   , conditionalGuard :: Maybe Node
                                     -- SMT variables and memory
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
emptyCompilerState = CompilerState M.empty M.empty [] Nothing

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

-- | Take a VarName (the AST's representation of a variable) and turn it into an
-- ASTVar---a variable that can be distinguished from other variables of the same
-- name by its caller context.
astVar :: VarName -> Compiler ASTVar
astVar varName = do
  stack <- callStack `liftM` get
  return $ ASTVar varName stack

-- | Declare a new variable, or error if the variable is already declared.
-- This adds the variable's version information (for SSA-ing) and type information
-- to the compiler state.
declareVar :: VarName -> Type -> Compiler ()
declareVar varName ty = do
  var <- astVar varName
  s0 <- get
  let allVars = vars s0
      allTys = tys s0
  case M.lookup var allVars of
    Just{} -> error $ unwords ["Already declared", varName, "in current scope"]
    _ -> put $ s0 { vars = M.insert var 0 allVars
                  , tys = M.insert var ty allTys
                  }

nextVar :: VarName -> Compiler ()
nextVar = undefined

getVarNode :: VarName -> Compiler ()
getVarNode name = undefined





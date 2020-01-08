{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.CompilerMonad where
import           AST.Simple
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List                  (intercalate, isInfixOf)
import qualified Data.Map                   as M
import           IR.SMT
import qualified Z3.Monad                   as Z

{-|

Module that defines the Compiler monad, the monad that keeps track of all internal
state for code generation. This state includes:
WHAT

There are many bits of low-hanging optimization fruit here. For example, we are doing
redundant state lookups, and storing lists of function names instead of a hash of
such a thing.

-}

type Version = Int

data CodegenVar = CodegenVar VarName Version
                deriving (Eq, Ord, Show)

-- | Internal state of the compiler for code generation
data CompilerState = CompilerState { -- Mapping AST variables etc to information
                                     vers              :: M.Map VarName Version
                                   , tys               :: M.Map VarName Type
                                   , funs              :: M.Map FunctionName Function
                                     -- Codegen context information
                                   , callStack         :: [FunctionName]
                                   , conditionalGuards :: [SMTNode]
                                   , returnValueGuards :: [[SMTNode]]
                                   , returnValues      :: [SMTNode]
                                   , ctr               :: Int -- To disambiguate retVals
                                     -- SMT variables: SSA'd versions of AST variables
                                   , vars              :: M.Map CodegenVar SMTNode
                                   }

newtype Compiler a = Compiler (StateT CompilerState IR a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

instance Z.MonadZ3 Compiler where
    getSolver = Compiler $ lift $ Z.getSolver
    getContext = Compiler $ lift $ Z.getContext

instance MonadFail Compiler where
    fail = error "FAILED"

prettyState :: Compiler ()
prettyState = do
  s0 <- get
  liftIO $ putStrLn $ unlines [ "----Versions----"
                              , show $ vers s0
                              , "----Types----"
                              , show $ tys s0
                              , "----Call stack---"
                              , unlines $ map show $ callStack s0
                              , "----Conditional guards----"
                              , show $ length $ conditionalGuards s0
                              , "----Return value guards----"
                              , show $ length $ returnValueGuards s0
                              , "----Return values----"
                              , show $ length $ returnValues s0
                              , show $ returnValues s0
                              , "----Ctr----"
                              , show $ ctr s0
                              , "----Variables----"
                              , show $ vars s0
                              ]

---
--- Setup, monad functions, etc
---

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState M.empty M.empty M.empty [] [] [[]] [] 1 M.empty

liftIR :: IR a -> Compiler a
liftIR = Compiler . lift

runCodegen :: Maybe Integer -- ^ Optional timeout
           -> Compiler a       -- ^ Codegen computation
           -> IO (a, CompilerState)
runCodegen mTimeout (Compiler act) = evalIR mTimeout $ runStateT act emptyCompilerState

evalCodegen :: Maybe Integer -> Compiler a -> IO a
evalCodegen mt act = fst <$> runCodegen mt act

execCodegen :: Maybe Integer -> Compiler a -> IO CompilerState
execCodegen mt act = snd <$> runCodegen mt act

runSolverOnSMT :: Compiler SMTResult
runSolverOnSMT = liftIR smtResult

---
---
---

-- Turning VarNames (the AST's representation of a variable) into other representations
-- of variables

codegenVar :: VarName -> Compiler CodegenVar
codegenVar var = do
  ver <- getVer var
  return $ CodegenVar var ver

-- | Human readable name.
-- We probably want to replace this with something faster (eg hash) someday, but
-- this is great for debugging
codegenToName :: CodegenVar -> String
codegenToName (CodegenVar varName ver) = varName ++ "_" ++ show ver

---
---
---

-- | Declare a new variable, or error if the variable is already declared.
-- This adds the variable's version information (for SSA-ing) and type information
-- to the compiler state.
declareVar :: VarName -> Type -> Compiler ()
declareVar var ty = do
  s0 <- get
  let allVers = vers s0
      allTys  = tys s0
  case M.lookup var allVers of
    Nothing -> put $ s0 { vers = M.insert var 0 allVers
                        , tys = M.insert var ty allTys
                        }
    _       -> error $ unwords ["Already declared", var, "in current scope"]

-- | Bump the given variable up in version (for SSA)
nextVer :: VarName -> Compiler ()
nextVer var = do
  s0 <- get
  let allVers = vers s0
  case M.lookup var allVers of
    Just ver -> put $ s0 { vers = M.insert var (ver + 1) allVers }
    _ -> error $ unwords ["Cannot increment version of undeclared", var]

-- | Get the current version of the variable
getVer :: VarName -> Compiler Version
getVer var = do
  allVers <- vers `liftM` get
  case M.lookup var allVers of
    Just ver -> return ver
    _        -> error $ unwords ["Cannot get version of undeclared", var]

-- | Get the C++ type of the variable
getType :: VarName -> Compiler Type
getType var = do
  allTys <- tys `liftM` get
  case M.lookup var allTys of
    Just ty -> return ty
    _       -> error $ unwords ["Cannot get type of undeclared", var]

-- | Get an SMT node representing the given var
getNodeFor :: VarName -> Compiler SMTNode
getNodeFor varName = do
  var <- codegenVar varName
  s0 <- get
  let allVars = vars s0
  case M.lookup var allVars of
    Just node -> return node
    Nothing -> do
      ty <- getType varName
      node <- liftIR $ newVar ty $ codegenToName var
      put $ s0 { vars = M.insert var node allVars }
      return node

---
--- Functions
---

clearBetweenAnalyzingFunctions :: Compiler ()
clearBetweenAnalyzingFunctions = do
  s0 <- get
  put $ s0 { callStack = []
           , conditionalGuards = []
           , returnValueGuards = []
           , returnValues = []
           }

pushFunction :: FunctionName
             -> SMTNode
             -> Compiler ()
pushFunction funName returnVal = do
  s0 <- get
  put $ s0 { callStack = funName:callStack s0
           , returnValues = returnVal:returnValues s0
           , returnValueGuards = []:returnValueGuards s0
           }

popFunction :: Compiler ()
popFunction = do
  s0 <- get
  when (null $ callStack s0) $ error "Tried to pop context off empty call stack"
  when (null $ returnValues s0) $ error "Tried to pop return val off empty stack"
  put $ s0 { callStack = tail $ callStack s0
           , returnValues = tail $ returnValues s0
           , returnValueGuards = tail $ returnValueGuards s0
           }

getReturnValName :: FunctionName
                 -> Compiler VarName
getReturnValName funName = do
  s0 <- get
  let num = ctr s0
  put $ s0 { ctr = num + 1 }
  return $ funName ++ "_retVal_" ++ show num

getReturnVal :: Compiler SMTNode
getReturnVal = do
  retVals <- returnValues `liftM` get
  return $ head retVals

getFunction :: FunctionName
             -> Compiler Function
getFunction funName = do
  functions <- funs `liftM` get
  case M.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords $ ["Called undeclared function", funName]

registerFunction :: Function
                 -> Compiler ()
registerFunction function = do
  forM_ (fArgs function) $ uncurry $ declareVar
  s0 <- get
  let funName = fName function
  case M.lookup funName $ funs s0 of
    Nothing -> put $ s0 { funs = M.insert funName function $ funs s0 }
    _       -> error $ unwords ["Already declared", fName function]

---
--- If-statements
---

pushCondGuard :: SMTNode
             -> Compiler ()
pushCondGuard guardNode = do
  s0 <- get
  put $ s0 { conditionalGuards = guardNode:conditionalGuards s0 }

popCondGuard :: Compiler ()
popCondGuard = do
  s0 <- get
  when (null $ conditionalGuards s0) $ error "Tried to pop from empty guard stack"
  put $ s0 { conditionalGuards = tail $ conditionalGuards s0 }

getCurrentGuardNode :: Compiler SMTNode
getCurrentGuardNode = do
  guards <- conditionalGuards `liftM` get
  liftIR $ if null guards
  then smtTrue
  else foldM cppAnd (head guards) (init guards)

---
--- Return values
--- We need to keep track of which return values we've already guarded with in order to
-- handle cases like this:
-- if (x) {
--   if (y) return 3;
--   return 4;
-- }
-- return 5;
--
-- This should become:
-- x && y => rv = 3
-- x && !(x && y) => rv = 4
-- !x && !(x && y) => rv = 5

addReturnGuard :: SMTNode
               -> Compiler ()
addReturnGuard guardNode = do
  s0 <- get
  notGuard <- liftIR $ cppBitwiseNeg guardNode
  let allGuards = returnValueGuards s0
      updatedGuards = notGuard:head allGuards
  put $ s0 { returnValueGuards = updatedGuards:tail allGuards }

getOldReturnGuard :: Compiler SMTNode
getOldReturnGuard = do
  s0 <- get
  let allGuards = returnValueGuards s0
      currentGuards = head allGuards
  if null currentGuards
  then do
    true <- liftIR smtTrue
    put $ s0 { returnValueGuards = [true]:tail allGuards }
    return true
  else liftIR $ foldM cppAnd (head currentGuards) (tail currentGuards)



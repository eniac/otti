{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.CompilerMonad where
import           AST.Simple
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                )
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes )
import           IR.CUtils                     as CUtils
import qualified IR.TySmt                      as Ty
import qualified IR.Memory                     as Mem
import           IR.Memory                      ( Mem )
import           Targets.SMT                    ( SMTResult )
import qualified Targets.SMT.Assert            as Assert
import           Targets.SMT.Assert             ( Assert )
import qualified Z3.Monad                      as Z

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
                                     vers              :: [M.Map VarName Version]
                                   , tys               :: M.Map VarName Type
                                   , funs              :: M.Map FunctionName Function
                                     -- Codegen context information
                                   , typedefs          :: M.Map VarName Type
                                   , callStack         :: [FunctionName]
                                     -- The conditional guards encode a
                                     -- conjunction of conditions required to
                                     -- reach the current path
                                   , conditionalGuards :: [Ty.TermBool]
                                     -- The return guards are a list of
                                     -- conjunctions representing the
                                     -- conditions under which a particular
                                     -- return happens.
                                   , returnValueGuards :: [[Ty.TermBool]]
                                   , returnValues      :: [CTerm]
                                   , ctr               :: Int -- To disambiguate retVals
                                   , loopBound         :: Int
                                     -- SMT variables: SSA'd versions of AST variables
                                   , vars              :: M.Map CodegenVar CTerm
                                   }

newtype Compiler a = Compiler (StateT CompilerState Mem a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

-- instance Z.MonadZ3 Compiler where
--     getSolver = Compiler $ lift $ Z.getSolver
--     getContext = Compiler $ lift $ Z.getContext

instance MonadFail Compiler where
  fail = error "FAILED"

prettyState :: Compiler ()
prettyState = do
  s0 <- get
  liftIO $ putStrLn $ unlines
    [ "----Versions----"
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
emptyCompilerState =
  CompilerState [M.empty] M.empty M.empty M.empty [] [] [[]] [] 1 4 M.empty

liftMem :: Mem a -> Compiler a
liftMem = Compiler . lift

liftAssert :: Assert a -> Compiler a
liftAssert = liftMem . Mem.liftAssert

runCodegen
  :: Maybe Integer -- ^ Optional timeout
  -> Compiler a       -- ^ Codegen computation
  -> Assert (a, CompilerState)
runCodegen mTimeout (Compiler act) =
  Mem.evalMem $ runStateT act emptyCompilerState

evalCodegen :: Maybe Integer -> Compiler a -> Assert a
evalCodegen mt act = fst <$> runCodegen mt act

execCodegen :: Maybe Integer -> Compiler a -> Assert CompilerState
execCodegen mt act = snd <$> runCodegen mt act

-- TODO Run solver on SMT
-- runSolverOnSMT :: Compiler SMTResult
-- runSolverOnSMT = liftMem smtResult

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

pushContext :: Compiler ()
pushContext = do
  s0 <- get
  put $ s0 { vers = M.empty : vers s0 }

popContext :: Compiler ()
popContext = do
  s0 <- get
  case vers s0 of
    [] -> error "Context stack should never be empty"
    ctxts | length ctxts > 1 -> put $ s0 { vers = tail ctxts }
    _ -> error "Cannot pop final context off context stack"

-- | Declare a new variable, or error if the variable is already declared.
-- This adds the variable's version information (for SSA-ing) and type information
-- to the compiler state.
declareVar :: VarName -> Type -> Compiler ()
declareVar var ty = do
  s0 <- get
  let allVers = head $ vers s0
      allTys  = tys s0
  case M.lookup var allVers of
    Nothing -> put $ s0 { vers = (M.insert var 0 allVers) : vers s0
                        , tys  = M.insert var ty allTys
                        }
    _ -> error $ unwords ["Already declared", var, "in current scope"]

-- | Bump the given variable up in version (for SSA)
nextVer :: VarName -> Compiler ()
nextVer var = do
  s0 <- get
  let allVers = head $ vers s0
  case M.lookup var allVers of
    Just ver -> put $ s0 { vers = (M.insert var (ver + 1) allVers) : vers s0 }
    _        -> error $ unwords ["Cannot increment version of undeclared", var]

-- | Get the current version of the variable
getVer :: VarName -> Compiler Version
getVer var = do
  allVers <- vers `liftM` get
  let vars = map (\verCtxt -> M.lookup var verCtxt) allVers
  case catMaybes vars of
    []        -> error $ unwords ["Cannot get version of undeclared", var]
    (ver : _) -> return ver

-- | Get the C++ type of the variable
getType :: VarName -> Compiler Type
getType var = do
  allTys <- tys `liftM` get
  case M.lookup var allTys of
    Just ty -> return ty
    _       -> error $ unwords ["Cannot get type of undeclared", var]

-- | Get an SMT node representing the given var
getNodeFor :: VarName -> Compiler CTerm
getNodeFor varName = do
  var <- codegenVar varName
  s0  <- get
  let allVars = vars s0
  case M.lookup var allVars of
    Just node -> return node
    Nothing   -> do
      ty   <- getType varName
      node <- liftAssert $ newVar ty $ codegenToName var
      put $ s0 { vars = M.insert var node allVars }
      return node

-- Bump the version of `var` and assign `value` to it.
ssaAssign :: VarName -> CTerm -> Compiler CTerm
ssaAssign var val = do
  oldNode <- getNodeFor var
  nextVer var
  newNode <- getNodeFor var
  guard   <- getCurrentGuardNode
  liftAssert $ Assert.assert $ Ty.Ite guard
                                      (cppAssignment newNode val)
                                      (cppAssignment newNode oldNode)
  return newNode

---
--- Functions
---

clearBetweenAnalyzingFunctions :: Compiler ()
clearBetweenAnalyzingFunctions = do
  s0 <- get
  put $ s0 { callStack         = []
           , conditionalGuards = []
           , returnValueGuards = []
           , returnValues      = []
           }

pushFunction :: FunctionName -> CTerm -> Compiler ()
pushFunction funName returnVal = do
  s0 <- get
  put $ s0 { callStack         = funName : callStack s0
           , returnValues      = returnVal : returnValues s0
           , returnValueGuards = [] : returnValueGuards s0
           }

popFunction :: Compiler ()
popFunction = do
  s0 <- get
  when (null $ callStack s0) $ error "Tried to pop context off empty call stack"
  when (null $ returnValues s0)
    $ error "Tried to pop return val off empty stack"
  put $ s0 { callStack         = tail $ callStack s0
           , returnValues      = tail $ returnValues s0
           , returnValueGuards = tail $ returnValueGuards s0
           }

getReturnValName :: FunctionName -> Compiler VarName
getReturnValName funName = do
  s0 <- get
  let num = ctr s0
  put $ s0 { ctr = num + 1 }
  return $ funName ++ "_retVal_" ++ show num

getReturnVal :: Compiler CTerm
getReturnVal = do
  retVals <- returnValues `liftM` get
  case retVals of
    [] -> error "Empty return value list"
    _  -> return $ head retVals

getFunction :: FunctionName -> Compiler Function
getFunction funName = do
  functions <- funs `liftM` get
  case M.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords $ ["Called undeclared function", funName]

registerFunction :: Function -> Compiler ()
registerFunction function = do
  forM_ (fArgs function) $ uncurry $ declareVar
  s0 <- get
  let funName = fName function
  case M.lookup funName $ funs s0 of
    Nothing -> put $ s0 { funs = M.insert funName function $ funs s0 }
    _       -> error $ unwords ["Already declared", fName function]

---
--- Typedefs
---

typedef :: VarName -> Type -> Compiler ()
typedef name ty = do
  s0 <- get
  let tds = typedefs s0
  case M.lookup name tds of
    Nothing -> put $ s0 { typedefs = M.insert name ty tds }
    Just t  -> error $ unwords $ ["Already td'd", name, "to", show t]

untypedef :: VarName -> Compiler Type
untypedef name = do
  tds <- typedefs `liftM` get
  case M.lookup name tds of
    Nothing -> error $ unwords ["No type defined for", name]
    Just ty -> return ty


---
--- If-statements
---

pushCondGuard :: Ty.TermBool -> Compiler ()
pushCondGuard guard = do
  s0 <- get
  put $ s0 { conditionalGuards = guard : conditionalGuards s0 }

popCondGuard :: Compiler ()
popCondGuard = do
  s0 <- get
  when (null $ conditionalGuards s0)
    $ error "Tried to pop from empty guard stack"
  put $ s0 { conditionalGuards = tail $ conditionalGuards s0 }

safeNary :: Ty.TermBool -> Ty.BoolNaryOp -> [Ty.TermBool] -> Ty.TermBool
safeNary id op xs = case xs of
  []  -> id
  [s] -> s
  _   -> Ty.BoolNaryExpr op xs

getCurrentGuardNode :: Compiler Ty.TermBool
getCurrentGuardNode =
  gets (safeNary (Ty.BoolLit True) Ty.And . conditionalGuards)

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

addReturnGuard :: Ty.TermBool -> Compiler ()
addReturnGuard guard = do
  s0 <- get
  let notGuard = Ty.Not guard
  let allGuards     = returnValueGuards s0
      updatedGuards = notGuard : head allGuards
  put $ s0 { returnValueGuards = updatedGuards : tail allGuards }

hasNotReturned :: Compiler Ty.TermBool
hasNotReturned = gets
  ( Ty.Not
  . safeNary (Ty.BoolLit False) Ty.Or
  . map (safeNary (Ty.BoolLit True) Ty.And)
  . returnValueGuards
  )

-- Loops

getLoopBound :: Compiler Int
getLoopBound = loopBound `liftM` get

setLoopBound :: Int -> Compiler ()
setLoopBound bound = do
  s0 <- get
  put $ s0 { loopBound = bound }

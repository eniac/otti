{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.CompilerMonad where
import           AST.Simple
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                , findIndex
                                                )
import           Data.Functor.Identity
import           Data.Foldable
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
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

-- TODO rename
data LexicalScopeState = LexicalScopeState { tys :: M.Map VarName Type
                                 , vers :: M.Map VarName Version
                                 , terms :: M.Map CodegenVar CTerm
                                 , lsPrefix :: String
                                 } deriving (Show)
printLs :: LexicalScopeState -> IO ()
printLs s = putStr $ unlines $
  [ "   LexicalScope:"
  , "    prefix: " ++ lsPrefix s
  , "    versions:"
  ] ++
  [ "     " ++ show var ++ ": " ++ show ver | (var, ver) <- M.toList (vers s) ]

-- Lexical scope functions

initialVersion :: Int
initialVersion = 0

lsWithPrefix :: String -> LexicalScopeState
lsWithPrefix s = LexicalScopeState { tys      = M.empty
                                   , vers     = M.empty
                                   , terms    = M.empty
                                   , lsPrefix = s
                                   }

unknownVar :: VarName -> a
unknownVar var = error $ unwords ["Variable", var, "is unknown"]

lsDeclareVar :: VarName -> Type -> LexicalScopeState -> LexicalScopeState
lsDeclareVar var ty scope = case M.lookup var (tys scope) of
  Nothing -> scope { vers = M.insert var initialVersion $ vers scope
                   , tys  = M.insert var ty $ tys scope
                   }
  Just actualTy ->
    error $ unwords ["Already declared", var, "to have type", show actualTy]

lsNextVer :: VarName -> LexicalScopeState -> LexicalScopeState
lsNextVer var scope =
  scope { vers = M.insert var (lsGetVer var scope + 1) $ vers scope }

-- | Get the current version of the variable
lsGetVer :: VarName -> LexicalScopeState -> Version
lsGetVer var scope = fromMaybe (unknownVar var) (M.lookup var (vers scope))

-- | Get current CodegenVar
lsGetCodegenVar :: VarName -> LexicalScopeState -> CodegenVar
lsGetCodegenVar var scope =
  CodegenVar (lsPrefix scope ++ "__" ++ var) (lsGetVer var scope)

-- | Get the C++ type of the variable
lsGetType :: VarName -> LexicalScopeState -> Type
lsGetType var scope = fromMaybe (unknownVar var) (M.lookup var (tys scope))

-- | Get a CTerm for the given var
lsEnsureTerm :: VarName -> LexicalScopeState -> Assert LexicalScopeState
lsEnsureTerm varName scope =
  let var = lsGetCodegenVar varName scope
  in  case M.lookup var (terms scope) of
        Just node -> return scope
        Nothing ->
          let ty = lsGetType varName scope
          in  do
                node <- newVar ty $ codegenToName var
                return scope { terms = M.insert var node $ terms scope }

lsGetTerm :: VarName -> LexicalScopeState -> CTerm
lsGetTerm var scope = fromMaybe
  (error $ unwords ["No term for", var])
  (M.lookup (lsGetCodegenVar var scope) (terms scope))

data FunctionScope = FunctionScope { -- Condition for current path
                                     conditionalGuards :: [Ty.TermBool]
                                     -- Conditions for each previous return
                                   , returnValueGuards :: [[Ty.TermBool]]
                                     -- Stack of lexical scopes. Innermost first.
                                   , lexicalScopes     :: [LexicalScopeState]
                                     -- number of next ls
                                   , lsCtr             :: Int
                                   , fsPrefix          :: String
                                   , retTerm           :: CTerm
                                   }

listModify :: Functor m => Int -> (a -> m a) -> [a] -> m [a]
listModify 0 f (x : xs) = (: xs) `fmap` f x
listModify n f (x : xs) = (x :) `fmap` listModify (n - 1) f xs

fsFindLexScope :: VarName -> FunctionScope -> Int
fsFindLexScope var scope =
  fromMaybe (error $ unwords ["Cannot find", var, "in current scope"])
    $ findIndex (M.member var . tys) (lexicalScopes scope)

-- | Apply a modification function to the first scope containing the variable.
fsModifyLexScope
  :: Monad m
  => VarName
  -> (LexicalScopeState -> m LexicalScopeState)
  -> FunctionScope
  -> m FunctionScope
fsModifyLexScope var f scope = do
  n <- listModify (fsFindLexScope var scope) f $ lexicalScopes scope
  return $ scope { lexicalScopes = n }

-- | Apply a modification function to the first scope containing the variable.
fsGetFromLexScope :: VarName -> (LexicalScopeState -> a) -> FunctionScope -> a
fsGetFromLexScope var f scope =
  let i  = fsFindLexScope var scope
      ls = lexicalScopes scope !! i
  in  f ls

fsDeclareVar :: VarName -> Type -> FunctionScope -> FunctionScope
fsDeclareVar var ty scope = scope
  { lexicalScopes = lsDeclareVar var ty (head $ lexicalScopes scope)
                      : tail (lexicalScopes scope)
  }

fsNextVer :: VarName -> FunctionScope -> FunctionScope
fsNextVer var = runIdentity . fsModifyLexScope var (Identity . lsNextVer var)

fsGetVer :: VarName -> FunctionScope -> Version
fsGetVer var = fsGetFromLexScope var (lsGetVer var)

fsGetTerm :: VarName -> FunctionScope -> Assert (CTerm, FunctionScope)
fsGetTerm var scope = do
  scope' <- fsModifyLexScope var (lsEnsureTerm var) scope
  return (fsGetFromLexScope var (lsGetTerm var) scope', scope')

fsEnterLexScope :: FunctionScope -> FunctionScope
fsEnterLexScope scope =
  let newLs = lsWithPrefix (fsPrefix scope ++ "_lex" ++ show (lsCtr scope))
  in  scope { lsCtr         = 1 + lsCtr scope
            , lexicalScopes = newLs : lexicalScopes scope
            }

printFs :: FunctionScope -> IO ()
printFs s = do
  putStrLn " FunctionScope:"
  putStrLn $ "  Lex counter: " ++ show (lsCtr s)
  putStrLn "  LexicalScopes:"
  traverse_ printLs (lexicalScopes s)

fsExitLexScope :: FunctionScope -> FunctionScope
fsExitLexScope scope = scope { lexicalScopes = tail $ lexicalScopes scope }

fsPushGuard :: Ty.TermBool -> FunctionScope -> FunctionScope
fsPushGuard guard scope =
  scope { conditionalGuards = guard : conditionalGuards scope }

fsPopGuard :: FunctionScope -> FunctionScope
fsPopGuard scope = scope { conditionalGuards = tail $ conditionalGuards scope }

fsCurrentGuard :: FunctionScope -> Ty.TermBool
fsCurrentGuard = safeNary (Ty.BoolLit True) Ty.And . conditionalGuards

-- | Set the return value if we have not returned, and block future returns
fsReturn :: CTerm -> FunctionScope -> Assert FunctionScope
fsReturn value scope =
  let returnCondition =
          Ty.BoolNaryExpr Ty.And [fsCurrentGuard scope, fsHasNotReturned scope]
      newScope = scope
        { returnValueGuards = conditionalGuards scope : returnValueGuards scope
        }
  in  do
        Assert.implies returnCondition (cppAssignment value (retTerm scope))
        return newScope

fsHasNotReturned :: FunctionScope -> Ty.TermBool
fsHasNotReturned =
  Ty.Not
    . safeNary (Ty.BoolLit False) Ty.Or
    . map (safeNary (Ty.BoolLit True) Ty.And)
    . returnValueGuards

fsWithPrefix :: String -> Type -> Assert FunctionScope
fsWithPrefix prefix ty = do
  retTerm <- newVar ty $ codegenToName $ CodegenVar (prefix ++ "__return") 0
  let fs = FunctionScope { conditionalGuards = []
                         , returnValueGuards = []
                         , retTerm           = retTerm
                         , lexicalScopes     = []
                         , lsCtr             = 0
                         , fsPrefix          = prefix
                         }
  return $ fsEnterLexScope fs

-- | Internal state of the compiler for code generation
data CompilerState = CompilerState { callStack         :: [FunctionScope]
                                   , funs              :: M.Map FunctionName Function
                                   , typedefs          :: M.Map VarName Type
                                   , loopBound         :: Int
                                   , prefix            :: [String]
                                   }

newtype Compiler a = Compiler (StateT CompilerState Mem a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO)

-- instance Z.MonadZ3 Compiler where
--     getSolver = Compiler $ lift $ Z.getSolver
--     getContext = Compiler $ lift $ Z.getContext

instance MonadFail Compiler where
  fail = error "FAILED"

---
--- Setup, monad functions, etc
---

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState { callStack = []
                                   , funs      = M.empty
                                   , typedefs  = M.empty
                                   , loopBound = 4
                                   , prefix    = []
                                   }

compilerRunOnTop :: (FunctionScope -> (a, FunctionScope)) -> Compiler a
compilerRunOnTop f = do
  s <- get
  let (r, s') = f $ head $ callStack s
  put $ s { callStack = s' : tail (callStack s) }
  return r

compilerModifyTop :: (FunctionScope -> FunctionScope) -> Compiler ()
compilerModifyTop f = compilerRunOnTop (\s -> ((), f s))

compilerGetsTop :: (FunctionScope -> a) -> Compiler a
compilerGetsTop f = compilerRunOnTop (\s -> (f s, s))

declareVar :: VarName -> Type -> Compiler ()
declareVar var ty = compilerModifyTop (fsDeclareVar var ty)

nextVer :: VarName -> Compiler ()
nextVer = compilerModifyTop . fsNextVer

getVer :: VarName -> Compiler Version
getVer = compilerGetsTop . fsGetVer

getTerm :: VarName -> Compiler CTerm
getTerm var = do
  s          <- get
  (t, newFs) <- liftAssert $ fsGetTerm var $ head $ callStack s
  put $ s { callStack = newFs : tail (callStack s) }
  return t

printComp :: Compiler ()
printComp = do
  s <- get
  liftIO $ traverse_ printFs (callStack s)

enterLexScope :: Compiler ()
enterLexScope = compilerModifyTop fsEnterLexScope

exitLexScope :: Compiler ()
exitLexScope = compilerModifyTop fsExitLexScope

pushGuard :: Ty.TermBool -> Compiler ()
pushGuard = compilerModifyTop . fsPushGuard

popGuard :: Compiler ()
popGuard = compilerModifyTop fsPopGuard

getGuard :: Compiler Ty.TermBool
getGuard = compilerGetsTop fsCurrentGuard

doReturn :: CTerm -> Compiler ()
doReturn value = do
  s       <- get
  newHead <- liftAssert $ fsReturn value (head $ callStack s)
  put s { callStack = newHead : tail (callStack s) }

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
codegenToName (CodegenVar varName ver) = varName ++ "_v" ++ show ver

-- Bump the version of `var` and assign `value` to it.
ssaAssign :: VarName -> CTerm -> Compiler CTerm
ssaAssign var val = do
  oldNode <- getTerm var
  nextVer var
  newNode <- getTerm var
  guard   <- getGuard
  liftAssert $ Assert.assert $ Ty.Ite guard
                                      (cppAssignment newNode val)
                                      (cppAssignment newNode oldNode)
  return newNode

---
--- Functions
---

pushFunction :: FunctionName -> Type -> Compiler ()
pushFunction name ty = do
  p <- gets prefix
  let p' = name : p
  fs <- liftAssert $ fsWithPrefix (intercalate "_" $ reverse p') ty
  modify (\s -> s { prefix = p', callStack = fs : callStack s })

popFunction :: Compiler ()
popFunction = modify (\s -> s { callStack = tail (callStack s) })

getFunction :: FunctionName -> Compiler Function
getFunction funName = do
  functions <- gets funs
  case M.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords ["Called undeclared function", funName]

-- registerFunction :: Function -> Compiler ()
-- registerFunction function = do
--   forM_ (fArgs function) $ uncurry $ declareVar
--   s0 <- get
--   let funName = fName function
--   case M.lookup funName $ funs s0 of
--     Nothing -> put $ s0 { funs = M.insert funName function $ funs s0 }
--     _       -> error $ unwords ["Already declared", fName function]

---
--- Typedefs
---

typedef :: VarName -> Type -> Compiler ()
typedef name ty = do
  s0 <- get
  let tds = typedefs s0
  case M.lookup name tds of
    Nothing -> put $ s0 { typedefs = M.insert name ty tds }
    Just t  -> error $ unwords ["Already td'd", name, "to", show t]

untypedef :: VarName -> Compiler Type
untypedef name = do
  tds <- gets typedefs
  case M.lookup name tds of
    Nothing -> error $ unwords ["No type defined for", name]
    Just ty -> return ty


---
--- If-statements
---

safeNary :: Ty.TermBool -> Ty.BoolNaryOp -> [Ty.TermBool] -> Ty.TermBool
safeNary id op xs = case xs of
  []  -> id
  [s] -> s
  _   -> Ty.BoolNaryExpr op xs

-- Loops

getLoopBound :: Compiler Int
getLoopBound = gets loopBound

setLoopBound :: Int -> Compiler ()
setLoopBound bound = modify (\s -> s { loopBound = bound })

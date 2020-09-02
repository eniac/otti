{-# LANGUAGE GADTs                      #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
module Codegen.C.CompilerMonad where
import           AST.Simple
import           Control.Monad.Fail
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.List                      ( intercalate
                                                , isInfixOf
                                                , findIndex
                                                )
import           Data.Functor.Identity
import           Data.Foldable
import qualified Data.Map                      as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                , isJust
                                                , fromJust
                                                , listToMaybe
                                                )
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDyn
                                                )
import           Language.C.Syntax.AST          ( CFunDef )
import           Codegen.C.CUtils              as CUtils
import qualified IR.SMT.TySmt                  as Ty
import qualified Codegen.C.Memory              as Mem
import           Codegen.C.Memory               ( Mem )
import           Targets.SMT                    ( SMTResult )
import qualified IR.SMT.Assert                 as Assert
import           IR.SMT.Assert                  ( Assert )
import           Util.Log
import qualified Util.ShowMap                  as SMap
import qualified Z3.Monad                      as Z

{-|

Module that defines the Compiler monad, the monad that keeps track of all internal
state for code generation. This state includes:
WHAT

There are many bits of low-hanging optimization fruit here. For example, we are doing
redundant state lookups, and storing lists of function names instead of a hash of
such a thing.

Structure: The compiler monad is defined in terms of three nested notions of state:
  * LexScope
  * FunctionScope
  * CompilerState
-}

type Version = Int

data SsaVar = SsaVar VarName Version
                deriving (Eq, Ord, Show)

-- TODO rename
data LexScope = LexScope { tys :: M.Map VarName Type
                         , vers :: M.Map VarName Version
                         , terms :: M.Map SsaVar SsaVal
                         , lsPrefix :: String
                         } deriving (Show)
printLs :: LexScope -> IO ()
printLs s =
  putStr
    $  unlines
    $  ["   LexicalScope:", "    prefix: " ++ lsPrefix s, "    versions:"]
    ++ [ "     " ++ show var ++ ": " ++ show ver
       | (var, ver) <- M.toList (vers s)
       ]

data ScopeIdx = ScopeGlobal
              | ScopeIdx { lsIdx :: Int, fsIdx :: Int } deriving (Show)

data Ref = Ref { scopeIdx :: ScopeIdx , refVarName :: VarName } deriving (Show)

-- Something that can be written to in a lexical system.
-- Either a name in the current scope, or an absolute reference to another scope.
data SsaLVal = SLVar VarName
             | SLRef Ref
             deriving (Show)

-- Something that can be stored.
-- Either an underlying C value or a reference to a value in a different scope.
data SsaVal  = Base   CTerm
             | RefVal Ref
             deriving (Show)

-- Lexical scope functions

initialVersion :: Int
initialVersion = 0

lsWithPrefix :: String -> LexScope
lsWithPrefix s =
  LexScope { tys = M.empty, vers = M.empty, terms = M.empty, lsPrefix = s }

unknownVar :: VarName -> a
unknownVar var = error $ unwords ["Variable", var, "is unknown"]

lsDeclareVar :: VarName -> Type -> LexScope -> Mem LexScope
lsDeclareVar var ty scope = case M.lookup var (tys scope) of
  Nothing -> do
    -- First we add type and version entries for this variable
    let withTyAndVer = scope { vers = M.insert var initialVersion $ vers scope
                             , tys  = M.insert var ty $ tys scope
                             }
        ssaVar = lsGetSsaVar var withTyAndVer
    -- Now we declare it to the SMT layer
    term <- Base <$> cppDeclVar ty (ssaVarAsString ssaVar)
    return $ withTyAndVer { terms = M.insert ssaVar term $ terms scope }
  Just actualTy ->
    error $ unwords ["Already declared", var, "to have type", show actualTy]


-- | Get the current version of the variable
lsGetVer :: VarName -> LexScope -> Version
lsGetVer var scope = fromMaybe (unknownVar var) (lsGetMaybeVer var scope)

lsGetMaybeVer :: VarName -> LexScope -> Maybe Version
lsGetMaybeVer var scope = M.lookup var (vers scope)

-- | Get current SsaVar
lsGetSsaVar :: VarName -> LexScope -> SsaVar
lsGetSsaVar var scope = SsaVar (lsScopedVar var scope) (lsGetVer var scope)

lsGetNextSsaVar :: VarName -> LexScope -> SsaVar
lsGetNextSsaVar var scope =
  SsaVar (lsScopedVar var scope) (lsGetVer var scope + 1)

lsScopedVar :: VarName -> LexScope -> String
lsScopedVar var scope = lsPrefix scope ++ "__" ++ var

-- | Get the C++ type of the variable
lsGetType :: VarName -> LexScope -> Type
lsGetType var scope = fromMaybe (unknownVar var) (M.lookup var (tys scope))

-- | Get a SsaVal for the given var
lsGetTerm :: VarName -> LexScope -> SsaVal
lsGetTerm var scope = fromMaybe
  (error $ unwords ["No term for", var])
  (M.lookup (lsGetSsaVar var scope) (terms scope))

lsSetTerm :: SsaVal -> VarName -> LexScope -> LexScope
lsSetTerm val var scope =
  scope { terms = M.insert (lsGetSsaVar var scope) val $ terms scope }

lsNextVer :: VarName -> LexScope -> LexScope
lsNextVer var scope =
  scope { vers = M.insert var (lsGetVer var scope + 1) $ vers scope }

data FunctionScope = FunctionScope { -- Condition for current path
                                     conditionalGuards :: [Ty.TermBool]
                                     -- Conditions for each previous return
                                   , returnValueGuards :: [[Ty.TermBool]]
                                     -- Stack of lexical scopes. Innermost first.
                                   , lexicalScopes     :: [LexScope]
                                   , nCurrentScopes    :: Int
                                     -- number of next ls
                                   , lsCtr             :: Int
                                   , fsPrefix          :: String
                                   , retTerm           :: CTerm
                                   , retTermName       :: String
                                   } deriving (Show)

listModify :: Functor m => Int -> (a -> m a) -> [a] -> m [a]
listModify 0 f (x : xs) = (: xs) `fmap` f x
listModify n f (x : xs) = (x :) `fmap` listModify (n - 1) f xs

-- | Find the scope containing this variable. Indexed from the back.
fsFindLexScope :: VarName -> FunctionScope -> Int
fsFindLexScope var scope =
  fromMaybe (error $ unwords ["Cannot find", var, "in current scope"])
    $ fsFindLexScopeOpt var scope

-- | Find the scope containing this variable. Indexed from the back.
fsFindLexScopeOpt :: VarName -> FunctionScope -> Maybe Int
fsFindLexScopeOpt var scope = (nCurrentScopes scope - 1 -)
  <$> findIndex (M.member var . tys) (lexicalScopes scope)

-- | Apply a modification function to the indexed scope.
fsModifyLexScope
  :: Monad m
  => Int
  -> (LexScope -> m LexScope)
  -> FunctionScope
  -> m FunctionScope
fsModifyLexScope i f scope = do
  n <- listModify (nCurrentScopes scope - i - 1) f $ lexicalScopes scope
  return $ scope { lexicalScopes = n }

-- | Apply a fetching function to the indexed scope.
fsGetFromLexScope :: Int -> (LexScope -> a) -> FunctionScope -> a
fsGetFromLexScope i f scope = if i < nCurrentScopes scope
  then f $ lexicalScopes scope !! (nCurrentScopes scope - i - 1)
  else error $ unwords ["Lexical scope index", show i, "is invalid", show scope]

fsDeclareVar :: VarName -> Type -> FunctionScope -> Mem FunctionScope
fsDeclareVar var ty scope = do
  head' <- lsDeclareVar var ty (head $ lexicalScopes scope)
  return $ scope { lexicalScopes = head' : tail (lexicalScopes scope) }

fsGetVer :: VarName -> Int -> FunctionScope -> Version
fsGetVer var i = fsGetFromLexScope i (lsGetVer var)

fsGetType :: VarName -> Int -> FunctionScope -> Type
fsGetType var i = fsGetFromLexScope i (lsGetType var)

fsGetSsaVar :: VarName -> Int -> FunctionScope -> SsaVar
fsGetSsaVar var i = fsGetFromLexScope i (lsGetSsaVar var)

fsGetNextSsaVar :: VarName -> Int -> FunctionScope -> SsaVar
fsGetNextSsaVar var i = fsGetFromLexScope i (lsGetNextSsaVar var)

fsGetTerm :: VarName -> Int -> FunctionScope -> SsaVal
fsGetTerm var i = fsGetFromLexScope i (lsGetTerm var)

fsSetTerm :: SsaVal -> VarName -> Int -> FunctionScope -> FunctionScope
fsSetTerm val var i =
  runIdentity . fsModifyLexScope i (Identity . lsSetTerm val var)

fsNextVer :: VarName -> Int -> FunctionScope -> FunctionScope
fsNextVer var i = runIdentity . fsModifyLexScope i (Identity . lsNextVer var)

fsEnterLexScope :: FunctionScope -> FunctionScope
fsEnterLexScope scope =
  let newLs = lsWithPrefix (fsPrefix scope ++ "_lex" ++ show (lsCtr scope))
  in  scope { lsCtr          = 1 + lsCtr scope
            , lexicalScopes  = newLs : lexicalScopes scope
            , nCurrentScopes = 1 + nCurrentScopes scope
            }

printFs :: FunctionScope -> IO ()
printFs s = do
  putStrLn " FunctionScope:"
  putStrLn $ "  Lex counter: " ++ show (lsCtr s)
  putStrLn "  LexicalScopes:"
  traverse_ printLs (lexicalScopes s)

fsExitLexScope :: FunctionScope -> FunctionScope
fsExitLexScope scope = scope { nCurrentScopes = nCurrentScopes scope - 1
                             , lexicalScopes  = tail $ lexicalScopes scope
                             }

fsPushGuard :: Ty.TermBool -> FunctionScope -> FunctionScope
fsPushGuard guard scope =
  scope { conditionalGuards = guard : conditionalGuards scope }

fsPopGuard :: FunctionScope -> FunctionScope
fsPopGuard scope = scope { conditionalGuards = tail $ conditionalGuards scope }

fsCurrentGuard :: FunctionScope -> Ty.TermBool
fsCurrentGuard = safeNary (Ty.BoolLit True) Ty.And . conditionalGuards

-- | Set the return value if we have not returned, and block future returns
fsReturn :: CTerm -> FunctionScope -> Compiler FunctionScope
fsReturn value scope =
  let
    returnCondition =
      Ty.BoolNaryExpr Ty.And [fsCurrentGuard scope, fsHasNotReturned scope]
    newScope = scope
      { returnValueGuards = conditionalGuards scope : returnValueGuards scope
      }
  in
    do
      a <- getAssignment
      let (retAssertion, retVal) = a (retTerm scope) value
      liftAssert $ Assert.implies returnCondition retAssertion
      whenM computingValues $ whenM (smtEvalBool returnCondition) $ setRetValue
        retVal
      return newScope

fsHasNotReturned :: FunctionScope -> Ty.TermBool
fsHasNotReturned =
  Ty.Not
    . safeNary (Ty.BoolLit False) Ty.Or
    . map (safeNary (Ty.BoolLit True) Ty.And)
    . returnValueGuards

fsWithPrefix :: String -> Type -> Mem FunctionScope
fsWithPrefix prefix ty = do
  let retTermName = ssaVarAsString $ SsaVar (prefix ++ "__return") 0
  retTerm <- cppDeclVar ty retTermName
  let fs = FunctionScope { conditionalGuards = []
                         , returnValueGuards = []
                         , retTerm           = retTerm
                         , retTermName       = retTermName
                         , lexicalScopes     = []
                         , nCurrentScopes    = 0
                         , lsCtr             = 0
                         , fsPrefix          = prefix
                         }
  return $ fsEnterLexScope fs

-- | Internal state of the compiler for code generation
data CompilerState = CompilerState { callStack         :: [FunctionScope]
                                   , nFrames           :: Int
                                   , globals           :: LexScope
                                   , funs              :: M.Map FunctionName CFunDef
                                   , typedefs          :: M.Map VarName Type
                                   , structdefs        :: M.Map VarName Type
                                   , loopBound         :: Int
                                   , prefix            :: [String]
                                   , fnCtr             :: Int
                                   , findUB            :: Bool
                                   , bugConditions     :: [Ty.TermBool]
                                   , values            :: Maybe (M.Map String Dynamic)
                                   -- Used for inputs that have no value.
                                   , defaultValue      :: Maybe Integer
                                   }

newtype Compiler a = Compiler (StateT CompilerState Mem a)
    deriving (Functor, Applicative, Monad, MonadState CompilerState, MonadIO, MonadLog)


instance MonadFail Compiler where
  fail = error "FAILED"

---
--- Setup, monad functions, etc
---

emptyCompilerState :: CompilerState
emptyCompilerState = CompilerState { callStack     = []
                                   , nFrames       = 0
                                   , globals       = lsWithPrefix "global"
                                   , funs          = M.empty
                                   , typedefs      = M.empty
                                   , structdefs    = M.empty
                                   , loopBound     = 4
                                   , prefix        = []
                                   , findUB        = True
                                   , bugConditions = []
                                   , fnCtr         = 0
                                   , values        = Nothing
                                   , defaultValue  = Nothing
                                   }

compilerRunOnTop :: (FunctionScope -> Compiler (a, FunctionScope)) -> Compiler a
compilerRunOnTop f = do
  s       <- get
  (r, s') <-
    f
    $ fromMaybe (error "Cannot run in function: no function!")
    $ listToMaybe
    $ callStack s
  modify $ \s -> s { callStack = s' : tail (callStack s) }
  return r

ssaLValName :: SsaLVal -> VarName
ssaLValName ssa = case ssa of
  SLVar n         -> n
  SLRef (Ref _ n) -> n

compilerFindScope :: SsaLVal -> Compiler ScopeIdx
compilerFindScope ssa = case ssa of
  SLVar name -> do
    stack <- gets callStack
    case listToMaybe stack >>= fsFindLexScopeOpt name of
      Just idx -> do
        n <- gets nFrames
        return $ ScopeIdx { lsIdx = idx, fsIdx = n - 1 }
      Nothing -> do
        global <- gets globals
        return $ if M.member name $ tys global
          then ScopeGlobal
          else error $ "Cannot find the variable `" ++ name ++ "` in scope"
  SLRef (Ref idx _) -> return idx

-- Runs a modification function on the scope of the provided variable
compilerRunInScopeIdx
  :: ScopeIdx
  -> (FunctionScope -> Compiler (a, FunctionScope))
  -> (LexScope -> Compiler (a, LexScope))
  -> Compiler a
compilerRunInScopeIdx scopeIdx fF lF = case scopeIdx of
  ScopeGlobal -> do
    global       <- gets globals
    (r, global') <- lF global
    modify $ \s -> s { globals = global' }
    return r
  ScopeIdx { lsIdx = l, fsIdx = f } -> do
    stack <- gets callStack
    n     <- gets nFrames
    let i     = n - f - 1
    let frame = stack !! i
    (r, frame') <- fF frame
    modify $ \s -> s
      { callStack =
        runIdentity $ listModify i (Identity . const frame') $ callStack s
      }
    return r

ssaValAsCTerm :: String -> SsaVal -> CTerm
ssaValAsCTerm reason v = case v of
  Base c -> c
  RefVal {} -> error $ "Cannot unwrap " ++ show v ++ " as a C term. It is a reference. Reason\n" ++ reason

ssaBool :: SsaVal -> Ty.TermBool
ssaBool = cppBool . ssaValAsCTerm "cppBool"

ssaType :: SsaVal -> Type
ssaType = cppType . ssaValAsCTerm "cppType"

liftCFun :: String -> (CTerm -> CTerm) -> SsaVal -> SsaVal
liftCFun name f x = case x of
  Base c -> Base $ f c
  RefVal{} -> error $ "Cannot apply c function " ++ name ++ " to reference " ++ show x

liftCFun2 :: String -> (CTerm -> CTerm -> CTerm) -> SsaVal -> SsaVal -> SsaVal
liftCFun2 name f x1 x2 = case (x1, x2) of
  (Base c1, Base c2) -> Base $ f c1 c2
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2)

liftCFun3 :: String -> (CTerm -> CTerm -> CTerm -> CTerm) -> SsaVal -> SsaVal -> SsaVal -> SsaVal
liftCFun3 name f x1 x2 x3 = case (x1, x2, x3) of
  (Base c1, Base c2, Base c3) -> Base $ f c1 c2 c3
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2, x3)

liftCFun2M :: Monad m => String -> (CTerm -> CTerm -> m CTerm) -> SsaVal -> SsaVal -> m SsaVal
liftCFun2M name f x1 x2 = case (x1, x2) of
  (Base c1, Base c2) -> Base <$> f c1 c2
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2)

compilerRunInLValScope
  :: SsaLVal
  -> (FunctionScope -> Compiler (a, FunctionScope))
  -> (LexScope -> Compiler (a, LexScope))
  -> Compiler a
compilerRunInLValScope lval fF lF = do
  idx <- compilerFindScope lval
  compilerRunInScopeIdx idx fF lF

compilerModifyInScope
  :: SsaLVal
  -> (VarName -> Int -> FunctionScope -> FunctionScope)
  -> (VarName -> LexScope -> LexScope)
  -> Compiler ()
compilerModifyInScope v fF lF = do
  idx <- compilerFindScope v
  compilerRunInScopeIdx idx
                        (return . ((), ) . fF (ssaLValName v) (lsIdx idx))
                        (return . ((), ) . lF (ssaLValName v))

compilerGetsInScope
  :: SsaLVal
  -> (VarName -> Int -> FunctionScope -> a)
  -> (VarName -> LexScope -> a)
  -> Compiler a
compilerGetsInScope ssa fF lF = do
  idx <- compilerFindScope ssa
  case idx of
    ScopeGlobal                       -> lF (ssaLValName ssa) <$> gets globals
    ScopeIdx { lsIdx = l, fsIdx = f } -> do
      stack <- gets callStack
      n     <- gets nFrames
      let i     = n - f - 1
      let frame = stack !! i
      return $ fF (ssaLValName ssa) l frame

compilerGetsFunction :: (FunctionScope -> a) -> Compiler a
compilerGetsFunction f = compilerRunOnTop (\s -> return (f s, s))

compilerModifyTopM :: (FunctionScope -> Compiler FunctionScope) -> Compiler ()
compilerModifyTopM f = compilerRunOnTop $ fmap ((), ) . f

compilerModifyTop :: (FunctionScope -> FunctionScope) -> Compiler ()
compilerModifyTop f = compilerModifyTopM (return . f)

compilerGetsTop :: (FunctionScope -> a) -> Compiler a
compilerGetsTop f = compilerRunOnTop (\s -> return (f s, s))

declareVar :: VarName -> Type -> Compiler ()
declareVar var ty = do
  --liftIO $ putStrLn $ "declareVar: " ++ var ++ ": " ++ show ty
  isGlobal <- gets (null . callStack)
  if isGlobal
    then do
      g  <- gets globals
      g' <- liftMem $ lsDeclareVar var ty g
      modify $ \s -> s { globals = g' }
    else compilerModifyTopM $ \s -> liftMem $ fsDeclareVar var ty s

getVer :: SsaLVal -> Compiler Version
getVer v = compilerGetsInScope v fsGetVer lsGetVer

nextVer :: SsaLVal -> Compiler ()
nextVer v = compilerModifyInScope v fsNextVer lsNextVer

getType :: SsaLVal -> Compiler Type
getType v = compilerGetsInScope v fsGetType lsGetType

getSsaVar :: SsaLVal -> Compiler SsaVar
getSsaVar v = compilerGetsInScope v fsGetSsaVar lsGetSsaVar

getNextSsaVar :: SsaLVal -> Compiler SsaVar
getNextSsaVar v = compilerGetsInScope v fsGetNextSsaVar lsGetNextSsaVar

getSsaName :: SsaLVal -> Compiler String
getSsaName n = ssaVarAsString <$> getSsaVar n

computingValues :: Compiler Bool
computingValues = gets (isJust . values)

getValues :: Compiler (M.Map String Dynamic)
getValues = gets $ fromMaybe (error "Not computing values") . values

modValues
  :: (M.Map String Dynamic -> Compiler (M.Map String Dynamic)) -> Compiler ()
modValues f = do
  s <- get
  case values s of
    Just vs -> do
      vs' <- f vs
      put $ s { values = Just vs' }
    Nothing -> return ()


smtEval :: Ty.SortClass s => Ty.Term s -> Compiler (Ty.Value s)
smtEval smt = flip Ty.eval smt <$> getValues

smtEvalBool :: Ty.TermBool -> Compiler Bool
smtEvalBool smt = Ty.valAsBool <$> smtEval smt

-- We don not record witness values for references.
setValue :: SsaLVal -> CTerm -> Compiler ()
setValue name cterm = modValues $ \vs -> do
  liftLog $ logIf "witness" $ show name ++ " -> " ++ show cterm
  -- TODO: check getSsaVar
  var  <- ssaVarAsString <$> getSsaVar name
  cval <- liftMem $ ctermEval vs cterm
  return $ M.insert var cval vs

-- We don not record witness values for references.
setRetValue :: CTerm -> Compiler ()
setRetValue cterm = modValues $ \vs -> do
  var  <- compilerGetsTop retTermName
  cval <- liftMem $ ctermEval vs cterm
  return $ M.insert var cval vs

getTerm :: SsaLVal -> Compiler SsaVal
getTerm var = compilerGetsInScope var fsGetTerm lsGetTerm

setTerm :: SsaLVal -> SsaVal -> Compiler ()
setTerm n v = compilerModifyInScope n (fsSetTerm v) (lsSetTerm v)

printComp :: Compiler ()
printComp = do
  gets callStack >>= liftIO . traverse_ printFs
  liftIO $ putStrLn "Typedefs:"
  gets typedefs
    >>= liftIO
    .   traverse_ (\(k, v) -> putStrLn ("  " ++ k ++ " -> " ++ show v))
    .   M.toList
  gets structdefs
    >>= liftIO
    .   traverse_ (\(k, v) -> putStrLn ("  " ++ k ++ " -> " ++ show v))
    .   M.toList

enterLexScope :: Compiler ()
enterLexScope = compilerModifyTop fsEnterLexScope

exitLexScope :: Compiler ()
exitLexScope = compilerModifyTop fsExitLexScope

pushGuard :: Ty.TermBool -> Compiler ()
pushGuard = compilerModifyTop . fsPushGuard

popGuard :: Compiler ()
popGuard = compilerModifyTop fsPopGuard

guarded :: Ty.TermBool -> Compiler a -> Compiler a
guarded cond action = pushGuard cond *> action <* popGuard

getGuard :: Compiler Ty.TermBool
getGuard = compilerGetsTop fsCurrentGuard

doReturn :: CTerm -> Compiler ()
doReturn value = compilerModifyTopM (fsReturn value)

getReturn :: Compiler CTerm
getReturn = compilerGetsTop retTerm

liftMem :: Mem a -> Compiler a
liftMem = Compiler . lift

liftAssert :: Assert a -> Compiler a
liftAssert = liftMem . Mem.liftAssert

runCodegen
  :: Bool -- ^ wether to check for UB
  -> Compiler a       -- ^ Codegen computation
  -> Assert (a, CompilerState)
runCodegen checkUB (Compiler act) =
  Mem.evalMem $ runStateT act $ emptyCompilerState { findUB = checkUB }

evalCodegen :: Bool -> Compiler a -> Assert a
evalCodegen checkUB act = fst <$> runCodegen checkUB act

execCodegen :: Bool -> Compiler a -> Assert CompilerState
execCodegen checkUB act = snd <$> runCodegen checkUB act


-- Turning VarNames (the AST's representation of a variable) into other representations
-- of variables

codegenVar :: SsaLVal -> Compiler SsaVar
codegenVar var = SsaVar (ssaLValName var) <$> getVer var

-- | Human readable name.
-- We probably want to replace this with something faster (eg hash) someday, but
-- this is great for debugging
ssaVarAsString :: SsaVar -> String
ssaVarAsString (SsaVar varName ver) = varName ++ "_v" ++ show ver

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = condition >>= flip when action

ifVal :: Monad m => SsaVal -> (CTerm -> m a) -> m ()
ifVal val f = case val of
  Base v -> void $ f v
  _      -> return ()

-- Assert that the current version of `var` is assign `value` to it.
-- Could return 
argAssign :: SsaLVal -> SsaVal -> Compiler SsaVal
argAssign var val = do
  --liftIO $ putStrLn $ "argAssign " ++ var ++ " = " ++ show val
  priorTerm  <- getTerm var
  ty         <- getType var
  trackUndef <- gets findUB
  ssaVar     <- getSsaVar var
  case val of
    Base cval -> do
      let castVal = cppCast ty cval
      t <- liftMem
        $ cppDeclInitVar trackUndef ty (ssaVarAsString ssaVar) castVal
      setTerm var (Base t)
      whenM computingValues $ setValue var castVal
      return (Base t)
    RefVal r -> do
      setTerm var val
      return val

-- Bump the version of `var` and assign `value` to it.
ssaAssign :: SsaLVal -> SsaVal -> Compiler SsaVal
ssaAssign var val = do
  priorTerm  <- getTerm var
  ty         <- getType var
  nextSsaVar <- getNextSsaVar var
  guard      <- getGuard
  case val of
    Base cval -> do
      let guardTerm = CTerm (CBool guard) (Ty.BoolLit False)
          castVal   = cppCast ty cval
          priorCval = case priorTerm of
            RefVal r ->
              error
                $  "Writing a cterm"
                ++ show cval
                ++ "to a location"
                ++ show var
                ++ "that held a ref"
                ++ show r
            Base c -> c
          guardVal = cppCond guardTerm castVal priorCval
      trackUndef <- gets findUB
      t          <- liftMem
        $ cppDeclInitVar trackUndef ty (ssaVarAsString nextSsaVar) guardVal
      nextVer var
      setTerm var (Base t)
      whenM computingValues $ do
        g <- smtEvalBool guard
        setValue var (if g then castVal else priorCval)
      return (Base t)
    RefVal r -> do
      setTerm var val
      return val

initAssign :: SsaLVal -> Integer -> Compiler ()
initAssign name value = do
  ty <- getType name
  setValue name (ctermInit ty value)

initValues :: Compiler ()
initValues = modify $ \s -> s { values = Just M.empty }

setDefaultValueZero :: Compiler ()
setDefaultValueZero = modify $ \s -> s { defaultValue = Just 0 }

getRef :: SsaLVal -> Compiler SsaVal
getRef lval = do
  idx <- compilerFindScope lval
  return $ RefVal (Ref idx (ssaLValName lval))

deref :: SsaVal -> SsaLVal
deref val = case val of
  RefVal r -> SLRef r
  Base c -> error $ "Cannot derefence base value: " ++ show c

-- Not quite right? Returns?  Our return stuff is super hacky anyways. We
-- should integrate it with the guard machinery.
bugIf :: Ty.TermBool -> Compiler ()
bugIf c = do
  g <- getGuard
  modify $ \s ->
    s { bugConditions = Ty.BoolNaryExpr Ty.And [g, c] : bugConditions s }

assertBug :: Compiler ()
assertBug = do
  cs <- gets bugConditions
  liftAssert $ Assert.assert $ Ty.BoolNaryExpr Ty.Or cs

---
--- Functions
---

pushFunction :: FunctionName -> Type -> Compiler ()
pushFunction name ty = do
  p <- gets prefix
  c <- gets fnCtr
  let p' = name : p
  fs <- liftMem
    $ fsWithPrefix ("f" ++ show c ++ "_" ++ intercalate "_" (reverse p')) ty
  modify
    (\s -> s { prefix    = p'
             , callStack = fs : callStack s
             , fnCtr     = c + 1
             , nFrames   = 1 + nFrames s
             }
    )

-- Pop a function, returning the return term
popFunction :: Compiler ()
popFunction = modify
  (\s -> s { callStack = tail (callStack s)
           , prefix    = tail (prefix s)
           , nFrames   = nFrames s - 1
           }
  )

registerFunction :: FunctionName -> CFunDef -> Compiler ()
registerFunction name function = do
  s0 <- get
  case M.lookup name $ funs s0 of
    Nothing -> put $ s0 { funs = M.insert name function $ funs s0 }
    _       -> error $ unwords ["Already declared", name]

getFunction :: FunctionName -> Compiler CFunDef
getFunction funName = do
  functions <- gets funs
  case M.lookup funName functions of
    Just function -> return function
    Nothing       -> error $ unwords ["Called undeclared function", funName]

---
--- Typedefs
---

typedef :: VarName -> Type -> Compiler ()
typedef name ty = do
  liftLog $ logIf "typedef" $ "typedef " ++ name ++ " to " ++ show ty
  modify $ \s -> case M.lookup name (typedefs s) of
    Nothing -> s { typedefs = M.insert name ty $ typedefs s }
    Just t  -> error $ unwords ["Already td'd", name, "to", show t]

untypedef :: VarName -> Compiler (Maybe Type)
untypedef name = M.lookup name <$> gets typedefs

---
--- Structdefs
---

defineStruct :: VarName -> Type -> Compiler ()
defineStruct name ty = do
  liftLog $ logIf "typedef" $ "structdef " ++ name ++ " to " ++ show ty
  modify $ \s -> case M.lookup name (typedefs s) of
    Nothing -> s { structdefs = M.insert name ty $ structdefs s }
    Just t  -> error $ unwords ["Already structdef'd", name, "to", show t]

getStruct :: VarName -> Compiler (Maybe Type)
getStruct name = M.lookup name <$> gets structdefs

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

-- UB

getAssignment :: Compiler (CTerm -> CTerm -> (Ty.TermBool, CTerm))
getAssignment = cppAssignment <$> gets findUB

liftCFunM :: Monad m => String -> (CTerm -> m CTerm) -> SsaVal -> m SsaVal
liftCFunM name f x = case x of
  Base c -> Base <$> f c
  RefVal{} -> error $ "Cannot apply c function " ++ name ++ " to reference " ++ show x

-- Load this CTerm, tracking UB
load :: CTerm -> Compiler CTerm
load ref = do
  (oob, val) <- liftMem $ cppLoad ref
  whenM (gets findUB) $ bugIf oob
  return val

ssaLoad :: SsaVal -> Compiler SsaVal
ssaLoad = liftCFunM "load" load

store :: CTerm -> CTerm -> Compiler ()
store ref val = do
  g   <- getGuard
  oob <- liftMem $ cppStore ref val g
  whenM (gets findUB) $ bugIf oob

ssaStore :: SsaVal -> SsaVal -> Compiler ()
ssaStore ref val = case (ref, val) of
  (Base a, Base b) -> store a b
  _ -> error $ "Cannot store " ++ show (ref, val)

ssaStructGet :: String -> SsaVal -> SsaVal
ssaStructGet n = liftCFun "cppStructGet" (`cppStructGet` n)

ssaStructSet :: String -> SsaVal -> SsaVal -> SsaVal
ssaStructSet n = liftCFun2 "cppStructSet" (cppStructSet n)

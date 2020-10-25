{-# LANGUAGE GADTs                      #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- For MonadCircify
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Codegen.Circify
  ( pushFunction
  , popFunction
  , pushGuard
  , popGuard
  , guarded
  , getGuard
  , enterLexScope
  , exitLexScope
  , scoped
  , pushBreakable
  , doBreak
  , Embeddable(..)
  , Circify(..)
  , CircifyState(..)
  , MonadCircify(..)
  , runCircify
  , typedef
  , untypedef
  , getTerm
  , VarName
  , SsaVal(..)
  , SsaLVal(..)
  , liftTermFun
  , liftTermFun2
  , liftTermFun2M
  , liftTermFun3
  , declareVar
  , declareInitVar
  , declareGlobal
  , ssaAssign
  , argAssign
  , deref
  , getRef
  , printComp
  , doReturn
  , ssaValAsTerm
  , getSsaName
  , getVer
  , reachable
  )
where

-- C imports
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify.Memory         ( Mem
                                                , MonadMem
                                                , liftMem
                                                )

-- Control imports
import           Control.Monad.Reader
import           Control.Monad.State.Strict

-- Data imports
import           Data.List                      ( intercalate
                                                , findIndex
                                                )
import           Data.Functor.Identity
import           Data.Foldable
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import           IR.SMT.Assert                  ( Assert
                                                , MonadAssert
                                                , liftAssert
                                                )
import qualified Targets.SMT.Z3         as Z3
import           Util.Cfg                       ( MonadCfg )
import           Util.Control                   ( MonadDeepState(..)
                                                , whenM
                                                )
import           Util.Log

{-|

Module that defines the Circify monad, the monad that keeps track of all internal
state for code generation. This state includes:
WHAT

There are many bits of low-hanging optimization fruit here. For example, we are doing
redundant state lookups, and storing lists of function names instead of a hash of
such a thing.

Structure: The compiler monad is defined in terms of three nested notions of state:
  * LexScope
  * FunctionScope
  * CircifyState
-}

type Version = Int
type VarName = String

data SsaVar = SsaVar VarName Version
  deriving (Eq, Ord, Show)

-- TODO rename
data LexScope ty term = LexScope
  { tys      :: M.Map VarName ty
  , vers     :: M.Map VarName Version
  , terms    :: M.Map SsaVar (SsaVal term)
  , lsPrefix :: String
  }
  deriving Show
printLs :: LexScope t v -> IO ()
printLs s =
  putStr
    $  unlines
    $  ["   LexicalScope:", "    prefix: " ++ lsPrefix s, "    versions:"]
    ++ [ "     " ++ show var ++ ": " ++ show ver
       | (var, ver) <- M.toList (vers s)
       ]

data ScopeIdx = ScopeGlobal
              | ScopeIdx { lsIdx :: Int, fsIdx :: Int } deriving (Show)

data Ref = Ref
  { scopeIdx   :: ScopeIdx
  , refVarName :: VarName
  }
  deriving Show

-- Something that can be written to in a lexical system.
-- Either a name in the current scope, or an absolute reference to another scope.
data SsaLVal = SLVar VarName
             | SLRef Ref
             deriving (Show)

-- Something that can be stored.
-- Either an underlying C value or a reference to a value in a different scope.
data SsaVal term  = Base   term
                  | RefVal Ref
                  deriving (Show)

-- Lexical scope functions

initialVersion :: Int
initialVersion = 0

lsWithPrefix :: String -> LexScope t v
lsWithPrefix s =
  LexScope { tys = M.empty, vers = M.empty, terms = M.empty, lsPrefix = s }

unknownVar :: VarName -> a
unknownVar var = error $ unwords ["Variable", var, "is unknown"]

lsDeclareVar
  :: (Embeddable t v c)
  => Bool
  -> VarName
  -> t
  -> LexScope t v
  -> Circify t v c (LexScope t v)
lsDeclareVar isInput var ty scope = case M.lookup var (tys scope) of
  Nothing -> do
    -- First we add type and version entries for this variable
    let withTyAndVer = scope { vers = M.insert var initialVersion $ vers scope
                             , tys  = M.insert var ty $ tys scope
                             }
        ssaVar = lsGetSsaVar var withTyAndVer
    -- Now we declare it to the SMT layer
    d    <- gets (declare . langCfg)
    term <- liftMem
      $ d ty (ssaVarAsString ssaVar) (if isInput then Just var else Nothing)
    return $ withTyAndVer { terms = M.insert ssaVar (Base term) $ terms scope }
  Just actualTy ->
    error $ unwords ["Already declared", var, "to have type", show actualTy]


-- | Get the current version of the variable
lsGetVer :: VarName -> LexScope t v -> Version
lsGetVer var scope = fromMaybe (unknownVar var) (lsGetMaybeVer var scope)

lsGetMaybeVer :: VarName -> LexScope t v -> Maybe Version
lsGetMaybeVer var scope = M.lookup var (vers scope)

-- | Get current SsaVar
lsGetSsaVar :: VarName -> LexScope t v -> SsaVar
lsGetSsaVar var scope = SsaVar (lsScopedVar var scope) (lsGetVer var scope)

lsGetNextSsaVar :: VarName -> LexScope t v -> SsaVar
lsGetNextSsaVar var scope =
  SsaVar (lsScopedVar var scope) (lsGetVer var scope + 1)

lsScopedVar :: VarName -> LexScope t v -> String
lsScopedVar var scope = lsPrefix scope ++ "__" ++ var

-- | Get the C++ type of the variable
lsGetType :: VarName -> LexScope t v -> t
lsGetType var scope = fromMaybe (unknownVar var) (M.lookup var (tys scope))

-- | Get a SsaVal for the given var
lsGetTerm :: VarName -> LexScope t v -> SsaVal v
lsGetTerm var scope = fromMaybe
  (error $ unwords ["No term for", var])
  (M.lookup (lsGetSsaVar var scope) (terms scope))

lsSetTerm :: SsaVal v -> VarName -> LexScope t v -> LexScope t v
lsSetTerm val var scope =
  scope { terms = M.insert (lsGetSsaVar var scope) val $ terms scope }

lsNextVer :: VarName -> LexScope t v -> LexScope t v
lsNextVer var scope =
  scope { vers = M.insert var (lsGetVer var scope + 1) $ vers scope }

-- A guard restricts effects base on
-- (a) whether the condition is true or
-- (b) whether none of the breaks are true.
-- the breaks have labels
data Guard = Guard Ty.TermBool
           | Break String [Ty.TermBool]
           deriving (Show)

printGuard :: Guard -> IO ()
printGuard g = case g of
  Guard b   -> putStrLn $ "   " ++ show b
  Break n b -> putStrLn $ "   " ++ n ++ ": " ++ show b

guardConditions :: Guard -> [Ty.TermBool]
guardConditions g = case g of
  Guard c    -> [c]
  Break _ cs -> map Ty.Not cs

data FunctionScope ty term = FunctionScope
  { -- Condition for current path
    guards         :: [Guard]
                                     -- Stack of lexical scopes. Innermost first.
  , lexicalScopes  :: [LexScope ty term]
  , nCurrentScopes :: Int
                                     -- number of next ls
  , lsCtr          :: Int
  , fsPrefix       :: String
  , retTy          :: Maybe ty
  }
  deriving Show

listModify :: Functor m => Int -> (a -> m a) -> [a] -> m [a]
listModify 0 f (x : xs) = (: xs) `fmap` f x
listModify n f (x : xs) = (x :) `fmap` listModify (n - 1) f xs
listModify _ _ []       = error "Could not modify at index"

-- | Find the scope containing this variable. Indexed from the back.
fsFindLexScopeOpt :: VarName -> FunctionScope t v -> Maybe Int
fsFindLexScopeOpt var scope = (nCurrentScopes scope - 1 -)
  <$> findIndex (M.member var . tys) (lexicalScopes scope)

-- | Apply a modification function to the indexed scope.
fsModifyLexScope
  :: Monad m
  => Int
  -> (LexScope t v -> m (LexScope t v))
  -> FunctionScope t v
  -> m (FunctionScope t v)
fsModifyLexScope i f scope = do
  n <- listModify (nCurrentScopes scope - i - 1) f $ lexicalScopes scope
  return $ scope { lexicalScopes = n }

-- | Apply a fetching function to the indexed scope.
fsGetFromLexScope :: Int -> (LexScope t v -> a) -> FunctionScope t v -> a
fsGetFromLexScope i f scope = if i < nCurrentScopes scope
  then f $ lexicalScopes scope !! (nCurrentScopes scope - i - 1)
  else error $ unwords ["Lexical scope index", show i, "is invalid"]

fsDeclareVar
  :: Embeddable t v c
  => Bool
  -> VarName
  -> t
  -> FunctionScope t v
  -> Circify t v c (FunctionScope t v)
fsDeclareVar isInput var ty scope = if null (lexicalScopes scope)
  then error "Cannot decalre variable: no lexical scopes!"
  else do
    head' <- lsDeclareVar isInput var ty (head $ lexicalScopes scope)
    return $ scope { lexicalScopes = head' : tail (lexicalScopes scope) }

fsGetType :: VarName -> Int -> FunctionScope t v -> t
fsGetType var i = fsGetFromLexScope i (lsGetType var)

fsGetSsaVar :: VarName -> Int -> FunctionScope t v -> SsaVar
fsGetSsaVar var i = fsGetFromLexScope i (lsGetSsaVar var)

fsGetNextSsaVar :: VarName -> Int -> FunctionScope t v -> SsaVar
fsGetNextSsaVar var i = fsGetFromLexScope i (lsGetNextSsaVar var)

fsGetTerm :: VarName -> Int -> FunctionScope t v -> SsaVal v
fsGetTerm var i = fsGetFromLexScope i (lsGetTerm var)

fsGetVer :: VarName -> Int -> FunctionScope t v -> Version
fsGetVer var i = fsGetFromLexScope i (lsGetVer var)

fsSetTerm
  :: SsaVal v -> VarName -> Int -> FunctionScope t v -> FunctionScope t v
fsSetTerm val var i =
  runIdentity . fsModifyLexScope i (Identity . lsSetTerm val var)

fsNextVer :: VarName -> Int -> FunctionScope t v -> FunctionScope t v
fsNextVer var i = runIdentity . fsModifyLexScope i (Identity . lsNextVer var)

fsEnterLexScope :: FunctionScope t v -> FunctionScope t v
fsEnterLexScope scope =
  let newLs = lsWithPrefix (fsPrefix scope ++ "_lex" ++ show (lsCtr scope))
  in  scope { lsCtr          = 1 + lsCtr scope
            , lexicalScopes  = newLs : lexicalScopes scope
            , nCurrentScopes = 1 + nCurrentScopes scope
            }

printFs :: FunctionScope t v -> IO ()
printFs s = do
  putStrLn " FunctionScope:"
  putStrLn $ "  Lex counter: " ++ show (lsCtr s)
  putStrLn "  LexicalScopes:"
  traverse_ printLs (lexicalScopes s)
  putStrLn "  Guards:"
  traverse_ printGuard (guards s)

fsExitLexScope :: FunctionScope t v -> FunctionScope t v
fsExitLexScope scope = if null (lexicalScopes scope)
  then error "There is no lexecical scope to exit"
  else scope { nCurrentScopes = nCurrentScopes scope - 1
             , lexicalScopes  = tail $ lexicalScopes scope
             }

fsPushBreakable :: String -> FunctionScope t v -> FunctionScope t v
fsPushBreakable name scope = scope { guards = Break name [] : guards scope }

fsPushGuard :: Ty.TermBool -> FunctionScope t v -> FunctionScope t v
fsPushGuard guard scope = scope { guards = Guard guard : guards scope }

fsPopGuard :: FunctionScope t v -> FunctionScope t v
fsPopGuard scope = if null (guards scope)
  then error "No guard to pop"
  else scope { guards = tail $ guards scope }

-- Walk to the named break point, accumulating conditions.
-- When you get there, add the accumulated condition.
fsDoBreak :: String -> FunctionScope t v -> FunctionScope t v
fsDoBreak name scope = scope { guards = go [] $ guards scope }
 where
  go acc gs = case gs of
    Guard g        : r -> Guard g : go (g : acc) r
    Break name' cs : r -> if name == name'
      then Break name' (safeNary Ty.And acc : cs) : r
      else Break name' cs : go (map Ty.Not cs ++ acc) r
    [] -> error $ "Could not find break " ++ show name

fsCurrentGuard :: FunctionScope t v -> [Ty.TermBool]
fsCurrentGuard = concatMap guardConditions . guards

returnBreakName :: String
returnBreakName = "CircifyMonadReturn"

fsWithPrefix :: String -> Maybe t -> FunctionScope t v
fsWithPrefix prefix ty = FunctionScope { guards         = []
                                       , retTy          = ty
                                       , lexicalScopes  = []
                                       , nCurrentScopes = 0
                                       , lsCtr          = 0
                                       , fsPrefix       = prefix
                                       }

class (Show t, Show v) => Embeddable t v c | v -> c, v -> t where
  declare :: c -> t -> String -> Maybe VarName -> Mem v
  -- | Returns a new term, equal to the ITE of the two argument term, on the
  -- argument condition.
  ite :: c -> Ty.TermBool -> v -> v -> Mem v
  -- | Create a new variable of the given type, set to the given term.
  -- Returns the term for the new variable, and the term that
  -- it was assigned to.
  -- These are distinguished to allow for casting.
  assign :: c -> t -> String -> v -> Mem (v, v)
  setValues :: c -> String -> v -> Assert ()

-- | Internal state of the compiler for code generation
data CircifyState t v c = CircifyState
  { callStack :: [FunctionScope t v]     -- ^ Function scopes
  , nFrames   :: Int                     -- ^ Number of them
  , globals   :: LexScope t v            -- ^ Global scope
  , typedefs  :: M.Map VarName t         -- ^ Type aliases
  , prefix    :: [String]                -- ^ Curreny name prefix
  , fnCtr     :: Int                     -- ^ Inline fn-call #
  , langCfg   :: c                       -- ^ Language configuration
  , guardCnt  :: Int                     -- ^ Number of guards so far
  }

newtype Circify t v c a = Circify (StateT (CircifyState t v c) Mem a)
    deriving (Functor, Applicative, Monad, MonadState (CircifyState t v c), MonadIO, MonadLog, MonadMem, MonadAssert, MonadCfg, MonadDeepState ((Assert.AssertState, Mem.MemState), CircifyState t v c))

class Monad m => MonadCircify t v c m | m -> v, m -> t, m -> c where
  liftCircify :: Circify t v c a -> m a
instance MonadCircify t v c (Circify t v c) where
  liftCircify = id
instance (MonadCircify t v c m) => MonadCircify t v c (StateT s m) where
  liftCircify = lift . liftCircify


---
--- Setup, monad functions, etc
---

emptyCircifyState :: c -> CircifyState t v c
emptyCircifyState lang = CircifyState { callStack = []
                                      , nFrames   = 0
                                      , globals   = lsWithPrefix "global"
                                      , typedefs  = M.empty
                                      , prefix    = []
                                      , fnCtr     = 0
                                      , langCfg   = lang
                                      , guardCnt  = 0
                                      }

compilerRunOnTop
  :: (FunctionScope t v -> Circify t v c (a, FunctionScope t v))
  -> Circify t v c a
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

compilerFindScope :: SsaLVal -> Circify t v c ScopeIdx
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
  -> (FunctionScope t v -> Circify t v c (a, FunctionScope t v))
  -> (LexScope t v -> Circify t v c (a, LexScope t v))
  -> Circify t v c a
compilerRunInScopeIdx scopeIdx fF lF = case scopeIdx of
  ScopeGlobal -> do
    global       <- gets globals
    (r, global') <- lF global
    modify $ \s -> s { globals = global' }
    return r
  -- TODO: use l
  ScopeIdx { lsIdx = _l, fsIdx = f } -> do
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

ssaValAsTerm :: Show v => String -> SsaVal v -> v
ssaValAsTerm reason v = case v of
  Base c -> c
  RefVal{} ->
    error
      $  "Cannot unwrap "
      ++ show v
      ++ " as a C term. It is a reference. Reason\n"
      ++ reason

liftTermFun :: Show v => String -> (v -> v) -> SsaVal v -> SsaVal v
liftTermFun name f x = case x of
  Base c -> Base $ f c
  RefVal{} ->
    error $ "Cannot apply c function " ++ name ++ " to reference " ++ show x

liftTermFun2
  :: Show v => String -> (v -> v -> v) -> SsaVal v -> SsaVal v -> SsaVal v
liftTermFun2 name f x1 x2 = case (x1, x2) of
  (Base c1, Base c2) -> Base $ f c1 c2
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2)

liftTermFun3
  :: Show v
  => String
  -> (v -> v -> v -> v)
  -> SsaVal v
  -> SsaVal v
  -> SsaVal v
  -> SsaVal v
liftTermFun3 name f x1 x2 x3 = case (x1, x2, x3) of
  (Base c1, Base c2, Base c3) -> Base $ f c1 c2 c3
  _ ->
    error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2, x3)

liftTermFun2M
  :: (Show v, Monad m)
  => String
  -> (v -> v -> m v)
  -> SsaVal v
  -> SsaVal v
  -> m (SsaVal v)
liftTermFun2M name f x1 x2 = case (x1, x2) of
  (Base c1, Base c2) -> Base <$> f c1 c2
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2)

compilerModifyInScope
  :: SsaLVal
  -> (VarName -> Int -> FunctionScope t v -> FunctionScope t v)
  -> (VarName -> LexScope t v -> LexScope t v)
  -> Circify t v c ()
compilerModifyInScope v fF lF = do
  idx <- compilerFindScope v
  compilerRunInScopeIdx idx
                        (return . ((), ) . fF (ssaLValName v) (lsIdx idx))
                        (return . ((), ) . lF (ssaLValName v))

compilerGetsInScope
  :: SsaLVal
  -> (VarName -> Int -> FunctionScope t v -> a)
  -> (VarName -> LexScope t v -> a)
  -> Circify t v c a
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

compilerModifyTopM
  :: (FunctionScope t v -> Circify t v c (FunctionScope t v))
  -> Circify t v c ()
compilerModifyTopM f = compilerRunOnTop $ fmap ((), ) . f

compilerModifyTop
  :: (FunctionScope t v -> FunctionScope t v) -> Circify t v c ()
compilerModifyTop f = compilerModifyTopM (return . f)

declareInitVar
  :: (Embeddable t v c) => VarName -> t -> SsaVal v -> Circify t v c ()
declareInitVar var ty term = do
  declareVar False var ty
  void $ argAssign (SLVar var) term

-- | Declares a global variable
declareGlobal :: (Embeddable t v c) => Bool -> VarName -> t -> Circify t v c ()
declareGlobal isInput var ty = do
  g  <- gets globals
  g' <- lsDeclareVar isInput var ty g
  modify $ \s -> s { globals = g' }

declareVar :: (Embeddable t v c) => Bool -> VarName -> t -> Circify t v c ()
declareVar isInput var ty = do
  isGlobal <- gets (null . callStack)
  if isGlobal
    then declareGlobal isInput var ty
    else compilerModifyTopM $ \s -> fsDeclareVar isInput var ty s

nextVer :: SsaLVal -> Circify t v c ()
nextVer v = compilerModifyInScope v fsNextVer lsNextVer

getType :: SsaLVal -> Circify t v c t
getType v = compilerGetsInScope v fsGetType lsGetType

getSsaName :: SsaLVal -> Circify t v c String
getSsaName v = ssaVarAsString <$> compilerGetsInScope v fsGetSsaVar lsGetSsaVar

getNextSsaName :: SsaLVal -> Circify t v c String
getNextSsaName v =
  ssaVarAsString <$> compilerGetsInScope v fsGetNextSsaVar lsGetNextSsaVar

computingValues :: Circify t v c Bool
computingValues = liftAssert Assert.isStoringValues

-- We do not record witness values for references.
setValue :: Embeddable t v c => SsaLVal -> v -> Circify t v c ()
setValue name cterm = do
  var <- getSsaName name
  setValueRaw var cterm

setValueRaw :: Embeddable t v c => String -> v -> Circify t v c ()
setValueRaw var cterm = whenM computingValues $ do
  e <- gets (setValues . langCfg)
  liftAssert $ e var cterm

getTerm :: SsaLVal -> Circify t v c (SsaVal v)
getTerm var = compilerGetsInScope var fsGetTerm lsGetTerm

getVer :: SsaLVal -> Circify t v c Version
getVer var = compilerGetsInScope var fsGetVer lsGetVer

setTerm :: SsaLVal -> SsaVal v -> Circify t v c ()
setTerm n v = compilerModifyInScope n (fsSetTerm v) (lsSetTerm v)

printComp :: Show t => Circify t v c ()
printComp = do
  gets callStack >>= liftIO . traverse_ printFs
  liftIO $ putStrLn "Typedefs:"
  gets typedefs
    >>= liftIO
    .   traverse_ (\(k, v) -> putStrLn ("  " ++ k ++ " -> " ++ show v))
    .   M.toList

enterLexScope :: Circify t v c ()
enterLexScope = compilerModifyTop fsEnterLexScope

exitLexScope :: Circify t v c ()
exitLexScope = compilerModifyTop fsExitLexScope

scoped :: MonadCircify t v c m => m a -> m a
scoped a = liftCircify enterLexScope *> a <* liftCircify exitLexScope

pushGuard :: Ty.TermBool -> Circify t v c ()
pushGuard test = do
  i <- gets guardCnt
  let guardName = "C_guard_" ++ show i
  modify $ \s -> s { guardCnt = 1 + guardCnt s }
  g <- liftAssert $ do
    g <- Assert.newVar guardName Ty.SortBool
    Assert.assign g test
    Assert.evalAndSetValue guardName test
    return g
  compilerModifyTop $ fsPushGuard g

popGuard :: Circify t v c ()
popGuard = compilerModifyTop fsPopGuard

guarded :: MonadCircify t v c m => Ty.TermBool -> m a -> m a
guarded c a = liftCircify (pushGuard c) *> a <* liftCircify popGuard

getGuard :: Circify t v c Ty.TermBool
getGuard = safeNary Ty.And . concatMap fsCurrentGuard . callStack <$> get

doReturn :: (Embeddable t v c) => v -> Circify t v c ()
doReturn value = do
  void $ ssaAssign (SLVar returnValueName) (Base value)
  doBreak returnBreakName

runCircify
  :: c -> Circify t v c a -> Assert ((a, CircifyState t v c), Mem.MemState)
runCircify langDef (Circify act) =
  Mem.runMem $ runStateT act $ emptyCircifyState langDef

-- | Human readable name.
-- We probably want to replace this with something faster (eg hash) someday, but
-- this is great for debugging
ssaVarAsString :: SsaVar -> String
ssaVarAsString (SsaVar varName ver) = varName ++ "_v" ++ show ver


-- Assert that the current version of `var` is assign `value` to it.
-- Could return 
argAssign :: Embeddable t v c => SsaLVal -> SsaVal v -> Circify t v c (SsaVal v)
argAssign var val = do
  --liftIO $ putStrLn $ "argAssign " ++ var ++ " = " ++ show val
  ty      <- getType var
  ssaName <- getSsaName var
  case val of
    Base cval -> do
      a            <- gets (assign . langCfg)
      (t, castVal) <- liftMem $ a ty ssaName cval
      setTerm var (Base t)
      whenM computingValues $ setValue var castVal
      return (Base t)
    RefVal{} -> do
      setTerm var val
      return val

-- Bump the version of `var` and assign `value` to it.
ssaAssign :: Embeddable t v c => SsaLVal -> SsaVal v -> Circify t v c (SsaVal v)
ssaAssign var val = do
  priorTerm   <- getTerm var
  ty          <- getType var
  nextSsaName <- getNextSsaName var
  guard       <- getGuard
  case (val, priorTerm) of
    (Base cval, Base priorCval) -> do
      a         <- gets (assign . langCfg)
      i         <- gets (ite . langCfg)
      cterm     <- liftMem $ i guard cval priorCval
      (t, val') <- liftMem $ a ty nextSsaName cterm
      nextVer var
      setTerm var (Base t)
      whenM computingValues $ setValue var val'
      return (Base t)
    (RefVal{}, _) -> do
      setTerm var val
      return val
    _ -> error $ unwords
      [ "Cannot assign"
      , show val
      , "to location"
      , show var
      , "which held a"
      , show priorTerm
      ]

getRef :: SsaLVal -> Circify t v c (SsaVal v)
getRef lval = do
  idx <- compilerFindScope lval
  return $ RefVal (Ref idx (ssaLValName lval))

deref :: Show v => SsaVal v -> SsaLVal
deref val = case val of
  RefVal r -> SLRef r
  Base   c -> error $ "Cannot derefence base value: " ++ show c


---
--- Functions
---

returnValueName :: String
returnValueName = "return"

pushBreakable :: (Embeddable t v c) => String -> Circify t v c ()
pushBreakable name = do
  logIf "break" $ "Break: " ++ name
  compilerModifyTop $ fsPushBreakable name

doBreak :: (Embeddable t v c) => String -> Circify t v c ()
doBreak = compilerModifyTop . fsDoBreak

pushFunction :: (Embeddable t v c) => String -> Maybe t -> Circify t v c ()
pushFunction name ty = do
  p <- gets prefix
  c <- gets fnCtr
  let p' = name : p
  let fs =
        fsWithPrefix ("f" ++ show c ++ "_" ++ intercalate "_" (reverse p')) ty
  modify
    (\s -> s { prefix    = p'
             , callStack = fs : callStack s
             , fnCtr     = c + 1
             , nFrames   = 1 + nFrames s
             }
    )
  enterLexScope
  pushBreakable returnBreakName
  forM_ ty $ \t -> declareVar False returnValueName t

-- Pop a function, returning the return term
popFunction :: (Embeddable t v c) => Circify t v c (Maybe v)
popFunction = do
  popGuard
  stack   <- gets callStack
  frame   <- gets (head . callStack)
  retTerm <- forM (retTy frame) $ \ty -> do
    t <- ssaValAsTerm "return get" <$> getTerm (SLVar returnValueName)
    let retName = fsPrefix frame ++ "__" ++ returnValueName
    a        <- gets (assign . langCfg)
    (t', v') <- liftMem $ a ty retName t
    whenM computingValues $ setValueRaw retName v'
    return t'
  exitLexScope
  if null stack
    then error "Cannot pop function, empty stack"
    else modify
      (\s -> s { callStack = tail (callStack s)
               , prefix    = tail (prefix s)
               , nFrames   = nFrames s - 1
               }
      )
  return retTerm

---
--- Typedefs
---

typedef :: Embeddable t v c => VarName -> t -> Circify t v c ()
typedef name ty = do
  logIf "typedef" $ "typedef " ++ name ++ " to " ++ show ty
  modify $ \s -> case M.lookup name (typedefs s) of
    Nothing -> s { typedefs = M.insert name ty $ typedefs s }
    Just t  -> error $ unwords ["Already td'd", name, "to", show t]

untypedef :: VarName -> Circify t v c (Maybe t)
untypedef name = M.lookup name <$> gets typedefs

---
--- Random
---

safeNary :: Ty.BoolNaryOp -> [Ty.TermBool] -> Ty.TermBool
safeNary op xs = case xs of
  []  -> Ty.boolNaryId op
  [s] -> s
  _   -> Ty.BoolNaryExpr op xs

-- | Is the current path reachable, for some inputs?
reachable :: Circify t v c Bool
reachable = do
  pathCondition <- getGuard
  f             <- liftAssert Assert.formula
  liftLog $ Z3.sat <$> Z3.evalZ3Model
    (Ty.BoolNaryExpr Ty.And [f, pathCondition])

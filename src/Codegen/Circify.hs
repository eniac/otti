{-# LANGUAGE GADTs                      #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- For MonadCircify
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Codegen.Circify where

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
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import           IR.SMT.Assert                  ( Assert
                                                , MonadAssert
                                                , liftAssert
                                                )
import           Util.Log
import           Util.Cfg                       ( MonadCfg )

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
data LexScope ty term = LexScope { tys :: M.Map VarName ty
                                 , vers :: M.Map VarName Version
                                 , terms :: M.Map SsaVar (SsaVal term)
                                 , lsPrefix :: String
                                 } deriving (Show)
printLs :: LexScope ty term -> IO ()
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
data SsaVal term  = Base   term
                  | RefVal Ref
                  deriving (Show)

-- Lexical scope functions

initialVersion :: Int
initialVersion = 0

lsWithPrefix :: String -> LexScope ty term
lsWithPrefix s =
  LexScope { tys = M.empty, vers = M.empty, terms = M.empty, lsPrefix = s }

unknownVar :: VarName -> a
unknownVar var = error $ unwords ["Variable", var, "is unknown"]

lsDeclareVar
  :: (Show ty)
  => VarName
  -> ty
  -> LexScope ty term
  -> Circify ty term (LexScope ty term)
lsDeclareVar var ty scope = case M.lookup var (tys scope) of
  Nothing -> do
    -- First we add type and version entries for this variable
    let withTyAndVer = scope { vers = M.insert var initialVersion $ vers scope
                             , tys  = M.insert var ty $ tys scope
                             }
        ssaVar = lsGetSsaVar var withTyAndVer
    -- Now we declare it to the SMT layer
    d    <- gets (declare . lang)
    term <- liftMem $ d ty (ssaVarAsString ssaVar)
    return $ withTyAndVer { terms = M.insert ssaVar (Base term) $ terms scope }
  Just actualTy ->
    error $ unwords ["Already declared", var, "to have type", show actualTy]


-- | Get the current version of the variable
lsGetVer :: VarName -> LexScope ty term -> Version
lsGetVer var scope = fromMaybe (unknownVar var) (lsGetMaybeVer var scope)

lsGetMaybeVer :: VarName -> LexScope ty term -> Maybe Version
lsGetMaybeVer var scope = M.lookup var (vers scope)

-- | Get current SsaVar
lsGetSsaVar :: VarName -> LexScope ty term -> SsaVar
lsGetSsaVar var scope = SsaVar (lsScopedVar var scope) (lsGetVer var scope)

lsGetNextSsaVar :: VarName -> LexScope ty term -> SsaVar
lsGetNextSsaVar var scope =
  SsaVar (lsScopedVar var scope) (lsGetVer var scope + 1)

lsScopedVar :: VarName -> LexScope ty term -> String
lsScopedVar var scope = lsPrefix scope ++ "__" ++ var

-- | Get the C++ type of the variable
lsGetType :: VarName -> LexScope ty term -> ty
lsGetType var scope = fromMaybe (unknownVar var) (M.lookup var (tys scope))

-- | Get a SsaVal for the given var
lsGetTerm :: VarName -> LexScope ty term -> SsaVal term
lsGetTerm var scope = fromMaybe
  (error $ unwords ["No term for", var])
  (M.lookup (lsGetSsaVar var scope) (terms scope))

lsSetTerm :: SsaVal term -> VarName -> LexScope ty term -> LexScope ty term
lsSetTerm val var scope =
  scope { terms = M.insert (lsGetSsaVar var scope) val $ terms scope }

lsNextVer :: VarName -> LexScope ty term -> LexScope ty term
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

data FunctionScope ty term = FunctionScope { -- Condition for current path
                                     guards :: [Guard]
                                     -- Stack of lexical scopes. Innermost first.
                                   , lexicalScopes     :: [LexScope ty term]
                                   , nCurrentScopes    :: Int
                                     -- number of next ls
                                   , lsCtr             :: Int
                                   , fsPrefix          :: String
                                   , retTy             :: Maybe ty
                                   } deriving (Show)

listModify :: Functor m => Int -> (a -> m a) -> [a] -> m [a]
listModify 0 f (x : xs) = (: xs) `fmap` f x
listModify n f (x : xs) = (x :) `fmap` listModify (n - 1) f xs

-- | Find the scope containing this variable. Indexed from the back.
fsFindLexScope :: VarName -> FunctionScope ty term -> Int
fsFindLexScope var scope =
  fromMaybe (error $ unwords ["Cannot find", var, "in current scope"])
    $ fsFindLexScopeOpt var scope

-- | Find the scope containing this variable. Indexed from the back.
fsFindLexScopeOpt :: VarName -> FunctionScope ty term -> Maybe Int
fsFindLexScopeOpt var scope = (nCurrentScopes scope - 1 -)
  <$> findIndex (M.member var . tys) (lexicalScopes scope)

-- | Apply a modification function to the indexed scope.
fsModifyLexScope
  :: Monad m
  => Int
  -> (LexScope ty term -> m (LexScope ty term))
  -> FunctionScope ty term
  -> m (FunctionScope ty term)
fsModifyLexScope i f scope = do
  n <- listModify (nCurrentScopes scope - i - 1) f $ lexicalScopes scope
  return $ scope { lexicalScopes = n }

-- | Apply a fetching function to the indexed scope.
fsGetFromLexScope
  :: Int -> (LexScope ty term -> a) -> FunctionScope ty term -> a
fsGetFromLexScope i f scope = if i < nCurrentScopes scope
  then f $ lexicalScopes scope !! (nCurrentScopes scope - i - 1)
  else error $ unwords ["Lexical scope index", show i, "is invalid"]

fsDeclareVar
  :: Show ty
  => VarName
  -> ty
  -> FunctionScope ty term
  -> Circify ty term (FunctionScope ty term)
fsDeclareVar var ty scope = if null (lexicalScopes scope)
  then error "Cannot decalre variable: no lexical scopes!"
  else do
    head' <- lsDeclareVar var ty (head $ lexicalScopes scope)
    return $ scope { lexicalScopes = head' : tail (lexicalScopes scope) }

fsGetVer :: VarName -> Int -> FunctionScope ty term -> Version
fsGetVer var i = fsGetFromLexScope i (lsGetVer var)

fsGetType :: VarName -> Int -> FunctionScope ty term -> ty
fsGetType var i = fsGetFromLexScope i (lsGetType var)

fsGetSsaVar :: VarName -> Int -> FunctionScope ty term -> SsaVar
fsGetSsaVar var i = fsGetFromLexScope i (lsGetSsaVar var)

fsGetNextSsaVar :: VarName -> Int -> FunctionScope ty term -> SsaVar
fsGetNextSsaVar var i = fsGetFromLexScope i (lsGetNextSsaVar var)

fsGetTerm :: VarName -> Int -> FunctionScope ty term -> SsaVal term
fsGetTerm var i = fsGetFromLexScope i (lsGetTerm var)

fsSetTerm
  :: SsaVal term
  -> VarName
  -> Int
  -> FunctionScope ty term
  -> FunctionScope ty term
fsSetTerm val var i =
  runIdentity . fsModifyLexScope i (Identity . lsSetTerm val var)

fsNextVer :: VarName -> Int -> FunctionScope ty term -> FunctionScope ty term
fsNextVer var i = runIdentity . fsModifyLexScope i (Identity . lsNextVer var)

fsEnterLexScope :: FunctionScope ty term -> FunctionScope ty term
fsEnterLexScope scope =
  let newLs = lsWithPrefix (fsPrefix scope ++ "_lex" ++ show (lsCtr scope))
  in  scope { lsCtr          = 1 + lsCtr scope
            , lexicalScopes  = newLs : lexicalScopes scope
            , nCurrentScopes = 1 + nCurrentScopes scope
            }

printFs :: FunctionScope ty term -> IO ()
printFs s = do
  putStrLn " FunctionScope:"
  putStrLn $ "  Lex counter: " ++ show (lsCtr s)
  putStrLn "  LexicalScopes:"
  traverse_ printLs (lexicalScopes s)
  putStrLn "  Guards:"
  traverse_ printGuard (guards s)

fsExitLexScope :: FunctionScope ty term -> FunctionScope ty term
fsExitLexScope scope = if null (lexicalScopes scope)
  then error "There is no lexecical scope to exit"
  else scope { nCurrentScopes = nCurrentScopes scope - 1
             , lexicalScopes  = tail $ lexicalScopes scope
             }

fsPushBreakable :: String -> FunctionScope ty term -> FunctionScope ty term
fsPushBreakable name scope = scope { guards = Break name [] : guards scope }

fsPushGuard :: Ty.TermBool -> FunctionScope ty term -> FunctionScope ty term
fsPushGuard guard scope = scope { guards = Guard guard : guards scope }

fsPopGuard :: FunctionScope ty term -> FunctionScope ty term
fsPopGuard scope = if null (guards scope)
  then error "No guard to pop"
  else scope { guards = tail $ guards scope }

-- Walk to the named break point, accumulating conditions.
-- When you get there, add the accumulated condition.
fsDoBreak :: String -> FunctionScope ty term -> FunctionScope ty term
fsDoBreak name scope = scope { guards = go [] $ guards scope }
 where
  go acc gs = case gs of
    Guard g        : r -> Guard g : go (g : acc) r
    Break name' cs : r -> if name == name'
      then Break name' (safeNary Ty.And acc : cs) : r
      else Break name' cs : go (map Ty.Not cs ++ acc) r

fsCurrentGuard :: FunctionScope ty term -> [Ty.TermBool]
fsCurrentGuard = concatMap guardConditions . guards

returnBreakName :: String
returnBreakName = "CircifyMonadReturn"

fsWithPrefix :: String -> Maybe ty -> FunctionScope ty term
fsWithPrefix prefix ty = FunctionScope { guards         = []
                                       , retTy          = ty
                                       , lexicalScopes  = []
                                       , nCurrentScopes = 0
                                       , lsCtr          = 0
                                       , fsPrefix       = prefix
                                       }

-- The function which define an SMT-embedded language.
data LangDef ty term = LangDef { declare :: ty -> String -> Mem term
                               , assign  :: ty -> String -> term -> Maybe (Ty.TermBool, term) -> Mem (term, term)
                               , setValues :: String -> term -> Assert ()
                               , termInit :: ty -> Integer -> term
                               }

-- | Internal state of the compiler for code generation
data CircifyState ty term = CircifyState { callStack         :: [FunctionScope ty term]
                                 , nFrames           :: Int
                                 , globals           :: LexScope ty term
                                 , typedefs          :: M.Map VarName ty
                                 , prefix            :: [String]
                                 , fnCtr             :: Int
                                 , lang              :: LangDef ty term
                                 }

newtype Circify ty term a = Circify (StateT (CircifyState ty term) Mem a)
    deriving (Functor, Applicative, Monad, MonadState (CircifyState ty term), MonadIO, MonadLog, MonadMem, MonadAssert, MonadCfg)

class Monad m => MonadCircify ty term m | m -> term, m -> ty where
  liftCircify :: Circify ty term a -> m a
instance MonadCircify ty term (Circify ty term) where
  liftCircify = id
instance (MonadCircify ty term m) => MonadCircify ty term (StateT s m) where
  liftCircify = lift . liftCircify


---
--- Setup, monad functions, etc
---

emptyCircifyState :: LangDef ty term -> CircifyState ty term
emptyCircifyState lang = CircifyState { callStack = []
                                      , nFrames   = 0
                                      , globals   = lsWithPrefix "global"
                                      , typedefs  = M.empty
                                      , prefix    = []
                                      , fnCtr     = 0
                                      , lang      = lang
                                      }

compilerRunOnTop
  :: (FunctionScope ty term -> Circify ty term (a, FunctionScope ty term))
  -> Circify ty term a
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

compilerFindScope :: SsaLVal -> Circify ty term ScopeIdx
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
  -> (FunctionScope ty term -> Circify ty term (a, FunctionScope ty term))
  -> (LexScope ty term -> Circify ty term (a, LexScope ty term))
  -> Circify ty term a
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

ssaValAsTerm :: Show term => String -> SsaVal term -> term
ssaValAsTerm reason v = case v of
  Base c -> c
  RefVal{} ->
    error
      $  "Cannot unwrap "
      ++ show v
      ++ " as a C term. It is a reference. Reason\n"
      ++ reason

liftTermFun
  :: Show term => String -> (term -> term) -> SsaVal term -> SsaVal term
liftTermFun name f x = case x of
  Base c -> Base $ f c
  RefVal{} ->
    error $ "Cannot apply c function " ++ name ++ " to reference " ++ show x

liftTermFun2
  :: Show term
  => String
  -> (term -> term -> term)
  -> SsaVal term
  -> SsaVal term
  -> SsaVal term
liftTermFun2 name f x1 x2 = case (x1, x2) of
  (Base c1, Base c2) -> Base $ f c1 c2
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2)

liftTermFun3
  :: Show term
  => String
  -> (term -> term -> term -> term)
  -> SsaVal term
  -> SsaVal term
  -> SsaVal term
  -> SsaVal term
liftTermFun3 name f x1 x2 x3 = case (x1, x2, x3) of
  (Base c1, Base c2, Base c3) -> Base $ f c1 c2 c3
  _ ->
    error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2, x3)

liftTermFun2M
  :: (Show term, Monad m)
  => String
  -> (term -> term -> m term)
  -> SsaVal term
  -> SsaVal term
  -> m (SsaVal term)
liftTermFun2M name f x1 x2 = case (x1, x2) of
  (Base c1, Base c2) -> Base <$> f c1 c2
  _ -> error $ "Cannot apply c function " ++ name ++ " to " ++ show (x1, x2)

compilerRunInLValScope
  :: SsaLVal
  -> (FunctionScope ty term -> Circify ty term (a, FunctionScope ty term))
  -> (LexScope ty term -> Circify ty term (a, LexScope ty term))
  -> Circify ty term a
compilerRunInLValScope lval fF lF = do
  idx <- compilerFindScope lval
  compilerRunInScopeIdx idx fF lF

compilerModifyInScope
  :: SsaLVal
  -> (VarName -> Int -> FunctionScope ty term -> FunctionScope ty term)
  -> (VarName -> LexScope ty term -> LexScope ty term)
  -> Circify ty term ()
compilerModifyInScope v fF lF = do
  idx <- compilerFindScope v
  compilerRunInScopeIdx idx
                        (return . ((), ) . fF (ssaLValName v) (lsIdx idx))
                        (return . ((), ) . lF (ssaLValName v))

compilerGetsInScope
  :: SsaLVal
  -> (VarName -> Int -> FunctionScope ty term -> a)
  -> (VarName -> LexScope ty term -> a)
  -> Circify ty term a
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

compilerGetsFunction :: (FunctionScope ty term -> a) -> Circify ty term a
compilerGetsFunction f = compilerRunOnTop (\s -> return (f s, s))

compilerModifyTopM
  :: (FunctionScope ty term -> Circify ty term (FunctionScope ty term))
  -> Circify ty term ()
compilerModifyTopM f = compilerRunOnTop $ fmap ((), ) . f

compilerModifyTop
  :: (FunctionScope ty term -> FunctionScope ty term) -> Circify ty term ()
compilerModifyTop f = compilerModifyTopM (return . f)

compilerGetsTop :: (FunctionScope ty term -> a) -> Circify ty term a
compilerGetsTop f = compilerRunOnTop (\s -> return (f s, s))

declCommon :: (Show ty, Show term) => VarName -> ty -> Circify ty term ()
declCommon var ty = do
  isGlobal <- gets (null . callStack)
  if isGlobal
    then do
      g  <- gets globals
      g' <- lsDeclareVar var ty g
      modify $ \s -> s { globals = g' }
    else compilerModifyTopM $ \s -> fsDeclareVar var ty s

declareInitVar
  :: (Show ty, Show term) => VarName -> ty -> SsaVal term -> Circify ty term ()
declareInitVar var ty term = do
  declCommon var ty
  void $ argAssign (SLVar var) term

declareVar :: (Show ty, Show term) => VarName -> ty -> Circify ty term ()
declareVar var ty = do
  declCommon var ty
  -- Kind of weird: we don't know this is the right values, but there's no harm
  -- in lying to the SMT evaluation layer for now.
  i <- gets (termInit . lang)
  whenM computingValues $ setValue (SLVar var) $ i ty 0

getVer :: SsaLVal -> Circify ty term Version
getVer v = compilerGetsInScope v fsGetVer lsGetVer

nextVer :: SsaLVal -> Circify ty term ()
nextVer v = compilerModifyInScope v fsNextVer lsNextVer

getType :: SsaLVal -> Circify ty term ty
getType v = compilerGetsInScope v fsGetType lsGetType

getSsaVar :: SsaLVal -> Circify ty term SsaVar
getSsaVar v = compilerGetsInScope v fsGetSsaVar lsGetSsaVar

getNextSsaVar :: SsaLVal -> Circify ty term SsaVar
getNextSsaVar v = compilerGetsInScope v fsGetNextSsaVar lsGetNextSsaVar

getSsaName :: SsaLVal -> Circify ty term String
getSsaName n = ssaVarAsString <$> getSsaVar n

computingValues :: Circify ty term Bool
computingValues = liftAssert Assert.isStoringValues

getValues :: Circify ty term (M.Map String Dynamic)
getValues =
  liftAssert $ gets $ fromMaybe (error "Not computing values") . Assert.vals

smtEval :: Ty.SortClass s => Ty.Term s -> Circify ty term (Ty.Value s)
smtEval smt = flip Ty.eval smt <$> getValues

smtEvalBool :: Ty.TermBool -> Circify ty term Bool
smtEvalBool smt = Ty.valAsBool <$> smtEval smt

-- We do not record witness values for references.
setValue :: Show term => SsaLVal -> term -> Circify ty term ()
setValue name cterm = do
  var <- ssaVarAsString <$> getSsaVar name
  setValueRaw var cterm

setValueRaw :: Show term => String -> term -> Circify ty term ()
setValueRaw var cterm = whenM computingValues $ do
  e <- gets (setValues . lang)
  liftAssert $ e var cterm

getTerm :: SsaLVal -> Circify ty term (SsaVal term)
getTerm var = compilerGetsInScope var fsGetTerm lsGetTerm

setTerm :: SsaLVal -> SsaVal term -> Circify ty term ()
setTerm n v = compilerModifyInScope n (fsSetTerm v) (lsSetTerm v)

printComp :: Show ty => Circify ty term ()
printComp = do
  gets callStack >>= liftIO . traverse_ printFs
  liftIO $ putStrLn "Typedefs:"
  gets typedefs
    >>= liftIO
    .   traverse_ (\(k, v) -> putStrLn ("  " ++ k ++ " -> " ++ show v))
    .   M.toList

enterLexScope :: Circify ty term ()
enterLexScope = compilerModifyTop fsEnterLexScope

exitLexScope :: Circify ty term ()
exitLexScope = compilerModifyTop fsExitLexScope

pushGuard :: Ty.TermBool -> Circify ty term ()
pushGuard = compilerModifyTop . fsPushGuard

popGuard :: Circify ty term ()
popGuard = compilerModifyTop fsPopGuard

guarded :: MonadCircify ty term m => Ty.TermBool -> m a -> m a
guarded cond action =
  liftCircify (pushGuard cond) *> action <* liftCircify popGuard

getGuard :: Circify ty term Ty.TermBool
getGuard = safeNary Ty.And . concatMap fsCurrentGuard . callStack <$> get

doReturn :: Show term => term -> Circify ty term ()
doReturn value = do
  ssaAssign (SLVar returnValueName) (Base value)
  compilerModifyTop (fsDoBreak returnBreakName)

runCodegen
  :: LangDef ty term
  -> Circify ty term a
  -> Assert ((a, CircifyState ty term), Mem.MemState)
runCodegen langDef (Circify act) =
  Mem.runMem $ runStateT act $ emptyCircifyState langDef

evalCodegen :: LangDef ty term -> Circify ty term a -> Assert a
evalCodegen langDef act = fst . fst <$> runCodegen langDef act

execCodegen
  :: LangDef ty term -> Circify ty term a -> Assert (CircifyState ty term)
execCodegen langDef act = snd . fst <$> runCodegen langDef act


-- Turning VarNames (the AST's representation of a variable) into other representations
-- of variables

codegenVar :: SsaLVal -> Circify ty term SsaVar
codegenVar var = SsaVar (ssaLValName var) <$> getVer var

-- | Human readable name.
-- We probably want to replace this with something faster (eg hash) someday, but
-- this is great for debugging
ssaVarAsString :: SsaVar -> String
ssaVarAsString (SsaVar varName ver) = varName ++ "_v" ++ show ver

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = condition >>= flip when action

ifVal :: Monad m => SsaVal term -> (term -> m a) -> m ()
ifVal val f = case val of
  Base v -> void $ f v
  _      -> return ()

-- Assert that the current version of `var` is assign `value` to it.
-- Could return 
argAssign
  :: Show term => SsaLVal -> SsaVal term -> Circify ty term (SsaVal term)
argAssign var val = do
  --liftIO $ putStrLn $ "argAssign " ++ var ++ " = " ++ show val
  ty     <- getType var
  ssaVar <- getSsaVar var
  case val of
    Base cval -> do
      a            <- gets (assign . lang)
      (t, castVal) <- liftMem $ a ty (ssaVarAsString ssaVar) cval Nothing
      setTerm var (Base t)
      whenM computingValues $ setValue var castVal
      return (Base t)
    RefVal r -> do
      setTerm var val
      return val

-- Bump the version of `var` and assign `value` to it.
ssaAssign
  :: Show term => SsaLVal -> SsaVal term -> Circify ty term (SsaVal term)
ssaAssign var val = do
  priorTerm  <- getTerm var
  ty         <- getType var
  nextSsaVar <- getNextSsaVar var
  guard      <- getGuard
  case (val, priorTerm) of
    (Base cval, Base priorCval) -> do
      a         <- gets (assign . lang)
      (t, val') <- liftMem
        $ a ty (ssaVarAsString nextSsaVar) cval (Just (guard, priorCval))
      nextVer var
      setTerm var (Base t)
      whenM computingValues $ setValue var val'
      return (Base t)
    (RefVal r, _) -> do
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

initValues :: Circify ty term ()
initValues = liftAssert Assert.initValues

getRef :: SsaLVal -> Circify ty term (SsaVal term)
getRef lval = do
  idx <- compilerFindScope lval
  return $ RefVal (Ref idx (ssaLValName lval))

deref :: Show term => SsaVal term -> SsaLVal
deref val = case val of
  RefVal r -> SLRef r
  Base   c -> error $ "Cannot derefence base value: " ++ show c


---
--- Functions
---

returnValueName :: String
returnValueName = "return"

pushFunction :: (Show ty, Show term) => String -> Maybe ty -> Circify ty term ()
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
  compilerModifyTop $ fsPushBreakable returnBreakName
  forM_ ty $ \t -> declareVar returnValueName t

-- Pop a function, returning the return term
popFunction :: Show term => Circify ty term (Maybe term)
popFunction = do
  popGuard
  stack   <- gets callStack
  frame   <- gets (head . callStack)
  retTerm <- forM (retTy frame) $ \ty -> do
    t <- ssaValAsTerm "return get" <$> getTerm (SLVar returnValueName)
    let retName = fsPrefix frame ++ "__" ++ returnValueName
    a        <- gets (assign . lang)
    (t', v') <- liftMem $ a ty retName t Nothing
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

typedef :: Show ty => VarName -> ty -> Circify ty term ()
typedef name ty = do
  liftLog $ logIf "typedef" $ "typedef " ++ name ++ " to " ++ show ty
  modify $ \s -> case M.lookup name (typedefs s) of
    Nothing -> s { typedefs = M.insert name ty $ typedefs s }
    Just t  -> error $ unwords ["Already td'd", name, "to", show t]

untypedef :: VarName -> Circify ty term (Maybe ty)
untypedef name = M.lookup name <$> gets typedefs

---
--- If-statements
---

safeNary :: Ty.BoolNaryOp -> [Ty.TermBool] -> Ty.TermBool
safeNary op xs = case xs of
  []  -> Ty.boolNaryId op
  [s] -> s
  _   -> Ty.BoolNaryExpr op xs

-- UB

liftTermFunM
  :: Monad m => String -> (term -> m term) -> SsaVal term -> m (SsaVal term)
liftTermFunM name f x = case x of
  Base c -> Base <$> f c
  RefVal r ->
    error $ "Cannot apply c function " ++ name ++ " to reference " ++ show r

{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- For MonadX instances
{-# LANGUAGE MultiParamTypeClasses #-} -- For instances of MonadState*
{-# LANGUAGE TypeApplications #-}  -- For constructing SMT variables
{-# LANGUAGE ScopedTypeVariables #-} -- For constructing SMT variables
{-# LANGUAGE TemplateHaskell #-} -- Lenses
{-# LANGUAGE TupleSections #-} -- monadic index zip
{-|
This module is an alternative version of of the SMT Assert/Constraint system monad.

This version tracks much more information, and is designed to allow for fast updates.
-}
module IR.SMT.Opt.Assert
  ( AssertState(..)
  , MonadAssert(..)
  , Assert(..)
  , ArraySizes
  , vars
  , assertions
  , vals
  , public
  , index
  , sizes
  , newVar
  , assert
  , assign
  , refresh
  , check
  , eval
  , isStoringValues
  , runAssert
  , evalAssert
  , execAssert
  , fromAssertState
  , toAssertState
  , listAssertionIdxs
  , listAssertions
  , useAssertions
  , getAssertion
  , getAssertionM
  , deleteAssertion
  , logAssertions
  , modifyAssertions
  , modifyAssertionsWith
  )
where
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Control.Monad.Reader           ( ReaderT )
import qualified Data.Dynamic                  as Dyn
import qualified Data.IntMap.Strict            as IMap
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.HashMap.Strict           as HMap
import           Data.HashMap.Strict            ( HashMap )
import           Data.List                      ( foldl' )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import qualified Data.IntSet                   as ISet
import           Data.IntSet                    ( IntSet )
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq
import qualified Data.Foldable                 as F
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.TySmt.Alg              as TyAlg
import qualified IR.SMT.Assert                 as A
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Util.Control                   ( MonadDeepState(..)
                                                , whenM
                                                )
import           Util.Cfg                       ( MonadCfg )
import           Util.Log                       ( logIf
                                                , logIfM
                                                , liftLog
                                                , Log
                                                , MonadLog
                                                )
import           Util.ShowMap                   ( ShowMap )

---
--- Monad defintions
---

type Index = HashMap String IntSet
type Formula = IntMap Ty.TermBool

indexTerm :: Ty.SortClass s => Int -> Ty.Term s -> Index -> Index
indexTerm i t idx =
  let uses = Set.toList $ TyAlg.vars t
      addOrSingleton =
          HMap.alter (Just . maybe (ISet.singleton i) (ISet.insert i))
  in  foldl' (flip addOrSingleton) idx uses


-- | Given and indexed collection of assertions, builds a map from SMT variable
-- names to the indices of the assertions in which they appear.
indexFormula :: Formula -> Index
indexFormula f = foldl' (flip $ uncurry indexTerm) HMap.empty (IMap.toList f)

type ArraySizes = ShowMap (Ty.TermArray Ty.DynBvSort Ty.DynBvSort) Int

-- | State for keeping track of SMT-layer information
data AssertState = AssertState
  { _vars       :: !(HashMap String Dyn.Dynamic)
  , _assertions :: !Formula
  , _vals       :: !(Maybe (Map String Dyn.Dynamic))
  , _public     :: !(Set.Set String)
  , _nextId     :: !Int
  , _index      :: !Index
  , _sizes      :: !ArraySizes
  }
  deriving Show

$(makeLenses ''AssertState)

newtype Assert a = Assert (StateT AssertState Log a)
    deriving (Functor, Applicative, Monad, MonadState AssertState, MonadIO, MonadLog, MonadCfg)

class Monad m => MonadAssert m where
  liftAssert :: Assert a -> m a
instance MonadAssert Assert where
  liftAssert = id
instance (MonadAssert m) => MonadAssert (StateT s m) where
  liftAssert = lift . liftAssert
instance (MonadAssert m) => MonadAssert (ReaderT s m) where
  liftAssert = lift . liftAssert

fromAssertState :: A.AssertState -> AssertState
fromAssertState a =
  let f = IMap.fromDistinctAscList $ zip [0 ..] $ F.toList $ A.asserted a
      i = indexFormula f
      n = IMap.size f
  in  AssertState { _vars       = HMap.fromList $ Map.toList $ A.vars a
                  , _assertions = f
                  , _vals       = A.vals a
                  , _public     = A.public a
                  , _nextId     = n
                  , _index      = i
                  , _sizes      = A.arraySizes a
                  }

toAssertState :: AssertState -> A.AssertState
toAssertState a =
  let f = IMap.fromDistinctAscList $ zip [0 ..] $ F.toList $ _assertions a
      n = IMap.size f
  in  A.AssertState { A.vars       = Map.fromList $ HMap.toList $ _vars a
                    , A.asserted   = Seq.fromList $ F.toList $ _assertions a
                    , A.vals       = _vals a
                    , A.public     = _public a
                    , A.arraySizes = _sizes a
                    , A.nextVarN   = n
                    }

---
--- Setup and monad getters and setters
---

isStoringValues :: Assert Bool
isStoringValues = gets (isJust . view vals)

eval :: Ty.SortClass s => Ty.Term s -> Assert (Ty.Value s)
eval t = flip TyAlg.eval t . fromMaybe (error "No values!") <$> gets _vals

evalAndSetValueM
  :: (MonadAssert m, Ty.SortClass s) => String -> m (Ty.Term s) -> m ()
evalAndSetValueM variable term = do
  t <- term
  liftAssert (eval t >>= setValue variable)

setValue :: Ty.SortClass s => String -> Ty.Value s -> Assert ()
setValue variable value = do
  logIf "opt::witness" $ show variable ++ " -> " ++ show value
  whenM isStoringValues $ modify $ over vals $ fmap
    (Map.insert variable (Dyn.toDyn value))

listAssertionIdxs :: Assert IntSet
listAssertionIdxs =
  gets (ISet.fromDistinctAscList . IMap.keys . view assertions)

useAssertions :: String -> Assert [(Int, Ty.TermBool)]
useAssertions v = do
  idxs <- maybe [] ISet.toList . HMap.lookup v <$> gets (view index)
  forM idxs $ \i -> (i, ) <$> getAssertion i

listAssertions :: AssertState -> [Ty.TermBool]
listAssertions = map snd . IMap.toAscList . view assertions

getAssertion :: Int -> Assert Ty.TermBool
getAssertion i =
  fromMaybe (error $ "No assertion at " ++ show i) <$> getAssertionM i

getAssertionM :: Int -> Assert (Maybe Ty.TermBool)
getAssertionM i = gets (IMap.lookup i . view assertions)

deleteAssertion :: Int -> Assert ()
deleteAssertion i = do
  vs <- TyAlg.vars <$> getAssertion i
  forM_ (Set.toAscList vs)
    $ \v -> modify $ over index $ HMap.adjust (ISet.delete i) v
  modify $ over assertions $ IMap.delete i

runAssert :: Assert a -> AssertState -> Log (a, AssertState)
runAssert (Assert act) = runStateT act

evalAssert :: Assert a -> AssertState -> Log a
evalAssert act s = fst <$> runAssert act s

execAssert :: Assert a -> AssertState -> Log AssertState
execAssert act s = snd <$> runAssert act s

assert :: Ty.Term Ty.BoolSort -> Assert ()
assert a = do
  i <- gets $ view nextId
  modify $ over nextId (+ 1)
  modify $ over assertions $ IMap.insert i a
  modify $ over index $ indexTerm i a
  logIf "opt::assertions" $ "ASSERT: " ++ show a

assign :: Ty.SortClass s => Ty.Term s -> Ty.Term s -> Assert ()
assign a b = assert $ Ty.mkEq a b

newVarM
  :: forall s m
   . (Ty.SortClass s, MonadAssert m)
  => String
  -> Ty.Sort
  -> m (Ty.Term s)
  -> m (Ty.Term s)
newVarM name sort term = do
  allVars <- liftAssert $ gets $ view vars
  case HMap.lookup name allVars of
    Nothing -> do
      let v = Ty.mkVar @s name sort
      liftAssert $ modify $ over vars $ HMap.insert name (Dyn.toDyn v)
      evalAndSetValueM name term
      return v
    Just v -> return $ Dyn.fromDyn
      v
      (error $ unwords
        ["Already created variable", name, "with wrong sort:", show v]
      )

newVar
  :: forall s m
   . (Ty.SortClass s, MonadAssert m)
  => String
  -> Ty.Sort
  -> Ty.Term s
  -> m (Ty.Term s)
newVar n s t = newVarM n s (return t)

modifyAssertions :: ([Ty.TermBool] -> Log [Ty.TermBool]) -> Assert ()
modifyAssertions f = do
  a  <- gets _assertions
  a' <- liftLog $ IMap.fromDistinctAscList . zip [0 ..] <$> f
    (map snd $ IMap.toList a)
  modify $ set assertions a'
  refresh

modifyAssertionsWith :: MonadAssert m => (Ty.TermBool -> m Ty.TermBool) -> m ()
modifyAssertionsWith f = do
  idxs <- liftAssert $ listAssertionIdxs
  forM_ (ISet.toAscList idxs) $ \i -> do
    a' <- liftAssert (getAssertion i) >>= f
    liftAssert $ modify $ over assertions $ IMap.insert i a'
  liftAssert refresh

refresh :: Assert ()
refresh = modify $ \s -> set index (indexFormula $ view assertions s) s

check :: Assert (Either String ())
check = do
  s <- get
  return $ forM_ (F.toList $ view assertions s) $ \c -> case view vals s of
    Just e -> if Ty.ValBool True == TyAlg.eval e c
      then Right ()
      else Left $ "Unsat constraint:\n" ++ show c ++ "\nin\n" ++ show e
    Nothing -> Left "Missing values"

instance MonadDeepState AssertState Assert where
  deepGet = get
  deepPut = put

logAssertions :: String -> String -> Assert ()
logAssertions tag context = logIfM tag $ do
  as <- gets (map snd . IMap.toList . view assertions)
  liftIO $ putStrLn $ context ++ ":"
  liftIO $ putStrLn $ show (length as) ++ " assertions"
  forM_ as $ \a -> liftIO $ putStrLn $ "  " ++ show a
  return ""

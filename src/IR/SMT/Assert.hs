{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.SMT.Assert where
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Dynamic                  as Dyn
import           Data.Maybe                     ( isJust )
import           Data.Sequence                  ( Seq )
import qualified Data.Foldable                 as F
import qualified Data.Sequence                 as Seq
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.TySmt.Alg              as Alg
import           Util.Control                   ( MonadDeepState(..) )
import           Util.Cfg                       ( Cfg
                                                , MonadCfg
                                                )
import           Util.Log
import           Util.ShowMap                   ( ShowMap )
import qualified Util.ShowMap                  as SMap

---
--- Monad defintions
---
--
type ArraySizes = ShowMap (Ty.TermArray Ty.DynBvSort Ty.DynBvSort) Int

-- | State for keeping track of SMT-layer information
data AssertState = AssertState { vars         :: !(M.Map String Dyn.Dynamic)
                               , asserted     :: !(Seq (Ty.Term Ty.BoolSort))
                               , vals         :: !(Maybe (M.Map String Dyn.Dynamic))
                               , public       :: !(S.Set String)
                               , arraySizes   :: !ArraySizes
                               , nextVarN     :: !Int
                               }
                               deriving (Show)

newtype Assert a = Assert (StateT AssertState Log a)
    deriving (Functor, Applicative, Monad, MonadState AssertState, MonadIO, MonadLog, MonadCfg)

class Monad m => MonadAssert m where
  liftAssert :: Assert a -> m a
instance MonadAssert Assert where
  liftAssert = id
instance (MonadAssert m) => MonadAssert (StateT s m) where
  liftAssert = lift . liftAssert

---
--- Setup and monad getters and setters
---

emptyAssertState :: AssertState
emptyAssertState = AssertState { vars       = M.empty
                               , asserted   = Seq.empty
                               , vals       = Nothing
                               , public     = S.empty
                               , arraySizes = SMap.empty
                               , nextVarN   = 0
                               }

initValues :: Assert ()
initValues = modify $ \s -> s { vals = Just M.empty }

isStoringValues :: Assert Bool
isStoringValues = gets (isJust . vals)

evalAndSetValue :: Ty.SortClass s => String -> Ty.Term s -> Assert ()
evalAndSetValue variable term = do
  e <- gets vals
  case e of
    Just env -> setValue variable $ Alg.eval env term
    Nothing  -> return ()

setValue :: Ty.SortClass s => String -> Ty.Value s -> Assert ()
setValue variable value = do
  logIf "witness" $ show variable ++ " -> " ++ show value
  modify $ \s -> s { vals = M.insert variable (Dyn.toDyn value) <$> vals s }

publicize :: String -> Assert ()
publicize n = do
  logIf "publicize" $ "Publicize: " ++ n
  modify $ \s -> s { public = S.insert n $ public s }

runAssert :: Assert a -> Cfg (a, AssertState)
runAssert (Assert act) = evalLog $ runStateT act emptyAssertState

evalAssert :: Assert a -> Cfg a
evalAssert act = fst <$> runAssert act

execAssert :: Assert a -> Cfg AssertState
execAssert act = snd <$> runAssert act

assert :: Ty.Term Ty.BoolSort -> Assert ()
assert a = do
  logIf "assertions" $ "ASSERT: " ++ show a
  modify (\s -> s { asserted = asserted s Seq.|> a })

assign :: Ty.SortClass s => Ty.Term s -> Ty.Term s -> Assert ()
assign a b = assert $ Ty.mkEq a b

implies :: Ty.Term Ty.BoolSort -> Ty.Term Ty.BoolSort -> Assert ()
implies a b = assert $ Ty.BoolBinExpr Ty.Implies a b

newVar :: forall s . Ty.SortClass s => String -> Ty.Sort -> Assert (Ty.Term s)
newVar name sort = do
  s0 <- get
  let allVars = vars s0
  case M.lookup name allVars of
    Nothing -> do
      let v = Ty.mkVar @s name sort
      modify $ \s -> s { vars = M.insert name (Dyn.toDyn v) $ vars s }
      return v
    Just v -> return $ Dyn.fromDyn
      v
      (error $ unwords
        ["Already created variable", name, "with wrong sort:", show v]
      )

setSize :: Ty.TermArray Ty.DynBvSort Ty.DynBvSort -> Int -> Assert ()
setSize array size =
  modify $ \s -> s { arraySizes = SMap.insert array size $ arraySizes s }

freshVar
  :: forall s . Ty.SortClass s => String -> Ty.Sort -> Assert (Ty.Term s)
freshVar name sort = do
  i <- gets nextVarN
  modify $ \s -> s { nextVarN = 1 + nextVarN s }
  let name' = "fresh_" ++ show i ++ "_" ++ name
  newVar name' sort

check :: AssertState -> Either String ()
check s = forM_ (F.toList $ asserted s) $ \c -> case vals s of
  Just e -> if Ty.ValBool True == Alg.eval e c
    then Right ()
    else Left $ "Unsat constraint:\n" ++ show c ++ "\nin\n" ++ show e
  Nothing -> Left "Missing values"

formula :: Assert Ty.TermBool
formula = gets (Ty.BoolNaryExpr Ty.And . F.toList . asserted)

instance MonadDeepState AssertState Assert where
  deepGet = get
  deepPut = put

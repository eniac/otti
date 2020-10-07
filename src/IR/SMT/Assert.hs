{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.SMT.Assert where
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as M
import qualified Data.Set                      as S
import qualified Data.Dynamic                  as Dyn
import           Data.Maybe                     ( isJust )
import qualified IR.SMT.TySmt                  as Ty
import           Util.Log
import           Util.Cfg                       ( Cfg
                                                , MonadCfg
                                                )

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data AssertState = AssertState { vars         :: !(M.Map String Dyn.Dynamic)
                               , asserted     :: ![Ty.Term Ty.BoolSort]
                               , vals         :: !(Maybe (M.Map String Dyn.Dynamic))
                               , public       :: !(S.Set String)
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
emptyAssertState = AssertState { vars     = M.empty
                               , asserted = []
                               , vals     = Nothing
                               , public   = S.empty
                               }

initValues :: Assert ()
initValues = modify $ \s -> s { vals = Just M.empty }

isStoringValues :: Assert Bool
isStoringValues = gets (isJust . vals)

evalAndSetValue :: Ty.SortClass s => String -> Ty.Term s -> Assert ()
evalAndSetValue variable term = do
  e <- gets vals
  case e of
    Just env -> setValue variable $ Ty.eval env term
    Nothing  -> return ()

setValue :: Ty.SortClass s => String -> Ty.Value s -> Assert ()
setValue variable value = do
  liftLog $ logIf "witness" $ show variable ++ " -> " ++ show value
  modify $ \s -> s { vals = M.insert variable (Dyn.toDyn value) <$> vals s }

publicize :: String -> Assert ()
publicize n = modify $ \s -> s { public = S.insert n $ public s }

runAssert :: Assert a -> Cfg (a, AssertState)
runAssert (Assert act) = evalLog $ runStateT act emptyAssertState

evalAssert :: Assert a -> Cfg a
evalAssert act = fst <$> runAssert act

execAssert :: Assert a -> Cfg AssertState
execAssert act = snd <$> runAssert act

assert :: Ty.Term Ty.BoolSort -> Assert ()
assert a = do
  liftLog $ logIf "assertions" $ "ASSERT: " ++ show a
  modify (\s -> s { asserted = a : asserted s })

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

check :: AssertState -> Either String ()
check s = forM_ (asserted s) $ \c -> case vals s of
  Just e -> if Ty.ValBool True == Ty.eval e c
    then Right ()
    else Left $ "Unsat constraint:\n" ++ show c ++ "\nin\n" ++ show e
  Nothing -> Left "Missing values"

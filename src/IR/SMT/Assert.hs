{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.SMT.Assert where
import           Control.Monad()
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as M
import qualified Data.Dynamic                  as Dyn
import qualified IR.SMT.TySmt                  as Ty

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data AssertState = AssertState { vars         :: M.Map String Dyn.Dynamic
                               , asserted     :: [Ty.Term Ty.BoolSort]
                               --, vals         :: M.Map String Dyn.Dynamic
                               }
                               deriving (Show)

newtype Assert a = Assert (StateT AssertState IO a)
    deriving (Functor, Applicative, Monad, MonadState AssertState, MonadIO)

---
--- Setup and monad getters and setters
---

emptyAssertState :: AssertState
emptyAssertState = AssertState { vars = M.empty, asserted = [] }

runAssert :: Assert a -> IO (a, AssertState)
runAssert (Assert act) = runStateT act emptyAssertState

evalAssert :: Assert a -> IO a
evalAssert act = fst <$> runAssert act

execAssert :: Assert a -> IO AssertState
execAssert act = snd <$> runAssert act

assert :: Ty.Term Ty.BoolSort -> Assert ()
assert a = modify (\s -> s { asserted = a : asserted s })

assign :: Ty.SortClass s => Ty.Term s -> Ty.Term s -> Assert ()
assign a b = assert $ Ty.Eq a b

implies :: Ty.Term Ty.BoolSort -> Ty.Term Ty.BoolSort -> Assert ()
implies a b = assert $ Ty.BoolBinExpr Ty.Implies a b
--
--setValue :: Ty.SortClass s => String -> Ty.Value s -> Assert ()
--setValue name value = modify $ \s -> s { vals = M.insert name (Dyn.toDyn value) $ vals s }

-- REMOVE
-- getVars :: Assert (M.Map String Dyn.Dynamic)
-- getVars = vars `liftM` get
-- 
newVar :: forall s . Ty.SortClass s => String -> Ty.Sort -> Assert (Ty.Term s)
newVar name sort = do
  s0 <- get
  let allVars = vars s0
  case M.lookup name allVars of
    Nothing -> do
      let v = Ty.Var @s name sort
      modify $ \s -> s { vars = M.insert name (Dyn.toDyn v) $ vars s }
      return v
    _ -> error $ unwords ["Already created variable", name]


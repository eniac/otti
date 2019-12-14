{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR.SMTMonad where
import           AST.Simple                 (Type (..))
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           IR.IR
import           Targets.SMT.Z3Wrapper
import           Z3.Monad                   as Z

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data SMTState = SMTState { vars         :: M.Map String Node
                         , solverResult :: SMTResult
                         }

newtype SMT a = SMT (StateT SMTState Z.Z3 a)
    deriving (Functor, Applicative, Monad, MonadState SMTState, MonadIO)

instance Z.MonadZ3 SMT where
    getSolver = SMT $ lift $ Z.getSolver
    getContext = SMT $ lift $ Z.getContext

-- | Run SMT computation
runSMT :: Maybe Integer -- ^ Optional timeout
       -> SMT a         -- ^ Verification computation
       -> IO (a, SMTState)
runSMT mTimeout (SMT act) =
  Z.evalZ3With Nothing (Z.opt "timeout" (5000 :: Int)) $ runStateT act emptySMTState

-- | Eval computation: link
evalSMT :: Maybe Integer -> SMT a -> IO a
evalSMT mt act = fst <$> runSMT mt act

-- | Exec computation:     link
execSMT :: Maybe Integer -> SMT a -> IO SMTState
execSMT mt act = snd <$> runSMT mt act

---
--- Setup and monad getters and setters
---

emptySMTState :: SMTState
emptySMTState = SMTState { vars = M.empty
                         , solverResult = SolverFailed
                         }

data SMTResult = SolverSat { example :: (M.Map String Double) }
               | SolverUnsat
               | SolverFailed
               deriving (Eq, Ord, Show)

getVars :: SMT (M.Map String Node)
getVars = vars `liftM` get

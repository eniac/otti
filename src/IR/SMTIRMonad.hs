{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR.SMTIRMonad where
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Targets.SMT
import qualified Z3.Monad                   as Z

---
--- Monad defintions
---

data MemoryStrategy = Flat { blockSize :: Int }

-- | State for keeping track of IR-layer information
data IRState = IRState { pointerSize    :: Int
                       , memoryStrategy :: MemoryStrategy
                       }

newtype IR a = IR (StateT IRState SMT a)
    deriving (Functor, Applicative, Monad, MonadState IRState, MonadIO)

instance Z.MonadZ3 IR where
    getSolver = IR $ lift $ Z.getSolver
    getContext = IR $ lift $ Z.getContext

emptyIRState :: IRState
emptyIRState = IRState 32 $ Flat 32

liftSMT :: SMT a -> IR a
liftSMT = IR . lift

-- | Run SMT computation
runIR :: Maybe Integer -- ^ Optional timeout
      -> IR a         -- ^ Verification computation
      -> IO (a, IRState)
runIR mTimeout (IR act) = evalSMT mTimeout $ runStateT act emptyIRState

evalIR :: Maybe Integer -> IR a -> IO a
evalIR mt act = fst <$> runIR mt act

execIR :: Maybe Integer -> IR a -> IO IRState
execIR mt act = snd <$> runIR mt act

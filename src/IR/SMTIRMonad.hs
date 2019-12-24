{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IR.SMTIRMonad where
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Targets.SMT
import qualified Z3.Monad                   as Z

---
--- Monad defintions
---

data MemoryStrategy = Flat { blockSize :: Int }

-- | State for keeping track of IR-layer information
data IRState = IRState { pointerSize    :: Int
                       , memoryStrategy :: MemoryStrategy
                       , memorySort     :: Maybe Sort -- ^ Redundant, but saves having to re-calc
                       , memories       :: [Node]
                       }

newtype IR a = IR (StateT IRState SMT a)
    deriving (Functor, Applicative, Monad, MonadState IRState, MonadIO)

instance Z.MonadZ3 IR where
    getSolver = IR $ lift $ Z.getSolver
    getContext = IR $ lift $ Z.getContext

emptyIRState :: IRState
emptyIRState = IRState 32 (Flat 32) Nothing []

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

---
--- Getters and setters
---

getPointerSize :: IR Int
getPointerSize = pointerSize `liftM` get

getMemoryStrategy :: IR MemoryStrategy
getMemoryStrategy = memoryStrategy `liftM` get

setPointerSize :: Int -> IR ()
setPointerSize size = do
  s0 <- get
  put $ s0 { pointerSize = size }

setMemoryStrategy :: MemoryStrategy -> IR ()
setMemoryStrategy strategy = do
  s0 <- get
  put $ s0 { memoryStrategy = strategy }

initMem :: IR ()
initMem = do
  strat <- getMemoryStrategy
  psize <- getPointerSize
  s0 <- get
  case strat of
    Flat blockSize -> do
      dSort <- bvSort psize
      rSort <- bvSort blockSize
      memSort <- arraySort dSort rSort
      firstMem <- liftSMT $ newVar "global__mem" memSort
      put $ s0 { memorySort = Just memSort
               , memories = [firstMem]
               }

currentMem :: IR Node
currentMem = do
  mems <- memories `liftM` get
  return $ head mems

nextMem :: IR Node
nextMem = do
  s0 <- get
  let curMems = memories s0
      memSort = fromJust $ memorySort s0
  newMem <- liftSMT $ newVar ("global__mem_" ++ (show $ length curMems)) memSort
  put $ s0 { memories = newMem:curMems }
  return newMem

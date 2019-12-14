{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Targets.SMT.SMTMonad where
import           AST.Simple                 (Type (..))
import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Binary.IEEE754
import           Data.Bits
import           Data.Char                  (digitToInt)
import           Data.List                  (foldl')
import           Data.List.Split
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes)
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

runSolver :: SMT SMTResult
runSolver = do
  z3result <- Z.solverCheck
  result <- case z3result of
    Z.Sat -> do
      model <- Z.solverGetModel
      strModel <- Z.modelToString model
      parsedModel <- liftIO $ parseModel strModel
      return $ SolverSat parsedModel
    Z.Unsat -> return SolverUnsat
    _ -> return SolverFailed
  s0 <- get
  put $ s0 { solverResult = result }
  return result

-- | We have this monstrosity because Z3's get model for numbers is just broken.
-- Fully just broken.
parseModel :: String -> IO (M.Map String Double)
parseModel str = do
  let modelLines = splitOn "\n" str
  vs <- forM modelLines $ \line -> case splitOn "->" line of
            [var, strVal] -> do
              let maybeHexVal = drop 2 strVal
                  val = case maybeHexVal of
                          -- Negative 0
                          '_':' ':'-':'z':'e':'r':'o':_ -> Just (-0.0)
                          '_':' ':'+':'z':'e':'r':'o':_ -> Just (0.0)
                          '_':' ':'N':'a':'N':_         -> Just $ 0 / 0
                          '_':' ':'-':_                 -> Just $ negate $ 1 / 0
                          '_':' ':'+':_                 -> Just $ 1 / 0
                          -- Boolean
                          'b':n                         -> Just (read n :: Double)
                          -- Hex
                          'x':_                         -> Just (read ('0':maybeHexVal) :: Double)
                          'f':'p':' ':rest              ->
                            let components = splitOn " " rest
                                sign = read (drop 2 $ components !! 0) :: Integer
                                exp = toDec $ drop 2 $ components !! 1
                                sig = read ('0':(drop 1 $ init $ components !! 2)) :: Integer
                                result = (sig .&. 0xfffffffffffff) .|. ((exp .&. 0x7ff) `shiftL` 52) .|. ((sign .&. 0x1) `shiftL` 63)
                            in Just $ wordToDouble $ fromIntegral $ result
                          _                             -> Nothing
              return $ case val of
                   -- gross for printing
                   Just v  -> Just (init var, v)
                   Nothing -> Nothing
            _ -> return Nothing
  return $ M.fromList $ catMaybes vs
  where
    -- https://stackoverflow.com/questions/5921573/convert-a-string-representing-a-binary-number-to-a-base-10-string-haskell
    toDec :: String -> Integer
    toDec = foldl' (\acc x -> acc * 2 + (fromIntegral $ digitToInt x)) 0

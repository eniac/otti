{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Log
  ( LogState(..)
  , Log
  , enableStream
  , enableStreams
  , logIf
  , logIfM
  , MonadLog
  , emptyLogState
  , liftLog
  , evalLog
  )
where
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import qualified Data.Set                      as S
import           Util.Cfg                       ( cfgGetList )

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data LogState = LogState { streams         :: S.Set String
                         } deriving (Show)


newtype Log a = Log (StateT LogState IO a)
    deriving (Functor, Applicative, Monad, MonadState LogState, MonadIO)

class Monad m => MonadLog m where
  liftLog :: Log a -> m a
instance MonadLog Log where
  liftLog = id
instance (MonadLog m) => MonadLog (StateT s m) where
  liftLog = lift . liftLog

emptyLogState :: LogState
emptyLogState = LogState { streams = S.fromList [] }

enableStream :: String -> Log ()
enableStream s = modify $ \l -> l { streams = S.insert s $ streams l }

enableStreams :: Foldable f => f String -> Log ()
enableStreams ss = forM_ ss enableStream

logIf :: String -> String -> Log ()
logIf stream msg = do
  enabled <- gets (S.member stream . streams)
  when enabled $ liftIO $ putStrLn msg

logIfM :: String -> Log String -> Log ()
logIfM stream msg = do
  enabled <- gets (S.member stream . streams)
  when enabled $ msg >>= liftIO . putStrLn

evalLog :: Log a -> IO a
evalLog l = do
  strms <- cfgGetList "log"
  let Log io = enableStreams strms >> l
  evalStateT io emptyLogState

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
  , MonadIO
  , emptyLogState
  , liftLog
  , evalLog
  )
where
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import           Control.Monad.Reader
import qualified Data.Set                      as S
import           Util.Control                   ( whenM )
import           Util.Cfg                       ( Cfg
                                                , MonadCfg(..)
                                                , _streams
                                                )

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data LogState = LogState
  { streams :: S.Set String
  }
  deriving Show


newtype Log a = Log (StateT LogState Cfg a)
    deriving (Functor, Applicative, Monad, MonadState LogState, MonadIO, MonadCfg)

class Monad m => MonadLog m where
  liftLog :: Log a -> m a
instance MonadLog Log where
  liftLog = id
instance MonadLog m => MonadLog (StateT s m) where
  liftLog = lift . liftLog
instance (Monoid w, MonadLog m) => MonadLog (WriterT w m) where
  liftLog = lift . liftLog

emptyLogState :: LogState
emptyLogState = LogState { streams = S.fromList [] }

enableStream :: String -> Log ()
enableStream s = modify $ \l -> l { streams = S.insert s $ streams l }

enableStreams :: Foldable f => f String -> Log ()
enableStreams ss = forM_ ss enableStream

-- | When the specified stream is enabled, log this message
logIf :: MonadLog m => String -> String -> m ()
logIf stream msg =
  liftLog $ whenM (gets $ S.member stream . streams) $ liftIO $ putStrLn msg

-- | When the specified stream is enabled, log this (monadically computed) message
logIfM :: MonadLog m => String -> m String -> m ()
logIfM stream msg = do
  enabled <- liftLog $ gets (S.member stream . streams)
  when enabled $ msg >>= liftLog . liftIO . putStrLn

-- | Enable streams from the configuration, and run a logging computation.
evalLog :: Log a -> Cfg a
evalLog l = do
  strms <- liftCfg $ asks _streams
  let Log io = enableStreams strms >> l
  evalStateT io emptyLogState

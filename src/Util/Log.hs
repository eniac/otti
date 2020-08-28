{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Util.Log
  ( LogState(..)
  , Log
  , enableStream
  , enableStreams
  , logIf
  , MonadLog
  , emptyLogState
  , liftLog
  , evalLog
  )
where
import           Control.Lens            hiding ( element )
import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import qualified Data.Set                      as S
import Data.List.Split (splitOn)
import           System.Environment           (lookupEnv)

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data LogState = LogState { _streams         :: S.Set String
                         } deriving (Show)


$(makeLenses ''LogState)

newtype Log a = Log (StateT LogState IO a)
    deriving (Functor, Applicative, Monad, MonadState LogState, MonadIO)

class Monad m => MonadLog m where
  liftLog :: Log a -> m a
instance MonadLog Log where
  liftLog = id
instance (MonadLog m) => MonadLog (StateT s m) where
  liftLog = lift . liftLog

emptyLogState :: LogState
emptyLogState = LogState { _streams = S.fromList [] }

enableStream :: String -> Log ()
enableStream s = modify $ over streams (S.insert s)

enableStreams :: Foldable f => f String -> Log ()
enableStreams ss = forM_ ss enableStream

logIf :: String -> String -> Log ()
logIf stream msg = do
  enabled <- gets (S.member stream . _streams)
  when enabled $ liftIO $ putStrLn msg

evalLog :: Log a -> IO a
evalLog l = do
  env <- lookupEnv "CLOG"
  let init = maybe (return ()) (\s -> enableStreams $ splitOn "," s) env
  let Log io = init >> l
  evalStateT io emptyLogState

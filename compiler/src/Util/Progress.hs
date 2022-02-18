{-|
 - A state monad with a progress flag that can be set, and passes can be run to
 - fixpoint.
 -}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Util.Progress
  ( modify
  , get
  , gets
  , put
  , setProgress
  , runToFixPoint
  , Progress
  )
where

import qualified Control.Monad.State.Strict    as S
import           Data.Bifunctor
import           Util.Log                       ( MonadLog
                                                , Log
                                                )
import           Util.Cfg                       ( MonadCfg )

newtype Progress s a = Progress (S.StateT (s, Bool) Log a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, S.MonadState (s, Bool))

get :: Progress s s
get = fst <$> S.get

put :: s -> Progress s ()
put = S.modify . first . const

setProgress :: Progress s ()
setProgress = S.modify $ second (const True)

modify :: (s -> s) -> Progress s ()
modify = S.modify . first


gets :: (s -> a) -> Progress s a
gets f = S.gets (f . fst)

runToFixPoint :: Progress s a -> s -> Log s
runToFixPoint (Progress pass) = go
 where
  go s = do
    (s', progress) <- S.execStateT pass (s, False)
    if progress then go s' else return s

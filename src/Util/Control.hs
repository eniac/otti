{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
module Util.Control
  ( whenM
  , whenJust
  , whileJustM
  , whileM
  , unlessM
  , MonadDeepState(..)
  )
where

import           Control.Monad                  ( unless
                                                , when
                                                )
import Control.Monad.State.Strict
import           Data.Functor.Identity          ( Identity )

whenM :: Monad m => m Bool -> m () -> m ()
whenM condition action = condition >>= flip when action

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM condition action = condition >>= flip unless action

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust test body = maybe (return ()) body test

whileJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJustM test body = do
  t <- test
  case t of
    Just v  -> body v >> whileJustM test body
    Nothing -> return ()

whileM :: Monad m => m Bool -> m a -> m ()
whileM condition step = do
  b <- condition
  if b then step *> whileM condition step else pure ()

class (Monad m) => MonadDeepState s m | m -> s where
  deepGet :: m s
  deepPut :: s -> m ()

instance MonadDeepState () Identity where
  deepGet = pure ()
  deepPut = const $ pure ()

instance MonadDeepState () IO where
  deepGet = pure ()
  deepPut = const $ pure ()

instance MonadDeepState s m => MonadDeepState (s, s') (StateT s' m) where
  deepGet = liftM2 (,) (lift deepGet) get
  deepPut (s, s') = lift (deepPut s) >> put s'

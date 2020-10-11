module Util.Control
  ( whenM
  , whenJust
  , whileJustM
  , whileM
  , unlessM
  )
where

import           Control.Monad

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


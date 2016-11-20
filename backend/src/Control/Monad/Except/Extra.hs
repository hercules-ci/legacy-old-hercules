module Control.Monad.Except.Extra
  ( failWith
  , failWithM
  , throwLeftWith
  , throwLeftWithM
  , module Control.Monad.Except
  ) where

import Control.Monad.Except

failWith :: MonadError e m => e -> Maybe a -> m a
failWith e = maybe (throwError e) pure

failWithM :: MonadError e m => e -> m (Maybe a) -> m a
failWithM e m = failWith e =<< m

throwLeftWith :: MonadError e m => (e' -> e) -> Either e' a -> m a
throwLeftWith f = either (throwError . f) pure

throwLeftWithM :: MonadError e m => (e' -> e) -> m (Either e' a) -> m a
throwLeftWithM f m = throwLeftWith f =<< m

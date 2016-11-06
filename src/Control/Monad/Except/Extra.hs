module Control.Monad.Except.Extra
  ( failWith
  , failWithM
  , module Control.Monad.Except
  ) where

import Control.Monad.Except

failWith :: MonadError e m => e -> Maybe a -> m a
failWith e = maybe (throwError e) pure

failWithM :: MonadError e m => e -> m (Maybe a) -> m a
failWithM e m = failWith e =<< m

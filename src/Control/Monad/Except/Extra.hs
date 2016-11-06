module Control.Monad.Except.Extra
  ( failWith
  , module Control.Monad.Except
  ) where

import Control.Monad.Except

failWith :: MonadError e m => e -> m (Maybe a) -> m a
failWith e m = maybe (throwError e) pure =<< m

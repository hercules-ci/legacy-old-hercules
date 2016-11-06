{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Servant.Mandatory
  ( mandatory2
  ) where

import Control.Monad.Except
import Data.Maybe           (fromMaybe)
import Servant

-- | Throw 'err400' if either parameter is missing
mandatory2 :: MonadError ServantErr f => (a -> b -> f c) -> Maybe a -> Maybe b -> f c
mandatory2 f a b = fromMaybe (throwError err400) (f <$> a <*> b)


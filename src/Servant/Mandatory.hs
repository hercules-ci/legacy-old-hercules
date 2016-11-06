{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Servant.Mandatory
  ( mandatory1
  , mandatory2
  ) where

import Control.Monad.Except
import Data.Maybe           (fromMaybe)
import Servant

-- | Throw 'err400' if the parameter is missing
mandatory1 :: MonadError ServantErr f => (a -> f b) -> Maybe a -> f b
-- mandatory1 f x = case x of
--                    Nothing -> throwError err400
--                    Just x  -> f x
mandatory1 f a = fromMaybe (throwError err400) (f <$> a)

-- | Throw 'err400' if either parameter is missing
mandatory2 :: MonadError ServantErr f => (a -> b -> f c) -> Maybe a -> Maybe b -> f c
mandatory2 f a b = fromMaybe (throwError err400) (f <$> a <*> b)


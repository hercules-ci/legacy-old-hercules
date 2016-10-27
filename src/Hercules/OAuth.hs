{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hercules.OAuth
  ( AuthState(..)
  , AuthCode(..)
  ) where

import Data.Text
import Servant

newtype AuthState = AuthState { unAuthState :: Text }
  deriving (FromHttpApiData)

newtype AuthCode = AuthCode { unAuthCode :: Text }
  deriving (FromHttpApiData)


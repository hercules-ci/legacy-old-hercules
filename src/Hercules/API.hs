{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API
  ( API
  ) where

import Data.Text
import Servant

import Hercules.Database

type API = "projectNames" :> Get '[JSON] [Text]
      :<|> "project" :> Capture "projectName" Text :> Get '[JSON] (Maybe Project)

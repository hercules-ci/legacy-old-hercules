{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Hercules.API
  ( API
  , Unprotected
  , Protected
  , User(..)
  ) where

import Data.Aeson
import Data.Text
import GHC.Generics        (Generic)
import Servant
import Servant.Auth.Server

import Hercules.Database (Project)

data User = User { userName :: Text }
  deriving(Generic)

instance ToJSON User
instance ToJWT User
instance FromJSON User
instance FromJWT User

type Unprotected =
      "projectNames" :> Get '[JSON] [Text]
 :<|> "project" :> Capture "projectName" Text :> Get '[JSON] (Maybe Project)

type Protected = "protected" :> Get '[JSON] Text

type API = Unprotected
      :<|> Auth '[JWT] User :> Protected

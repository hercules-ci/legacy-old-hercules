{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main
  ( main
  ) where

import Control.Lens        (over, (|>))
import Data.Aeson          (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Int            (Int32)
import Data.Proxy          (Proxy (..))
import Data.Text           (Text)
import Servant             ((:>), Capture)
import Servant.Auth.Server
import Servant.Docs
import Servant.Swagger
import Data.Swagger        hiding (headers)

import qualified Hercules.API            as Hercules
import           Hercules.Database.Extra

-- TODO: Implement proper HasSwagger for servant-auth
instance (HasSwagger api) => HasSwagger (Auth '[JWT] Hercules.UserId :> api) where
  toSwagger Proxy =
    toSwagger proxy
      where
        proxy :: Proxy api
        proxy = Proxy

writeDocs :: Proxy Hercules.QueryAPI -> IO ()
writeDocs api = writeFile "api.yml" (BL8.unpack $ encode $ toSwagger api)

main :: IO ()
main = writeDocs (Proxy :: Proxy Hercules.QueryAPI)

instance ToSchema Project where
instance ToSchema Jobset where
instance ToSchema ProjectWithJobsets where

instance ToSample Project where

instance ToSample Jobset where

instance ToSample ProjectWithJobsets where

instance ToSample Text where
  toSamples _ = singleSample "some text"

instance ToSample Int32 where
  toSamples _ = singleSample 0

instance ToCapture (Capture "projectName" Text) where
  toCapture _ = DocCapture "projectName" "Name of the project"

instance (HasDocs api) => HasDocs (Auth auths usr :> api) where
  docsFor Proxy (endpoint, action) =
    docsFor (Proxy :: Proxy api) (endpoint, action')
    where
     authProxy = Proxy :: Proxy (Auth auths usr)
     action' = over headers (|> toRequiredHeader authProxy) action

class ToRequiredHeader a where
  toRequiredHeader :: Proxy a -> Text

instance ToRequiredHeader (Auth auths usr) where
  toRequiredHeader _ = "Authorization"

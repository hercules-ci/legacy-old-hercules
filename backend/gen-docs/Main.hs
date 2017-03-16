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
import Servant.Auth.Swagger
import Servant.Docs
import Servant.Swagger
import Data.Swagger        hiding (headers)

import qualified Hercules.API            as Hercules
import           Hercules.Database.Extra

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

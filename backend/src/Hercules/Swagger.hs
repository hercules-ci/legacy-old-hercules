{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hercules.Swagger
  ( swaggerDoc
  ) where

import Control.Lens            hiding ((.=))
import Data.Proxy
import Data.Swagger
import Servant.Auth.Swagger    ()
import Servant.Swagger

import Hercules.API            (QueryAPI)
import Hercules.Database.Extra

instance ToSchema Project where
instance ToSchema Jobset where
instance ToSchema ProjectWithJobsets where

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy QueryAPI)
    & info.title       .~ "Hercules CI API"
    & info.version     .~ "1.0"
    & info.description ?~ ""

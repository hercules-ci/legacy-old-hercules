{-# LANGUAGE Arrows            #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  ) where

import Control.Arrow              (returnA)
import Control.Monad.IO.Class
import Data.Text
import Database.PostgreSQL.Simple hiding (Query)
import Network.Wai
import Network.Wai.Handler.Warp
import Opaleye
import Safe                       (headMay)
import Servant

import Hercules.API
import Hercules.Config
import Hercules.Database

startApp :: Config -> IO ()
startApp Config{..} = do
  connection <- connectPostgreSQL configConnectionString
  run configPort (app connection)

app :: Connection -> Application
app connection = serve (Proxy :: Proxy API) (server connection)

server :: Connection -> Server API
server connection = getProjectNames connection :<|> getProject connection

getProjectNames :: Connection -> Handler [Text]
getProjectNames connection = liftIO $ runQuery connection nameQuery
  where
    nameQuery :: Query (Column PGText)
    nameQuery = proc () -> do
      Project{..} <- queryTable projectTable -< ()
      returnA -< projectName

getProject :: Connection -> Text -> Handler (Maybe Project)
getProject connection name = do
  projects <- liftIO $ runQuery connection projectQuery
  pure $ headMay projects

  where

    projectQuery :: Query ProjectReadColumns
    projectQuery = proc () -> do
      project@Project{..} <- queryTable projectTable -< ()
      restrict -< projectName .== pgStrictText name
      returnA -< project

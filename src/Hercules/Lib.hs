{-# LANGUAGE Arrows            #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Hercules.Lib
  ( startApp
  ) where

import Control.Arrow              (returnA)
import Control.Monad.IO.Class
import Data.Text
import Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import Network.Wai
import Network.Wai.Handler.Warp
import Opaleye
import Safe                       (headMay)
import Servant
import Servant.Auth.Server

import Hercules.API
import Hercules.Config
import Hercules.Database (Project, Project' (..), ProjectReadColumns,
                          projectTable)

startApp :: Config -> IO ()
startApp Config{..} = do
  connection <- connectPostgreSQL configConnectionString
  run configPort =<< app connection

app :: Connection -> IO Application
app connection = do
  key <- generateKey
  let api = Proxy :: Proxy API
      jwtConfig = defaultJWTSettings key
      authConfig = defaultCookieSettings :. jwtConfig :. EmptyContext
  print =<< makeJWT (User "joe") jwtConfig Nothing
  pure $ serveWithContext api authConfig (server connection)

server :: Connection -> Server API
server connection = unprotected :<|> protected
  where unprotected = getProjectNames connection
                 :<|> getProject connection
        protected = getUser

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

getUser :: AuthResult User -> Handler Text
getUser = \case
  (Authenticated (User name)) -> pure name
  _                           -> throwAll err401

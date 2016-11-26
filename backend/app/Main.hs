{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Foldable         (fold)
import qualified Data.Text             as T
import           Data.Yaml             (decodeFileEither,
                                        prettyPrintParseException)
import           Options.Applicative
import           System.Exit
import           System.IO             (hPutStrLn, stderr)

import Hercules.Config
import Hercules.Lib

main :: IO ()
main = getConfig >>= startApp

-- | Parse the command line options. If incorrect options are given exit with
-- 'exitFailure'.
getConfig :: IO Config
getConfig = execParser options >>= \case
  Left f -> decodeFileEither f >>= \case
    Left err -> do
      hPutStrLn stderr (prettyPrintParseException err)
      exitFailure
    Right c -> pure c
  Right c -> pure c

-- | Catch any 'IOException's and return Nothing, otherwise the file contents
maybeReadFile :: FilePath -> IO (Maybe BS.ByteString)
maybeReadFile f = catch (Just <$> BS.readFile f) h
  where h :: IOException -> IO (Maybe a)
        h = pure . const Nothing

-- | A parser for the hercules config or a filepath to load one from
options :: ParserInfo (Either FilePath Config)
options = info (helper <*> parser) description
  where
    parser = Left <$> configFileParser <|> Right <$> configParser
    configFileParser = strOption (fold [ long "config"
                                       , short 'c'
                                       , metavar "FILE"
                                       , help "Configuration in Haskell syntax"
                                       ])
    configParser = Config
      <$> option auto (fold [ long "port"
                            , short 'p'
                            , metavar "PORT"
                            , help "port to listen on"
                            , value 8080
                            , showDefault
                            ]
                      )
      <*> (T.pack <$> strOption (fold [ long "hostname"
                                      , short 'h'
                                      , metavar "HOST"
                                      , help "The hostname of this server"
                                      ]
                                )
          )
      <*> (T.pack <$> strOption (fold [ long "connection"
                                      , short 'o'
                                      , metavar "CONNECTION_STRING"
                                      , help "postgres connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                                      ]
                                )
          )
      <*> optional (authInfoParser "google")
      <*> optional (authInfoParser "github")

    authInfoParser name = AuthClientInfo
      <$> (BSC.pack <$> strOption (fold [ long (name ++ "-client")
                                        , metavar "CLIENT_ID"
                                        , help (name ++ "Google OAuth2 Client ID")
                                        ]
                                  )
          )
      <*> (BSC.pack <$> strOption (fold [ long (name ++ "-secret")
                                        , metavar "CLIENT_SECRET"
                                        , help (name ++ "OAuth2 Client Secret")
                                        ]
                                  )
          )

    description = fold
      [ fullDesc
      , header "hercules"
      , progDesc "A program to query a Hydra CI database"
      ]


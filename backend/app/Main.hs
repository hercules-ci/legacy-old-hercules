{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Extra as BS
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
      <*> option auto (fold [ long "access-log-level"
                            , short 'a'
                            , metavar "LOG_LEVEL"
                            , help "Level at which to log http accesses"
                            , value Disabled
                            , showDefault
                            ]
                      )
      <*> strOption (fold [ long "secret-key-file"
                          , short 'k'
                          , metavar "FILE"
                          , help "A file containing a 256 bit key for encrypting github tokens"
                          ]
                    )
      <*> (T.pack <$> strOption (fold [ long "hercules-connection"
                                      , short 'e'
                                      , metavar "CONNECTION_STRING"
                                      , help "hercules database postgres connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                                      ]
                                )
          )
      <*> (T.pack <$> strOption (fold [ long "hydra-connection"
                                      , short 'y'
                                      , metavar "CONNECTION_STRING"
                                      , help "hydra database postgres connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
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


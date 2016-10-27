{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.ByteString.Char8 (pack)
import           Data.Foldable         (fold)
import qualified Data.Text             as T
import           Options.Applicative
import           System.IO.Strict      as Strict
import           Text.Read             (readMaybe)

import Hercules.Config
import Hercules.Lib

main :: IO ()
main = do
  config <- execParser options >>= \case
    Left f -> do
      contents <- Strict.readFile f
      case readMaybe contents of
        Just config -> pure config
        -- TODO: proper erorr handling
        Nothing     -> error "error reading config"
    Right c -> pure c
  startApp config

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
      <*> (pack <$> strOption (fold [ long "connection"
                                    , short 'o'
                                    , metavar "CONNECTION_STRING"
                                    , help "postgres connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                                    ]
                              )
          )
      <*> authInfoParser "google"

    authInfoParser name = AuthInfo
      <$> (pack <$> strOption (fold [ long (name ++ "-client")
                                    , metavar "CLIENT_ID"
                                    , help (name ++ "Google OAuth2 Client ID")
                                    ]
                              )
          )
      <*> (pack <$> strOption (fold [ long (name ++ "-secret")
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


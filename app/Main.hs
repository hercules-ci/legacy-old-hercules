{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception
import           Data.ByteString.Char8 (pack)
import           Data.Foldable         (fold)
import qualified Data.Text             as T
import           Options.Applicative
import           System.Exit
import           System.IO             (hPutStrLn, stderr)
import           System.IO.Strict      as Strict
import           Text.Read             (readMaybe)

import Hercules.Config
import Hercules.Lib

main :: IO ()
main = getConfig >>= startApp

-- | Parse the command line options. If incorrect options are given exit with
-- 'exitFailure'.
getConfig :: IO Config
getConfig = execParser options >>= \case
  Left f -> do
    let readErr = do
          hPutStrLn stderr ("Unable to read config file: " ++ f)
          exitFailure

    let parseErr = do
          hPutStrLn stderr ("Unable to parse config file: " ++ f)
          exitFailure

    contents <- maybe readErr pure =<< maybeReadFile f

    maybe parseErr pure (readMaybe contents)

  Right c -> pure c

-- | Catch any 'IOException's and return Nothing, otherwise the file contents
maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile f = catch (Just <$> Strict.readFile f) h
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
      <*> (pack <$> strOption (fold [ long "connection"
                                    , short 'o'
                                    , metavar "CONNECTION_STRING"
                                    , help "postgres connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                                    ]
                              )
          )
      <*> optional (authInfoParser "google")
      <*> optional (authInfoParser "github")

    authInfoParser name = AuthClientInfo
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


module Main where

import Data.Foldable       (fold)
import Options.Applicative

import Hercules.Config
import Hercules.Lib

main :: IO ()
main = startApp =<< execParser options

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser = Config
      <$> option auto (fold [ long "port"
                            , short 'p'
                            , metavar "PORT"
                            , help "port to listen on"
                            , value 8080
                            , showDefault
                            ]
                      )
      <*> option auto (fold [ long "connection"
                            , short 'c'
                            , metavar "CONNECTION_STRING"
                            , help "postgres connection string, see https://www.postgresql.org/docs/9.5/static/libpq-connect.html#LIBPQ-CONNSTRING"
                            ]
                      )

    description = fold
      [ fullDesc
      , header "hercules"
      , progDesc "A program to query a Hydra CI database"
      ]


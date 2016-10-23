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


    description = fold
      [ fullDesc
      , header "hercules"
      , progDesc "A program to query a Hydra CI database"
      ]


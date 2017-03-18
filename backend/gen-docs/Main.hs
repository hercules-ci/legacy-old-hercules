module Main
  ( main
  ) where

import Data.Aeson                              (encode)
import qualified Data.ByteString.Lazy.Char8 as BL8

import Hercules.Swagger                        (swaggerDoc)

main :: IO ()
main = writeFile "api.yml" $ BL8.unpack $ encode swaggerDoc

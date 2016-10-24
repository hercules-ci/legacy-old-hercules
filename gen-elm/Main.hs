module Main
  ( main
  ) where

import Servant.Elm

import Hercules.API

spec :: Spec
spec = Spec ["Hercules"]
            (defElmImports : generateElmForAPI (Proxy :: Proxy API))

main :: IO ()
main = specsToDir [spec] "gen-elm"

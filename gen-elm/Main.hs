module Main
  ( main
  ) where

import Servant.Elm

import Hercules.API

-- Only write out the unprotected api for now until servant-foreign supports
-- the Auth path
spec :: Spec
spec = Spec ["Hercules"]
            (defElmImports : generateElmForAPI (Proxy :: Proxy Unprotected))

main :: IO ()
main = specsToDir [spec] "gen-elm"

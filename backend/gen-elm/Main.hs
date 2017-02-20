{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main
  ( main
  ) where

import Data.Text (Text, replace, pack)
import Data.Monoid ((<>))
import Elm
import Servant.Auth.Server
import Servant.Elm
import Servant.Foreign
import Servant.Foreign.Internal (Elem)
import Options.Applicative

import Hercules.API
import Hercules.Database.Extra


elmoptions :: Options
elmoptions = Options {fieldLabelModifier = replace "'" ""}

spec :: ElmOptions -> Spec
spec elmexportoptions = Spec ["Hercules"]
            (defElmImports
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Project)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Project)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy Jobset)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy Jobset)
              : toElmTypeSourceWith elmoptions         (Proxy :: Proxy ProjectWithJobsets)
              : toElmDecoderSourceWith elmoptions      (Proxy :: Proxy ProjectWithJobsets)
              : generateElmForAPIWith elmexportoptions (Proxy :: Proxy QueryAPI)
            )

-- Generate Authorization header for Elm protected URLs
-- https://github.com/plow-technologies/servant-auth/issues/8
instance forall lang ftype api auths a.
    ( HasForeign lang ftype api
    , HasForeignType lang ftype Text
    , JWT `Elem` auths
    )
  => HasForeign lang ftype (Auth auths a :> api) where
  type Foreign ftype (Auth auths a :> api) = Foreign ftype api

  foreignFor lang Proxy Proxy subR =
    foreignFor lang Proxy (Proxy :: Proxy api) req
    where
      req = subR{ _reqHeaders = HeaderArg arg : _reqHeaders subR }
      arg = Arg
        { _argName = PathSegment "authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
        }

data ElmConfig = ElmConfig
  { elmpath :: String
  , elmherculesurl :: String
  }

parser :: Parser ElmConfig
parser =
      ElmConfig
  <$> argument str (metavar "FOLDER")
  <*> argument str (metavar "HERCULES-URL")

main :: IO ()
main = do
  elmconfig <- execParser $ info (helper <*> parser)
    (fullDesc <> progDesc "Generate types for Elm frontend")
  let elmexportoptions = defElmOptions { elmExportOptions = elmoptions , urlPrefix = Servant.Elm.Static $ pack (elmherculesurl elmconfig) }
  specsToDir [spec elmexportoptions] $ elmpath elmconfig

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

import Data.Text
import Elm
import Servant.Auth.Server
import Servant.Elm
import Servant.Foreign
import Servant.Foreign.Internal (Elem)

import Hercules.API
import Hercules.Database.Extra

spec :: Spec
spec = Spec ["Hercules"]
            (defElmImports
              : toElmTypeSource    (Proxy :: Proxy Project)
              : toElmDecoderSource (Proxy :: Proxy Project)
              : toElmTypeSource    (Proxy :: Proxy Jobset)
              : toElmDecoderSource (Proxy :: Proxy Jobset)
              : toElmTypeSource    (Proxy :: Proxy ProjectWithJobsets)
              : toElmDecoderSource (Proxy :: Proxy ProjectWithJobsets)
              : generateElmForAPI  (Proxy :: Proxy QueryAPI)
            )

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
        { _argName = PathSegment "Authorization"
        , _argType = typeFor lang (Proxy :: Proxy ftype) (Proxy :: Proxy Text)
        }

main :: IO ()
main = specsToDir [spec] "gen-elm"

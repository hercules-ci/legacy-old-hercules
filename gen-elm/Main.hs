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

import Hercules.API

spec :: Spec
spec = Spec ["Hercules"]
            (defElmImports : generateElmForAPI (Proxy :: Proxy API))

instance forall lang ftype api a.
    ( HasForeign lang ftype api
    , HasForeignType lang ftype Text
    )
  => HasForeign lang ftype (Auth '[JWT] a :> api) where
  type Foreign ftype (Auth '[JWT] a :> api) = Foreign ftype api

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

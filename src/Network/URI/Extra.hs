module Network.URI.Extra
  ( uriToByteString
  , module Network.URI
  ) where

import Data.ByteString.Char8
import Network.URI

uriToByteString :: URI -> ByteString
uriToByteString = pack . ($ "") . uriToString id

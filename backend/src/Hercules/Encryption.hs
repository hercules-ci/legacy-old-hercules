{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| A module for encrypting and decrypting data for Hercules
-}
module Hercules.Encryption
  ( encrypt
  , decrypt
  ) where

import Control.Monad.Log
import Control.Monad.Reader
import Crypto.Cipher.AES
import Crypto.Cipher.Types
import Crypto.Random.Entropy
import Data.ByteArray        as BA
import Data.ByteString       as BS
import Data.Semigroup
import Servant

import Hercules.ServerEnv

-- | Encrypt a 'ByteString' with a new IV.
encrypt :: ByteString -> App ByteString
encrypt bs = do
  cipher <- asks envCipher
  iv <- newIV
  let ivBytes = BS.pack . BA.unpack $ iv
  pure $ ivBytes <> ctrCombine cipher iv bs

-- | Decrypt a 'ByteString' encrypted with 'encrypt'.
decrypt :: ByteString -> App ByteString
decrypt bs = do
  cipher <- asks envCipher
  let ivLen = blockSize cipher
  if BS.length bs < ivLen
    then do
      logError "Trying to decrypt too short bytestring"
      throwError err500
    else do
      let (ivBytes, message) = BS.splitAt ivLen bs
      iv <- makeIV' ivBytes
      pure $ ctrCombine cipher iv message

-- | Get a new random IV
newIV :: App (IV AES256)
newIV = do
  cipher <- asks envCipher
  makeIV' =<< liftIO (getEntropy (blockSize cipher))

-- | Convert some bytes into an IV
makeIV' :: ByteString -> App (IV AES256)
makeIV' bs = case makeIV bs of
    Nothing -> do
      logError "Unable to create IV for encryption"
      throwError err500
    Just iv -> pure iv

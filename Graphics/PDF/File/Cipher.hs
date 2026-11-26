{-# LANGUAGE RankNTypes #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The ciphers and the hashes of the standard security handler: MD5,
-- SHA-2, RC4, AES, and the hash of a password for /R 6
---------------------------------------------------------
module Graphics.PDF.File.Cipher
  ( md5
  , sha256
  , rc4
  , int32LE
  , hash2B
  , aesEncrypt
  , aesDecrypt
  , aesNoPadding
  , aesEcb
  , newGenerator
  , randomBytes
  ) where

import Control.Monad.State.Strict (State, state)
import Crypto.Random (ChaChaDRG, MonadRandom(..), drgNewSeed, randomBytesGenerate, seedFromInteger)
import qualified Crypto.Cipher.AES as AES
import qualified Crypto.Cipher.RC4 as RC4
import Crypto.Cipher.Types (BlockCipher, Cipher(..), cbcDecrypt, cbcEncrypt, ecbDecrypt, ecbEncrypt, makeIV)
import Crypto.Error (eitherCryptoError)
import qualified Crypto.Hash as H
import qualified Data.ByteArray as BA
import qualified Data.ByteString as S

md5, sha256 :: S.ByteString -> S.ByteString
md5 = BA.convert . H.hashWith H.MD5
sha256 = BA.convert . H.hashWith H.SHA256

rc4 :: S.ByteString -> S.ByteString -> S.ByteString
rc4 key = snd . RC4.combine (RC4.initialize key)

-- | A 32-bit integer in little-endian, modulo 2^32 (the /P of a file can
-- be signed or not)
int32LE :: Integer -> S.ByteString
int32LE n = S.pack [ fromInteger ((n `mod` 2 ^ (32 :: Int)) `div` 256 ^ k) | k <- [0 .. 3 :: Int] ]

-- | AES with a key of 16 bytes (AES-128) or 32 bytes (AES-256). Nothing
-- for another key.
with_aes :: S.ByteString -> (forall c. BlockCipher c => c -> Maybe a) -> Maybe a
with_aes key f = case S.length key of
  16 -> either (const Nothing) (\c -> f (c :: AES.AES128)) (eitherCryptoError (cipherInit key))
  32 -> either (const Nothing) (\c -> f (c :: AES.AES256)) (eitherCryptoError (cipherInit key))
  _ -> Nothing

-- | AES-CBC with no padding: encrypt (True) or decrypt (False). Nothing
-- for data that is not complete blocks.
aes_cbc :: Bool -> S.ByteString -> S.ByteString -> S.ByteString -> Maybe S.ByteString
aes_cbc encrypt key iv bytes
  | S.length bytes `mod` 16 /= 0 = Nothing
  | otherwise = with_aes key $ \c -> do
      i <- makeIV iv
      Just ((if encrypt then cbcEncrypt else cbcDecrypt) c i bytes)

-- | AES-CBC with an IV and a padding (PKCS #5): the IV, then the
-- encrypted bytes
aesEncrypt :: S.ByteString -> S.ByteString -> S.ByteString -> Maybe S.ByteString
aesEncrypt key iv plain =
  let n = 16 - S.length plain `mod` 16
  in (iv <>) <$> aes_cbc True key iv (plain <> S.replicate n (fromIntegral n))

-- | The inverse of 'aesEncrypt'. A string with no bytes, or of 16 bytes
-- (an IV only), is an empty string, as some writers give it.
aesDecrypt :: S.ByteString -> S.ByteString -> Maybe S.ByteString
aesDecrypt key bytes
  | S.null bytes || S.length bytes == 16 = Just S.empty
  | S.length bytes < 32 = Nothing
  | otherwise = do
      plain <- aes_cbc False key (S.take 16 bytes) (S.drop 16 bytes)
      let n = fromIntegral (S.last plain)
      if n >= 1 && n <= 16 && S.all (== S.last plain) (S.drop (S.length plain - n) plain)
        then Just (S.take (S.length plain - n) plain)
        else Nothing

-- | AES-CBC with a zero IV and no padding: encrypt (True) or decrypt
-- (False) a key of the file (/UE, /OE)
aesNoPadding :: Bool -> S.ByteString -> S.ByteString -> Maybe S.ByteString
aesNoPadding encrypt key = aes_cbc encrypt key (S.replicate 16 0)

-- | AES-ECB of one block: encrypt (True) or decrypt (False) (/Perms)
aesEcb :: Bool -> S.ByteString -> S.ByteString -> Maybe S.ByteString
aesEcb encrypt key bytes
  | S.length bytes /= 16 = Nothing
  | otherwise = with_aes key (\c -> Just ((if encrypt then ecbEncrypt else ecbDecrypt) c bytes))

-- | A ChaCha generator with a seed of 40 bytes from a 'MonadRandom'. The
-- work that uses it is then pure.
newGenerator :: MonadRandom m => m ChaChaDRG
newGenerator = do
  seed <- getRandomBytes 40
  return (drgNewSeed (seedFromInteger (S.foldl' (\a w -> a * 256 + fromIntegral w) 0 (seed :: S.ByteString))))

randomBytes :: Int -> State ChaChaDRG S.ByteString
randomBytes n = state (randomBytesGenerate n)

-- | The hash of a password for /R 6 (ISO 32000-2, 7.6.4.3.4, algorithm
-- 2.B): rounds of AES-128 and SHA-2, 64 at least and 288 at most.
hash2B :: S.ByteString -> S.ByteString -> S.ByteString -> S.ByteString
hash2B password salt extra = go 0 (sha256 (S.concat [password, salt, extra]))
  where
    go :: Int -> S.ByteString -> S.ByteString
    go i k =
      let k1 = S.concat (replicate 64 (S.concat [password, k, extra]))
          -- k has 32 bytes at least, and k1 is complete blocks: AES-128
          -- cannot fail
          e = maybe (S.singleton 0) id (aes_cbc True (S.take 16 k) (S.take 16 (S.drop 16 k)) k1)
          -- the 16 first bytes as a number, modulo 3: 256 is 1 modulo 3
          next = case S.foldl' (\a w -> a + fromIntegral w) (0 :: Int) (S.take 16 e) `mod` 3 of
            0 -> sha256 e
            1 -> BA.convert (H.hashWith H.SHA384 e)
            _ -> BA.convert (H.hashWith H.SHA512 e)
          i' = i + 1
      in if i' >= 64 && fromIntegral (S.last e) <= i' - 32 then S.take 32 next else go i' next

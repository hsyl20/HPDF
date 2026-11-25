---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- A detached CMS (PKCS #7) signature in DER
---------------------------------------------------------
module Graphics.PDF.LowLevel.CMS
  ( cmsSignature
  ) where

import qualified Crypto.Hash as H
import qualified Crypto.PubKey.ECC.ECDSA as ECDSA
import Crypto.PubKey.ECC.Types (getCurveByName)
import qualified Crypto.PubKey.RSA.PKCS15 as RSA
import Crypto.Random (MonadRandom)
import Data.Bits (shiftR, (.&.), (.|.))
import qualified Data.ByteArray as BA
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import Data.List (sort)
import Data.Word (Word8)
import Data.X509 (PrivKey(..), PrivKeyEC(..))

-- Note [CMS signature of a document]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The signature of a PDF document is a CMS SignedData object (RFC 5652)
-- without the content (a "detached" signature):
--
--   ContentInfo { signedData,
--     SignedData { version 1, { sha256 }, { data },
--       certificates,
--       { SignerInfo { version 1, issuer and serial number of the
--           certificate, sha256, signed attributes, algorithm,
--           signature } } } }
--
-- * The signed attributes are the content type, the SHA-256 digest of the
--   document and the SHA-256 digest of the certificate of the signer
--   (signing-certificate-v2, RFC 5035). PAdES needs the last one, and
--   forbids the signing time: the time is in /M of the signature
--   dictionary.
-- * The signature is made on the DER of the signed attributes as a SET OF:
--   the attributes are in the order of their bytes, and the tag is 0x31.
--   In the SignerInfo, the same bytes have the tag 0xA0.
-- * The certificates are copied as they are. The issuer and the serial
--   number are read from the bytes of the certificate ('issuer_and_serial')
--   and not encoded again, because they must be the same bytes.
--
-- The DER that this module writes has only a few types, thus it has its
-- own small writer ('tlv') and no ASN.1 library.

-- | A tag, a length and a content
tlv :: Word8 -> S.ByteString -> S.ByteString
tlv tag content = S.concat [S.singleton tag, der_length (S.length content), content]

der_length :: Int -> S.ByteString
der_length n
  | n < 0x80 = S.singleton (fromIntegral n)
  | otherwise = let bytes = base256 (fromIntegral n)
                in S.cons (0x80 .|. fromIntegral (length bytes)) (S.pack bytes)

-- | The bytes of a number that is not negative, most significant first
base256 :: Integer -> [Word8]
base256 n = go n []
  where
    go x acc | x < 256 = fromIntegral x : acc
             | otherwise = go (x `shiftR` 8) (fromIntegral (x .&. 0xFF) : acc)

der_sequence, der_set :: [S.ByteString] -> S.ByteString
der_sequence = tlv 0x30 . S.concat
der_set = tlv 0x31 . S.concat

der_octets :: S.ByteString -> S.ByteString
der_octets = tlv 0x04

der_null :: S.ByteString
der_null = tlv 0x05 S.empty

-- | An INTEGER that is not negative
der_integer :: Integer -> S.ByteString
der_integer n = tlv 0x02 . S.pack $ case base256 (max 0 n) of
  bytes@(b : _) | b >= 0x80 -> 0 : bytes
  bytes -> bytes

der_oid :: [Integer] -> S.ByteString
der_oid (a : b : rest) = tlv 0x06 (S.pack (concatMap base128 (a * 40 + b : rest)))
  where
    base128 n = go (n `shiftR` 7) [fromIntegral (n .&. 0x7F)]
    go 0 acc = acc
    go x acc = go (x `shiftR` 7) ((0x80 .|. fromIntegral (x .&. 0x7F)) : acc)
der_oid _ = S.empty

oid_data, oid_signed_data, oid_sha256, oid_content_type, oid_message_digest,
  oid_signing_certificate_v2, oid_rsa, oid_ecdsa_sha256 :: S.ByteString
oid_data = der_oid [1, 2, 840, 113549, 1, 7, 1]
oid_signed_data = der_oid [1, 2, 840, 113549, 1, 7, 2]
oid_sha256 = der_oid [2, 16, 840, 1, 101, 3, 4, 2, 1]
oid_content_type = der_oid [1, 2, 840, 113549, 1, 9, 3]
oid_message_digest = der_oid [1, 2, 840, 113549, 1, 9, 4]
oid_signing_certificate_v2 = der_oid [1, 2, 840, 113549, 1, 9, 16, 2, 47]
oid_rsa = der_oid [1, 2, 840, 113549, 1, 1, 1]
oid_ecdsa_sha256 = der_oid [1, 2, 840, 10045, 4, 3, 2]

-- | Read one element: its tag, its content, its complete bytes and the
-- bytes after it.
read_tlv :: S.ByteString -> Maybe (Word8, S.ByteString, S.ByteString, S.ByteString)
read_tlv s = do
    (tag, r) <- S.uncons s
    (l0, r') <- S.uncons r
    (len, body) <-
      if l0 < 0x80 then Just (fromIntegral l0, r')
      else do
        let n = fromIntegral (l0 .&. 0x7F)
        -- no indefinite length, and no length of more than 4 bytes
        if n == 0 || n > 4 || S.length r' < n then Nothing
          else Just (S.foldl' (\a w -> a * 256 + fromIntegral w) 0 (S.take n r'), S.drop n r')
    if len > S.length body then Nothing
      else let whole = S.take (S.length s - S.length body + len) s
           in Just (tag, S.take len body, whole, S.drop len body)

-- | The IssuerAndSerialNumber of a certificate in DER:
--
-- > Certificate { TBSCertificate { [0] version (optional), serialNumber,
-- >                                signature, issuer, ... }, ... }
--
-- See Note [CMS signature of a document]
issuer_and_serial :: S.ByteString -> Maybe S.ByteString
issuer_and_serial cert = do
    (0x30, certificate, _, _) <- read_tlv cert
    (0x30, tbs, _, _) <- read_tlv certificate
    after_version <- case read_tlv tbs of
      Just (0xA0, _, _, r) -> Just r
      _ -> Just tbs
    (0x02, _, serial, r1) <- read_tlv after_version
    (0x30, _, _, r2) <- read_tlv r1
    (0x30, _, issuer, _) <- read_tlv r2
    return (der_sequence [issuer, serial])

sha256 :: B.ByteString -> S.ByteString
sha256 bytes = BA.convert (H.hashlazy bytes :: H.Digest H.SHA256)

-- | A detached CMS signature of the content, in DER.
--
-- The first certificate is the certificate of the key. The other
-- certificates (its chain) are only copied in the signature.
--
-- See Note [CMS signature of a document]
cmsSignature :: MonadRandom m
             => PrivKey -- ^ RSA key, or EC key on a named curve
             -> [S.ByteString] -- ^ Certificates in DER
             -> B.ByteString -- ^ Content to sign
             -> m (Either String S.ByteString)
cmsSignature _ [] _ = return (Left "the signer has no certificate")
cmsSignature key certs@(cert : _) content =
    case issuer_and_serial cert of
      Nothing -> return (Left "the certificate of the signer cannot be read")
      Just sid -> do
        signed <- sign_bytes key (der_set attributes)
        return (fmap (signed_data sid) signed)
  where
    sha256_algorithm = der_sequence [oid_sha256, der_null]
    attribute oid value = der_sequence [oid, der_set [value]]
    -- in the order of their bytes: DER of a SET OF
    attributes = sort
      [ attribute oid_content_type oid_data
      , attribute oid_message_digest (der_octets (sha256 content))
      , attribute oid_signing_certificate_v2
          (der_sequence [der_sequence [der_sequence [der_octets (sha256 (B.fromStrict cert))]]])
      ]
    signed_data sid (algorithm, sig) =
      der_sequence
        [ oid_signed_data
        , tlv 0xA0 $ der_sequence
            [ der_integer 1
            , der_set [sha256_algorithm]
            , der_sequence [oid_data]
            , tlv 0xA0 (S.concat certs)
            , der_set
                [ der_sequence
                    [ der_integer 1
                    , sid
                    , sha256_algorithm
                    , tlv 0xA0 (S.concat attributes)
                    , algorithm
                    , der_octets sig
                    ]
                ]
            ]
        ]

-- | Sign bytes with SHA-256: the algorithm identifier and the signature
sign_bytes :: MonadRandom m => PrivKey -> S.ByteString -> m (Either String (S.ByteString, S.ByteString))
sign_bytes (PrivKeyRSA k) msg = do
    r <- RSA.signSaferDigest k (H.hashWith H.SHA256 msg)
    return $ case r of
      Left e -> Left ("the RSA signature failed: " ++ show e)
      Right sig -> Right (der_sequence [oid_rsa, der_null], sig)
sign_bytes (PrivKeyEC (PrivKeyEC_Named curve d)) msg = do
    sig <- ECDSA.sign (ECDSA.PrivateKey (getCurveByName curve) d) H.SHA256 msg
    return (Right ( der_sequence [oid_ecdsa_sha256]
                  , der_sequence [der_integer (ECDSA.sign_r sig), der_integer (ECDSA.sign_s sig)] ))
sign_bytes _ _ = return (Left "this type of key is not supported: use an RSA key or an EC key on a named curve")

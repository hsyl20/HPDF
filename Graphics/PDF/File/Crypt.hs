{-# LANGUAGE ScopedTypeVariables #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The standard security handler of PDF (PDF 32000-1, 7.6; ISO 32000-2,
-- 7.6.4): the keys of an encrypted file, and the decryption of its strings
-- and streams
---------------------------------------------------------
module Graphics.PDF.File.Crypt
  ( Decryptor
  , decryptorPermissions
  , openEncryption
  , decryptObject
  , aes256Decryptor
  , encryptObject
  , notEncrypted
  , withLength
  ) where

import Control.Monad (unless, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans (lift)
import Data.Bits (xor)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Graphics.PDF.File.Cipher
import Graphics.PDF.File.Object
import Graphics.PDF.File.Permissions (Permissions, permissionsFromValue)
import Graphics.PDF.File.Text (pdfDocEncoding)

-- Note [Standard security handler]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- An encrypted file has an /Encrypt dictionary in its trailer. The
-- strings and the streams of its objects are encrypted with a key of the
-- file, which a password gives:
--
--   /R   /V   method                      key of the file
--   2    1    RC4, 40 bits                MD5 of the password, /O, /P, /ID
--   3    2    RC4, 40 to 128 bits         the same, then 50 rounds of MD5
--   4    4    RC4 or AES-128 (/CF)        the same as /R 3
--   5    5    AES-256 (Adobe, deprecated) in /UE or /OE, with SHA-256
--   6    5    AES-256 (PDF 2.0)           in /UE or /OE, with 'hash2B'
--
-- For /R 2 to 4, each object has its own key: the MD5 of the key of the
-- file, of the number and of the generation of the object. For /R 5 and
-- 6, the key of the file is the key of each object.
--
-- AES data is a random IV of 16 bytes, then the encrypted bytes with a
-- padding (PKCS #5). RC4 data has the length of the plain data.
--
-- The user password opens the file; the owner password opens it too, and
-- gives the permissions to the owner. See Note [Passwords of an encrypted
-- file].

-- Note [Passwords of an encrypted file]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A password is text, and the revision of the handler gives its bytes:
--
-- * /R 2 to 4: PDFDocEncoding, padded or truncated to 32 bytes
--   ('pdfDocEncoding' in Graphics.PDF.File.Text). PDFDocEncoding is ISO
--   Latin 1, except for some codes: for example "’" is 0x90 and "€" is
--   0xA0. A character that PDFDocEncoding does not have cannot be a
--   character of such a password.
-- * /R 5 and 6: UTF-8 after SASLprep (RFC 4013), truncated to 127 bytes.
--   The reader does not apply SASLprep: SASLprep does not change a
--   password in ASCII.
--
-- The reader tries the password as the owner password, then as the user
-- password (ISO 32000-2, 7.6.4.4.1): a password that is both gives the
-- rights of the owner.

-- Note [Permissions of a decrypted file]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The permissions (/P) are for the user password: the owner password
-- gives all the rights. The reader decrypts a file with either password,
-- and records the permissions in 'pdfPermissions' when the user password
-- opened it. The reader does not apply them, and a file that is written
-- again ('renderPdfFile') is not encrypted: the caller applies them.
-- 'Graphics.PDF.File.encryptPdfFile' refuses a file with permissions
-- that are not all the permissions, unless the caller says to ignore
-- them: an encryption with a new owner password would remove them. An
-- incremental update needs the permission to modify the file (Note
-- [Restrictions of an update] in Graphics.PDF.File.Update).

-- Note [Objects that are not encrypted]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- These are not encrypted (ISO 32000-2, 7.6.2):
--
-- * the trailer, thus the /ID; the /Encrypt dictionary;
-- * the cross-reference streams;
-- * the strings of an object in an object stream: the object stream is
--   encrypted;
-- * the /Contents of a signature dictionary ('isSignatureDict'): a
--   signature is made on the bytes of the file, and it is written after
--   the encryption;
-- * the metadata streams (/Type /Metadata) when /EncryptMetadata is false
--   (/V 4 and 5 only: the other versions do not have this key).

-- | How the strings or the streams of a file are encrypted
data Method = Identity | Rc4 | AesV2 | AesV3
  deriving (Eq, Show)

-- | The key and the methods of an encrypted file. They decrypt its
-- objects, and encrypt the objects of an update ('encryptObject').
data Decryptor = Decryptor
  { dec_key :: !S.ByteString -- ^ The key of the file
  , dec_strings :: !Method
  , dec_streams :: !Method
  , dec_metadata :: !Bool -- ^ /EncryptMetadata
  , dec_permissions :: !(Maybe Permissions) -- ^ Nothing: the owner password opened the file
  }

-- | The permissions of a file that the user password opened: Nothing for
-- the owner password. See Note [Permissions of a decrypted file]
decryptorPermissions :: Decryptor -> Maybe Permissions
decryptorPermissions = dec_permissions

-- | The values of an /Encrypt dictionary that give the key of the file
data Handler = Handler
  { h_revision :: !Integer -- ^ /R
  , h_bits :: !Integer -- ^ The length of the key, in bits
  , h_permissions :: !Integer -- ^ /P
  , h_metadata :: !Bool -- ^ /EncryptMetadata
  , h_owner :: !S.ByteString -- ^ /O
  , h_user :: !S.ByteString -- ^ /U
  }

-- | The key of the file, from the /Encrypt dictionary, the first string
-- of the /ID of the trailer, and a password.
-- See Note [Standard security handler] and Note [Passwords of an encrypted file]
openEncryption :: Dict -> S.ByteString -> T.Text -> Either String Decryptor
openEncryption d file_id password = do
  unless (dictLookup (C.pack "Filter") d == Just (Name (C.pack "Standard"))) $
    Left "the file is encrypted with a security handler that is not /Standard"
  v <- integer_key d "V"
  r <- integer_key d "R"
  p <- integer_key d "P"
  (strings, streams, bits) <- methods d v
  when (M.member (C.pack "EFF") d && dictLookup (C.pack "EFF") d /= dictLookup (C.pack "StmF") d) $
    Left "the embedded files of the file have their own encryption (/EFF)"
  unless (bits `mod` 8 == 0 && bits >= 40 && bits <= 256) $
    Left "the /Length of the encryption is not correct"
  o <- string_key d "O"
  u <- string_key d "U"
  -- /EncryptMetadata is a key of /V 4 and 5 only
  let encrypt_metadata = v < 4 || dictLookup (C.pack "EncryptMetadata") d /= Just (Bool False)
      handler = Handler r bits p encrypt_metadata o u
  (key, owner) <- if r >= 2 && r <= 4 && v /= 5 then md5_key handler file_id password
                  else if (r == 5 || r == 6) && v == 5 then sha_key d handler password
                  else Left ("the /R " ++ show r ++ " of the encryption is not known")
  -- See Note [Permissions of a decrypted file]
  let permissions = if owner then Nothing else Just (permissionsFromValue p)
  Right (Decryptor key strings streams encrypt_metadata permissions)

integer_key :: Dict -> String -> Either String Integer
integer_key d k = maybe (Left ("the encryption has no /" ++ k)) Right (optional_integer d k)

optional_integer :: Dict -> String -> Maybe Integer
optional_integer d k = case dictLookup (C.pack k) d of
  Just (Integer n) | abs n < 2 ^ (32 :: Int) -> Just n
  _ -> Nothing

string_key :: Dict -> String -> Either String S.ByteString
string_key d k = case dictLookup (C.pack k) d of
  Just (String s) -> Right s
  _ -> Left ("the encryption has no /" ++ k)

-- | The methods of the strings and of the streams, and the length of the
-- key in bits, from /V. See Note [Standard security handler]
methods :: Dict -> Integer -> Either String (Method, Method, Integer)
methods d v = case v of
  1 -> Right (Rc4, Rc4, 40)
  2 -> Right (Rc4, Rc4, maybe 40 id (optional_integer d "Length"))
  4 -> (,,) <$> crypt_filter "StrF" <*> crypt_filter "StmF" <*> Right v4_bits
  5 -> (,,) <$> crypt_filter "StrF" <*> crypt_filter "StmF" <*> Right 256
  _ -> Left ("the /V " ++ show v ++ " of the encryption is not known")
  where
    -- the /Length of /V 4 can be in the crypt filter only: in bytes (in
    -- bits for some writers, as pdf.js reads it)
    v4_bits = case (optional_integer d "Length", filter_length) of
      (Just n, _) -> n
      (Nothing, Just n) | n < 40 -> 8 * n
                        | otherwise -> n
      (Nothing, Nothing) -> 128
    filter_length = do
      Name n <- dictLookup (C.pack "StmF") d
      Dict cf <- dictLookup (C.pack "CF") d
      Dict f <- dictLookup n cf
      Integer l <- dictLookup (C.pack "Length") f
      if l >= 0 && l < 2 ^ (32 :: Int) then Just l else Nothing
    crypt_filter k = case dictLookup (C.pack k) d of
      Nothing -> Right Identity
      Just (Name n) | n == C.pack "Identity" -> Right Identity
      Just (Name n) -> case dictLookup (C.pack "CF") d of
        Just (Dict cf) | Just (Dict f) <- dictLookup n cf -> case dictLookup (C.pack "CFM") f of
          Just (Name m) | m == C.pack "V2" -> Right Rc4
                        | m == C.pack "AESV2" -> Right AesV2
                        | m == C.pack "AESV3" -> Right AesV3
                        | m == C.pack "None" -> Right Identity
          _ -> Left "the crypt filter of the encryption is not known"
        _ -> Left "the crypt filter of the encryption is not in /CF"
      Just _ -> Left ("the /" ++ k ++ " of the encryption is not a name")

wrong_password :: String
wrong_password = "the file is encrypted, and the password is not correct"

-- | The first value that is there
first_of :: [Maybe a] -> Either String a
first_of xs = case [ x | Just x <- xs ] of
  (x : _) -> Right x
  [] -> Left wrong_password

-- | The 32 bytes that pad a password (PDF 32000-1, 7.6.3.3)
password_padding :: S.ByteString
password_padding = S.pack
  [ 0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41, 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08
  , 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80, 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A ]

pad_password :: S.ByteString -> S.ByteString
pad_password pw = S.take 32 (pw <> password_padding)

-- | The key of the file for /R 2 to 4, from the owner password or the
-- user password (PDF 32000-1, 7.6.3.3 and 7.6.3.4, algorithms 2, 6 and 7),
-- and True for the owner password
md5_key :: Handler -> S.ByteString -> T.Text -> Either String (S.ByteString, Bool)
md5_key h file_id password = do
  let r = h_revision h
      o = S.take 32 (h_owner h)
  unless (S.length (h_owner h) >= 32 && S.length (h_user h) >= 32 && h_bits h <= 128) $
    Left "the /O or the /U of the encryption is not correct"
  -- See Note [Passwords of an encrypted file]
  bytes <- maybe (Left wrong_password) Right (pdfDocEncoding password)
  let n = if r == 2 then 5 else fromInteger (h_bits h) `div` 8
      file_key pw =
        let first = md5 (S.concat [ pad_password pw, o, int32LE (h_permissions h), file_id
                                  , if r >= 4 && not (h_metadata h) then S.replicate 4 0xFF else S.empty ])
        in S.take n (iterate (md5 . S.take n) first !! (if r >= 3 then 50 else 0))
      -- algorithm 6: the key gives /U
      as_user pw = let k = file_key pw in if user_check k then Just k else Nothing
      user_check k
        | r == 2 = rc4 k password_padding == S.take 32 (h_user h)
        | otherwise = rc4_rounds k [0 .. 19] (md5 (password_padding <> file_id)) == S.take 16 (h_user h)
      -- algorithm 7: the owner password decrypts /O, which is the user password
      owner_to_user owner =
        let h0 = md5 (pad_password owner)
            k = S.take n (if r >= 3 then iterate md5 h0 !! 50 else h0)
        in if r == 2 then rc4 k o else rc4_rounds k [19, 18 .. 0] o
  first_of [ (\k -> (k, True)) <$> as_user (owner_to_user bytes), (\k -> (k, False)) <$> as_user bytes ]

-- | RC4 with the key XOR each number of a list, one after the other
rc4_rounds :: S.ByteString -> [Word8] -> S.ByteString -> S.ByteString
rc4_rounds key is x0 = foldl (\x i -> rc4 (S.map (xor i) key) x) x0 is

-- | The key of the file for /R 5 and 6, from the owner password or the
-- user password (ISO 32000-2, 7.6.4.3.3, algorithm 2.A), and True for the
-- owner password. For /R 6, /Perms must agree with /P (algorithm 13).
sha_key :: Dict -> Handler -> T.Text -> Either String (S.ByteString, Bool)
sha_key d h password = do
  oe <- string_key d "OE"
  ue <- string_key d "UE"
  perms <- string_key d "Perms"
  let r = h_revision h
      o = h_owner h
      u = h_user h
  unless (S.length o >= 48 && S.length u >= 48 && S.length oe == 32 && S.length ue == 32 && S.length perms == 16) $
    Left "the /O, /U, /OE, /UE or /Perms of the encryption is not correct"
  -- See Note [Passwords of an encrypted file]
  let pw = S.take 127 (TE.encodeUtf8 password)
      hash salt extra = if r == 5 then sha256 (S.concat [pw, salt, extra]) else hash2B pw salt extra
      slice from k = S.take k . S.drop from
      u48 = S.take 48 u
      -- the hash of the password with the validation salt, then the key
      -- salt gives the key that decrypts /UE or /OE
      open validation key_salt extra hashed encrypted_key =
        if hash validation extra == hashed then aesNoPadding False (hash key_salt extra) encrypted_key else Nothing
  (key, owner) <- first_of [ (\k -> (k, True)) <$> open (slice 32 8 o) (slice 40 8 o) u48 (S.take 32 o) oe
                           , (\k -> (k, False)) <$> open (slice 32 8 u) (slice 40 8 u) S.empty (S.take 32 u) ue ]
  let plain = aesEcb False key perms
  when (r == 6 && (fmap (S.take 4) plain /= Just (int32LE (h_permissions h)) || fmap (slice 9 3) plain /= Just (C.pack "adb"))) $
    Left "the /Perms of the encryption does not agree with its /P"
  Right (key, owner)

-- | The key of an object (algorithm 1)
object_key :: Decryptor -> Method -> Int -> Int -> S.ByteString
object_key dec method n g
  | method == AesV3 = dec_key dec
  | otherwise =
      let k = dec_key dec
          bytes = S.pack [ fromIntegral (n `div` 256 ^ i) | i <- [0 .. 2 :: Int] ]
               <> S.pack [ fromIntegral (g `div` 256 ^ i) | i <- [0 .. 1 :: Int] ]
               <> (if method == AesV2 then C.pack "sAlT" else S.empty)
      in S.take (min 16 (S.length k + 5)) (md5 (k <> bytes))

decrypt_bytes :: Decryptor -> Method -> Int -> Int -> S.ByteString -> Either String S.ByteString
decrypt_bytes dec method n g bytes = case method of
  Identity -> Right bytes
  Rc4 -> Right (rc4 (object_key dec Rc4 n g) bytes)
  _ -> maybe (Left ("the encrypted data of the object " ++ show n ++ " is not correct")) Right
             (aesDecrypt (object_key dec method n g) bytes)

-- | The methods of /V 5 with a key of the file of 32 bytes: AES-256 for
-- the strings, the streams and the metadata
aes256Decryptor :: S.ByteString -> Decryptor
aes256Decryptor key = Decryptor key AesV3 AesV3 True Nothing

-- | Encrypt the strings and the stream data of an object with its number
-- and its generation: the inverse of 'decryptObject'. The action gives
-- the random IV (16 bytes) of each AES string or stream.
-- See Note [Objects that are not encrypted]
encryptObject :: forall m. Monad m => m S.ByteString -> Decryptor -> Int -> Int -> Object -> m (Either String Object)
encryptObject random_iv dec n g o = runExceptT $ case o of
  Plain v -> Plain <$> value v
  Stream d _ | is_type "XRef" d -> return o
  Stream d bytes -> do
    d' <- traverse value d
    if is_type "Metadata" d && not (dec_metadata dec)
      then return (Stream d' bytes)
      else withLength d' <$> encrypt (dec_streams dec) bytes
  where
    is_type t d = dictLookup (C.pack "Type") d == Just (Name (C.pack t))
    encrypt :: Method -> S.ByteString -> ExceptT String m S.ByteString
    encrypt method bytes = case method of
      Identity -> return bytes
      Rc4 -> return (rc4 (object_key dec Rc4 n g) bytes)
      _ -> do
        iv <- lift random_iv
        -- AES-128 needs a key of 16 bytes: a short /Length gives another
        maybe (throwError ("the key of the object " ++ show n ++ " is not a key of AES")) return
              (aesEncrypt (object_key dec method n g) iv bytes)
    value :: Value -> ExceptT String m Value
    value v = case v of
      String s -> String <$> encrypt (dec_strings dec) s
      Array vs -> Array <$> traverse value vs
      Dict d -> Dict <$> M.traverseWithKey (\k x -> if notEncrypted d k then return x else value x) d
      _ -> return v

-- | A stream whose /Length is the length of its data
withLength :: Dict -> S.ByteString -> Object
withLength d bytes = Stream (M.insert (C.pack "Length") (Integer (fromIntegral (S.length bytes))) d) bytes

-- | Is a value one that is not encrypted? The /Contents of a signature
-- dictionary. See Note [Objects that are not encrypted]
notEncrypted :: Dict -> S.ByteString -> Bool
notEncrypted d k = k == C.pack "Contents" && isSignatureDict d

-- | Decrypt the strings and the stream data of an object with its number
-- and its generation.
-- See Note [Objects that are not encrypted]
decryptObject :: Decryptor -> Int -> Int -> Object -> Either String Object
decryptObject dec n g o = case o of
  Plain v -> Plain <$> value v
  -- a cross-reference stream has the trailer in its dictionary
  Stream d _ | is_type "XRef" d -> Right o
  Stream d bytes -> do
    d' <- traverse value d
    let filters = case dictLookup (C.pack "Filter") d of
          Just (Name f) -> [f]
          Just (Array fs) -> [ f | Name f <- fs ]
          _ -> []
    when (C.pack "Crypt" `elem` filters) $
      Left ("the stream of the object " ++ show n ++ " has a /Crypt filter")
    if is_type "Metadata" d && not (dec_metadata dec)
      then Right (Stream d' bytes)
      else withLength d' <$> decrypt_bytes dec (dec_streams dec) n g bytes
  where
    is_type t d = dictLookup (C.pack "Type") d == Just (Name (C.pack t))
    value v = case v of
      String s -> String <$> decrypt_bytes dec (dec_strings dec) n g s
      Array vs -> Array <$> traverse value vs
      Dict d -> Dict <$> M.traverseWithKey (\k x -> if notEncrypted d k then Right x else value x) d
      _ -> Right v

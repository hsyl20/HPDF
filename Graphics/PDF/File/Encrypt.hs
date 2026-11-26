---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Encryption of a PDF file with the standard security handler: AES-256,
-- /R 6 (ISO 32000-2, 7.6.4)
---------------------------------------------------------
module Graphics.PDF.File.Encrypt
  ( Encryption(..)
  , encryption
  , encryptPdfFile
  , renderEncryptedPdfFile
  ) where

import Control.Monad (unless, when)
import Control.Monad.State.Strict (State, evalState)
import Crypto.Random (ChaChaDRG, MonadRandom)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Graphics.PDF.File.Cipher (aesEcb, aesNoPadding, hash2B, int32LE, newGenerator, randomBytes)
import Graphics.PDF.File.Crypt (aes256Decryptor, encryptObject)
import Graphics.PDF.File.Filter (decodeStream)
import Graphics.PDF.File.Object
import Graphics.PDF.File.Permissions
import Graphics.PDF.File.Write (Packed(..), RenderOptions(..), infoObject, objectStreamsPossible, packObjects, renderPacked, renderPdfFileWith)

-- | The encryption of a document
data Encryption = Encryption
  { encUserPassword :: !T.Text -- ^ The password that opens the document. Empty: all can open the document, with the permissions
  , encOwnerPassword :: !T.Text -- ^ The password that gives all the permissions. It cannot be empty, or the user password
  , encPermissions :: !Permissions
  , encIgnoreSourcePermissions :: !Bool
    -- ^ Encrypt a file whose permissions are restricted ('pdfPermissions':
    -- the reader opened it with the user password). The new owner password
    -- then removes the restrictions. False by default: such a file gives
    -- 'Left'. See Note [Permissions of a decrypted file] in
    -- Graphics.PDF.File.Crypt
  } deriving (Eq, Show)

-- | An encryption with a user password and an owner password, all the
-- permissions, and the permissions of the source file respected
encryption :: T.Text -- ^ User password
           -> T.Text -- ^ Owner password
           -> Encryption
encryption user owner = Encryption user owner allPermissions False

-- Note [Encryption of a file]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'encryptPdfFile' gives the file the /Encrypt of the standard security
-- handler with /V 5 /R 6 (AES-256, PDF 2.0), and encrypts each string and
-- each stream of its objects with AES-256-CBC, a new IV and a padding.
-- See Note [Standard security handler] in Graphics.PDF.File.Crypt.
--
-- The key of the file is random. /U and /O have the hash of a password
-- ('hash2B') with random salts; /UE and /OE have the key, encrypted with
-- the hash of a password; /Perms has /P, encrypted with the key.
--
-- The file also changes so that the readers open it:
--
-- * a trailer /Info that is a dictionary becomes an object: the strings
--   of the trailer are not encrypted, and the /Info must be a reference;
-- * a file with no /ID gets a random one;
-- * the version is 1.7 at least, with the extension level 8 of Adobe
--   (/Extensions of the catalog), as qpdf writes it: /R 6 is a part of
--   PDF 2.0 and of this extension.
--
-- The random bytes come from a ChaCha generator, with a seed from the
-- 'MonadRandom': the encryption of the objects is then pure.
--
-- 'renderEncryptedPdfFile' puts the objects in object streams before the
-- encryption: then each object stream is encrypted, and the objects in it
-- are not. 'encryptPdfFile' gives a 'PdfFile' whose objects are already
-- encrypted: the writer cannot put them in object streams. See Note
-- [Object streams of the writer] in Graphics.PDF.File.Write.

-- | Encrypt a file. See Note [Encryption of a file].
--
-- 'Left' for:
--
-- * an empty owner password, or an owner password that is the user
--   password (a reader then gives the rights of the owner to all);
-- * a password that is not printable ASCII (the reader would have to
--   apply SASLprep to it);
-- * a PDF/A file (PDF/A forbids encryption): an output intent of PDF/A,
--   or the @pdfaid:part@ of the XMP metadata;
-- * a file with an /Encrypt (a 'PdfFile' that the reader gives has none);
-- * a file with restricted permissions ('pdfPermissions'), unless
--   'encIgnoreSourcePermissions';
-- * a signed file: the encryption would break the signature.
--
-- A signature field that is not signed stays one: the file can be signed
-- after the encryption (see Note [Objects that are not encrypted] in
-- Graphics.PDF.File.Crypt). The random bytes must be random: a
-- 'MonadRandom' with a fixed seed gives the same key to all the files.
--
-- The writer cannot put the objects of the result in object streams:
-- use 'renderEncryptedPdfFile' for that.
encryptPdfFile :: MonadRandom m => Encryption -> PdfFile -> m (Either String PdfFile)
encryptPdfFile enc file = fmap packedFile <$> encrypt_with (\f -> Packed f IM.empty) enc file

-- | Encrypt a file and give its bytes, with the options of the writer.
-- With 'renderObjectStreams', the objects are in object streams, and the
-- object streams are encrypted (not for a file whose object numbers are
-- too large: see Note [Object streams of the writer] in
-- Graphics.PDF.File.Write). 'Left' for the cases of 'encryptPdfFile'.
-- See Note [Encryption of a file]
renderEncryptedPdfFile :: MonadRandom m => RenderOptions -> Encryption -> PdfFile -> m (Either String L.ByteString)
renderEncryptedPdfFile options enc file
  | renderObjectStreams options && objectStreamsPossible file = fmap renderPacked <$> encrypt_with packObjects enc file
  | otherwise = fmap (renderPdfFileWith options . packedFile) <$> encrypt_with (\f -> Packed f IM.empty) enc file

-- | Check a file, put its objects in object streams (or not) with the
-- first argument, then encrypt it
encrypt_with :: MonadRandom m => (PdfFile -> Packed) -> Encryption -> PdfFile -> m (Either String Packed)
encrypt_with pack enc file = case check of
  Left e -> return (Left e)
  Right () -> evalState (encrypt_file pack enc file) <$> newGenerator
  where
    check = do
      when (T.null (encOwnerPassword enc)) $ Left "the owner password cannot be empty"
      when (encOwnerPassword enc == encUserPassword enc) $
        Left "the owner password cannot be the user password: the permissions would do nothing"
      unless (all (T.all (\c -> c >= ' ' && c <= '~')) [encUserPassword enc, encOwnerPassword enc]) $
        Left "a password must be printable ASCII"
      when (M.member (C.pack "Encrypt") (pdfTrailer file)) $ Left "the file is already encrypted"
      case pdfPermissions file of
        Just p | p /= allPermissions, not (encIgnoreSourcePermissions enc) ->
          Left "the permissions of the file are restricted (the user password opened it): use the owner password, or encIgnoreSourcePermissions"
        _ -> Right ()
      when (is_pdfa file) $ Left "a PDF/A file cannot be encrypted"
      when (or [ is_signed d | (_, Plain (Dict d)) <- IM.elems (pdfObjects file) ]) $
        Left "the file is signed: the encryption would break the signature"
    is_signed d = case M.lookup (C.pack "Contents") d of
      Just (String s) -> isSignatureDict d && S.any (/= 0) s
      _ -> False

-- | Is a file PDF/A? An output intent of PDF/A in the catalog, or the
-- PDF/A part in the XMP metadata of the catalog (ISO 19005)
is_pdfa :: PdfFile -> Bool
is_pdfa file = case resolveValue file <$> dictLookup (C.pack "Root") (pdfTrailer file) of
  Just (Dict catalog) -> output_intent catalog || xmp catalog
  _ -> False
  where
    output_intent catalog = case resolveValue file <$> dictLookup (C.pack "OutputIntents") catalog of
      Just (Array intents) -> or [ (resolveValue file <$> dictLookup (C.pack "S") i) == Just (Name (C.pack "GTS_PDFA1"))
                                 | Dict i <- map (resolveValue file) intents ]
      _ -> False
    xmp catalog = case dictLookup (C.pack "Metadata") catalog of
      Just (Ref n _) | Just (Stream d raw) <- getObject file n ->
        -- the limit of a stream of the reader
        either (const False) (C.pack "pdfaid:part" `S.isInfixOf`) (decodeStream (256 * 1024 * 1024) d raw)
      _ -> False

-- See Note [Encryption of a file]
encrypt_file :: (PdfFile -> Packed) -> Encryption -> PdfFile -> State ChaChaDRG (Either String Packed)
encrypt_file pack enc file = do
  key <- randomBytes 32
  user_validation <- randomBytes 8
  user_key_salt <- randomBytes 8
  owner_validation <- randomBytes 8
  owner_key_salt <- randomBytes 8
  perms_random <- randomBytes 4
  new_id <- randomBytes 16
  let user = S.take 127 (TE.encodeUtf8 (encUserPassword enc))
      owner = S.take 127 (TE.encodeUtf8 (encOwnerPassword enc))
      p = permissionsValue (encPermissions enc)
      u = S.concat [hash2B user user_validation S.empty, user_validation, user_key_salt]
      o = S.concat [hash2B owner owner_validation u, owner_validation, owner_key_salt]
      -- the keys have 32 bytes: AES cannot fail
      ue = maybe S.empty id (aesNoPadding True (hash2B user user_key_salt S.empty) key)
      oe = maybe S.empty id (aesNoPadding True (hash2B owner owner_key_salt u) key)
      perms = maybe S.empty id (aesEcb True key (S.concat [int32LE p, S.replicate 4 0xFF, C.pack "Tadb", perms_random]))
      std_cf = Dict (M.fromList [ (C.pack "AuthEvent", Name (C.pack "DocOpen")), (C.pack "CFM", Name (C.pack "AESV3"))
                                , (C.pack "Length", Integer 32) ])
      encrypt_dict = M.fromList
        [ (C.pack "Filter", Name (C.pack "Standard")), (C.pack "V", Integer 5), (C.pack "R", Integer 6)
        , (C.pack "Length", Integer 256), (C.pack "CF", Dict (M.singleton (C.pack "StdCF") std_cf))
        , (C.pack "StmF", Name (C.pack "StdCF")), (C.pack "StrF", Name (C.pack "StdCF"))
        , (C.pack "O", String o), (C.pack "U", String u), (C.pack "OE", String oe), (C.pack "UE", String ue)
        , (C.pack "P", Integer p), (C.pack "Perms", String perms), (C.pack "EncryptMetadata", Bool True) ]
      Packed prepared in_streams = pack (with_extension (infoObject file))
      trailer = pdfTrailer prepared
      with_id = if M.member (C.pack "ID") trailer then trailer
                else M.insert (C.pack "ID") (Array [String new_id, String new_id]) trailer
  objects <- IM.traverseWithKey (\n (g, obj) -> fmap (\x -> (g, x)) <$> encryptObject (randomBytes 16) (aes256Decryptor key) n g obj)
                                (pdfObjects prepared)
  return $ (\os -> Packed prepared
    { pdfVersion = max (1, 7) (pdfVersion prepared)
    , pdfTrailer = M.insert (C.pack "Encrypt") (Dict encrypt_dict) with_id
    , pdfObjects = os
    , pdfPermissions = Nothing
    } in_streams) <$> sequence objects

-- | The extension level 8 of Adobe in the catalog of a file before PDF 2.0.
-- An extension of Adobe of a level of 8 or more stays. In PDF 2.0, the
-- /ADBE of /Extensions can be an array of extensions.
-- See Note [Encryption of a file]
with_extension :: PdfFile -> PdfFile
with_extension file
  | pdfVersion file >= (2, 0) = file
  | otherwise = case dictLookup (C.pack "Root") (pdfTrailer file) of
      Just (Ref n g) | Just (g', Plain (Dict catalog)) <- IM.lookup n (pdfObjects file), g == g' ->
        let adbe = Dict (M.fromList [(C.pack "BaseVersion", Name (C.pack "1.7")), (C.pack "ExtensionLevel", Integer 8)])
            extensions = case resolveValue file <$> dictLookup (C.pack "Extensions") catalog of
              Just (Dict e) -> e
              _ -> M.empty
            level_8 v = case resolveValue file v of
              Dict e | Just (Integer l) <- dictLookup (C.pack "ExtensionLevel") e -> l >= 8
              _ -> False
            adbe' = case resolveValue file <$> dictLookup (C.pack "ADBE") extensions of
              Just (Array es) | any level_8 es -> Array es
                              | otherwise -> Array (es ++ [adbe])
              Just e | level_8 e -> e
              _ -> adbe
            catalog' = M.insert (C.pack "Extensions") (Dict (M.insert (C.pack "ADBE") adbe' extensions)) catalog
        in file { pdfObjects = IM.insert n (g, Plain (Dict catalog')) (pdfObjects file) }
      _ -> file

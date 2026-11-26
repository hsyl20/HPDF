---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Encryption of a document with a password: AES-256 (the standard
-- security handler of PDF 2.0, /R 6), with the permissions of the
-- document.
--
-- The encryption is a step after the rendering, because it needs random
-- bytes:
--
-- > Right doc <- return (renderPdfE standardDocInfo rect pages)
-- > Right encrypted <- encryptPdf (encryption (pack "user") (pack "owner")) doc
-- > BL.writeFile "encrypted.pdf" encrypted
--
-- A document with a signature field ('Graphics.PDF.Signature.addSignature')
-- is encrypted, then signed:
--
-- > Right encrypted <- encryptPdf enc doc
-- > Right signed <- signPdfWith signer encrypted
--
-- A PDF/A document cannot be encrypted. 'Graphics.PDF.File.parsePdfFileWithPassword'
-- reads an encrypted file.
---------------------------------------------------------
module Graphics.PDF.Encryption
  ( Encryption(..)
  , encryption
  , Permissions(..)
  , allPermissions
  , noPermissions
  , encryptPdf
  , encryptPdfWith
  , RenderOptions(..)
  , defaultRenderOptions
  ) where

import Crypto.Random (MonadRandom)
import qualified Data.ByteString.Lazy as L
import Graphics.PDF.File.Encrypt
import Graphics.PDF.File.Parse (parsePdfFile)
import Graphics.PDF.File.Permissions
import Graphics.PDF.File.Write (RenderOptions(..), defaultRenderOptions)

-- | Encrypt a rendered document, with 'defaultRenderOptions': the
-- objects that are not streams are in object streams. The objects of the
-- document keep their numbers, and the /Info of the trailer becomes a new
-- object.
--
-- The input can be another PDF file. An encrypted file is read if its
-- user password is empty, and it is then encrypted with the new
-- passwords, only if it has all the permissions or
-- 'encIgnoreSourcePermissions' is True.
--
-- 'Left' for a file that cannot be read, and for the cases of
-- 'Graphics.PDF.File.encryptPdfFile': an empty owner password, an owner
-- password that is the user password, a password that is not printable
-- ASCII, a PDF/A document, restricted permissions, a signed document.
encryptPdf :: MonadRandom m => Encryption -> L.ByteString -> m (Either String L.ByteString)
encryptPdf = encryptPdfWith defaultRenderOptions

-- | Encrypt a rendered document, with the options of the writer (see
-- 'Graphics.PDF.File.renderEncryptedPdfFile')
encryptPdfWith :: MonadRandom m => RenderOptions -> Encryption -> L.ByteString -> m (Either String L.ByteString)
encryptPdfWith options enc doc = case parsePdfFile (L.toStrict doc) of
  Left e -> return (Left e)
  Right file -> renderEncryptedPdfFile options enc file

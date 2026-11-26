---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Read a PDF file, and write it again.
--
-- > Right file <- readPdfFile "in.pdf"
-- > writePdfFile "out.pdf" file
--
-- The reader knows the cross-reference tables (with the updates of a
-- file, /Prev), the cross-reference streams and the object streams of
-- PDF 1.5, and the hybrid files. It decrypts a file that the standard
-- security handler encrypts (RC4, AES-128, AES-256), with a password
-- ('parsePdfFileWithPassword'), and records the permissions of the file
-- ('pdfPermissions'): it does not apply them, and the writer writes a
-- file that is not encrypted. It does not repair a file whose
-- cross-reference is not correct. The writer gives
-- a file with one cross-reference table. A file is the objects of the
-- document, as values ('PdfFile'): the structure of the file (object
-- streams, cross-reference streams, linearization) is not kept.
--
-- The constructors of 'Value' have short names ('String', 'Integer'),
-- thus this module is for a qualified import:
--
-- > import qualified Graphics.PDF.File as F
---------------------------------------------------------
module Graphics.PDF.File
  ( -- * Values
    Dict
  , Value(..)
  , Object(..)
  , PdfFile(..)
    -- * Files
  , readPdfFile
  , parsePdfFile
  , parsePdfFileWith
  , readPdfFileWithPassword
  , parsePdfFileWithPassword
  , Limits(..)
  , defaultLimits
  , renderPdfFile
  , writePdfFile
    -- * Permissions of an encrypted file
  , Permissions(..)
  , allPermissions
  , noPermissions
  , permissionsValue
  , permissionsFromValue
    -- * Objects
  , dictLookup
  , realToRational
  , getObject
  , resolveValue
    -- * Values as bytes
  , parseValue
  , parseValueWith
  , renderValue
    -- * Streams
  , decodeStream
  ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import Graphics.PDF.File.Filter
import Graphics.PDF.File.Parse
import Graphics.PDF.File.Lexer (parseValue, parseValueWith)
import Graphics.PDF.File.Object
import Graphics.PDF.File.Permissions
import Graphics.PDF.File.Write

-- | Read a PDF file. 'Left' gives the reason the file cannot be read. An
-- encrypted file is read if its user password is empty.
readPdfFile :: FilePath -> IO (Either String PdfFile)
readPdfFile path = read_with parsePdfFile path

-- | Read a PDF file, and decrypt it with a password (the user password or
-- the owner password) if it is encrypted. See 'parsePdfFileWithPassword'.
readPdfFileWithPassword :: T.Text -> FilePath -> IO (Either String PdfFile)
readPdfFileWithPassword password = read_with (parsePdfFileWithPassword defaultLimits password)

read_with :: (S.ByteString -> Either String PdfFile) -> FilePath -> IO (Either String PdfFile)
read_with parse path = do
  r <- try (S.readFile path)
  return $ case r of
    Left e -> Left (show (e :: IOException))
    Right bytes -> parse bytes

-- | Write a PDF file
writePdfFile :: FilePath -> PdfFile -> IO ()
writePdfFile path = L.writeFile path . renderPdfFile

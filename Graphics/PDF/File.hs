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
-- PDF 1.5, and the hybrid files. It does not decrypt a file, and it does
-- not repair a file whose cross-reference is not correct. The writer gives
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
  , Limits(..)
  , defaultLimits
  , renderPdfFile
  , writePdfFile
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
import Graphics.PDF.File.Filter
import Graphics.PDF.File.Parse
import Graphics.PDF.File.Lexer (parseValue, parseValueWith)
import Graphics.PDF.File.Object
import Graphics.PDF.File.Write

-- | Read a PDF file. 'Left' gives the reason the file cannot be read.
readPdfFile :: FilePath -> IO (Either String PdfFile)
readPdfFile path = do
  r <- try (S.readFile path)
  return $ case r of
    Left e -> Left (show (e :: IOException))
    Right bytes -> parsePdfFile bytes

-- | Write a PDF file
writePdfFile :: FilePath -> PdfFile -> IO ()
writePdfFile path = L.writeFile path . renderPdfFile

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Font
---------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
module Graphics.PDF.Fonts.Type1(
      IsFont
    , GlyphSize
    , Type1Font(..)
    , AFMData
    , Type1FontStructure(..)
    , readAfmData
    , parseAfmData
    , mkType1FontStructure
) where 

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font
-- import Graphics.PDF.Fonts.AFMParser
import Graphics.PDF.Fonts.Encoding
import Graphics.PDF.Fonts.FontTypes
import Graphics.PDF.Fonts.AFMParser (AFMFont, fontToStructure, parseAfm)
import qualified Data.ByteString as B
import Data.List
import Data.Bifunctor (Bifunctor(second))
import Text.Parsec.Error (ParseError)

data Type1Font = Type1Font FontStructure (PDFReference EmbeddedFont) deriving Show

instance IsFont Type1Font where 
  getDescent (Type1Font fs _) s = trueSize s $ descent fs 
  getHeight (Type1Font fs _) s = trueSize s $ height fs 
  getKern (Type1Font fs _) s a b = trueSize s $ M.findWithDefault 0 (GlyphPair a b) (kernMetrics fs)
  glyphWidth (Type1Font fs _) s a = trueSize s  $ M.findWithDefault 0 a (widthData fs)
  charGlyph (Type1Font fs _) c = M.findWithDefault 0 c (encoding fs)
  name (Type1Font fs _) = baseFont fs 
  hyphenGlyph (Type1Font fs _) = hyphen fs 
  spaceGlyph (Type1Font fs _) = space fs

data AFMData = AFMData AFMFont deriving Show
data Type1FontStructure = Type1FontStructure FontData FontStructure

readAfmData :: FilePath -> IO (Either ParseError AFMData)
readAfmData path = second AFMData . parseAfm path <$> B.readFile path

parseAfmData :: B.ByteString -> Either ParseError AFMData
parseAfmData bs = second AFMData $ parseAfm "<bytestring>" bs

mkType1FontStructure :: Encodings -> FontData -> AFMData -> IO Type1FontStructure
mkType1FontStructure encoding pdfRef (AFMData f)  = do
  theEncoding <- getEncoding encoding
  let theFont = fontToStructure f theEncoding Nothing
  return $ Type1FontStructure pdfRef theFont

 

instance PdfResourceObject Type1Font where
   toRsrc (Type1Font f ref) =  
                AnyPdfObject . dictFromList $
                           [entry "Type" (PDFName $ "Font")
                           , entry "Subtype" (PDFName $ "Type1")
                           , entry "BaseFont" (PDFName $ baseFont f)
                           , entry "FirstChar" (PDFInteger $ fromIntegral firstChar)
                           , entry "LastChar" (PDFInteger $ fromIntegral lastChar)
                           , entry "Widths" widths
                           , entry "FontDescriptor" descriptor
                           ] 
          where 
            codes = map fst . M.toList $ widthData f
            firstChar = head . sort $ codes
            lastChar = head . reverse . sort $ codes
            findWidth c = PDFInteger . fromIntegral $ M.findWithDefault 0 c (widthData f)
            widths = map findWidth [firstChar .. lastChar] 
            descriptor = dictFromList $
              [ entry "Type" (PDFName $ "Font")
              , entry "Subtype" (PDFName $ "Type1")
              , entry "BaseFont" (PDFName $ baseFont f)
              , entry "FontFile" ref
              , entry "Flags" (PDFInteger . fromIntegral . mkFlags $ f)
              , entry "FontBBox" (fontBBox f)
              , entry "ItalicAngle" (italicAngle f)
              , entry "Ascent" (PDFInteger . fromIntegral $ ascent f)
              , entry "Descent" (PDFInteger . fromIntegral $ descent f)
              , entry "CapHeight" (PDFInteger . fromIntegral $ capHeight f)
                  ]

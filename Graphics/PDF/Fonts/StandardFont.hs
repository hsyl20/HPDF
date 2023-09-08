{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
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
module Graphics.PDF.Fonts.StandardFont(
      IsFont
    , GlyphSize
    , FontName(..)
    , StdFont(..)
    , mkStdFont
    , embeddedFont
) where 

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font
import Graphics.PDF.Fonts.AFMParser(fontToStructure, parseAfm)
import Graphics.PDF.Fonts.Encoding
import Graphics.PDF.Fonts.FontTypes
import Text.Parsec.Error(ParseError)


data FontName = Helvetica 
              | Helvetica_Bold
              | Helvetica_Oblique
              | Helvetica_BoldOblique
              | Times_Roman 
              | Times_Bold
              | Times_Italic
              | Times_BoldItalic
              | Courier
              | Courier_Bold
              | Courier_Oblique
              | Courier_BoldOblique
              | Symbol
              | ZapfDingbats
              deriving(Eq,Ord,Enum)


instance Show FontName where
    show Helvetica = "Helvetica"
    show Helvetica_Bold = "Helvetica-Bold"
    show Helvetica_Oblique = "Helvetica-Oblique"
    show Helvetica_BoldOblique = "Helvetica-BoldOblique"
    show Times_Roman = "Times-Roman"
    show Times_Bold = "Times-Bold"
    show Times_Italic = "Times-Italic"
    show Times_BoldItalic = "Times-BoldItalic"
    show Courier = "Courier"
    show Courier_Bold = "Courier-Bold"
    show Courier_Oblique = "Courier-Oblique"
    show Courier_BoldOblique = "Courier-BoldOblique"
    show Symbol = "Symbol"
    show ZapfDingbats = "ZapfDingbats"

embeddedFont :: FontName -> ByteString
embeddedFont Helvetica = $(embedFile "Core14_AFMs/Helvetica.afm")
embeddedFont Helvetica_Bold = $(embedFile "Core14_AFMs/Helvetica-Bold.afm")
embeddedFont Helvetica_Oblique = $(embedFile "Core14_AFMs/Helvetica-Oblique.afm")
embeddedFont Helvetica_BoldOblique = $(embedFile "Core14_AFMs/Helvetica-BoldOblique.afm")
embeddedFont Times_Roman = $(embedFile "Core14_AFMs/Times-Roman.afm")
embeddedFont Times_Bold = $(embedFile "Core14_AFMs/Times-Bold.afm")
embeddedFont Times_Italic = $(embedFile "Core14_AFMs/Times-Italic.afm")
embeddedFont Times_BoldItalic = $(embedFile "Core14_AFMs/Times-BoldItalic.afm")
embeddedFont Courier = $(embedFile "Core14_AFMs/Courier.afm")
embeddedFont Courier_Bold = $(embedFile "Core14_AFMs/Courier-Bold.afm")
embeddedFont Courier_Oblique = $(embedFile "Core14_AFMs/Courier-Oblique.afm")
embeddedFont Courier_BoldOblique = $(embedFile "Core14_AFMs/Courier-BoldOblique.afm")
embeddedFont Symbol = $(embedFile "Core14_AFMs/Symbol.afm")
embeddedFont ZapfDingbats = $(embedFile "Core14_AFMs/ZapfDingbats.afm")

data StdFont = StdFont FontStructure deriving Show

instance PdfResourceObject StdFont where
   toRsrc (StdFont f) =  AnyPdfObject . dictFromList $
                           [(PDFName "Type",AnyPdfObject . PDFName $ "Font")
                           , (PDFName "Subtype",AnyPdfObject . PDFName $ "Type1")
                           , (PDFName "BaseFont",AnyPdfObject . PDFName $ baseFont f)
                           ] ++ encoding'
          where encoding' | baseFont f == show Symbol = [] 
                          | baseFont f == show ZapfDingbats = []
                          | otherwise = [(PDFName "Encoding",AnyPdfObject . PDFName $ "MacRomanEncoding")]

instance IsFont StdFont where 
  getDescent (StdFont fs) s = trueSize s $ descent fs 
  getHeight (StdFont fs) s = trueSize s $ height fs 
  getKern (StdFont fs) s a b = trueSize s $ M.findWithDefault 0 (GlyphPair a b) (kernMetrics fs)
  glyphWidth (StdFont fs) s a = trueSize s  $ M.findWithDefault 0 a (widthData fs)
  charGlyph (StdFont fs) c = M.findWithDefault 0 c (encoding fs)
  name (StdFont fs) = baseFont fs 
  hyphenGlyph (StdFont fs) = hyphen fs 
  spaceGlyph (StdFont fs) = space fs

mkStdFont :: FontName -> IO (Either ParseError AnyFont)
mkStdFont f = do
  theEncoding <- case f of  
                    ZapfDingbats -> getEncoding ZapfDingbatsEncoding  
                    _ -> getEncoding AdobeStandardEncoding
  theMacEncoding <- case f of 
                     ZapfDingbats -> return Nothing
                     Symbol -> return Nothing 
                     _ -> parseMacEncoding >>= return . Just
  return $ case parseAfm "<embedded>" $ embeddedFont f of 
    Left pe -> Left pe 
    Right r -> Right $ let theFont = fontToStructure r theEncoding theMacEncoding
                           f' = theFont { baseFont = show f }
                       in AnyFont $ StdFont f'

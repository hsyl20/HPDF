{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TemplateHaskell #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- AFM AFMParser
---------------------------------------------------------
module Graphics.PDF.Fonts.AFMParser(
      AFMFont(..)
    , EncodingScheme(..)
    , Metric(..)
    , KX(..)
    , parseAfm
    , fontToStructure
    ) where 

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Text.Parsec.Error(ParseError, Message(..), newErrorMessage)
import Text.Parsec.Pos(newPos)
import Data.Char(digitToInt, isDigit, isHexDigit)
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font(emptyFontStructure)
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Fonts.Encoding(PostscriptName)
import Graphics.PDF.Fonts.FontTypes

data Metric = Metric { charCode :: Int
                     , metricWidth :: Int
                     , name :: String
                     , bounds :: [Double]
                     }
                     deriving(Eq,Show)
                     
data EncodingScheme = AFMAdobeStandardEncoding 
                    | AFMFontSpecific
                    | AFMUnsupportedEncoding
                    deriving(Eq,Read,Show)

data KX = KX String String Int  
        deriving(Eq,Ord,Show)  

data AFMFont = AFMFont { metrics :: [Metric]
                       , underlinePosition :: Int
                       , underlineThickness :: Int
                       , afmAscent :: Int
                       , afmDescent :: Int
                       , kernData :: Maybe [KX]
                       , type1BaseFont :: String
                       , encodingScheme :: EncodingScheme
                       , afmItalic :: Double 
                       , afmCapHeight :: Int
                       , afmStemV :: Int
                       , afmBBox :: [Double]
                       , afmFixedPitch :: Bool
                       , afmSymbolic :: Bool
                       }
                       deriving(Eq,Show)


emptyAFM :: AFMFont
emptyAFM = AFMFont { metrics = []
                   , underlinePosition = 0
                   , underlineThickness = 0
                   , afmAscent = 0
                   , afmDescent = 0
                   , kernData = Nothing
                   , type1BaseFont = ""
                   , encodingScheme = AFMAdobeStandardEncoding
                   , afmItalic = 0.0
                   , afmCapHeight = 0
                   , afmStemV = 0
                   , afmBBox = []
                   , afmFixedPitch = False
                   , afmSymbolic = False
                   }

-- Note [AFM parser]
-- ~~~~~~~~~~~~~~~~~
-- An AFM file (Adobe Technical Note 5004) is a list of lines. Each line
-- starts with a key. The parser reads the keys it uses and ignores the
-- others, because a file can have keys that the specification does not
-- give (and comments in any position).
--
--   StartFontMetrics 4.1
--   FontName Times-Roman               global keys
--   ...
--   StartCharMetrics 2
--   C 32 ; WX 250 ; N space ; B 0 0 0 0 ;
--   C 102 ; WX 333 ; N f ; B 20 0 383 683 ; L i fi ; L l fl ;
--   EndCharMetrics
--   StartKernData
--   StartKernPairs 1
--   KPX A y -92
--   EndKernPairs
--   EndKernData
--   EndFontMetrics
--
-- * A glyph name is a PostScript name: it can contain characters such as
--   "_", "." and "-" (f_i, a.sc, uni0416).
-- * A number can be a real number. The fields of 'AFMFont' and 'Metric'
--   that are integers get the nearest integer.
-- * The fields of a character metric are separated by ";". Only C, CH, WX,
--   W0X, N and B are used. The first occurrence of a field is used.
-- * Only the KPX pairs of writing direction 0 are used.
-- * A global key or a kerning pair with a value that is not a number is
--   ignored. A character metric with such a value is an error.
-- * The parser is total: a file that is not an AFM file gives 'Left'.

-- | The part of the file that the parser reads
data Section = Global | CharMetrics | KernPairs | Ignored String
             deriving(Eq)

-- | Parse an AFM file.
-- See Note [AFM parser]
parseAfm :: FilePath -> ByteString -> Either ParseError AFMFont
parseAfm path bs =
    case dropWhile (null . snd) numbered of
      (_, "StartFontMetrics" : _) : ls -> go Global emptyAFM [] [] ls >>= finish
      (n, _) : _ -> failure n "expecting StartFontMetrics"
      [] -> failure 1 "expecting StartFontMetrics"
  where
    numbered = zip [1 :: Int ..] (map words (afm_lines (unpack bs)))
    last_line = length numbered

    failure :: Int -> String -> Either ParseError a
    failure n msg = Left (newErrorMessage (Message msg) (newPos path n 1))

    -- The metrics and the kerning pairs are in reverse order
    go _ font ms ks [] = Right (font, ms, ks)
    go section font ms ks ((n, ws) : ls) =
      let continue_with font' = go section font' ms ks ls
          -- A global key or a kerning pair with a value that is not a
          -- number is ignored: the default value stays.
          number w k = maybe (go section font ms ks ls) k (afm_number w)
          integer w k = number w (k . round)
      in case (section, ws) of
        (_, []) -> go section font ms ks ls
        (Global, ["EndFontMetrics"]) -> Right (font, ms, ks)
        (Global, "StartCharMetrics" : _) -> go CharMetrics font ms ks ls
        (Global, k : _) | k == "StartKernPairs" || k == "StartKernPairs0" -> go KernPairs font ms ks ls
        (Global, k : _) | k `elem` ["StartKernPairs1", "StartTrackKern", "StartComposites"] ->
          go (Ignored ("End" ++ drop 5 (if k == "StartKernPairs1" then "StartKernPairs" else k))) font ms ks ls
        (Global, ["FontName", v]) -> continue_with font {type1BaseFont = v}
        (Global, ["UnderlinePosition", v]) -> integer v $ \x -> continue_with font {underlinePosition = x}
        (Global, ["UnderlineThickness", v]) -> integer v $ \x -> continue_with font {underlineThickness = x}
        (Global, ["EncodingScheme", v]) -> continue_with font {encodingScheme = encoding_scheme v}
        (Global, ["CapHeight", v]) -> integer v $ \x -> continue_with font {afmCapHeight = x}
        (Global, ["StdVW", v]) -> integer v $ \x -> continue_with font {afmStemV = x}
        (Global, ["Ascender", v]) -> integer v $ \x -> continue_with font {afmAscent = x}
        (Global, ["Descender", v]) -> integer v $ \x -> continue_with font {afmDescent = x}
        (Global, ["ItalicAngle", v]) -> number v $ \x -> continue_with font {afmItalic = x}
        (Global, ["IsFixedPitch", v]) -> continue_with font {afmFixedPitch = v == "true"}
        (Global, "FontBBox" : vs@[_, _, _, _]) | Just xs <- mapM afm_number vs -> continue_with font {afmBBox = xs}
        (Global, _) -> go section font ms ks ls
        (CharMetrics, "EndCharMetrics" : _) -> go Global font ms ks ls
        (CharMetrics, k : _) | k == "C" || k == "CH" ->
          char_metric n (split_fields ws) >>= \m -> go section font (m : ms) ks ls
        (CharMetrics, _) -> go section font ms ks ls
        (KernPairs, "EndKernPairs" : _) -> go Global font ms ks ls
        (KernPairs, ["KPX", a, b, v]) -> integer v $ \x -> go section font ms (KX a b x : ks) ls
        (KernPairs, _) -> go section font ms ks ls
        (Ignored end, k : _) | k == end -> go Global font ms ks ls
        (Ignored _, _) -> go section font ms ks ls

    encoding_scheme "AdobeStandardEncoding" = AFMAdobeStandardEncoding
    encoding_scheme "FontSpecific" = AFMFontSpecific
    encoding_scheme _ = AFMUnsupportedEncoding

    -- "C 102 ; WX 333 ; N f ;" gives [["C","102"],["WX","333"],["N","f"]]
    split_fields ws = case break (== ";") ws of
      (f, []) -> [f]
      (f, _ : r) -> f : split_fields r

    char_metric n fields = foldr add (Right (Metric (-1) 0 "" [])) fields
      where
        number w = maybe (failure n ("not a number: " ++ w)) Right (afm_number w)
        add field r = r >>= \m -> case field of
          ["C", v] -> number v >>= \x -> Right m {charCode = round x}
          ["CH", v] -> maybe (failure n ("not a hexadecimal number: " ++ v)) (\x -> Right m {charCode = x}) (afm_hexadecimal v)
          [k, v] | k == "WX" || k == "W0X" -> number v >>= \x -> Right m {metricWidth = round x}
          ["N", v] -> Right m {name = v}
          "B" : vs@[_, _, _, _] -> mapM number vs >>= \xs -> Right m {bounds = xs}
          _ -> Right m

    finish (font, ms, ks)
      | null ms = failure last_line "no character metric"
      | otherwise =
          let font' = font { metrics = reverse ms
                           , kernData = if null ks then Nothing else Just (reverse ks)
                           }
              bbox_height = case afmBBox font' of
                [_, ymin, _, ymax] -> floor (ymax - ymin)
                _ -> 0
          in Right $ if afmAscent font' /= 0 then font'
                     else if afmCapHeight font' /= 0 then font' {afmAscent = afmCapHeight font'}
                     else font' {afmAscent = bbox_height, afmDescent = 0}

-- | The lines of a file. The end of a line is CR LF, LF or CR. A ";" is
-- always a word, also when it has no space before it.
afm_lines :: String -> [String]
afm_lines = lines . eol
  where
    eol ('\r' : '\n' : r) = '\n' : eol r
    eol ('\r' : r) = '\n' : eol r
    eol (';' : r) = ' ' : ';' : ' ' : eol r
    eol (c : r) = c : eol r
    eol [] = []

-- | A number of an AFM file: 12, -12, +12, 1.5, .5, 5., 1e3
afm_number :: String -> Maybe Double
afm_number w = case reads (fraction (integer_part (sign w))) of
    [(x, "")] -> Just x
    _ -> Nothing
  where
    sign ('+' : r) = r
    sign r = r
    -- 'reads' needs a digit on each side of the "."
    integer_part ('-' : '.' : r) = '-' : '0' : '.' : r
    integer_part ('.' : r) = '0' : '.' : r
    integer_part r = r
    fraction r = case break (== '.') r of
      (a, ['.']) -> a ++ ".0"
      (a, '.' : e : b) | not (isDigit e) -> a ++ ".0" ++ (e : b)
      _ -> r

-- | A character code in hexadecimal: <20> or 20
afm_hexadecimal :: String -> Maybe Int
afm_hexadecimal w =
    let digits = filter (`notElem` "<>") w
    in if not (null digits) && length digits <= 6 && all isHexDigit digits
         then Just (foldl (\a c -> a * 16 + digitToInt c) 0 digits)
         else Nothing

addMetric :: M.Map PostscriptName GlyphCode -> Metric -> FontStructure -> FontStructure 
addMetric nameToGlyph m fs = 
    let c = M.lookup (name m) nameToGlyph
        fs' = case c of 
                Just glyphCode -> 
                  fs { widthData = M.insert (fromIntegral glyphCode) (fromIntegral $ metricWidth m) (widthData fs)}
                Nothing -> fs
    in 
    -- A glyph without a character code (C -1) cannot be shown
    case (name m) of 
      "space" | charCode m >= 0 -> fs' {space = fromIntegral $ charCode m}
      "hyphen" | charCode m >= 0 -> fs' {hyphen = Just (fromIntegral $ charCode m)}
      _ -> fs'

-- Note [Glyph names that give a Unicode value]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The Adobe glyph naming convention has names that contain the Unicode
-- value of the glyph. Font tools use them for the glyphs that the Adobe
-- glyph list does not name:
--
--   uni0416    U+0416: "uni" and four hexadecimal digits in upper case
--   u1F600     U+1F600: "u" and four to six hexadecimal digits in upper case
--
-- These names are used after the names of the encoding: a character that
-- the encoding gives to a glyph keeps this glyph. A name with a suffix
-- (uni0416.alt) or with more than one component (uni00660069, f_i) is not
-- used: it is a variant or a ligature, not the glyph of one character.

-- | The character of a glyph name such as uni0416 or u1F600.
-- See Note [Glyph names that give a Unicode value]
uniGlyphName :: PostscriptName -> Maybe Char
uniGlyphName pname = case pname of
    'u' : 'n' : 'i' : ds | length ds == 4 -> scalar ds
    'u' : ds | length ds >= 4 && length ds <= 6 -> scalar ds
    _ -> Nothing
  where
    upper_hex c = isDigit c || (c >= 'A' && c <= 'F')
    scalar ds
      | all upper_hex ds =
          let v = foldl (\a c -> a * 16 + digitToInt c) 0 ds
          in if v <= 0x10FFFF && not (v >= 0xD800 && v <= 0xDFFF) then Just (toEnum v) else Nothing
      | otherwise = Nothing

addKern :: M.Map String GlyphCode -> KX -> FontStructure -> FontStructure 
addKern d (KX sa sb c) fs = 
  let caM = M.lookup sa d 
      cbM = M.lookup sb d
  in 
  case (caM,cbM) of
    (Just ca, Just cb) -> fs {kernMetrics = M.insert (GlyphPair ca cb) (fromIntegral c) (kernMetrics fs)}
    _ -> fs

-- If the maybe argument is not nothing, we use the specific encoding for
-- the postscript names.
-- Otherwise we use the encoding we found in the afm file.
-- It is used to force MacRomanEncoding on not symbolic default fonts.
fontToStructure :: AFMFont 
                -> M.Map PostscriptName Char   -- ^ Glyph name to unicode
                -> Maybe (M.Map PostscriptName GlyphCode)  -- ^ Glyph name to glyph code if not standard coding
                -> FontStructure 
fontToStructure afm' encoding' maybeMapNameToGlyph =
  let h = (afmAscent afm' - afmDescent afm') 
      fs = emptyFontStructure { descent = fromIntegral $ - (afmDescent afm')
                              , height = fromIntegral $ h 
                              , ascent = fromIntegral $ afmAscent afm'
                              , fontBBox = afmBBox afm'
                              , italicAngle = afmItalic afm'
                              , capHeight = fromIntegral $ afmCapHeight afm'
                              , fixedPitch = afmFixedPitch afm'
                              , serif = False
                              , symbolic = afmSymbolic afm'
                              , script = False
                              , nonSymbolic = not (afmSymbolic afm')
                              , italic = False
                              , allCap = False
                              , smallCap = False
                              , forceBold = False
                              , baseFont = type1BaseFont afm'
                              , stemV = fromIntegral $ afmStemV afm'
                              , glyphNames = foldr add_glyph_name M.empty (metrics afm')
                              }
      addName m d | charCode m == -1 = d
                  | otherwise = M.insert (name m) (fromIntegral $ charCode m) d 
      nameToGlyph = maybe (foldr addName M.empty (metrics afm')) id maybeMapNameToGlyph
      add_glyph_name m d | charCode m == -1 = d
                         | otherwise = M.insert (fromIntegral $ charCode m) (name m) d
      fs1 = foldr (addMetric nameToGlyph) fs (metrics afm')
      addEncodingMapping (pname,glyphcode) d = 
         let unicodeM = M.lookup pname encoding' 
         in 
         case unicodeM of 
          Nothing -> d 
          Just code -> M.insert code glyphcode d 
      -- See Note [Glyph names that give a Unicode value]
      add_uni_mapping (pname,glyphcode) d =
         case uniGlyphName pname of
          Just code | not (M.member pname encoding') -> M.insertWith (\_ old -> old) code glyphcode d
          _ -> d
      mapping = foldr add_uni_mapping (foldr addEncodingMapping M.empty (M.toList nameToGlyph)) (M.toList nameToGlyph)
      fs2 = fs1 { encoding = mapping}
  in
  case kernData afm' of
    Nothing -> fs2
    Just k -> foldr (addKern nameToGlyph) fs2 k

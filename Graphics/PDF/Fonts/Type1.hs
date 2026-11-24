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
    , type1FontFile
) where 

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import qualified Data.Map.Strict as M
import Graphics.PDF.Fonts.Font
-- import Graphics.PDF.Fonts.AFMParser
import Graphics.PDF.Fonts.Encoding (Encodings, PostscriptName, getEncoding)
import Graphics.PDF.Fonts.FontTypes
import Graphics.PDF.Fonts.AFMParser (AFMFont(..), EncodingScheme(..), fontToStructure, parseAfm)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Bits (shiftL, (.|.))
import Data.Char (isHexDigit, isSpace)
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

-- Note [Type 1 font embedding]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- PDF readers ignore an embedded Type 1 font, and use a text font, if one of
-- the following is not valid.
--
-- Font dictionary:
--   /Encoding is necessary. /Differences gives the glyph name of each
--   character code. Then the PDF does not use the built-in encoding of the
--   font program, and it does not use the Symbolic flag to select an
--   encoding.
--
-- Font descriptor:
--   /Type /FontDescriptor, /FontName and /StemV are mandatory.
--   The Symbolic flag is set unless the AFM encoding scheme is
--   AdobeStandardEncoding.
--
-- Font file stream:
--   A PFB file has segments. Each segment has a 6-byte header:
--   0x80, the segment type (1: text, 2: binary, 3: end of file) and the
--   length as a little-endian 32-bit integer.
--   The stream must contain the segment data without these headers.
--   /Length1, /Length2 and /Length3 are the lengths of the clear text
--   portion, of the encrypted portion and of the trailer.
--   A PFA file has no segment headers and the encrypted portion is in
--   hexadecimal. The encrypted portion is converted to binary.

readAfmData :: FilePath -> IO (Either ParseError AFMData)
readAfmData path = second AFMData . parseAfm path <$> B.readFile path

parseAfmData :: B.ByteString -> Either ParseError AFMData
parseAfmData bs = second AFMData $ parseAfm "<bytestring>" bs

mkType1FontStructure :: Encodings -> FontData -> AFMData -> IO Type1FontStructure
mkType1FontStructure encoding pfb (AFMData f)  = do
  theEncoding <- getEncoding encoding
  let theFont = (fontToStructure f theEncoding Nothing)
                  { symbolic = sym, nonSymbolic = not sym }
      sym = encodingScheme f /= AFMAdobeStandardEncoding
  return $ Type1FontStructure pfb theFont

type1FontFile :: B.ByteString -> (B.ByteString, Int, Int, Int)
type1FontFile d
  | Just r <- pfb_segments d = r
  | otherwise = pfa_segments d

-- A PFB file is a sequence of segments. Each segment has a 6-byte header:
-- 0x80, the segment type (1: text, 2: binary, 3: end of file) and the length
-- as a little-endian 32-bit integer.
pfb_segments :: B.ByteString -> Maybe (B.ByteString, Int, Int, Int)
pfb_segments = go [] [] []
  where
    go as bs cs d
      | B.length d < 2 || B.index d 0 /= 0x80 = Nothing
      | B.index d 1 == 3 = finish as bs cs
      | B.length d < 6 = Nothing
      | otherwise =
          let n = foldr (\i acc -> (acc `shiftL` 8) .|. fromIntegral (B.index d i))
                        0 [2,3,4,5] :: Int
              (seg, rest) = B.splitAt n (B.drop 6 d)
          in if B.length seg /= n then Nothing else
             case (B.index d 1, bs, cs) of
               (1, [], []) -> go (seg:as) bs cs rest
               (2, _, [])  -> go as (seg:bs) cs rest
               (1, _:_, _) -> go as bs (seg:cs) rest
               _           -> Nothing
    finish as bs cs
      | null as || null bs = Nothing
      | otherwise =
          let part1 = B.concat (reverse as)
              part2 = B.concat (reverse bs)
              part3 = B.concat (reverse cs)
          in Just (B.concat [part1, part2, part3], B.length part1, B.length part2, B.length part3)

-- The font program is not in the PFB format: this is a best effort for a PFA
-- file. If the encrypted portion cannot be converted, the data is embedded
-- as it is.
pfa_segments :: B.ByteString -> (B.ByteString, Int, Int, Int)
pfa_segments d =
  case B.breakSubstring marker d of
    (_, r) | B.null r -> raw
    (before, r) ->
      let part1 = before `B.append` B.take (B.length marker) r
          rest = skip_eol (B.drop (B.length marker) r)
      in if B.null rest then raw
         else if not (all_hex (C.take 4 rest))
           then (part1, B.length part1, B.length rest, 0)
           else case split_trailer rest of
                  Nothing -> raw
                  Just (hex, part3) ->
                    case from_hex hex of
                      Nothing -> raw
                      Just part2 -> (B.concat [part1, part2, part3], B.length part1, B.length part2, B.length part3)
  where
    marker = C.pack "currentfile eexec"
    raw = (d, B.length d, 0, 0)
    skip_eol s = case C.uncons s of
      Just ('\r', t) | Just ('\n', u) <- C.uncons t -> u
                      | otherwise -> t
      Just ('\n', t) -> t
      Just (c, t) | c == ' ' || c == '\t' -> skip_eol t
      _ -> s
    all_hex = C.all (\c -> isHexDigit c || isSpace c)
    from_hex s =
      let digits = B.filter (not . is_space) s
      in if odd (B.length digits) || not (B.all is_hex digits) then Nothing
         else Just (B.pack (hex_bytes (B.unpack digits)))
    hex_bytes (a:b:t) = (hex_val a * 16 + hex_val b) : hex_bytes t
    hex_bytes _ = []
    -- Offset of the last occurrence of the pattern, or -1.
    last_index pat str = go (B.length str - B.length pat)
      where go i | i < 0 = -1
                 | B.isPrefixOf pat (B.drop i str) = i
                 | otherwise = go (i - 1)
    is_space c = c == 0x20 || (c >= 0x09 && c <= 0x0d)
    is_hex c = (c >= 0x30 && c <= 0x39) || (c >= 0x41 && c <= 0x46) || (c >= 0x61 && c <= 0x66)
    hex_val c | c <= 0x39 = c - 0x30
              | c <= 0x46 = c - 0x37
              | otherwise = c - 0x57
    -- The trailer is 512 zeros (with any whitespace) and "cleartomark".
    -- The search goes from the end of the file.
    split_trailer s =
      let ix = last_index (C.pack "cleartomark") s
      in if ix <= 0 then Nothing
         else let body = B.take ix s
                  after = B.drop ix s
                  hex = B.dropWhileEnd (\c -> c == 0x30 || is_space c) body
                  zeros = B.drop (B.length hex) body
              in if B.count 0x30 zeros < 512 then Nothing
                 else Just (hex, zeros `B.append` after)

-- See Note [Type 1 font embedding]
instance PdfResourceObject Type1Font where
   toRsrc (Type1Font f ref) =
       AnyPdfObject . dictFromList $
         [ entry "Type" (PDFName "Font")
         , entry "Subtype" (PDFName "Type1")
         , entry "BaseFont" (PDFName $ baseFont f)
         , entry "FontDescriptor" descriptor
         ] ++ width_entries ++ encoding_entry
     where
       width_entries = case (M.lookupMin (widthData f), M.lookupMax (widthData f)) of
         (Just (first_char, _), Just (last_char, _)) ->
           [ entry "FirstChar" (PDFInteger $ fromIntegral first_char)
           , entry "LastChar" (PDFInteger $ fromIntegral last_char)
           , entry "Widths" (map find_width [first_char .. last_char])
           ]
         _ -> []
       find_width c = PDFInteger . fromIntegral $ M.findWithDefault 0 c (widthData f)
       encoding_entry
         | M.null (glyphNames f) = []
         | otherwise = [ entry "Encoding" . dictFromList $
                           [ entry "Type" (PDFName "Encoding")
                           , entry "Differences" (differences (M.toAscList (glyphNames f)))
                           ]
                       ]
       -- [c1 /n1 /n2 c3 /n3] : a code, then the names of the consecutive codes
       differences :: [(GlyphCode, PostscriptName)] -> [AnyPdfObject]
       differences [] = []
       differences ((c, n) : r) =
           AnyPdfObject (PDFInteger $ fromIntegral c) : AnyPdfObject (PDFName n) : run c r
         where
           run prev ((c', n') : r') | c' == prev + 1 = AnyPdfObject (PDFName n') : run c' r'
           run _ r' = differences r'
       descriptor = dictFromList $
         [ entry "Type" (PDFName "FontDescriptor")
         , entry "FontName" (PDFName $ baseFont f)
         , entry "Flags" (PDFInteger . fromIntegral . mkFlags $ f)
         , entry "FontBBox" (fontBBox f)
         , entry "ItalicAngle" (italicAngle f)
         , entry "Ascent" (PDFInteger . fromIntegral $ ascent f)
         , entry "Descent" (PDFInteger . negate . fromIntegral $ descent f)
         , entry "CapHeight" (PDFInteger . fromIntegral $ capHeight f)
         , entry "StemV" (PDFInteger . fromIntegral $ stemV f)
         , entry "FontFile" ref
         ]

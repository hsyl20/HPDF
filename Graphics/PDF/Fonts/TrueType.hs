{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2026, HPDF contributors
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Embedded TrueType fonts with Unicode text.
--
-- A TrueType font (@.ttf@, @glyf@ outlines) is embedded whole as a
-- @FontFile2@ and written as a Type0 font over a @CIDFontType2@ descendant,
-- with the @Identity-H@ encoding: a text string is a run of two-byte glyph
-- ids, which the font's @cmap@ gives for each character. A @ToUnicode@ CMap
-- maps the glyph ids back to Unicode, so text extracts and copies out of the
-- PDF correctly.
--
-- > Right ttf <- readTrueTypeFont "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
-- > runPdf "out.pdf" standardDocInfo rect $ do
-- >   font <- mkTrueTypeFont ttf
-- >   page <- addPage Nothing
-- >   drawWithPage page $ drawText $ text (PDFFont font 12) 10 200 "Привіт, світ"
--
-- One glyph per character: there is no shaping (no ligatures, no
-- contextual forms), so scripts that need it (Arabic, Indic) do not render
-- correctly. No kerning is applied. The font is not subset.
---------------------------------------------------------
module Graphics.PDF.Fonts.TrueType
  ( TrueTypeFontData
  , TrueTypeFont
  , readTrueTypeFont
  , parseTrueTypeFont
  , mkTrueTypeFont
  , trueTypePostScriptName
  , trueTypeCharacters
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Char (chr, isAlphaNum, ord)
import Data.Int (Int16, Int32)
import Data.List (sortOn)
import Control.Monad.Writer (tell)
import Data.Binary.Builder (fromByteString)
import Text.Printf (printf)

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import Graphics.PDF.Draw
import Graphics.PDF.Pages (createContent, addObject)
import Graphics.PDF.Fonts.Font

-- | A parsed TrueType font, not yet in a document.
data TrueTypeFontData = TrueTypeFontData
  { ttBytes :: !B.ByteString
  , ttPsName :: !String
  , ttUnitsPerEm :: !Int
  , ttAscent :: !Int
  , ttDescent :: !Int -- ^ negative, below the baseline
  , ttCapHeight :: !Int
  , ttBBox :: ![Int]
  , ttItalicAngle :: !Double
  , ttFixedPitch :: !Bool
  , ttAdvances :: !(V.Vector Int) -- ^ advance width of each glyph id
  , ttCmap :: !(IM.IntMap Int) -- ^ code point to glyph id
  }

instance Show TrueTypeFontData where
  show t = "TrueTypeFontData " ++ show (ttPsName t)

-- | The font's PostScript name (@name@ table, name id 6), used as its
-- @BaseFont@ and its resource name.
trueTypePostScriptName :: TrueTypeFontData -> String
trueTypePostScriptName = ttPsName

-- | The characters the font has a glyph for.
trueTypeCharacters :: TrueTypeFontData -> [Char]
trueTypeCharacters = map chr . IM.keys . ttCmap

-- | A TrueType font embedded in a document. Make one with 'mkTrueTypeFont'.
data TrueTypeFont = TrueTypeFont !TrueTypeFontData !(PDFReference PDFDictionary)

instance Show TrueTypeFont where
  show (TrueTypeFont d _) = "TrueTypeFont " ++ show (ttPsName d)

readTrueTypeFont :: FilePath -> IO (Either String TrueTypeFontData)
readTrueTypeFont path = parseTrueTypeFont <$> B.readFile path

-- Reading ----------------------------------------------------------------

u8 :: B.ByteString -> Int -> Int
u8 bs i = fromIntegral (B.index bs i)

u16 :: B.ByteString -> Int -> Int
u16 bs i = (u8 bs i `shiftL` 8) .|. u8 bs (i + 1)

i16 :: B.ByteString -> Int -> Int
i16 bs i = fromIntegral (fromIntegral (u16 bs i) :: Int16)

u32 :: B.ByteString -> Int -> Int
u32 bs i = (u16 bs i `shiftL` 16) .|. u16 bs (i + 2)

i32 :: B.ByteString -> Int -> Int
i32 bs i = fromIntegral (fromIntegral (u32 bs i) :: Int32)

-- | Parses the tables a PDF needs (@head@, @hhea@, @hmtx@, @maxp@, @cmap@,
-- @OS/2@, @post@, @name@). Only TrueType outlines (@glyf@) are accepted:
-- an OpenType font with CFF outlines would need a @FontFile3@.
parseTrueTypeFont :: B.ByteString -> Either String TrueTypeFontData
parseTrueTypeFont bs = do
  check (B.length bs >= 12) "file too short"
  let sfnt = B.take 4 bs
  check (sfnt == B.pack [0, 1, 0, 0] || sfnt == "true") $
    if sfnt == "OTTO" then "CFF outlines (OpenType/CFF) are not supported, only TrueType outlines" else "not a TrueType font"
  let numTables = u16 bs 4
      dir = M.fromList
        [ (C.unpack (B.take 4 (B.drop o bs)), (u32 bs (o + 8), u32 bs (o + 12)))
        | i <- [0 .. numTables - 1], let o = 12 + 16 * i ]
      table t = case M.lookup t dir of
        Just (off, len) | off + len <= B.length bs -> Right (B.take len (B.drop off bs))
        Just _ -> Left ("table " ++ t ++ " runs past the end of the file")
        Nothing -> Left ("no " ++ t ++ " table")
      optTable t = either (const Nothing) Just (table t)
  check (M.member "glyf" dir) "no glyf table (only TrueType outlines are supported)"
  hd <- table "head"
  hh <- table "hhea"
  mx <- table "maxp"
  hm <- table "hmtx"
  cm <- table "cmap"
  let upem = u16 hd 18
      bbox = [i16 hd 36, i16 hd 38, i16 hd 40, i16 hd 42]
      asc = i16 hh 4
      desc = i16 hh 6
      nHMetrics = u16 hh 34
      numGlyphs = u16 mx 4
  check (upem > 0) "unitsPerEm is 0"
  check (nHMetrics > 0 && B.length hm >= 4 * nHMetrics) "short hmtx table"
  let lastAdv = u16 hm (4 * (nHMetrics - 1))
      advances = V.generate numGlyphs $ \g -> if g < nHMetrics then u16 hm (4 * g) else lastAdv
      os2 = optTable "OS/2"
      capH = case os2 of
        Just o | u16 o 0 >= 2 && B.length o >= 90 -> i16 o 88
        _ -> asc
      (italicA, fixed) = case optTable "post" of
        Just p | B.length p >= 16 -> (fromIntegral (i32 p 4) / 65536, u32 p 12 /= 0)
        _ -> (0, False)
      psName = maybe "TrueTypeFont" sanitize (optTable "name" >>= postScriptName)
  cmap <- parseCmap cm
  pure TrueTypeFontData
    { ttBytes = bs
    , ttPsName = psName
    , ttUnitsPerEm = upem
    , ttAscent = asc
    , ttDescent = desc
    , ttCapHeight = capH
    , ttBBox = bbox
    , ttItalicAngle = italicA
    , ttFixedPitch = fixed
    , ttAdvances = advances
    , ttCmap = IM.filter (< numGlyphs) cmap
    }
  where
    check True _ = Right ()
    check False e = Left e
    sanitize s = case filter (\c -> isAlphaNum c || c `elem` ("-_." :: String)) s of
      "" -> "TrueTypeFont"
      s' -> s'

-- | Name id 6, from a Windows (UTF-16BE) or Macintosh (Roman) record.
postScriptName :: B.ByteString -> Maybe String
postScriptName nt
  | B.length nt < 6 = Nothing
  | otherwise =
      let count = u16 nt 2
          strOff = u16 nt 4
          recs = [ (u16 nt o, u16 nt (o + 6), u16 nt (o + 8), u16 nt (o + 10))
                 | i <- [0 .. count - 1], let o = 6 + 12 * i, o + 12 <= B.length nt ]
          str off len = B.take len (B.drop (strOff + off) nt)
          utf16 b = [chr (u16 b i) | i <- [0, 2 .. B.length b - 2]]
          found = [utf16 (str off len) | (3, 6, len, off) <- recs]
               ++ [C.unpack (str off len) | (1, 6, len, off) <- recs]
       in case found of
            (n : _) | not (null n) -> Just n
            _ -> Nothing

-- | The best Unicode subtable: format 12 (full Unicode), else format 4
-- (Basic Multilingual Plane).
parseCmap :: B.ByteString -> Either String (IM.IntMap Int)
parseCmap cm = do
  let n = u16 cm 2
      recs = [ (u16 cm o, u16 cm (o + 2), u32 cm (o + 4)) | i <- [0 .. n - 1], let o = 4 + 8 * i ]
      sub off = B.drop off cm
      fmt off = u16 cm off
      unicode (p, e, _) = (p == 3 && (e == 1 || e == 10)) || p == 0
      candidates = [ off | r@(_, _, off) <- recs, unicode r, off < B.length cm ]
      f12 = [ off | off <- candidates, fmt off == 12 ]
      f4 = [ off | off <- candidates, fmt off == 4 ]
  case (f12, f4) of
    (off : _, _) -> Right (format12 (sub off))
    (_, off : _) -> Right (format4 (sub off))
    _ -> Left "no Unicode cmap subtable in format 4 or 12"

format4 :: B.ByteString -> IM.IntMap Int
format4 t =
  let segX2 = u16 t 6
      seg = segX2 `div` 2
      endAt i = u16 t (14 + 2 * i)
      startAt i = u16 t (16 + segX2 + 2 * i)
      deltaAt i = u16 t (16 + 2 * segX2 + 2 * i)
      rangeOffPos i = 16 + 3 * segX2 + 2 * i
      gidFor i c
        | ro == 0 = (c + deltaAt i) .&. 0xFFFF
        | otherwise =
            let addr = rangeOffPos i + ro + 2 * (c - startAt i)
                g = if addr + 1 < B.length t then u16 t addr else 0
             in if g == 0 then 0 else (g + deltaAt i) .&. 0xFFFF
        where ro = u16 t (rangeOffPos i)
   in IM.fromList
        [ (c, g)
        | i <- [0 .. seg - 1]
        , c <- [startAt i .. endAt i]
        , c /= 0xFFFF
        , let g = gidFor i c
        , g /= 0 ]

format12 :: B.ByteString -> IM.IntMap Int
format12 t =
  let groups = u32 t 12
   in IM.fromList
        [ (c, sg + (c - s))
        | i <- [0 .. groups - 1]
        , let o = 16 + 12 * i
        , o + 12 <= B.length t
        , let s = u32 t o
              e = u32 t (o + 4)
              sg = u32 t (o + 8)
        , c <- [s .. min e 0x10FFFF] ]

-- Metrics ----------------------------------------------------------------

scaled :: TrueTypeFontData -> FontSize -> Int -> PDFFloat
scaled d s v = fromIntegral v * fromIntegral s / fromIntegral (ttUnitsPerEm d)

-- | In thousandths of the em, as PDF font dictionaries want them.
perMille :: TrueTypeFontData -> Int -> Int
perMille d v = round (fromIntegral v * 1000 / fromIntegral (ttUnitsPerEm d) :: Double)

advance :: TrueTypeFontData -> GlyphCode -> Int
advance d g = maybe 0 id (ttAdvances d V.!? fromIntegral g)

-- | Metrics of a font not yet embedded, e.g. to lay text out before the
-- document is built. Same answers as the embedded 'TrueTypeFont'.
instance IsFont TrueTypeFontData where
  name = ttPsName
  getDescent d s = scaled d s (negate (ttDescent d))
  getHeight d s = scaled d s (ttAscent d - ttDescent d)
  getKern _ _ _ _ = 0
  glyphWidth d s g = scaled d s (advance d g)
  hyphenGlyph d = fromIntegral <$> IM.lookup (ord '-') (ttCmap d)
  spaceGlyph d = maybe 0 fromIntegral (IM.lookup (ord ' ') (ttCmap d))
  charGlyph d c = maybe 0 fromIntegral (IM.lookup (ord c) (ttCmap d))
  glyphCodeBytes _ = 2

instance IsFont TrueTypeFont where
  name (TrueTypeFont d _) = name d
  getDescent (TrueTypeFont d _) = getDescent d
  getHeight (TrueTypeFont d _) = getHeight d
  getKern (TrueTypeFont d _) = getKern d
  glyphWidth (TrueTypeFont d _) = glyphWidth d
  hyphenGlyph (TrueTypeFont d _) = hyphenGlyph d
  spaceGlyph (TrueTypeFont d _) = spaceGlyph d
  charGlyph (TrueTypeFont d _) = charGlyph d
  glyphCodeBytes _ = 2

-- | The font dictionary is an indirect object, written once however many
-- pages use the font.
instance PdfResourceObject TrueTypeFont where
  toRsrc (TrueTypeFont _ type0) = AnyPdfObject type0

-- Embedding --------------------------------------------------------------

-- | Embeds the font program, its CID font and font descriptor, and a
-- @ToUnicode@ CMap covering every character of the font's @cmap@.
mkTrueTypeFont :: TrueTypeFontData -> PDF AnyFont
mkTrueTypeFont d = do
  fontFile <- createContent (embed (ttBytes d) [entry "Length1" (PDFInteger (B.length (ttBytes d)))]) Nothing
  descriptor <- addObject . dictFromList $
    [ entry "Type" (PDFName "FontDescriptor")
    , entry "FontName" (PDFName (ttPsName d))
    , entry "Flags" (PDFInteger flags)
    , entry "FontBBox" (map (PDFInteger . perMille d) (ttBBox d))
    , entry "ItalicAngle" (ttItalicAngle d)
    , entry "Ascent" (PDFInteger (perMille d (ttAscent d)))
    , entry "Descent" (PDFInteger (perMille d (ttDescent d)))
    , entry "CapHeight" (PDFInteger (perMille d (ttCapHeight d)))
    , entry "StemV" (PDFInteger 80)
    , entry "FontFile2" fontFile
    ]
  cidFont <- addObject . dictFromList $
    [ entry "Type" (PDFName "Font")
    , entry "Subtype" (PDFName "CIDFontType2")
    , entry "BaseFont" (PDFName (ttPsName d))
    , entry "CIDSystemInfo" (dictFromList
        [ entry "Registry" (toAsciiString "Adobe")
        , entry "Ordering" (toAsciiString "Identity")
        , entry "Supplement" (PDFInteger 0) ])
    , entry "FontDescriptor" descriptor
    , entry "CIDToGIDMap" (PDFName "Identity")
    , entry "DW" (PDFInteger 1000)
    , entry "W" widths
    ]
  toUnicode <- createContent (embed (toUnicodeCMap d) []) Nothing
  type0 <- addObject . dictFromList $
    [ entry "Type" (PDFName "Font")
    , entry "Subtype" (PDFName "Type0")
    , entry "BaseFont" (PDFName (ttPsName d))
    , entry "Encoding" (PDFName "Identity-H")
    , entry "DescendantFonts" [cidFont]
    , entry "ToUnicode" toUnicode
    ]
  pure (AnyFont (TrueTypeFont d type0))
  where
    embed :: B.ByteString -> [(PDFName, AnyPdfObject)] -> Draw ()
    embed bytes extra = do
      modifyStrict $ \s -> s {otherRsrcs = dictFromList extra}
      tell (fromByteString bytes)
    flags = (if ttFixedPitch d then 1 else 0) .|. 32 .|. (if ttItalicAngle d /= 0 then 64 else 0)
    -- one run: first glyph id, then every advance in order
    widths = [AnyPdfObject (PDFInteger 0), AnyPdfObject (map (PDFInteger . perMille d) (V.toList (ttAdvances d)))]

-- | A CMap from two-byte glyph ids to UTF-16BE, for text extraction. A
-- glyph shared by several characters maps to the lowest one.
toUnicodeCMap :: TrueTypeFontData -> B.ByteString
toUnicodeCMap d =
  C.unlines $
    [ "/CIDInit /ProcSet findresource begin"
    , "12 dict begin"
    , "begincmap"
    , "/CIDSystemInfo << /Registry (Adobe) /Ordering (UCS) /Supplement 0 >> def"
    , "/CMapName /Adobe-Identity-UCS def"
    , "/CMapType 2 def"
    , "1 begincodespacerange"
    , "<0000> <FFFF>"
    , "endcodespacerange"
    ]
    ++ concatMap block (chunks 100 pairs)
    ++ [ "endcmap"
       , "CMapName currentdict /CMap defineresource pop"
       , "end"
       , "end"
       ]
  where
    -- control characters (NUL, CR…) would only add noise to extracted text
    byGlyph = IM.fromListWith min [ (g, c) | (c, g) <- IM.toList (ttCmap d), c >= 0x20, c /= 0x7F ]
    pairs = sortOn fst (IM.toList byGlyph)
    block ps =
      [C.pack (show (length ps) ++ " beginbfchar")]
        ++ [ C.pack ("<" ++ printf "%04X" g ++ "> <" ++ utf16Hex c ++ ">") | (g, c) <- ps ]
        ++ ["endbfchar"]
    chunks _ [] = []
    chunks k xs = let (a, b) = splitAt k xs in a : chunks k b

utf16Hex :: Int -> String
utf16Hex c
  | c < 0x10000 = printf "%04X" c
  | otherwise =
      let c' = c - 0x10000
          hi = 0xD800 + (c' `shiftR` 10) :: Int
          lo = 0xDC00 + (c' .&. 0x3FF) :: Int
       in printf "%04X%04X" hi lo

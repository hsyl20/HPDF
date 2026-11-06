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
-- A TrueType font (@.ttf@, @glyf@ outlines) is embedded, as a subset or
-- whole, as a @FontFile2@ and written as a Type0 font over a @CIDFontType2@ descendant,
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
-- correctly. No kerning is applied. By default only the glyphs the
-- document shows are embedded (see 'mkTrueTypeFontWith').
---------------------------------------------------------
module Graphics.PDF.Fonts.TrueType
  ( TrueTypeFontData
  , TrueTypeFont
  , readTrueTypeFont
  , parseTrueTypeFont
  , mkTrueTypeFont
  , mkTrueTypeFontWith
  , TrueTypeEmbedding (..)
  , subsetTrueType
  , trueTypeGlyphComponents
  , trueTypePostScriptName
  , trueTypeCharacters
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Vector.Unboxed as V
import Data.Bits (shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (chr, isAlphaNum, ord)
import Data.Int (Int16, Int32)
import Data.List (sortOn)
import Data.Word (Word64)
import Control.Monad.Writer (tell)
import Data.Binary.Builder (fromByteString)
import Text.Printf (printf)

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import Graphics.PDF.Draw
import Graphics.PDF.Pages (createContent, addObject, updateObject)
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

-- | How much of the font program goes into the document.
data TrueTypeEmbedding
  = -- | Only the glyphs the document shows (plus glyph 0 and the
    -- components of composite glyphs), written when the document is saved.
    -- The default.
    EmbedSubset
  | -- | The whole font file, as it is.
    EmbedWhole
  deriving (Eq, Show)

-- | Embeds a subset of the font: 'mkTrueTypeFontWith' 'EmbedSubset'.
mkTrueTypeFont :: TrueTypeFontData -> PDF AnyFont
mkTrueTypeFont = mkTrueTypeFontWith EmbedSubset

-- | Embeds the font program, its CID font and font descriptor, and a
-- @ToUnicode@ CMap.
--
-- With 'EmbedSubset', nothing but the font dictionary exists until the
-- document is saved; then the font program keeps only the glyphs shown
-- with this font in any page, XObject or pattern (glyph ids unchanged, so
-- @CIDToGIDMap@ stays @/Identity@ and the content streams are untouched),
-- @/W@ and the @ToUnicode@ CMap cover those glyphs, and the @BaseFont@
-- takes a six-letter subset tag (@ABCDEF+Name@). Glyphs reach the subset
-- through 'Graphics.PDF.Text.recordGlyphs', which the text and typesetting
-- functions call.
mkTrueTypeFontWith :: TrueTypeEmbedding -> TrueTypeFontData -> PDF AnyFont
mkTrueTypeFontWith mode d = do
  type0 <- addObject emptyDictionary
  case mode of
    EmbedWhole -> writeTrueTypeFont d Nothing type0
    EmbedSubset ->
      let finalize used = writeTrueTypeFont d (Just (M.findWithDefault IS.empty (ttPsName d) used)) type0
       in modifyStrict $ \s -> s {fontFinalizers = finalize : fontFinalizers s}
  pure (AnyFont (TrueTypeFont d type0))

-- | Writes the font's objects and fills in its Type0 dictionary. @Nothing@
-- embeds the whole font; @Just gs@ a subset with the glyphs @gs@.
writeTrueTypeFont :: TrueTypeFontData -> Maybe IS.IntSet -> PDFReference PDFDictionary -> PDF ()
writeTrueTypeFont d subset type0 = do
  let (program, glyphs, baseName) = case subset of
        Nothing -> (ttBytes d, Nothing, ttPsName d)
        Just used -> case subsetTrueType (ttBytes d) used of
          Right (bytes, kept) -> (bytes, Just kept, subsetTag (ttPsName d) kept ++ "+" ++ ttPsName d)
          -- a font this module parsed but cannot subset: embed it whole
          Left _ -> (ttBytes d, Nothing, ttPsName d)
  fontFile <- createContent (embed program [entry "Length1" (PDFInteger (B.length program))]) Nothing
  descriptor <- addObject . dictFromList $
    [ entry "Type" (PDFName "FontDescriptor")
    , entry "FontName" (PDFName baseName)
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
    , entry "BaseFont" (PDFName baseName)
    , entry "CIDSystemInfo" (dictFromList
        [ entry "Registry" (toAsciiString "Adobe")
        , entry "Ordering" (toAsciiString "Identity")
        , entry "Supplement" (PDFInteger 0) ])
    , entry "FontDescriptor" descriptor
    , entry "CIDToGIDMap" (PDFName "Identity")
    , entry "DW" (PDFInteger 1000)
    , entry "W" (widthArray d glyphs)
    ]
  toUnicode <- createContent (embed (toUnicodeCMap d glyphs) []) Nothing
  updateObject type0 . dictFromList $
    [ entry "Type" (PDFName "Font")
    , entry "Subtype" (PDFName "Type0")
    , entry "BaseFont" (PDFName baseName)
    , entry "Encoding" (PDFName "Identity-H")
    , entry "DescendantFonts" [cidFont]
    , entry "ToUnicode" toUnicode
    ]
  where
    embed :: B.ByteString -> [(PDFName, AnyPdfObject)] -> Draw ()
    embed bytes extra = do
      modifyStrict $ \s -> s {otherRsrcs = dictFromList extra}
      tell (fromByteString bytes)
    flags = (if ttFixedPitch d then 1 else 0) .|. 32 .|. (if ttItalicAngle d /= 0 then 64 else 0)

-- | The @/W@ array: one run per range of consecutive glyph ids, either of
-- every glyph (@Nothing@) or of the given ones.
widthArray :: TrueTypeFontData -> Maybe IS.IntSet -> [AnyPdfObject]
widthArray d glyphs = concat [ [AnyPdfObject (PDFInteger g0), AnyPdfObject (map w run)] | run@(g0 : _) <- runs ]
  where
    n = V.length (ttAdvances d)
    ids = maybe [0 .. n - 1] (filter (< n) . IS.toAscList) glyphs
    w g = PDFInteger (perMille d (ttAdvances d V.! g))
    runs = consecutive ids
    consecutive [] = []
    consecutive (x : xs) = let (r, rest) = go x xs in (x : r) : consecutive rest
    go p (y : ys) | y == p + 1 = let (r, rest) = go y ys in (y : r, rest)
    go _ ys = ([], ys)

-- | A six-letter uppercase tag naming a subset (PDF 32000-1, 9.6.4),
-- derived from the font and the glyphs it keeps, so the same subset gets
-- the same tag.
subsetTag :: String -> IS.IntSet -> String
subsetTag psName glyphs = take 6 (letters h0)
  where
    fnv :: Word64 -> Int -> Word64
    fnv acc x = (acc `xor` fromIntegral x) * 1099511628211
    h0 = foldl' fnv 14695981039346656037 (map ord psName ++ IS.toList glyphs)
    letters h = chr (ord 'A' + fromIntegral (h `mod` 26)) : letters (h `div` 26)

-- | A CMap from two-byte glyph ids to UTF-16BE, for text extraction, for
-- every glyph of the cmap (@Nothing@) or the given ones. A glyph shared by
-- several characters maps to the lowest one.
toUnicodeCMap :: TrueTypeFontData -> Maybe IS.IntSet -> B.ByteString
toUnicodeCMap d glyphs =
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
    byGlyph = IM.fromListWith min [ (g, c) | (c, g) <- IM.toList (ttCmap d), c >= 0x20, c /= 0x7F, keep g ]
    keep g = maybe True (IS.member g) glyphs
    pairs = sortOn fst (IM.toList byGlyph)
    block ps =
      [C.pack (show (length ps) ++ " beginbfchar")]
        ++ [ C.pack ("<" ++ printf "%04X" g ++ "> <" ++ utf16Hex c ++ ">") | (g, c) <- ps ]
        ++ ["endbfchar"]
    chunks _ [] = []
    chunks k xs = let (a, b) = splitAt k xs in a : chunks k b

-- Subsetting -------------------------------------------------------------

-- | A subset of a TrueType font file that keeps the given glyphs, glyph 0
-- (@.notdef@) and, transitively, the components of the composite glyphs
-- kept. Glyph ids do not change: the other glyphs become empty, and the
-- font ends after the highest glyph kept. The font keeps the tables a PDF
-- font program needs (PDF 32000-1, 9.9: @head@, @hhea@, @maxp@, @hmtx@,
-- @loca@, @glyf@ and, when the font has them, the hinting tables @cvt @,
-- @fpgm@ and @prep@); @loca@ is rewritten in the long format.
-- Returns the font file and the glyphs it kept.
subsetTrueType :: B.ByteString -> IS.IntSet -> Either String (B.ByteString, IS.IntSet)
subsetTrueType bs wanted = do
  check (B.length bs >= 12) "file too short"
  let numTables = u16 bs 4
      dir = M.fromList
        [ (C.unpack (B.take 4 (B.drop o bs)), (u32 bs (o + 8), u32 bs (o + 12)))
        | i <- [0 .. numTables - 1], let o = 12 + 16 * i, o + 16 <= B.length bs ]
      table t = case M.lookup t dir of
        Just (off, len) | off + len <= B.length bs -> Right (B.take len (B.drop off bs))
        Just _ -> Left ("table " ++ t ++ " runs past the end of the file")
        Nothing -> Left ("no " ++ t ++ " table")
      optTable t = either (const Nothing) Just (table t)
  hd <- table "head"
  hh <- table "hhea"
  mx <- table "maxp"
  hm <- table "hmtx"
  lc <- table "loca"
  gl <- table "glyf"
  check (B.length hd >= 54 && B.length hh >= 36 && B.length mx >= 6) "short head, hhea or maxp table"
  let numGlyphs = u16 mx 4
      longLoca = i16 hd 50 /= 0
      nH = u16 hh 34
      locaAt g
        | longLoca = u32 lc (4 * g)
        | otherwise = 2 * u16 lc (2 * g)
  check (B.length lc >= (if longLoca then 4 else 2) * (numGlyphs + 1)) "short loca table"
  check (nH > 0 && B.length hm >= 4 * nH + 2 * (numGlyphs - min nH numGlyphs)) "short hmtx table"
  let glyphData g
        | g < 0 || g >= numGlyphs = B.empty
        | otherwise =
            let a = locaAt g
                b = locaAt (g + 1)
             in if b > a && b <= B.length gl then B.take (b - a) (B.drop a gl) else B.empty
      kept = closure (glyphComponents . glyphData) numGlyphs (IS.insert 0 wanted)
      newN = IS.findMax kept + 1
      glyphsOut = [ if IS.member g kept then pad4 (glyphData g) else B.empty | g <- [0 .. newN - 1] ]
      offsets = scanl (+) 0 (map B.length glyphsOut)
      glyf' = B.concat glyphsOut
      loca' = B.concat (map be32 offsets)
      nH' = min nH newN
      lsbOf g
        | g < nH = i16 hm (4 * g + 2)
        | otherwise = i16 hm (4 * nH + 2 * (g - nH))
      hmtx' = B.concat $
        [ if IS.member g kept then B.take 4 (B.drop (4 * g) hm) else be16 0 <> be16 0 | g <- [0 .. nH' - 1] ]
          ++ [ be16 (if IS.member g kept then lsbOf g else 0) | g <- [nH' .. newN - 1] ]
      head' = patch 8 (be32 0) (patch 50 (be16 1) hd)
      hhea' = patch 34 (be16 nH') hh
      maxp' = patch 4 (be16 newN) mx
      tables =
        [ ("glyf", glyf'), ("head", head'), ("hhea", hhea'), ("hmtx", hmtx')
        , ("loca", loca'), ("maxp", maxp') ]
          ++ [ (t, b) | t <- ["cvt ", "fpgm", "prep"], Just b <- [optTable t] ]
  pure (writeSfnt tables, kept)
  where
    check True _ = Right ()
    check False e = Left e

-- | The glyph ids a glyph of a font file is built from, when it is a
-- composite glyph (none for a simple glyph, or a glyph id out of range).
trueTypeGlyphComponents :: B.ByteString -> Int -> [Int]
trueTypeGlyphComponents bs g = either (const []) id $ do
  let numTables = u16 bs 4
      dir = M.fromList
        [ (C.unpack (B.take 4 (B.drop o bs)), (u32 bs (o + 8), u32 bs (o + 12)))
        | i <- [0 .. numTables - 1], let o = 12 + 16 * i, o + 16 <= B.length bs ]
      table t = maybe (Left t) (\(off, len) -> Right (B.take len (B.drop off bs))) (M.lookup t dir)
  hd <- table "head"
  mx <- table "maxp"
  lc <- table "loca"
  gl <- table "glyf"
  let locaAt i = if i16 hd 50 /= 0 then u32 lc (4 * i) else 2 * u16 lc (2 * i)
  if g < 0 || g >= u16 mx 4
    then Right []
    else Right (glyphComponents (B.take (locaAt (g + 1) - locaAt g) (B.drop (locaAt g) gl)))

-- | The glyphs reachable from a set through a successor function.
closure :: (Int -> [Int]) -> Int -> IS.IntSet -> IS.IntSet
closure next limit start = go IS.empty (filter ok (IS.toList start))
  where
    ok g = g >= 0 && g < limit
    go seen [] = seen
    go seen (g : rest)
      | IS.member g seen = go seen rest
      | otherwise = go (IS.insert g seen) (filter ok (next g) ++ rest)

-- | The glyph ids a composite glyph is built from (none for a simple one).
glyphComponents :: B.ByteString -> [Int]
glyphComponents g
  | B.length g >= 10 && i16 g 0 < 0 = go 10
  | otherwise = []
  where
    go o
      | o + 4 > B.length g = []
      | otherwise =
          let flags = u16 g o
              args = if flags .&. 0x0001 /= 0 then 4 else 2
              scale
                | flags .&. 0x0008 /= 0 = 2
                | flags .&. 0x0040 /= 0 = 4
                | flags .&. 0x0080 /= 0 = 8
                | otherwise = 0
           in u16 g (o + 2) : if flags .&. 0x0020 /= 0 then go (o + 4 + args + scale) else []

-- | An sfnt file from its tables: directory sorted by tag, tables 4-byte
-- aligned, checksums and @head.checkSumAdjustment@ set.
writeSfnt :: [(String, B.ByteString)] -> B.ByteString
writeSfnt tables0 = patchAdjustment file
  where
    tables = sortOn fst tables0
    n = length tables
    entrySelector = floor (logBase 2 (fromIntegral n :: Double)) :: Int
    searchRange = 16 * 2 ^ entrySelector
    header = be32 0x00010000 <> be16 n <> be16 searchRange <> be16 entrySelector <> be16 (16 * n - searchRange)
    dirSize = 12 + 16 * n
    offsets = scanl (+) dirSize [B.length (pad4 b) | (_, b) <- tables]
    dirEntries = B.concat
      [ C.pack t <> be32 (checksum b) <> be32 off <> be32 (B.length b)
      | ((t, b), off) <- zip tables offsets ]
    file = B.concat (header : dirEntries : [pad4 b | (_, b) <- tables])
    headOffset = lookup "head" (zip (map fst tables) offsets)
    patchAdjustment f = case headOffset of
      Just off -> patch (off + 8) (be32 ((0xB1B0AFBA - checksum f) .&. 0xFFFFFFFF)) f
      Nothing -> f

checksum :: B.ByteString -> Int
checksum b = foldl' (\acc i -> (acc + u32 p i) .&. 0xFFFFFFFF) 0 [0, 4 .. B.length p - 4]
  where p = pad4 b

pad4 :: B.ByteString -> B.ByteString
pad4 b = b <> B.replicate ((4 - B.length b `mod` 4) `mod` 4) 0

patch :: Int -> B.ByteString -> B.ByteString -> B.ByteString
patch off new b = B.take off b <> new <> B.drop (off + B.length new) b

be16 :: Int -> B.ByteString
be16 x = B.pack [fromIntegral (x `shiftR` 8), fromIntegral x]

be32 :: Int -> B.ByteString
be32 x = B.pack [fromIntegral (x `shiftR` 24), fromIntegral (x `shiftR` 16), fromIntegral (x `shiftR` 8), fromIntegral x]

utf16Hex :: Int -> String
utf16Hex c
  | c < 0x10000 = printf "%04X" c
  | otherwise =
      let c' = c - 0x10000
          hi = 0xD800 + (c' `shiftR` 10) :: Int
          lo = 0xDC00 + (c' .&. 0x3FF) :: Int
       in printf "%04X%04X" hi lo

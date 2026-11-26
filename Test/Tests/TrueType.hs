{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
-- | TrueType fonts: parsing, subsets, text with two-byte glyph codes
module Tests.TrueType (htf_thisModulesTests) where

import Test.Framework
import Tests.Common
import Graphics.PDF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IS
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Exception (evaluate)
import Data.Char (isUpper, ord)
import Data.Either (isRight)
import Text.Printf (printf)

test_subsetKeepsGlyphsAndIds :: IO ()
test_subsetKeepsGlyphsAndIds = withTestFont $ \bytes d -> do
  let wanted = IS.fromList (map (glyphOf d) sampleText)
  case subsetTrueType bytes wanted of
    Left e -> assertFailure e
    Right (sub, kept) -> do
      assertBool (BS.length sub < BS.length bytes `div` 4)
      assertBool (IS.member 0 kept)
      assertBool (wanted `IS.isSubsetOf` kept)
      -- every composite glyph kept has its components kept
      assertBool (and [ IS.member c kept | g <- IS.toList kept, c <- trueTypeGlyphComponents bytes g ])
      -- glyph ids and advances are what they were, the font ends after
      -- the last glyph kept, and outlines are copied as they are
      assertEqual (Just (IS.findMax kept + 1)) (fmap (`u16` 4) (sfntTable "maxp" sub))
      assertEqual [Just (round (glyphWidth d 1000 (fromIntegral g) * fromIntegral (unitsPerEm bytes) / 1000) :: Int) | g <- IS.toList kept]
                  [advanceOf sub g | g <- IS.toList kept]
      assertEqual [trueTypeGlyphComponents bytes g | g <- IS.toList kept]
                  [trueTypeGlyphComponents sub g | g <- IS.toList kept]

test_subsetClosesOverComposites :: IO ()
test_subsetClosesOverComposites = withTestFont $ \bytes d -> do
  let composites = [ (g, cs) | c <- ['À' .. 'ž'] ++ ['Ѐ' .. 'ӿ'], let g = glyphOf d c, g /= 0
                             , let cs = trueTypeGlyphComponents bytes g, not (null cs) ]
  case composites of
    [] -> unitTestPending "the test font has no composite glyph in Latin-1/Latin Extended-A/Cyrillic"
    (g, cs) : _ -> case subsetTrueType bytes (IS.singleton g) of
      Left e -> assertFailure e
      Right (_, kept) -> assertBool (all (`IS.member` kept) (g : cs))

test_subsetFontInDocument :: IO ()
test_subsetFontInDocument = withTestFont $ \_ d -> do
  let doc mode = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFontWith mode d
        page <- addPage Nothing
        drawWithPage page $ drawText $ text (PDFFont font 12) 10 100 (T.pack sampleText)
      subset = doc EmbedSubset
      whole = doc EmbedWhole
      name = C.pack (trueTypePostScriptName d)
      afterKey pdf = [ C.takeWhile (\c -> c /= ' ' && c /= '\n' && c /= '/' && c /= '>') (C.drop 11 (C.drop i pdf))
                     | i <- occurrences (C.pack "/BaseFont /") pdf ]
      isTagged n = C.length n > 7 && C.all isUpper (C.take 6 n) && C.index n 6 == '+' && C.drop 7 n == name
  assertBool (BS.length subset < BS.length whole `div` 4)
  -- the BaseFont of the Type0 and CID fonts carry an ABCDEF+ subset tag
  let subsetNames = afterKey subset
  assertEqual 2 (length subsetNames)
  assertBool (all isTagged subsetNames)
  assertEqual [name, name] (afterKey whole)
  -- the ToUnicode CMap maps the glyphs shown, and only those
  let cmapLines = filter (C.isPrefixOf (C.pack "<")) (C.lines subset)
      bfchars = [ l | l <- cmapLines, C.count '<' l == 2, C.length l <= 20 ]
  assertBool (C.pack (printf "<%04X> <%04X>" (glyphOf d 'П') (ord 'П')) `elem` bfchars)
  assertBool (length bfchars < 40)

-- | Text set by the typesetting code reaches the subset too (words and the
-- space glyphs of the glue).
test_subsetFontTypeset :: IO ()
test_subsetFontTypeset = withTestFont $ \_ d -> do
  let pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFont d
        page <- addPage Nothing
        drawWithPage page $
          displayFormattedText (Rectangle (10 :+ 10) (290 :+ 190)) NormalParagraph (Font (PDFFont font 12) black black) $
            paragraph (txt (T.pack "Съешь же ещё этих мягких французских булок, да выпей чаю."))
      bfchars = [ l | l <- C.lines pdf, C.isPrefixOf (C.pack "<") l, C.count '<' l == 2, C.length l <= 20 ]
      mapped c = C.pack (printf "<%04X> <%04X>" (glyphOf d c) (ord c)) `elem` bfchars
  assertBool (all mapped ("Съешьжеёхфиз ,." :: String))

-- PDF/A and associated files --------------------------------------------

-- Two fonts with the same PostScript name are two fonts of the document.
test_fontsWithTheSameName :: IO ()
test_fontsWithTheSameName = withTestFont $ \_ d -> do
  let pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        f1 <- mkTrueTypeFont d
        f2 <- mkTrueTypeFont d
        page <- addPage Nothing
        drawWithPage page $ do
          drawText $ text (PDFFont f1 12) 10 100 (T.pack "abc")
          drawText $ text (PDFFont f2 12) 10 50 (T.pack "xyz")
      fontsSet = [ C.takeWhile (/= ' ') l | l <- C.lines pdf, C.pack " Tf" `BS.isSuffixOf` l ]
      bfchars = [ l | l <- C.lines pdf, C.isPrefixOf (C.pack "<") l, C.count '<' l == 2, C.length l <= 20 ]
      mapped c = length (filter (== C.pack (printf "<%04X> <%04X>" (glyphOf d c) (ord c))) bfchars)
  assertEqual 2 (length (occurrences (C.pack "/Subtype /Type0") pdf))
  assertEqual 2 (S.size (S.fromList fontsSet))
  -- each subset has the glyphs of its own text only
  assertEqual [1, 1] (map mapped "ax")

-- A text object can show glyphs with the font of the text object before it.
-- See Note [Font of a text object] in Graphics.PDF.Text
test_fontOfThePreviousTextObject :: IO ()
test_fontOfThePreviousTextObject = withTestFont $ \_ d -> do
  let pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFont d
        page <- addPage Nothing
        let pf = PDFFont font 12
        drawWithPage page $ do
          withNewContext $ drawText $ text pf 10 150 (T.pack "q")
          drawText $ text pf 10 100 (T.pack "ab")
          drawText $ textStart 10 50 >> displayGlyphs (pdfGlyph pf (T.pack "XYZ"))
      bfchars = [ l | l <- C.lines pdf, C.isPrefixOf (C.pack "<") l, C.count '<' l == 2, C.length l <= 20 ]
      mapped c = C.pack (printf "<%04X> <%04X>" (glyphOf d c) (ord c)) `elem` bfchars
  assertBool (all mapped ("qabXYZ" :: String))

-- A control character and a soft hyphen show no glyph with a TrueType font.
test_charactersThatAreNotShown :: IO ()
test_charactersThatAreNotShown = withTestFont $ \_ d -> do
  let pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFont d
        page <- addPage Nothing
        drawWithPage page $ do
          drawText $ text (PDFFont font 12) 10 150 (T.pack "a\nb\173c\td")
          displayFormattedText (Rectangle (10 :+ 10) (290 :+ 100)) NormalParagraph (Font (PDFFont font 12) black black) $
            paragraph (txt (T.pack "extra\173ordinary\ntext"))
      hex :: String -> String
      hex = concatMap (printf "%04X" . glyphOf d)
      shown = filter (\l -> C.pack " Tj" `BS.isSuffixOf` l || C.pack " TJ" `BS.isSuffixOf` l) (C.lines pdf)
  assertBool (any (C.pack ("<" ++ hex "abcd" ++ ">") `BS.isInfixOf`) shown)
  assertBool (any (C.pack (hex "extraordinary") `BS.isInfixOf`) shown)
  assertBool (not (any (C.pack "<0000" `BS.isInfixOf`) shown))
  assertBool (not (any (C.pack (printf "%04X" (glyphOf d '\173')) `BS.isInfixOf`) (filter (C.pack "extra" `BS.isInfixOf`) shown)))

-- A font whose loca table is not in increasing order is not subset: the
-- whole font is embedded.
test_locaNotIncreasing :: IO ()
test_locaNotIncreasing = withTestFont $ \bytes d -> do
  let Just headTable = sfntTable "head" bytes
      long = u16 headTable 50 /= 0
      Just loca = sfntTable "loca" bytes
      -- the offset of glyph 2 becomes 0
      loca' = if long then BS.concat [BS.take 8 loca, BS.replicate 4 0, BS.drop 12 loca]
                      else BS.concat [BS.take 4 loca, BS.replicate 2 0, BS.drop 6 loca]
      bad = withTable "loca" loca' bytes
  assertBool (isRight (subsetTrueType bytes (IS.singleton (glyphOf d 'A'))))
  assertBool (not (isRight (subsetTrueType bad (IS.singleton (glyphOf d 'A')))))
  case parseTrueTypeFont bad of
    Left e -> assertFailure e
    Right d' -> do
      let pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
            font <- mkTrueTypeFont d'
            page <- addPage Nothing
            drawWithPage page $ drawText $ text (PDFFont font 12) 10 100 (T.pack "A")
      -- no subset tag
      assertBool (C.pack ("/BaseFont /" ++ trueTypePostScriptName d') `BS.isInfixOf` pdf)

-- The PostScript name keeps ASCII letters only, and a cmap that gives
-- glyph 0 to a character does not give the character to the font.
test_fontNameAndGlyphZero :: IO ()
test_fontNameAndGlyphZero = withTestFont $ \bytes d0 -> do
  let be16 x = BS.pack [fromIntegral (x `div` 256), fromIntegral x]
      be32 x = be16 (x `div` 65536) <> be16 (x `mod` 65536)
      -- "Foo", U+0125 (its low byte is '%'), "Bar", U+5B8B
      psName = BS.concat (map be16 [0x46, 0x6F, 0x6F, 0x125, 0x42, 0x61, 0x72, 0x5B8B])
      nameTable = BS.concat [be16 0, be16 1, be16 18, be16 3, be16 1, be16 0x409, be16 6, be16 (BS.length psName), be16 0, psName]
      groups = [(0x41, 0x41, 0), (0x42, 0x42, glyphOf d0 'B')]
      subtable = BS.concat $ [be16 12, be16 0, be32 (16 + 12 * length groups), be32 0, be32 (length groups)]
                   ++ [BS.concat [be32 s, be32 e, be32 g] | (s, e, g) <- groups]
      cmap = BS.concat [be16 0, be16 1, be16 3, be16 10, be32 12, subtable]
  case parseTrueTypeFont (withTable "name" nameTable (withTable "cmap" cmap bytes)) of
    Left e -> assertFailure e
    Right d -> do
      assertEqual "FooBar" (trueTypePostScriptName d)
      assertEqual "B" (trueTypeCharacters d)

-- Each character code of a cmap is read one time at most: this font has
-- 20000 groups that are the whole code space.
test_cmapRangesAreBounded :: IO ()
test_cmapRangesAreBounded = withTestFont $ \bytes d0 -> do
  let be16 x = BS.pack [fromIntegral (x `div` 256), fromIntegral x]
      be32 x = be16 (x `div` 65536) <> be16 (x `mod` 65536)
      groups = 20000
      subtable = BS.concat $ [be16 12, be16 0, be32 (16 + 12 * groups), be32 0, be32 groups]
                   ++ replicate groups (BS.concat [be32 0x41, be32 0x10FFFF, be32 (glyphOf d0 'A')])
      cmap = BS.concat [be16 0, be16 1, be16 3, be16 10, be32 12, subtable]
  case parseTrueTypeFont (withCmap cmap bytes) of
    Left e -> assertFailure e
    Right d -> do
      assertEqual (glyphOf d0 'A') (glyphOf d 'A')
      assertEqual (glyphOf d0 'A' + 1) (glyphOf d 'B')

-- A malformed font gives Left (or a font), and never an exception.
test_malformedFontsDoNotThrow :: IO ()
test_malformedFontsDoNotThrow = do
  -- 64 tables declared in a file of 40 bytes
  let short = BS.concat [BS.pack [0, 1, 0, 0, 0, 64], BS.replicate 34 0]
  assertBool (not (isRight (parseTrueTypeFont short)))
  assertBool (not (isRight (subsetTrueType short (IS.singleton 1))))
  assertEqual [] (trueTypeGlyphComponents short 1)
  withTestFont $ \bytes d -> do
    let n = BS.length bytes
        cuts = [0 .. 64] ++ [65, 65 + max 1 (n `div` 150) .. n - 1]
        g = glyphOf d 'A'
        total b = case parseTrueTypeFont b of
          Left e -> length e
          Right d' -> glyphOf d' 'A' + round (glyphWidth d' 1000 (charGlyph d' 'A'))
        total_subset b = either length (BS.length . fst) (subsetTrueType b (IS.singleton g))
                           + length (trueTypeGlyphComponents b g)
    mapM_ (\k -> let b = BS.take k bytes in evaluate (total b + total_subset b)) cuts
    -- each table in turn made short: its length in the table directory is
    -- changed, the rest of the font is as it was
    let be32 x = BS.pack (map (fromIntegral . (x `div`)) [16777216, 65536, 256, 1])
        shortTable i keep = BS.concat [BS.take (12 + 16 * i + 12) bytes, be32 keep, BS.drop (12 + 16 * i + 16) bytes]
        shorts = [ shortTable i keep | i <- [0 .. u16 bytes 4 - 1], keep <- [0, 3, 13, 40], keep < u32 bytes (12 + 16 * i + 12) ]
    mapM_ (\b -> evaluate (total b + total_subset b)) shorts
    -- a short head, hhea, maxp, hmtx or cmap table is not a font
    let required = map C.pack ["head", "hhea", "maxp", "cmap"]
    assertBool (and [ not (isRight (parseTrueTypeFont (shortTable i 3)))
                    | i <- [0 .. u16 bytes 4 - 1], BS.take 4 (BS.drop (12 + 16 * i) bytes) `elem` required ])

-- The glyphs of a subset keep their advance when the hmtx table has fewer
-- entries than glyphs.
test_subsetKeepsSharedAdvance :: IO ()
test_subsetKeepsSharedAdvance = withTestMonoFont $ \bytes d -> do
  let wanted = IS.fromList (map (glyphOf d) (filter (/= ' ') sampleText))
      Just entries = fmap (`u16` 34) (sfntTable "hhea" bytes)
  -- the glyphs take the shared advance, and the glyph of the last entry is
  -- not in the subset, or the test shows nothing
  assertBool (IS.findMin wanted >= entries)
  case subsetTrueType bytes wanted of
    Left e -> assertFailure e
    Right (sub, kept) ->
      assertEqual [advanceOf bytes g | g <- IS.toList kept] [advanceOf sub g | g <- IS.toList kept]

-- A font with no space character: the glue between two words must not
-- show glyph 0.
test_glueWithoutSpaceGlyph :: IO ()
test_glueWithoutSpaceGlyph = withTestFont $ \bytes d0 -> do
  let typeset d = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFont d
        page <- addPage Nothing
        drawWithPage page $
          displayFormattedText (Rectangle (10 :+ 10) (290 :+ 190)) NormalParagraph (Font (PDFFont font 12) black black) $
            paragraph (txt (T.pack "HPDF HPDF HPDF"))
      space = C.pack (printf "<%04X>" (glyphOf d0 ' '))
  -- with the space character: the glue shows the space glyph
  assertBool (space `BS.isInfixOf` typeset d0)
  case parseTrueTypeFont (withOnlyCharacters "HPDF" d0 bytes) of
    Left e -> assertFailure e
    Right d -> do
      assertEqual 0 (glyphOf d ' ')
      assertEqual (glyphOf d0 'H') (glyphOf d 'H')
      -- the width of the glue: the advance of the space glyph, or a quarter
      -- of the font size and not the advance of glyph 0
      assertEqual (glyphWidth d0 12 (spaceGlyph d0)) (spaceAdvance d0 12)
      assertEqual 3 (spaceAdvance d 12)
      assertBool (glyphWidth d 12 0 /= 3)
      -- the text: the lines that show glyphs
      let shown = filter (C.pack " TJ" `BS.isSuffixOf`) (C.lines (typeset d))
      assertBool (not (null shown))
      assertBool (all (C.pack (printf "%04X" (glyphOf d 'H')) `BS.isInfixOf`) shown)
      assertBool (not (any (C.pack "<0000>" `BS.isInfixOf`) shown))
      assertBool (not (any (space `BS.isInfixOf`) shown))

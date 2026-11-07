{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework
import Data.Either (isRight)

import Graphics.PDF.Image(JpegFile, readJpegDataURL, jpegBounds)
import Graphics.PDF
import Graphics.PDF.Archive
import Graphics.PDF.Fonts.TrueType
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IS
import qualified Data.Text as T
import Control.Monad (filterM)
import Data.Char (isUpper, ord)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Text.Printf (printf)

main = htfMain htf_thisModulesTests

raw_jpeg :: String
raw_jpeg = "data:image/jpeg;base64,/9j/4AAQSkZJRgABAQEAYABgAAD/4RDcRXhpZgAATU0AKgAAAAgABAE7AAIAAAAGAAAISodpAAQAAAABAAAIUJydAAEAAAAMAAAQyOocAAcAAAgMAAAAPgAAAAAc6gAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAFNjb3R0AAAFkAMAAgAAABQAABCekAQAAgAAABQAABCykpEAAgAAAAMwNwAAkpIAAgAAAAMwNwAA6hwABwAACAwAAAiSAAAAABzqAAAACAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAxNDoxMjowNiAwOTo0Mzo1MQAyMDE0OjEyOjA2IDA5OjQzOjUxAAAAUwBjAG8AdAB0AAAA/+ELGGh0dHA6Ly9ucy5hZG9iZS5jb20veGFwLzEuMC8APD94cGFja2V0IGJlZ2luPSfvu78nIGlkPSdXNU0wTXBDZWhpSHpyZVN6TlRjemtjOWQnPz4NCjx4OnhtcG1ldGEgeG1sbnM6eD0iYWRvYmU6bnM6bWV0YS8iPjxyZGY6UkRGIHhtbG5zOnJkZj0iaHR0cDovL3d3dy53My5vcmcvMTk5OS8wMi8yMi1yZGYtc3ludGF4LW5zIyI+PHJkZjpEZXNjcmlwdGlvbiByZGY6YWJvdXQ9InV1aWQ6ZmFmNWJkZDUtYmEzZC0xMWRhLWFkMzEtZDMzZDc1MTgyZjFiIiB4bWxuczpkYz0iaHR0cDovL3B1cmwub3JnL2RjL2VsZW1lbnRzLzEuMS8iLz48cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0idXVpZDpmYWY1YmRkNS1iYTNkLTExZGEtYWQzMS1kMzNkNzUxODJmMWIiIHhtbG5zOnhtcD0iaHR0cDovL25zLmFkb2JlLmNvbS94YXAvMS4wLyI+PHhtcDpDcmVhdGVEYXRlPjIwMTQtMTItMDZUMDk6NDM6NTEuMDcyPC94bXA6Q3JlYXRlRGF0ZT48L3JkZjpEZXNjcmlwdGlvbj48cmRmOkRlc2NyaXB0aW9uIHJkZjphYm91dD0idXVpZDpmYWY1YmRkNS1iYTNkLTExZGEtYWQzMS1kMzNkNzUxODJmMWIiIHhtbG5zOmRjPSJodHRwOi8vcHVybC5vcmcvZGMvZWxlbWVudHMvMS4xLyI+PGRjOmNyZWF0b3I+PHJkZjpTZXEgeG1sbnM6cmRmPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5LzAyLzIyLXJkZi1zeW50YXgtbnMjIj48cmRmOmxpPlNjb3R0PC9yZGY6bGk+PC9yZGY6U2VxPg0KCQkJPC9kYzpjcmVhdG9yPjwvcmRmOkRlc2NyaXB0aW9uPjwvcmRmOlJERj48L3g6eG1wbWV0YT4NCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgCiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAKICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICAgIAogICAgICAgICAgICAgICAgICAgICAgICAgICAgPD94cGFja2V0IGVuZD0ndyc/Pv/bAEMABwUFBgUEBwYFBggHBwgKEQsKCQkKFQ8QDBEYFRoZGBUYFxseJyEbHSUdFxgiLiIlKCkrLCsaIC8zLyoyJyorKv/bAEMBBwgICgkKFAsLFCocGBwqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKioqKv/AABEIABMAEwMBIgACEQEDEQH/xAAfAAABBQEBAQEBAQAAAAAAAAAAAQIDBAUGBwgJCgv/xAC1EAACAQMDAgQDBQUEBAAAAX0BAgMABBEFEiExQQYTUWEHInEUMoGRoQgjQrHBFVLR8CQzYnKCCQoWFxgZGiUmJygpKjQ1Njc4OTpDREVGR0hJSlNUVVZXWFlaY2RlZmdoaWpzdHV2d3h5eoOEhYaHiImKkpOUlZaXmJmaoqOkpaanqKmqsrO0tba3uLm6wsPExcbHyMnK0tPU1dbX2Nna4eLj5OXm5+jp6vHy8/T19vf4+fr/xAAfAQADAQEBAQEBAQEBAAAAAAAAAQIDBAUGBwgJCgv/xAC1EQACAQIEBAMEBwUEBAABAncAAQIDEQQFITEGEkFRB2FxEyIygQgUQpGhscEJIzNS8BVictEKFiQ04SXxFxgZGiYnKCkqNTY3ODk6Q0RFRkdISUpTVFVWV1hZWmNkZWZnaGlqc3R1dnd4eXqCg4SFhoeIiYqSk5SVlpeYmZqio6Slpqeoqaqys7S1tre4ubrCw8TFxsfIycrS09TV1tfY2dri4+Tl5ufo6ery8/T19vf4+fr/2gAMAwEAAhEDEQA/APoH+3tJOvf2INRtjqnlecbMSDzAn97b1xXB/Er4rN4ev4fC3g62Gr+Lb0hIbZBuW2z/ABSe+OcenJwOvi3xBk8W+G/2l9SufCVrO+qaioWzxCX8xXhVSy54O0g89AV56V7d8J/hVD4FspNT1iX+0PE2oAveXrtvKZOSik84z1PVj+ApRXPFSe3X/L/Njk+WTS36f5v9Ed7pS3q6PZrqzRvfiBBctEPlMm0biPbOaKtUVTd3clKysIVBYMQMjocdKWiikMKKKKAP/9k="
raw_jpeg_bounds :: (Int, Int)
raw_jpeg_bounds = (19, 19)

jpg :: JpegFile
jpg = jpegFile
    where Right jpegFile = readJpegDataURL raw_jpeg

test_decodeTick :: IO()
test_decodeTick = case (readJpegDataURL raw_jpeg) of
                    Right _ -> assertBool True 
                    Left msg -> assertEqual "Error:" msg

test_bounds :: IO()
test_bounds =  assertEqual raw_jpeg_bounds (jpegBounds jpg)

-- Embedded TrueType fonts --------------------------------------------------
--
-- These need a TrueType font on the machine: $HPDF_TEST_TTF, or one of the
-- usual system paths. Without one they are reported pending.

testFontPaths :: [FilePath]
testFontPaths =
  [ "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
  , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/TTF/DejaVuSans.ttf"
  , "/usr/share/fonts/dejavu/DejaVuSans.ttf"
  ]

withTestFont :: (B.ByteString -> TrueTypeFontData -> IO ()) -> IO ()
withTestFont k = do
  env <- lookupEnv "HPDF_TEST_TTF"
  found <- filterM doesFileExist (maybe id (:) env testFontPaths)
  case found of
    [] -> unitTestPending "no TrueType font found (set HPDF_TEST_TTF)"
    path : _ -> do
      bytes <- B.readFile path
      case parseTrueTypeFont bytes of
        Left e -> assertFailure ("cannot parse " ++ path ++ ": " ++ e)
        Right d -> k bytes d

glyphOf :: TrueTypeFontData -> Char -> Int
glyphOf d c = fromIntegral (charGlyph d c)

sampleText :: String
sampleText = "Привіт, світ! Ça déçoit."

test_subsetKeepsGlyphsAndIds :: IO ()
test_subsetKeepsGlyphsAndIds = withTestFont $ \bytes d -> do
  let wanted = IS.fromList (map (glyphOf d) sampleText)
  case subsetTrueType bytes wanted of
    Left e -> assertFailure e
    Right (sub, kept) -> do
      assertBool (B.length sub < B.length bytes `div` 4)
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

-- A few raw sfnt reads, to look at a subset (which has no cmap, so
-- parseTrueTypeFont does not take it).
u16 :: B.ByteString -> Int -> Int
u16 b i = fromIntegral (B.index b i) * 256 + fromIntegral (B.index b (i + 1))

u32 :: B.ByteString -> Int -> Int
u32 b i = u16 b i * 65536 + u16 b (i + 2)

sfntTable :: String -> B.ByteString -> Maybe B.ByteString
sfntTable tag b =
  case [ (u32 b (o + 8), u32 b (o + 12)) | i <- [0 .. u16 b 4 - 1], let o = 12 + 16 * i, C.unpack (B.take 4 (B.drop o b)) == tag ] of
    (off, len) : _ -> Just (B.take len (B.drop off b))
    [] -> Nothing

unitsPerEm :: B.ByteString -> Int
unitsPerEm b = maybe 0 (`u16` 18) (sfntTable "head" b)

advanceOf :: B.ByteString -> Int -> Maybe Int
advanceOf b g = do
  hh <- sfntTable "hhea" b
  hm <- sfntTable "hmtx" b
  let n = u16 hh 34
  pure (u16 hm (4 * min g (n - 1)))

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
test_subsetFontInDocument = withTestFont $ \bytes d -> do
  let doc mode = BL.toStrict $ pdfByteString standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFontWith mode d
        page <- addPage Nothing
        drawWithPage page $ drawText $ text (PDFFont font 12) 10 100 (T.pack sampleText)
      subset = doc EmbedSubset
      whole = doc EmbedWhole
      name = C.pack (trueTypePostScriptName d)
      afterKey pdf = [ C.takeWhile (\c -> c /= ' ' && c /= '\n' && c /= '/' && c /= '>') (C.drop 11 (C.drop i pdf))
                     | i <- occurrences (C.pack "/BaseFont /") pdf ]
      isTagged n = C.length n > 7 && C.all isUpper (C.take 6 n) && C.index n 6 == '+' && C.drop 7 n == name
  assertBool (B.length subset < B.length whole `div` 4)
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

-- | Offsets of every occurrence of a needle.
occurrences :: B.ByteString -> B.ByteString -> [Int]
occurrences needle = go 0
  where
    go off hay = case B.breakSubstring needle hay of
      (before, rest)
        | B.null rest -> []
        | otherwise -> let i = off + B.length before
                        in i : go (i + 1) (B.drop 1 rest)

-- | Text set by the typesetting code reaches the subset too (words and the
-- space glyphs of the glue).
test_subsetFontTypeset :: IO ()
test_subsetFontTypeset = withTestFont $ \_ d -> do
  let pdf = BL.toStrict $ pdfByteString standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        font <- mkTrueTypeFont d
        page <- addPage Nothing
        drawWithPage page $
          displayFormattedText (Rectangle (10 :+ 10) (290 :+ 190)) NormalParagraph (Font (PDFFont font 12) black black) $
            paragraph (txt (T.pack "Съешь же ещё этих мягких французских булок, да выпей чаю."))
      bfchars = [ l | l <- C.lines pdf, C.isPrefixOf (C.pack "<") l, C.count '<' l == 2, C.length l <= 20 ]
      mapped c = C.pack (printf "<%04X> <%04X>" (glyphOf d c) (ord c)) `elem` bfchars
  assertBool (all mapped ("Съешьжеёхфиз ,." :: String))

-- PDF/A and associated files --------------------------------------------

-- | A stand-in for an ICC profile: the tests read the PDF structure, not
-- the profile.
fakeIcc :: BL.ByteString
fakeIcc = BL.fromStrict (C.pack "not really an ICC profile")

exampleExtension :: XMPExtension
exampleExtension = XMPExtension (T.pack "Example schema") (T.pack "http://example.org/ns/") (T.pack "ex")
  [XMPProperty (T.pack "Level") (T.pack "Text") (T.pack "external") (T.pack "A level") (T.pack "BASIC")]

pdfaDoc :: AnyFont -> PDF ()
pdfaDoc font = do
  setPDFA (pdfa3b fakeIcc) { pdfaTitle = T.pack "Facture 42"
                           , pdfaExtensions = [exampleExtension] }
  img <- createPDFRawImageFromByteString 1 1 True NoFilter (BL.pack [255, 0, 0])
  page <- addPage Nothing
  drawWithPage page $ do
    drawText $ text (PDFFont font 12) 10 100 (T.pack "Ça déçoit.")
    drawXObject img

test_pdfaStructure :: IO ()
test_pdfaStructure = withTestFont $ \_ d -> do
  let info = standardDocInfo {compressed = False, author = T.pack "Salmon"}
      Right pdf = fmap BL.toStrict . pdfByteStringE info (PDFRect 0 0 300 200) $
        mkTrueTypeFont d >>= pdfaDoc
      has s = assertBool' s (C.pack s `B.isInfixOf` pdf)
      assertBool' s b = if b then return () else assertFailure ("missing: " ++ s)
  -- header with a binary comment
  assertBool (C.pack "%PDF-1.7\n%\xE2\xE3\xCF\xD3\n" `B.isPrefixOf` pdf)
  mapM_ has
    [ "/Type /Metadata", "/Subtype /XML", "<pdfaid:part>3</pdfaid:part>"
    , "<pdfaid:conformance>B</pdfaid:conformance>"
    , "<ex:Level>BASIC</ex:Level>", "<pdfaSchema:prefix>ex</pdfaSchema:prefix>"
    , "<dc:creator><rdf:Seq><rdf:li>Salmon</rdf:li></rdf:Seq></dc:creator>"
    , "/S /GTS_PDFA1", "/OutputIntents", "/DestOutputProfile"
    , "/Title (Facture 42)", "/Author (Salmon)", "/ID [<", "/Interpolate false" ]
  assertBool (not (C.pack "/Interpolate true" `B.isInfixOf` pdf))
  -- the metadata stream is not compressed
  assertBool (not (C.pack "/Filter" `B.isInfixOf` C.takeWhile (/= 's') (snd (B.breakSubstring (C.pack "/Subtype /XML") pdf))))

test_pdfaDeterministic :: IO ()
test_pdfaDeterministic = withTestFont $ \_ d -> do
  let render = pdfByteStringE standardDocInfo (PDFRect 0 0 300 200) (mkTrueTypeFont d >>= pdfaDoc)
  assertEqual (render) (render)
  assertBool (isRight render)

test_pdfaRefusesStandardFonts :: IO ()
test_pdfaRefusesStandardFonts = do
  Right helvetica <- mkStdFont Helvetica
  case pdfByteStringE standardDocInfo (PDFRect 0 0 300 200) (pdfaDoc helvetica) of
    Left [problem] -> assertBool ("Helvetica" `elem` words problem)
    other -> assertFailure ("expected one problem, got " ++ either show (const "a document") other)

-- A document that does not ask for PDF/A is written as before: PDF 1.5,
-- the Info dictionary in the trailer, no /ID.
test_plainDocumentUnchanged :: IO ()
test_plainDocumentUnchanged = do
  let pdf = BL.toStrict $ pdfByteString standardDocInfo (PDFRect 0 0 300 200) (addPage Nothing >> return ())
  assertBool (C.pack "%PDF-1.5\n" `B.isPrefixOf` pdf)
  assertBool (C.pack "/Info <<" `B.isInfixOf` pdf)
  assertBool (not (C.pack "/ID [" `B.isInfixOf` pdf))
  assertBool (not (C.pack "/Metadata" `B.isInfixOf` pdf))

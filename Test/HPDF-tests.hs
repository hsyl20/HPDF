{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Framework

import Graphics.PDF
import Graphics.PDF.Image(JpegFile, readJpegDataURL, jpegBounds)
import Graphics.PDF.Archive
import Graphics.PDF.Signature
import Graphics.PDF.Fonts.TrueType
import Graphics.PDF.Fonts.Font (charGlyph, getKern, glyphWidth, spaceAdvance, spaceGlyph)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (filterM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (isUpper, ord)
import Data.Either (isRight)
import Data.IORef
import Data.Word (Word8)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import System.Environment (lookupEnv)
import System.IO (hClose, openBinaryTempFile)
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

-- A Type 1 font that is not in the Adobe standard encoding.
-- The glyphs "alpha" and "beta" have the character codes 33 and 34.
type1_afm :: BS.ByteString
type1_afm = C.pack $ unlines
    [ "StartFontMetrics 2.0"
    , "FontName Test-Icons"
    , "EncodingScheme FontSpecific"
    , "FontBBox 0 -200 1000 800"
    , "CapHeight 700"
    , "Ascender 800"
    , "Descender -200"
    , "StdVW 80"
    , "StartCharMetrics 2"
    , "C 33 ; WX 600 ; N alpha ; B 0 0 600 700 ;"
    , "C 34 ; WX 700 ; N beta ; B 0 0 700 700 ;"
    , "EndCharMetrics"
    , "EndFontMetrics"
    ]

type1_pfb_clear, type1_pfb_binary, type1_pfb_trailer :: BS.ByteString
type1_pfb_clear = C.pack "%!PS-AdobeFont-1.0: Test-Icons\ncurrentfile eexec\n"
type1_pfb_binary = BS.pack [1, 2, 3, 4, 5, 6, 7, 8]
type1_pfb_trailer = C.pack "0000\ncleartomark\n"

-- A PFB file with one segment for each portion of the font program.
type1_pfb :: BS.ByteString
type1_pfb = BS.concat
    [ segment 1 type1_pfb_clear
    , segment 2 type1_pfb_binary
    , segment 1 type1_pfb_trailer
    , BS.pack [0x80, 3]
    ]
  where
    segment t d = BS.concat [BS.pack (0x80 : t : le32 (BS.length d)), d]
    le32 :: Int -> [Word8]
    le32 n = [fromIntegral (n `div` (256 ^ i) `mod` 256) | i <- [0 .. 3 :: Int]]

-- | The Type 1 test font, read from files made of the test data changed by
-- this function.
readType1Fixture :: (BS.ByteString -> BS.ByteString) -> IO Type1FontStructure
readType1Fixture change = do
    let encs = OtherEncoding (M.fromList [("alpha", '\xE000'), ("beta", '\xE001')])
    dir <- getTemporaryDirectory
    let pfb = dir ++ "/hpdf-test-type1.pfb"
        afmPath = dir ++ "/hpdf-test-type1.afm"
    BS.writeFile pfb (change type1_pfb)
    BS.writeFile afmPath (change type1_afm)
    Right fs <- readType1FontWithEncoding encs pfb afmPath
    removeFile pfb
    removeFile afmPath
    return fs

-- | A custom Type 1 font has a font descriptor, an encoding with the glyph
-- names and a font file without the PFB segment headers.
-- See Note [Type 1 font embedding] in Graphics.PDF.Fonts.Type1
test_type1FontDescriptor :: IO ()
test_type1FontDescriptor = do
    let rect = PDFRect 0 0 600 400
    fs <- readType1Fixture id
    let doc :: PDF ()
        doc = do
            font <- mkType1Font fs
            page <- addPage Nothing
            drawWithPage page $ drawText $ text (PDFFont font 12) 10 10 "\xE000\xE001"
        bs = BL.toStrict (renderPdf standardDocInfo{compressed = False} rect doc)
        has x = assertBool (C.pack x `BS.isInfixOf` bs)
        hasNot x = assertBool (not (C.pack x `BS.isInfixOf` bs))
    has "/Type /FontDescriptor"
    has "/FontName /Test-Icons"
    has "/StemV 80"
    has "/Differences [33 /alpha /beta ]"
    has "/Length1 49"
    has "/Length2 8"
    has "/Length3 17"
    hasNot "\x80\x01"
    hasNot "\x80\x02"

-- An AFM file from a font tool: CR LF line ends, real numbers, glyph names
-- with "_", "." and digits, ligature fields, keys that the parser does not
-- use, no EndFontMetrics.
tool_afm :: BS.ByteString
tool_afm = C.pack $ concatMap (++ "\r\n")
    [ "StartFontMetrics 2.0"
    , "Comment Generated by a font tool"
    , "FontName Test_Font-1.0"
    , "FullName Test Font"
    , "ItalicAngle -12.5"
    , "IsFixedPitch false"
    , "UnderlinePosition -130.5"
    , "EncodingScheme ISO10646-1"
    , "FontBBox -10.5 -200 1000.25 800"
    , "CapHeight 700.4"
    , "Ascender 800"
    , "Descender -200"
    , "StartCharMetrics 9"
    , "C 65 ; WX 600.4 ; N A ; B 0 0 600 700 ;"
    , "C 102 ; WX 300 ; N f ; B 0 0 300 700 ; L i f_i ; L 6 six.alt ;"
    , "C 128 ; WX 610 ; N uni0416 ; B 0 0 610 700 ;"
    , "C 129 ; WX 620 ; N u1F600 ; B 0 0 620 700 ;"
    , "CH <82> ; W0X 630 ; N f_i ; B 0 0 630 700 ;"
    , "C 131 ; WX 640 ; N six.alt ; B 0 0 640 700 ;"
    , "C 132 ; WX 650 ; N uni0416.alt ; B 0 0 650 700 ;"
    , "C 133 ; WX 660 ; N uni0041 ; B 0 0 660 700 ;"
    , "C -1 ; WX 250 ; N space ; B 0 0 0 0 ;"
    , "EndCharMetrics"
    , "StartKernData"
    , "StartKernPairs 3"
    , "KPX A uni0416 -50.2"
    , "KPX f_i six.alt -10"
    , "KPH <41> <66> -20"
    , "EndKernPairs"
    , "StartTrackKern 1"
    , "TrackKern 0 8 0 72 0"
    , "EndTrackKern"
    , "EndKernData"
    ]

withType1Afm :: BS.ByteString -> (Either String Type1FontStructure -> IO a) -> IO a
withType1Afm afmBytes act = do
    dir <- getTemporaryDirectory
    let pfb = dir ++ "/hpdf-test-afm.pfb"
        afmPath = dir ++ "/hpdf-test-afm.afm"
    BS.writeFile pfb type1_pfb
    BS.writeFile afmPath afmBytes
    r <- readType1Font pfb afmPath
    removeFile pfb
    removeFile afmPath
    act (either (Left . show) Right r)

-- The AFM parser reads the files that font tools write, and the glyph
-- names uniXXXX and uXXXX give their character.
-- See Note [AFM parser] and Note [Glyph names that give a Unicode value]
-- in Graphics.PDF.Fonts.AFMParser
test_afmFromFontTool :: IO ()
test_afmFromFontTool = withType1Afm tool_afm $ \r -> case r of
  Left e -> assertFailure e
  Right fs -> do
    ref <- newIORef Nothing
    bytes <- renderPdfT standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
      font <- mkType1Font fs
      page <- addPage Nothing
      drawWithPage page $ drawText $ text (PDFFont font 12) 10 10 (T.pack "A\x0416\x1F600")
      liftIO (writeIORef ref (Just font))
    Just font <- readIORef ref
    let pdf = BL.toStrict bytes
        has x = assertBoolVerbose x (C.pack x `BS.isInfixOf` pdf)
    -- the glyph list name "A" is used before uni0041
    assertEqual 65 (charGlyph font 'A')
    assertEqual 128 (charGlyph font '\x0416')
    assertEqual 129 (charGlyph font '\x1F600')
    -- a real number gives the nearest integer
    assertEqual 600 (glyphWidth font 1000 65)
    assertEqual 630 (glyphWidth font 1000 0x82)
    assertEqual (-50) (getKern font 1000 65 128)
    -- a glyph with no character code is not used
    assertBool (spaceGlyph font /= 65535)
    has "/BaseFont /Test_Font-1.0"
    has "/uni0416 /u1F600 /f_i /six.alt /uni0416.alt /uni0041 ]"
    has "/ItalicAngle -12.5"

-- A file that is not an AFM file gives Left, not an exception.
test_afmMalformed :: IO ()
test_afmMalformed = do
  let rejected afmBytes = withType1Afm afmBytes $ \r -> case r of
        Left e -> assertBool (not (null e))
        Right _ -> assertFailure "expected a parse error"
      accepted afmBytes = withType1Afm afmBytes (assertBool . isRight)
      replace old new b = let (h, t) = BS.breakSubstring (C.pack old) b
                          in BS.concat [h, C.pack new, BS.drop (length old) t]
  rejected BS.empty
  rejected (C.pack "not a font")
  rejected (BS.pack [0 .. 255])
  -- no character metric
  rejected (C.pack "StartFontMetrics 2.0\nFontName X\nEndFontMetrics\n")
  -- a character metric with a width that is not a number
  rejected (replace "WX 600 ;" "WX 6oo ;" type1_afm)
  -- a global key with a value that is not a number is ignored
  accepted (replace "CapHeight 700" "CapHeight 7,5" type1_afm)
  accepted type1_afm

-- | The PDF monad transformer runs base monad effects in order and generates
-- the same document as the pure PDF monad
test_pdft :: IO()
test_pdft = do
    ref <- newIORef ([] :: [String])
    let rect = PDFRect 0 0 600 400
        doc :: PDFT IO ()
        doc = do
            liftIO $ modifyIORef ref ("start":)
            page <- addPage Nothing
            img <- createPDFJpeg jpg
            liftIO $ modifyIORef ref ("image":)
            drawWithPage page $ drawXObject img
            liftPDF pureTail
            liftIO $ modifyIORef ref ("end":)
        pureTail :: PDF ()
        pureTail = do
            page <- addPage Nothing
            drawWithPage page $ return ()
        pureDoc :: PDF ()
        pureDoc = do
            page <- addPage Nothing
            img <- createPDFJpeg jpg
            drawWithPage page $ drawXObject img
            pureTail
    bs <- renderPdfT standardDocInfo rect doc
    effects <- readIORef ref
    assertEqual ["end","image","start"] effects
    assertBool (BL.isPrefixOf "%PDF-1.5" bs)
    assertEqual (renderPdf standardDocInfo rect pureDoc) bs

-- | The step interface builds the same document as the PDF monad, and
-- rendering an intermediate builder does not alter it
test_steps :: IO()
test_steps = do
    let rect = PDFRect 0 0 600 400
        addImagePage :: PDF ()
        addImagePage = do
            page <- addPage Nothing
            img <- createPDFJpeg jpg
            drawWithPage page $ drawXObject img
        addEmptyPage :: PDF ()
        addEmptyPage = do
            page <- addPage Nothing
            drawWithPage page $ return ()
        s0 = emptyPdfBuilder standardDocInfo rect
        ((), s1) = stepPDF addImagePage s0
        snapshot = buildPdf s1
    ((), s2) <- stepPDFT (liftPDF addEmptyPage) s1
    assertEqual (renderPdf standardDocInfo rect addImagePage) snapshot
    assertEqual (renderPdf standardDocInfo rect (addImagePage >> addEmptyPage))
                (buildPdf s2)
    -- the snapshot did not alter s1
    assertEqual snapshot (buildPdf s1)

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

-- | Monospaced fonts: their hmtx table has few entries, and most glyphs
-- share the advance of the last one.
testMonoFontPaths :: [FilePath]
testMonoFontPaths =
  [ "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
  , "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
  , "/usr/share/fonts/dejavu/DejaVuSansMono.ttf"
  ]

withTestFont :: (BS.ByteString -> TrueTypeFontData -> IO ()) -> IO ()
withTestFont = withFontFrom "HPDF_TEST_TTF" testFontPaths

withTestMonoFont :: (BS.ByteString -> TrueTypeFontData -> IO ()) -> IO ()
withTestMonoFont = withFontFrom "HPDF_TEST_TTF_MONO" testMonoFontPaths

withFontFrom :: String -> [FilePath] -> (BS.ByteString -> TrueTypeFontData -> IO ()) -> IO ()
withFontFrom var paths k = do
  env <- lookupEnv var
  found <- filterM doesFileExist (maybe id (:) env paths)
  ci <- lookupEnv "CI"
  case found of
    -- on a CI machine the tests must run: a font that is not there is a failure
    [] | ci /= Nothing -> assertFailure ("no TrueType font found (set " ++ var ++ ")")
       | otherwise -> unitTestPending ("no TrueType font found (set " ++ var ++ ")")
    path : _ -> do
      bytes <- BS.readFile path
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

-- A few raw sfnt reads, to look at a subset (which has no cmap, so
-- parseTrueTypeFont does not take it).
u16 :: BS.ByteString -> Int -> Int
u16 b i = fromIntegral (BS.index b i) * 256 + fromIntegral (BS.index b (i + 1))

u32 :: BS.ByteString -> Int -> Int
u32 b i = u16 b i * 65536 + u16 b (i + 2)

sfntTable :: String -> BS.ByteString -> Maybe BS.ByteString
sfntTable tag b =
  case [ (u32 b (o + 8), u32 b (o + 12)) | i <- [0 .. u16 b 4 - 1], let o = 12 + 16 * i, C.unpack (BS.take 4 (BS.drop o b)) == tag ] of
    (off, len) : _ -> Just (BS.take len (BS.drop off b))
    [] -> Nothing

unitsPerEm :: BS.ByteString -> Int
unitsPerEm b = maybe 0 (`u16` 18) (sfntTable "head" b)

advanceOf :: BS.ByteString -> Int -> Maybe Int
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

-- | Offsets of every occurrence of a needle.
occurrences :: BS.ByteString -> BS.ByteString -> [Int]
occurrences needle = go 0
  where
    go off hay = case BS.breakSubstring needle hay of
      (before, rest)
        | BS.null rest -> []
        | otherwise -> let i = off + BS.length before
                        in i : go (i + 1) (BS.drop 1 rest)

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

-- | A stand-in for an ICC profile: the tests read the PDF structure, not
-- the profile.
fakeIcc :: BL.ByteString
fakeIcc = fakeIccOf "RGB "

-- | An ICC header with this colour space signature, and nothing after it.
fakeIccOf :: String -> BL.ByteString
fakeIccOf space = BL.fromStrict (BS.concat [BS.replicate 16 0, C.pack space, BS.replicate 108 0])

exampleExtension :: XMPExtension
exampleExtension = XMPExtension (T.pack "Example schema") (T.pack "http://example.org/ns/") (T.pack "ex")
  [XMPProperty (T.pack "Level") (T.pack "Text") (T.pack "external") (T.pack "A level") (T.pack "BASIC")]

pdfaDoc :: AnyFont -> PDF ()
pdfaDoc font = do
  setPDFA (pdfa3b fakeIcc) { pdfaTitle = T.pack "Facture 42"
                           , pdfaExtensions = [exampleExtension, facturXExtension (T.pack "BASIC")] }
  _ <- embedAssociatedFile (associatedFile (T.pack "factur-x.xml") "text/xml" (BL.fromStrict (C.pack "<x/>")))
         { afRelationship = Alternative, afDescription = T.pack "Factur-X invoice"
         , afCreationDate = Just "D:20261106000000Z", afModDate = Just "D:20261106000000Z" }
  img <- createPDFRawImageFromByteString 1 1 True NoFilter (BL.pack [255, 0, 0])
  page <- addPage Nothing
  drawWithPage page $ do
    drawText $ text (PDFFont font 12) 10 100 (T.pack "Ça déçoit.")
    drawXObject img

test_pdfaStructure :: IO ()
test_pdfaStructure = withTestFont $ \_ d -> do
  let info = standardDocInfo {compressed = False, author = T.pack "Salmon"}
      Right pdf = fmap BL.toStrict . renderPdfE info (PDFRect 0 0 300 200) $
        mkTrueTypeFont d >>= pdfaDoc
      has s = assertBool' s (C.pack s `BS.isInfixOf` pdf)
      assertBool' s b = if b then return () else assertFailure ("missing: " ++ s)
  -- header with a binary comment
  assertBool (C.pack "%PDF-1.7\n%\xE2\xE3\xCF\xD3\n" `BS.isPrefixOf` pdf)
  mapM_ has
    [ "/Type /Metadata", "/Subtype /XML", "<pdfaid:part>3</pdfaid:part>"
    , "<pdfaid:conformance>B</pdfaid:conformance>"
    , "<ex:Level>BASIC</ex:Level>", "<pdfaSchema:prefix>ex</pdfaSchema:prefix>"
    , "<fx:ConformanceLevel>BASIC</fx:ConformanceLevel>"
    , "<fx:DocumentFileName>factur-x.xml</fx:DocumentFileName>"
    , "<pdfaSchema:prefix>fx</pdfaSchema:prefix>"
    , "<dc:creator><rdf:Seq><rdf:li>Salmon</rdf:li></rdf:Seq></dc:creator>"
    , "/S /GTS_PDFA1", "/OutputIntents", "/DestOutputProfile"
    , "/Type /EmbeddedFile", "/Subtype /text#2Fxml", "/AFRelationship /Alternative"
    , "/UF (factur-x.xml)", "/EmbeddedFiles", "/AF [", "/Title (Facture 42)"
    , "/Author (Salmon)", "/ID [<", "/Interpolate false"
    , "/Size 4", "/CreationDate (D:20261106000000Z)", "/ModDate (D:20261106000000Z)" ]
  assertBool (not (C.pack "/Interpolate true" `BS.isInfixOf` pdf))
  -- the metadata stream is not compressed
  assertBool (not (C.pack "/Filter" `BS.isInfixOf` C.takeWhile (/= 's') (snd (BS.breakSubstring (C.pack "/Subtype /XML") pdf))))

-- The document identifier comes from the document: the same document built
-- step by step has the same one, another document has another one.
test_pdfaDocumentId :: IO ()
test_pdfaDocumentId = withTestFont $ \_ d -> do
  let rect = PDFRect 0 0 300 200
      idOf = fmap (C.take 40 . snd . BS.breakSubstring (C.pack "/ID [<") . BL.toStrict)
      doc = mkTrueTypeFont d >>= pdfaDoc
      whole = renderPdfE standardDocInfo rect doc
      (font, b1) = stepPDF (mkTrueTypeFont d) (emptyPdfBuilder standardDocInfo rect)
      (_, b2) = stepPDF (pdfaDoc font) b1
      other = renderPdfE standardDocInfo {author = T.pack "Another"} rect doc
  assertBool (isRight whole)
  assertEqual whole (buildPdfE b2)
  assertEqual (fmap C.length (idOf whole)) (Right 40)
  assertBool (idOf whole /= idOf other)

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

-- The settings and the contents that cannot give a PDF/A document.
test_pdfaSettingsAreChecked :: IO ()
test_pdfaSettingsAreChecked = do
  let rect = PDFRect 0 0 300 200
      render settings body = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} rect $ do
        setPDFA settings
        page <- addPage Nothing
        body page
      nothing _ = return ()
      base = pdfa3b fakeIcc
      file mime = \_ -> () <$ embedAssociatedFile (associatedFile (T.pack "a.xml") mime (BL.fromStrict (C.pack "<x/>")))
      extension prefix prop = XMPExtension (T.pack "Schema") (T.pack "http://example.org/ns/") (T.pack prefix)
        [XMPProperty (T.pack prop) (T.pack "Text") (T.pack "external") (T.pack "A level") (T.pack "BASIC")]
      refused = not . isRight
  assertBool (isRight (render base nothing))
  assertBool (isRight (render base {pdfaPart = 2} nothing))
  assertBool (refused (render base {pdfaPart = 1} nothing))
  assertBool (refused (render base {pdfaConformance = "A"} nothing))
  assertBool (isRight (render base (file "text/xml")))
  assertBool (refused (render base {pdfaPart = 2} (file "text/xml")))
  assertBool (refused (render base (file "")))
  assertBool (isRight (render base {pdfaExtensions = [extension "ex" "Level"]} nothing))
  assertBool (refused (render base {pdfaExtensions = [extension "a b" "Level"]} nothing))
  assertBool (refused (render base {pdfaExtensions = [extension "ex" "p q"]} nothing))
  -- a file without a MIME type in a document that is not PDF/A: no /Subtype
  let plain = BL.toStrict . renderPdf standardDocInfo {compressed = False} rect $
        addPage Nothing >>= file ""
  assertBool (C.pack "/Type /EmbeddedFile" `BS.isInfixOf` plain)
  assertBool (not (C.pack "/Subtype /\n" `BS.isInfixOf` plain))
  -- a character that XML cannot have is in no part of the document
  let Right controls = render base {pdfaTitle = T.pack "Ti\SOHtle"} nothing
  assertBool (C.pack "<rdf:li xml:lang=\"x-default\">Title</rdf:li>" `BS.isInfixOf` controls)
  assertBool (C.pack "/Title (Title)" `BS.isInfixOf` controls)

-- The annotations of a PDF/A document: a link has the Print flag, a text
-- annotation is a problem. The other documents are not changed.
test_pdfaAnnotations :: IO ()
test_pdfaAnnotations = do
  let rect = PDFRect 0 0 300 200
      render pdfa annotation = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} rect $ do
        pdfa
        page <- addPage Nothing
        drawWithPage page (annotation page)
      link page = newAnnotation (PDFLink (T.pack "link") [10, 10, 50, 50] page 0 0 False)
      note _ = newAnnotation (TextAnnotation (T.pack "note") [10, 10, 50, 50] Note)
      archive = setPDFA (pdfa3b fakeIcc)
      flagged = either (const False) (C.pack "/F 4\n" `BS.isInfixOf`)
  assertBool (flagged (render archive link))
  assertBool (isRight (render (return ()) link))
  assertBool (not (flagged (render (return ()) link)))
  assertBool (isRight (render (return ()) note))
  case render archive note of
    Left [problem] -> assertBool ("annotation" `elem` words problem)
    other -> assertFailure ("expected one problem, got " ++ either show (const "a document") other)

-- 'newSection' and 'newSectionWithPage' give the result of their contents.
test_sectionResult :: IO ()
test_sectionResult = do
  ref <- newIORef (0 :: Int, 0 :: Int)
  _ <- renderPdfT standardDocInfo (PDFRect 0 0 300 200) $ do
    page <- addPage Nothing
    a <- newSection (T.pack "a") Nothing Nothing $
      newSection (T.pack "b") Nothing Nothing (return 1)
    b <- newSectionWithPage (T.pack "c") Nothing Nothing page (return 2)
    liftIO (writeIORef ref (a, b))
  readIORef ref >>= assertEqual (1, 2)

-- A page can be drawn in more than one step: the content is in the order
-- of the drawings, also for a page with many drawings.
-- See Note [Memory of the drawings] in Graphics.PDF.Document
test_pageDrawnInSteps :: IO ()
test_pageDrawnInSteps = do
  let steps = 3000 :: Int
      pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        first <- addPage Nothing
        second <- addPage Nothing
        mapM_ (\i -> drawWithPage (if even i then first else second) (moveto (fromIntegral i :+ 0))) [1 .. steps]
      moves = [ fst n | l <- C.lines pdf, [x, _, m] <- [C.words l], m == C.pack "m", Just n <- [C.readInt x] ]
  -- the two pages are two streams: the odd steps and the even steps, each in order
  assertEqual steps (length moves)
  assertEqual ([2, 4 .. steps] ++ [1, 3 .. steps]) moves

-- 'drawWithPage' is lazy: a drawing can use a value that is known only
-- after it, here the total number of pages. 'drawWithPage'' evaluates the
-- drawing immediately, and writes the same content.
-- See Note [Memory of the drawings] in Graphics.PDF.Document
test_lazyAndStrictDrawings :: IO ()
test_lazyAndStrictDrawings = do
  let rect = PDFRect 0 0 300 200
      -- each page shows its number and the total, and says if a page follows
      pagesWith :: Int -> Int -> PDF Int
      pagesWith total k = do
        page <- addPage Nothing
        more <- drawWithPage page $ do
          moveto (fromIntegral k :+ fromIntegral total)
          return (k < 3)
        if more then pagesWith total (k + 1) else return k
      -- the total is the result of the document that uses it
      (n, builder) = stepPDF (pagesWith n 1) (emptyPdfBuilder standardDocInfo {compressed = False} rect)
      lazyPdf = BL.toStrict (buildPdf builder)
      number t = read (C.unpack t) :: Double
      marks pdf = [ (number a, number b) | l <- C.lines pdf, [a, b, m] <- [C.words l], m == C.pack "m" ]
  assertEqual 3 n
  assertEqual [(1, 3), (2, 3), (3, 3)] (marks lazyPdf)
  -- the same document with the strict function, when the total is known
  let strictPdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} rect $
        mapM_ (\k -> addPage Nothing >>= \page -> drawWithPage' page (moveto (fromIntegral k :+ 3))) [1 :: Int, 2, 3]
  assertEqual lazyPdf strictPdf
  -- the strict function gives the result of the drawing
  let (r, _) = stepPDF (addPage Nothing >>= \page -> drawWithPage' page (return (42 :: Int))) (emptyPdfBuilder standardDocInfo rect)
  assertEqual 42 r

-- A document without a page is a problem, and not an exception when the
-- bytes are read.
test_documentWithoutPage :: IO ()
test_documentWithoutPage =
  assertEqual (Left ["the document has no page"]) (renderPdfE standardDocInfo (PDFRect 0 0 300 200) (return ()))

test_pdfaRefusesStandardFonts :: IO ()
test_pdfaRefusesStandardFonts = do
  Right helvetica <- mkStdFont Helvetica
  case renderPdfE standardDocInfo (PDFRect 0 0 300 200) (pdfaDoc helvetica) of
    Left [problem] -> assertBool ("Helvetica" `elem` words problem)
    other -> assertFailure ("expected one problem, got " ++ either show (const "a document") other)

-- The functions that cannot return the problems of a document raise an
-- exception, and write no file.
test_pdfaProblemsAreNotIgnored :: IO ()
test_pdfaProblemsAreNotIgnored = do
  Right helvetica <- mkStdFont Helvetica
  tmp <- getTemporaryDirectory
  let path = tmp ++ "/hpdf-test-not-pdfa.pdf"
      rect = PDFRect 0 0 300 200
      raises :: IO a -> IO Bool
      raises act = (act >> return False) `catch` \e -> return (length (show (e :: SomeException)) >= 0)
  assertBool =<< raises (writePdf path standardDocInfo rect (pdfaDoc helvetica))
  assertBool . not =<< doesFileExist path
  assertBool =<< raises (evaluate (BL.length (renderPdf standardDocInfo rect (pdfaDoc helvetica))))
  let (_, builder) = stepPDF (pdfaDoc helvetica) (emptyPdfBuilder standardDocInfo rect)
  assertBool (not (isRight (buildPdfE builder)))
  assertBool =<< raises (evaluate (BL.length (buildPdf builder)))

-- The font check looks at the font, not at its name: a standard font that
-- is only loaded, and not used for text, is not a problem.
test_pdfaFontCheckIsNotByName :: IO ()
test_pdfaFontCheckIsNotByName = withTestFont $ \_ d -> do
  Right _ <- mkStdFont Helvetica
  assertBool (isRight (renderPdfE standardDocInfo (PDFRect 0 0 300 200) (mkTrueTypeFont d >>= pdfaDoc)))

-- An embedded Type 1 font that has the name of a standard font is not a
-- problem.
test_pdfaAcceptsEmbeddedType1 :: IO ()
test_pdfaAcceptsEmbeddedType1 = do
  -- the two names have the same length: the PFB segments keep their size
  let rename b = case BS.breakSubstring (C.pack "Test-Icons") b of
        (before, rest) | BS.null rest -> before
                       | otherwise -> BS.concat [before, C.pack "Times-Bold", rename (BS.drop 10 rest)]
  fs <- readType1Fixture rename
  let render = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        setPDFA (pdfa3b fakeIcc)
        font <- mkType1Font fs
        page <- addPage Nothing
        drawWithPage page $ drawText $ text (PDFFont font 12) 10 10 (T.pack "\xE000\xE001")
  case render of
    Left problems -> assertFailure (unlines problems)
    Right pdf -> do
      assertBool (C.pack "/BaseFont /Times-Bold" `BS.isInfixOf` pdf)
      assertBool (C.pack "/FontFile " `BS.isInfixOf` pdf)

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

-- The output intent must be an RGB profile, and an image must not be
-- DeviceCMYK.
test_pdfaDeviceColours :: IO ()
test_pdfaDeviceColours = do
  let rect = PDFRect 0 0 300 200
      render icc = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} rect $ do
        setPDFA (pdfa3b icc)
        _ <- addPage Nothing
        return ()
  assertBool (either (const False) (C.pack "/N 3" `BS.isInfixOf`) (render (fakeIccOf "RGB ")))
  assertBool (not (isRight (render (fakeIccOf "CMYK"))))
  assertBool (not (isRight (render (fakeIccOf "GRAY"))))
  assertBool (not (isRight (render (BL.fromStrict (C.pack "too short")))))
  -- the header of a CMYK JPEG image of 1 x 1
  let Right cmyk = readJpegDataURL "data:image/jpeg;base64,/9j/wAAUCAABAAEEAREAAhEAAxEABBEA/9k="
      withImage pdfa = renderPdfE standardDocInfo rect $ do
        pdfa
        img <- createPDFJpeg cmyk
        page <- addPage Nothing
        drawWithPage page (drawXObject img)
  assertBool (isRight (withImage (return ())))
  case withImage (setPDFA (pdfa3b fakeIcc)) of
    Left [problem] -> assertBool ("DeviceCMYK;" `elem` words problem)
    other -> assertFailure ("expected one problem, got " ++ either show (const "a document") other)

attach :: String -> PDF ()
attach n = () <$ embedAssociatedFile (associatedFile (T.pack n) "text/xml" (BL.fromStrict (C.pack "<x/>")))

-- The font dictionary of a standard font is one object, and each page
-- refers to it.
-- See Note [Font dictionaries are indirect objects] in Graphics.PDF
test_fontDictionaryIsShared :: IO ()
test_fontDictionaryIsShared = do
  Right courier <- mkStdFont Courier
  Right times <- mkStdFont Times_Roman
  let pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        mapM_ (const (onePage [courier, times])) [1 :: Int, 2, 3]
        onePage [courier]
      onePage fonts = do
        page <- addPage Nothing
        drawWithPage page $ mapM_ (\f -> drawText (text (PDFFont f 12) 10 10 (T.pack "text"))) fonts
      count x = occurrences (C.pack x) pdf
      occurrences pat b = case BS.breakSubstring pat b of
        (_, r) | BS.null r -> 0 :: Int
               | otherwise -> 1 + occurrences pat (BS.drop (BS.length pat) r)
  assertEqual 1 (count "/BaseFont /Courier\n")
  assertEqual 1 (count "/BaseFont /Times-Roman\n")
  -- the pages refer to the two objects
  assertBool (not (C.pack "/Courier <<" `BS.isInfixOf` pdf))
  case [ w | l <- C.lines pdf, (k : w@[_, _, _]) <- [C.words l], k == C.pack "/Courier" ] of
    refs@(r : _) -> do
      assertEqual 4 (length refs)
      assertBool (all (== r) refs)
      assertEqual [C.pack "0", C.pack "R"] (drop 1 r)
    [] -> assertFailure "no reference to the font"

-- /F is a byte string (PDFDocEncoding), /UF a text string.
test_filespecNames :: IO ()
test_filespecNames = do
  let pdf = BL.toStrict . renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $
              attach "Rechnung_M\228rz \8364.xml" >> addPage Nothing
  assertBool (C.pack "/F (Rechnung_M\228rz _.xml)" `BS.isInfixOf` pdf)
  assertBool (C.pack "/UF <FEFF005200650063" `BS.isInfixOf` pdf)

-- The keys of the EmbeddedFiles name tree are in the order of their bytes,
-- and /AF in the order of the calls.
test_embeddedFilesOrder :: IO ()
test_embeddedFilesOrder = do
  let render names = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $
        mapM_ attach names >> addPage Nothing
      -- U+FF5A sorts after U+1F600 as text, and before it as UTF-16BE
      Right pdf = render ["\65370.xml", "z.xml", "\128512.xml", "\228.xml", "a.xml"]
      keys = [ C.take 9 (C.drop i pdf) | i <- concatMap (`occurrences` pdf) (map C.pack ["(a.xml)", "(z.xml)", "<FEFF00E4", "<FEFFD83D", "<FEFFFF5A"]) ]
      afterNames = snd (BS.breakSubstring (C.pack "/EmbeddedFiles") pdf)
      position k = BS.length (fst (BS.breakSubstring (C.pack k) afterNames))
      positions = map position ["(a.xml)", "(z.xml)", "<FEFF00E4", "<FEFFD83D", "<FEFFFF5A"]
  assertBool (not (null keys))
  assertBool (all (< BS.length afterNames) positions)
  assertBool (and (zipWith (<) positions (drop 1 positions)))
  -- two files with the same name: the keys are different, the names are not
  let Right twice = render ["a.xml", "a.xml (2)", "a.xml", "a.xml"]
      count k = length (occurrences (C.pack k) twice)
  assertEqual 3 (count "/UF (a.xml)")
  -- "a.xml (2)" is a name (/F, /UF) and its key; the others are keys only
  assertEqual [3, 1, 1] (map count ["(a.xml \\(2\\))", "(a.xml \\(3\\))", "(a.xml \\(4\\))"])

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

-- | The font with other bytes for this table. The new table is put after
-- the font, and the table directory changed to give its place.
withTable :: String -> BS.ByteString -> BS.ByteString -> BS.ByteString
withTable tag table bytes = BS.concat [BS.take 12 bytes, BS.concat (map dirEntry [0 .. u16 bytes 4 - 1]), BS.drop (12 + 16 * u16 bytes 4) bytes, table]
  where
    dirEntry i
      | BS.take 4 e == C.pack tag = BS.concat [BS.take 8 e, be32 (BS.length bytes), be32 (BS.length table)]
      | otherwise = e
      where e = BS.take 16 (BS.drop (12 + 16 * i) bytes)
    be32 x = BS.pack (map (fromIntegral . (x `div`)) [16777216, 65536, 256, 1])

withCmap :: BS.ByteString -> BS.ByteString -> BS.ByteString
withCmap = withTable "cmap"

-- | The font with these characters only, and the glyphs they had.
withOnlyCharacters :: String -> TrueTypeFontData -> BS.ByteString -> BS.ByteString
withOnlyCharacters chars d = withCmap cmap
  where
    be16 x = BS.pack [fromIntegral (x `div` 256), fromIntegral x]
    be32 x = be16 (x `div` 65536) <> be16 (x `mod` 65536)
    cs = map ord (S.toAscList (S.fromList chars))
    segs = [ (c, c, (glyphOf d (toEnum c) - c) `mod` 65536) | c <- cs ] ++ [(0xFFFF, 0xFFFF, 1)]
    count = length segs
    subtable = BS.concat $
      [be16 4, be16 (16 + 8 * count), be16 0, be16 (2 * count), be16 0, be16 0, be16 0]
      ++ [be16 e | (_, e, _) <- segs] ++ [be16 0]
      ++ [be16 s | (s, _, _) <- segs] ++ [be16 delta | (_, _, delta) <- segs]
      ++ replicate count (be16 0)
    cmap = BS.concat [be16 0, be16 1, be16 3, be16 1, be32 12, subtable]

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

-- Text that does not fit in a rectangle continues in the next one. The
-- boxes that fit in no rectangle are returned, and can be drawn later.
test_textInSeveralRectangles :: IO ()
test_textInSeveralRectangles = do
  Right helvetica <- mkStdFont Helvetica
  ref <- newIORef []
  let total = 300 :: Int
      style = Font (PDFFont helvetica 10) black black
      content = paragraph (txt (T.unwords (replicate total (T.pack "zq"))))
      column x = Rectangle (x :+ 10) ((x + 80) :+ 190)
      -- the number of words that a document shows
      shown doc = do
        bytes <- renderPdfT standardDocInfo {compressed = False} (PDFRect 0 0 300 200) doc
        return (occurrences (BL.toStrict bytes))
      -- a word is a string in hexadecimal: "zq" is <7A71>
      occurrences b = case BS.breakSubstring (C.pack "<7A71>") b of
        (_, r) | BS.null r -> 0 :: Int
               | otherwise -> 1 + occurrences (BS.drop 6 r)
      one rects = do
        page <- addPage Nothing
        (_, rest) <- drawWithPage page (displayFormattedTextIn rects NormalParagraph style content)
        liftIO (modifyIORef ref (++ [length rest]))
        return rest
  n0 <- shown (one [])
  n1 <- shown (one [column 10])
  n2 <- shown (one [column 10, column 100])
  -- the same text with the old function: one rectangle
  n1' <- shown $ do
    page <- addPage Nothing
    drawWithPage page (displayFormattedText (column 10) NormalParagraph style content)
  -- the boxes that are left go to a second page
  n3 <- shown $ do
    rest <- one [column 10, column 100]
    page <- addPage Nothing
    rest' <- drawWithPage page (displayBoxes NormalParagraph [column 10, column 100, column 190] rest)
    liftIO (modifyIORef ref (++ [length rest']))
  rests <- readIORef ref
  assertEqual 0 n0
  assertBool (n1 > 0 && n1 < n2 && n2 < total)
  assertEqual n1 n1'
  assertEqual total n3
  case rests of
    [r0, r1, r2, r3, r4] -> do
      assertBool (all (> 0) [r0, r1, r2, r3])
      assertEqual 0 r4
    _ -> assertFailure "expected five results"

-- A document that does not ask for PDF/A is written as before: PDF 1.5,
-- the Info dictionary in the trailer, no /ID.
test_plainDocumentUnchanged :: IO ()
test_plainDocumentUnchanged = do
  let pdf = BL.toStrict $ renderPdf standardDocInfo (PDFRect 0 0 300 200) (addPage Nothing >> return ())
  assertBool (C.pack "%PDF-1.5\n" `BS.isPrefixOf` pdf)
  assertBool (C.pack "/Info <<" `BS.isInfixOf` pdf)
  assertBool (not (C.pack "/ID [" `BS.isInfixOf` pdf))
  assertBool (not (C.pack "/Metadata" `BS.isInfixOf` pdf))

-- A signature field reserves the space of the signature, and 'signPdf'
-- writes the signature of the other bytes of the document.
-- See Note [Layout of a signature dictionary] in
-- Graphics.PDF.LowLevel.Signature
test_signature :: IO ()
test_signature = do
  let rect = PDFRect 0 0 300 200
      render doc = renderPdfE standardDocInfo {compressed = False} rect doc
      signedDoc sig = do
        -- a file with text that looks like a byte range, before the signature
        _ <- embedAssociatedFile (associatedFile (T.pack "decoy.txt") "text/plain"
               (BL.fromStrict (C.pack "/ByteRange [0 5 10 20] /Contents <0000>")))
        page <- addPage Nothing
        addSignature page sig
      mark = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
  case render (signedDoc signature {sigName = T.pack "A. Signer", sigSize = 64}) of
    Left ps -> assertFailure (unlines ps)
    Right unsignedLazy -> do
      let unsigned = BL.toStrict unsignedLazy
          has x = assertBoolVerbose x (C.pack x `BS.isInfixOf` unsigned)
      has "/AcroForm <<"
      has "/SigFlags 3"
      has "/FT /Sig"
      has "/SubFilter /adbe.pkcs7.detached"
      has "/Name (A. Signer)"
      -- the real byte range is the one after "/Type /Sig"
      let sigDict = snd (BS.breakSubstring (C.pack "/Type /Sig") unsigned)
          range = case BS.breakSubstring (C.pack "/ByteRange [0 ") sigDict of
            (_, r) | BS.null r -> Nothing
                   | otherwise -> case map C.readInt (C.words (C.takeWhile (/= ']') (BS.drop 14 r))) of
                       [Just (a, _), Just (b, _), Just (c, _)] | b + c == BS.length unsigned -> Just (a, b)
                       _ -> Nothing
      case range of
        Nothing -> assertFailure "no byte range that agrees with the length of the document"
        Just (a, b) -> do
          -- /Contents is a string of 64 bytes in hexadecimal, all zeros
          assertEqual (2 * 64 + 2) (b - a)
          assertEqual (C.pack ("<" ++ replicate 128 '0' ++ ">")) (BS.take (b - a) (BS.drop a unsigned))
          ref <- newIORef BS.empty
          r <- signPdf (\bytes -> writeIORef ref (BL.toStrict bytes) >> return mark) unsignedLazy
          toSign <- readIORef ref
          assertEqual (BS.take a unsigned `BS.append` BS.drop b unsigned) toSign
          case r of
            Left e -> assertFailure e
            Right signedLazy -> do
              let signed = BL.toStrict signedLazy
              assertEqual (BS.length unsigned) (BS.length signed)
              assertEqual (C.pack ("<DEADBEEF" ++ replicate 120 '0' ++ ">")) (BS.take (b - a) (BS.drop a signed))
              assertEqual (BS.take a unsigned) (BS.take a signed)
              assertEqual (BS.drop b unsigned) (BS.drop b signed)
              -- a signed document is not signed again
              again <- signPdf (const (return mark)) signedLazy
              assertEqual (Left "the document is already signed") again
          -- a signature that is longer than the reserved space
          long <- signPdf (const (return (BS.replicate 65 1))) unsignedLazy
          assertBool (either (("sigSize" `elem`) . words) (const False) long)
          empty <- signPdf (const (return BS.empty)) unsignedLazy
          assertBool (either (const True) (const False) empty)
  -- a document without a signature field
  plain <- signPdf (const (return mark)) (renderPdf standardDocInfo rect (addPage Nothing >> return ()))
  assertBool (either (("addSignature" `elem`) . words) (const False) plain)
  none <- signPdf (const (return mark)) BL.empty
  assertBool (either (const True) (const False) none)
  -- one signature only, with a field name and a size
  let problems doc = either length (const 0) (render doc)
  assertEqual 1 (problems (addPage Nothing >>= \p -> addSignature p signature >> addSignature p signature))
  assertEqual 1 (problems (addPage Nothing >>= \p -> addSignature p signature {sigFieldName = T.empty}))
  assertEqual 1 (problems (addPage Nothing >>= \p -> addSignature p signature {sigSize = 0}))

-- A key for the tests only (EC, P-256) with its certificate, and a second
-- key. They are not secrets.
fixture_key_pem :: BS.ByteString
fixture_key_pem = C.pack $ unlines
    [ "-----BEGIN PRIVATE KEY-----"
    , "MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgLrZEHE8Z03CYNC5x"
    , "7+tb7P2xgeJ1uc/AmIWGFgCE5YihRANCAAQjkmV/ZhRh4fjFMp6vG2pqmBOaGPDt"
    , "qRbE8ygztoVi+Wx/WVTY8CJc/kECd4bqQYEsXfkvoXB9+lzKwIh+c3je"
    , "-----END PRIVATE KEY-----"
    ]

fixture_cert_pem :: BS.ByteString
fixture_cert_pem = C.pack $ unlines
    [ "-----BEGIN CERTIFICATE-----"
    , "MIIBfDCCASKgAwIBAgIULa9txngEbCZ9QX7m7L5ChVesvuYwCgYIKoZIzj0EAwIw"
    , "IzEhMB8GA1UEAwwYSFBERiB0ZXN0IChub3QgYSBzZWNyZXQpMCAXDTI2MTEyNTEz"
    , "MjgzOVoYDzIxMjYxMTAxMTMyODM5WjAjMSEwHwYDVQQDDBhIUERGIHRlc3QgKG5v"
    , "dCBhIHNlY3JldCkwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAAQjkmV/ZhRh4fjF"
    , "Mp6vG2pqmBOaGPDtqRbE8ygztoVi+Wx/WVTY8CJc/kECd4bqQYEsXfkvoXB9+lzK"
    , "wIh+c3jeozIwMDAdBgNVHQ4EFgQUxFsQNsvKe/9/lt/1hAhXOFqPN6QwDwYDVR0T"
    , "AQH/BAUwAwEB/zAKBggqhkjOPQQDAgNIADBFAiBn11kiiy2Hdh3H/YanMtuz5Jvv"
    , "mgcOyaFG4oT1yGO68gIhAOVrJzB8eYJELIWx+XY9HXBXdg7vU8LkQvXNb0E7TsEh"
    , "-----END CERTIFICATE-----"
    ]

fixture_other_key_pem :: BS.ByteString
fixture_other_key_pem = C.pack $ unlines
    [ "-----BEGIN PRIVATE KEY-----"
    , "MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgfhuwE7zzpILSLYTj"
    , "brP94J7ycjjFahF2YMbQwxb2khGhRANCAAStHxxb7D53o//28pm1OInaNw8vPdGi"
    , "DWXwZxIXU06oAFUiwEuxtYw+KuyOfwTpyZd5lc4m10JXTagGKVhAYquJ"
    , "-----END PRIVATE KEY-----"
    ]

-- 'signPdfWith' writes a CMS signature with the certificate of the signer.
-- CI verifies a signature with pdfsig: see .github/workflows/ci.yml
test_signatureWithKey :: IO ()
test_signatureWithKey = do
  let unsigned = renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        page <- addPage Nothing
        addSignature page signature
      isLeft = either (const True) (const False)
      -- the DER of the certificate: the base64 of the PEM file starts with it
      contents pdf = let (_, r) = BS.breakSubstring (C.pack "/Contents <") (BL.toStrict pdf)
                     in C.takeWhile (/= '>') (BS.drop 11 r)
  case parseSigner fixture_key_pem fixture_cert_pem of
    Left e -> assertFailure e
    Right signer -> do
      r <- signPdfWith signer unsigned
      case r of
        Left e -> assertFailure e
        Right signed -> do
          assertEqual (BL.length unsigned) (BL.length signed)
          -- a DER SEQUENCE with a length of two bytes, then the OID of signedData
          assertBool (C.pack "3082" `BS.isPrefixOf` contents signed)
          assertBool (C.pack "06092A864886F70D010702" `BS.isInfixOf` contents signed)
          -- the name of the certificate ("HPDF test") is in the signature
          assertBool (C.pack "4850444620746573742028" `BS.isInfixOf` contents signed)
          again <- signPdfWith signer signed
          assertEqual (Left "the document is already signed") again
      none <- signPdfWith signer (renderPdf standardDocInfo (PDFRect 0 0 300 200) (addPage Nothing >> return ()))
      assertBool (isLeft none)
  -- a certificate that is not the certificate of the key, and files that
  -- are not PEM files
  assertBool (isLeft (fmap (const ()) (parseSigner fixture_other_key_pem fixture_cert_pem)))
  assertBool (isLeft (fmap (const ()) (parseSigner fixture_cert_pem fixture_cert_pem)))
  assertBool (isLeft (fmap (const ()) (parseSigner fixture_key_pem fixture_key_pem)))
  assertBool (isLeft (fmap (const ()) (parseSigner BS.empty BS.empty)))

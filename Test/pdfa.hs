{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2026, HPDF contributors
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- A PDF/A-3b document with an attached file: XMP metadata, an sRGB
-- output intent, an embedded (subset) TrueType font, and a CSV file
-- embedded and associated with the document.
--
-- > cabal run HPDF-PDFA-Demo -- [font.ttf] [sRGB.icc]
--
-- Writes pdfa.pdf. Without arguments, it uses DejaVu Sans or Noto Sans and
-- an sRGB profile from the usual system locations.
---------------------------------------------------------
module Main where

import Graphics.PDF
import Graphics.PDF.Archive
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Control.Monad (filterM)
import Data.List (isSuffixOf)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)

systemFonts :: [FilePath]
systemFonts =
  [ "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/TTF/DejaVuSans.ttf"
  , "/usr/share/fonts/dejavu/DejaVuSans.ttf"
  , "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
  ]

systemProfiles :: [FilePath]
systemProfiles =
  [ "/usr/share/color/icc/colord/sRGB.icc"
  , "/usr/share/color/icc/ghostscript/srgb.icc"
  ]

firstFile :: String -> [FilePath] -> IO FilePath
firstFile what paths = do
  found <- filterM doesFileExist paths
  case found of
    p : _ -> return p
    [] -> do
      putStrLn ("usage: HPDF-PDFA-Demo [FONT.ttf] [sRGB.icc] (no " ++ what ++ " found in the usual places)")
      exitFailure

main :: IO ()
main = do
  args <- getArgs
  fontPath <- firstFile "TrueType font" (filter (".ttf" `isSuffixOf`) args ++ systemFonts)
  iccPath <- firstFile "sRGB ICC profile" (filter (".icc" `isSuffixOf`) args ++ systemProfiles)
  Right fontData <- readTrueTypeFont fontPath
  icc <- BL.readFile iccPath
  let info = standardDocInfo { author = "HPDF", compressed = True }
  -- pdfByteStringE refuses a PDF/A document that uses a standard 14 font
  -- (they are not embedded)
  case pdfByteStringE info (PDFRect 0 0 420 200) (document fontData icc) of
    Left problems -> mapM_ putStrLn problems >> exitFailure
    Right bytes -> do
      BL.writeFile "pdfa.pdf" bytes
      putStrLn ("pdfa.pdf written with " ++ fontPath ++ " and " ++ iccPath)

document :: TrueTypeFontData -> BL.ByteString -> PDF ()
document fontData icc = do
  -- XMP metadata, an output intent with this ICC profile, a trailer /ID
  setPDFA (pdfa3b icc) { pdfaTitle = "PDF/A-3b in HPDF" }
  -- An EmbeddedFile, its file specification, the catalog's /AF and
  -- /Names /EmbeddedFiles
  _ <- embedAssociatedFile (associatedFile "greetings.csv" "text/csv" greetings)
         { afRelationship = Data, afDescription = "The greetings on the page" }
  font <- mkTrueTypeFont fontData
  page <- addPage Nothing
  drawWithPage page $ do
    drawText $ text (PDFFont font 16) 24 166 "PDF/A-3b in HPDF"
    drawText $ text (PDFFont font 10) 24 140 "This document embeds greetings.csv:"
    drawText $ text (PDFFont font 10) 40 120 "Bonjour · Γειά σου · Привет"
    drawText $ text (PDFFont font 9) 24 24 "Every font is embedded; the metadata says PDF/A-3, conformance B."

greetings :: BL.ByteString
greetings = BL.fromStrict . TE.encodeUtf8 . T.unlines $
  [ "language,greeting", "French,Bonjour", "Greek,Γειά σου", "Russian,Привет" ]

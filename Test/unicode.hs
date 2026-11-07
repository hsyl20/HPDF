{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2026, HPDF contributors
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Unicode text with an embedded TrueType font: Latin with accents, Greek
-- and Cyrillic. Only the glyphs the page shows are embedded (a subset),
-- and a ToUnicode CMap makes the text copy out of the PDF.
--
-- > cabal run HPDF-Unicode-Demo -- [font.ttf]
--
-- Writes unicode.pdf. Without a font argument, it uses DejaVu Sans or Noto
-- Sans from the usual system locations.
---------------------------------------------------------
module Main where

import Graphics.PDF
import qualified Data.Text as T
import Control.Monad (filterM, forM_)
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

samples :: [(T.Text, T.Text)]
samples =
  [ ("French", "Ça déçoit, mais l’été arrive à Noël.")
  , ("German", "Grüße aus Köln: 20 € für Straßenkäse.")
  , ("Polish", "Zażółć gęślą jaźń.")
  , ("Czech", "Příliš žluťoučký kůň úpěl ďábelské ódy.")
  , ("Greek", "Καλημέρα κόσμε· ξεσκεπάζω την ψυχοφθόρα.")
  , ("Russian", "Съешь же ещё этих мягких булок.")
  , ("Ukrainian", "Чуєш їх, доцю, га? Кумедна ж ти, прощайся без ґольфів!")
  ]

main :: IO ()
main = do
  args <- getArgs
  candidates <- filterM doesFileExist (args ++ systemFonts)
  case candidates of
    [] -> do
      putStrLn "usage: HPDF-Unicode-Demo FONT.ttf (no TrueType font found in the usual places)"
      exitFailure
    path : _ -> do
      ttf <- readTrueTypeFont path
      case ttf of
        Left e -> putStrLn ("cannot read " ++ path ++ ": " ++ e) >> exitFailure
        Right fontData -> do
          runPdf "unicode.pdf" (standardDocInfo { author = "HPDF", compressed = True }) (PDFRect 0 0 460 240) $ do
            -- Type0 / CIDFontType2, Identity-H, /W widths, a ToUnicode CMap;
            -- the font program is subset when the document is saved.
            font <- mkTrueTypeFont fontData
            page <- addPage Nothing
            drawWithPage page $ do
              drawText $ text (PDFFont font 16) 24 206 "Unicode text in HPDF"
              drawText $ text (PDFFont font 9) 24 190 (T.pack ("Font: " ++ trueTypePostScriptName fontData ++ ", embedded as a subset"))
              strokeColor (Rgb 0.6 0.6 0.6)
              stroke (Line 24 182 436 182)
              forM_ (zip samples [160, 140 ..]) $ \((lang, line), y) -> do
                drawText $ text (PDFFont font 10) 24 y lang
                drawText $ text (PDFFont font 10) 96 y line
          putStrLn ("unicode.pdf written with " ++ path)

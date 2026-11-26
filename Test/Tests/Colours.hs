{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Colour operators
module Tests.Colours (htf_thisModulesTests) where

import Test.Framework
import Graphics.PDF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- A colour is written with the operators of its device colour space.
-- See Note [Device colours] in Graphics.PDF.Draw
test_deviceColours :: IO ()
test_deviceColours = do
  Right helvetica <- mkStdFont Helvetica
  let rect = PDFRect 0 0 300 200
      pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} rect $ do
        page <- addPage Nothing
        newSection (T.pack "section") (Just (Cmyk 0 1 1 0)) Nothing $
          drawWithPage page $ do
            fillColor (Rgb 1 0 0.5) >> strokeColor (Rgb 0 0.25 1)
            fillColor (Gray 0.5) >> strokeColor (Gray 0.25)
            fillColor (Cmyk 0 1 0.5 0.25) >> strokeColor (Cmyk 1 0 0 0.5)
            fillColor (Hsv 0 1 1)
            drawText $ do
              setFont (PDFFont helvetica 12)
              fillColor (Gray 0.75)
              strokeColor (Cmyk 0.5 0 0 0)
              textStart 10 10
              displayText (T.pack "text")
      has x = assertBoolVerbose x (C.pack ("\n" ++ x ++ "\n") `BS.isInfixOf` pdf)
  mapM_ has [ "1 0 0.5 rg", "0 0.25 1 RG", "0.5 g", "0.25 G", "0 1 0.5 0.25 k", "1 0 0 0.5 K", "1 0 0 rg"
            , "0.75 g", "0.5 0 0 0 K" ]
  -- the colour of an outline is an RGB colour: red for this CMYK colour
  assertBool (C.pack "/C [1 0 0 ]" `BS.isInfixOf` pdf)

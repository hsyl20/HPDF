{-# LANGUAGE OverloadedStrings #-}
-- | Typeset text
module Tests.Typesetting (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import Graphics.PDF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Monad.IO.Class (liftIO)
import Data.IORef

tests :: TestTree
tests = testGroup "Typesetting"
  [ test "textInSeveralRectangles" test_textInSeveralRectangles
  ]


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

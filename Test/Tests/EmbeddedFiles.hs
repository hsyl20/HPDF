{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Embedded and associated files
module Tests.EmbeddedFiles (htf_thisModulesTests) where

import Test.Framework
import Tests.Common
import Graphics.PDF
import Graphics.PDF.Archive
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

attach :: String -> PDF ()
attach n = () <$ embedAssociatedFile (associatedFile (T.pack n) "text/xml" (BL.fromStrict (C.pack "<x/>")))

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

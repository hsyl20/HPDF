-- | The test suite of HPDF. Each module of Tests has the tests of one
-- feature.
module Main (main) where

import Test.Tasty (defaultMain, testGroup)
import qualified Tests.Image
import qualified Tests.Fonts
import qualified Tests.TrueType
import qualified Tests.Document
import qualified Tests.Typesetting
import qualified Tests.Numbers
import qualified Tests.Colours
import qualified Tests.PDFA
import qualified Tests.EmbeddedFiles
import qualified Tests.Signature
import qualified Tests.PdfFile
import qualified Tests.Encryption
import qualified Tests.Update
import qualified Tests.Repair
import qualified Tests.ObjectStreams
import qualified Tests.OptionalContent

main :: IO ()
main = defaultMain $ testGroup "HPDF"
  [ Tests.Image.tests
  , Tests.Fonts.tests
  , Tests.TrueType.tests
  , Tests.Document.tests
  , Tests.Typesetting.tests
  , Tests.Numbers.tests
  , Tests.Colours.tests
  , Tests.PDFA.tests
  , Tests.EmbeddedFiles.tests
  , Tests.Signature.tests
  , Tests.PdfFile.tests
  , Tests.Encryption.tests
  , Tests.Update.tests
  , Tests.Repair.tests
  , Tests.ObjectStreams.tests
  , Tests.OptionalContent.tests
  ]

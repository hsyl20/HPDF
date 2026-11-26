{-# OPTIONS_GHC -F -pgmF htfpp #-}
-- htfpp adds a list of the tests of this module, which has no test
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- | The test suite of HPDF. Each module of Tests has the tests of one
-- feature.
module Main (main) where

import Test.Framework
import {-@ HTF_TESTS @-} Tests.Image
import {-@ HTF_TESTS @-} Tests.Fonts
import {-@ HTF_TESTS @-} Tests.TrueType
import {-@ HTF_TESTS @-} Tests.Document
import {-@ HTF_TESTS @-} Tests.Typesetting
import {-@ HTF_TESTS @-} Tests.Numbers
import {-@ HTF_TESTS @-} Tests.Colours
import {-@ HTF_TESTS @-} Tests.PDFA
import {-@ HTF_TESTS @-} Tests.EmbeddedFiles
import {-@ HTF_TESTS @-} Tests.Signature

main :: IO ()
main = htfMain htf_importedTests

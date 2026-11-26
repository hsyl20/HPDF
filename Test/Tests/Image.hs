{-# LANGUAGE OverloadedStrings #-}
-- | JPEG images
module Tests.Image (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import Graphics.PDF

tests :: TestTree
tests = testGroup "Image"
  [ test "decodeTick" test_decodeTick
  , test "bounds" test_bounds
  ]


test_decodeTick :: IO()
test_decodeTick = case (readJpegDataURL raw_jpeg) of
                    Right _ -> assertBool True 
                    Left msg -> assertEqual "Error:" msg

test_bounds :: IO()
test_bounds =  assertEqual raw_jpeg_bounds (jpegBounds jpg)

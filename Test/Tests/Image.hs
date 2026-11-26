{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
-- | JPEG images
module Tests.Image (htf_thisModulesTests) where

import Test.Framework
import Tests.Common
import Graphics.PDF

test_decodeTick :: IO()
test_decodeTick = case (readJpegDataURL raw_jpeg) of
                    Right _ -> assertBool True 
                    Left msg -> assertEqual "Error:" msg

test_bounds :: IO()
test_bounds =  assertEqual raw_jpeg_bounds (jpegBounds jpg)

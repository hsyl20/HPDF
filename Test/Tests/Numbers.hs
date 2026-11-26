{-# LANGUAGE OverloadedStrings #-}
-- | The form of the numbers in a document
module Tests.Numbers (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import Graphics.PDF
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import Control.Exception (SomeException, catch, evaluate)
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
import Text.Printf (printf)

tests :: TestTree
tests = testGroup "Numbers"
  [ test "numbersWithFiveDecimals" test_numbersWithFiveDecimals
  , test "numbersThatAreNotFinite" test_numbersThatAreNotFinite
  ]


-- A real number has five decimals at most, without the zeros at the end.
-- It is the nearest to the exact value, and the even one on a tie.
-- See Note [Numbers with five decimals] in Graphics.PDF.LowLevel.Serializer
test_numbersWithFiveDecimals :: IO ()
test_numbersWithFiveDecimals = do
  let -- numbers near a tie of the last decimal, on the two sides
      ties = [ (k + 0.5) / 100000 + e * 1.0e-12 | k <- [0, 1, 7, 12345, 99999, 123456789], e <- [-20, -1, 0, 1, 20] ]
      special = [ 0, -0.0, 0.000005, -0.000005, 0.000004999, 1.5e-5, 2.5e-5, 0.5, 0.125, -1.0e-7, 1.0e-300
                , 9999.999994, 9999.999996, -9999.999996, 10000, 123456789.123456, 1.0e15
                -- ties of the exact value: 1/64 and 3/64
                , 0.015625, -0.015625, 0.046875
                -- the two Doubles next to 10000, the limit of the fast path
                , 9999.999999999998, 10000.000000000002, -9999.999999999998 ]
      -- a linear congruential generator
      random = take 20000 (drop 1 (iterate (\x -> (x * 6364136223846793005 + 1442695040888963407) `mod` (2 ^ (64 :: Int))) (42 :: Integer)))
      scale = [0.5, 3, 700, 40000]
      values = ties ++ special ++
        [ (fromIntegral (r `div` 4096 `mod` 2000001) / 1000000 - 1) * (scale !! fromIntegral (r `mod` 4)) | r <- random ]
      pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        page <- addPage Nothing
        drawWithPage page $ mapM_ (\x -> moveto (x :+ (-x))) values
      written = [ (a, b) | l <- C.lines pdf, [a, b, m] <- [C.words l], m == C.pack "m" ]
      -- the exact value as a Rational; 'round' gives the even number on a tie
      exact x =
        let n = round (abs (toRational x) * 100000) :: Integer
            (q, r) = n `quotRem` 100000
            decimals = reverse (dropWhile (== '0') (reverse (printf "%05d" r)))
        in C.pack $ if n == 0 then "0"
                    else (if x < 0 then "-" else "") ++ show q ++ (if null decimals then "" else '.' : decimals)
      expected = [ (exact x, exact (-x)) | x <- values ]
  assertEqual (length expected) (length written)
  assertEqual [] (take 5 [ (w, e) | (w, e) <- zip written expected, w /= e ])
  -- examples: the exact value of 0.000005 is more than 5e-6, and 1/64 is a tie
  let one x = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        page <- addPage Nothing
        drawWithPage page (moveto (x :+ 0))
  assertBool (C.pack "\n0.00001 0 m" `BS.isInfixOf` one 0.000005)
  assertBool (C.pack "\n0.01562 0 m" `BS.isInfixOf` one 0.015625)
  assertBool (C.pack "\n0.04688 0 m" `BS.isInfixOf` one 0.046875)
  -- no zeros at the end, no point for an integer, no negative zero
  assertBool (C.pack "\n12.5 0 m" `BS.isInfixOf` one 12.5)
  assertBool (C.pack "\n-3 0 m" `BS.isInfixOf` one (-3))
  assertBool (C.pack "\n0.1 0 m" `BS.isInfixOf` one 0.1)
  assertBool (C.pack "\n10000 0 m" `BS.isInfixOf` one 9999.999996)
  assertBool (C.pack "\n0 0 m" `BS.isInfixOf` one (-0.000001))
  assertBool (C.pack "\n0 0 m" `BS.isInfixOf` one (-0.0))

-- A number that is not finite is an error, and 'writePdf' leaves no
-- incomplete file.
-- See Note [Numbers with five decimals] in Graphics.PDF.LowLevel.Serializer
test_numbersThatAreNotFinite :: IO ()
test_numbersThatAreNotFinite = do
  tmp <- getTemporaryDirectory
  let path = tmp ++ "/hpdf-test-not-finite.pdf"
      rect = PDFRect 0 0 300 200
      zero = 0 :: Double
      doc x = do
        page <- addPage Nothing
        -- enough content before the number to fill the buffer of the file
        drawWithPage page $ mapM_ (\i -> moveto (fromIntegral i :+ 0)) [1 .. 20000 :: Int] >> moveto (x :+ 0)
      message :: IO a -> IO String
      message act = (act >> return "") `catch` \e -> return (show (e :: SomeException))
  mapM_ (\x -> do
          m <- message (evaluate (BL.length (renderPdf standardDocInfo {compressed = False} rect (doc x))))
          assertBoolVerbose m ("finite" `elem` words m)
          w <- message (writePdf path standardDocInfo {compressed = False} rect (doc x))
          assertBoolVerbose w ("finite" `elem` words w)
          assertBool . not =<< doesFileExist path)
        [zero / zero, 1 / zero, -1 / zero]
  -- a document with finite numbers is written
  writePdf path standardDocInfo rect (doc 1)
  assertBool =<< doesFileExist path
  removeFile path

---------------------------------------------------------
-- |
-- Read a PDF file, and write it again with one cross-reference table.
--
-- > cabal run HPDF-Rewrite-Demo -- in.pdf out.pdf
---------------------------------------------------------
module Main (main) where

import qualified Data.IntMap as IM
import qualified Graphics.PDF.File as F
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  (input, output) <- case args of
    [i, o] -> return (i, o)
    _ -> die "usage: HPDF-Rewrite-Demo IN.pdf OUT.pdf"
  file <- F.readPdfFile input >>= either die return
  F.writePdfFile output file
  putStrLn (output ++ " written: " ++ show (IM.size (F.pdfObjects file)) ++ " objects")

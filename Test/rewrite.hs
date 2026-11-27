---------------------------------------------------------
-- |
-- Read a PDF file, and write it again with one cross-reference table.
-- The other commands change the layers of the file.
--
-- > cabal run HPDF-Rewrite-Demo -- in.pdf out.pdf
-- > cabal run HPDF-Rewrite-Demo -- layers in.pdf
-- > cabal run HPDF-Rewrite-Demo -- hide in.pdf LAYER out.pdf
-- > cabal run HPDF-Rewrite-Demo -- remove in.pdf LAYER out.pdf
-- > cabal run HPDF-Rewrite-Demo -- page in.pdf PAGE NAME out.pdf
--
-- LAYER is the object number of a layer ("layers" lists them). "page"
-- puts the content of a page (0 is the first one) in a new layer, which
-- is hidden.
---------------------------------------------------------
module Main (main) where

import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Graphics.PDF.File as F
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["layers", i] -> do
      file <- read_file i
      layers <- either die return (F.readLayers file)
      mapM_ (\(n, name, o) -> putStrLn (show n ++ "\t" ++ T.unpack name ++ "\t" ++ show o)) (F.fileLayers layers)
      putStrLn ("panel: " ++ show (F.filePanel layers))
      putStrLn ("radio groups: " ++ show (F.fileRadioGroups layers))
      mapM_ (\c -> putStrLn ("configuration: " ++ show c)) (F.fileConfigs layers)
    ["hide", i, l, o] -> do
      file <- read_file i
      n <- number l
      layers <- either die return (F.readLayers file)
      let hidden = layers {F.fileLayers = [ if m == n then (m, t, opts {F.layerVisible = False}) else x
                                          | x@(m, t, opts) <- F.fileLayers layers ]}
      either die (write o) (F.writeLayers hidden file)
    ["remove", i, l, o] -> do
      file <- read_file i
      n <- number l
      either die (write o) (F.removeLayer F.defaultLimits n file)
    ["page", i, p, name, o] -> do
      file <- read_file i
      page <- number p
      (n, file') <- either die return (F.addLayer (T.pack name) F.defaultLayerOptions {F.layerVisible = False} file)
      either die (write o) (F.putPageInLayer n page file' >>= F.putAnnotationsInLayer n page)
    [i, o] -> read_file i >>= write o
    _ -> die "usage: HPDF-Rewrite-Demo IN.pdf OUT.pdf | layers IN.pdf | hide IN.pdf LAYER OUT.pdf | remove IN.pdf LAYER OUT.pdf | page IN.pdf PAGE NAME OUT.pdf"
  where
    read_file i = F.readPdfFile i >>= either die return
    number s = maybe (die ("not a number: " ++ s)) return (readMaybe s)
    write o file = do
      F.writePdfFile o file
      putStrLn (o ++ " written: " ++ show (IM.size (F.pdfObjects file)) ++ " objects")

-- The source documents of the encrypted fixtures (Test/encrypted):
-- source.pdf, and source-metadata.pdf (the same with a metadata stream)
import Graphics.PDF
import qualified Graphics.PDF.File as F
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

main :: IO ()
main = do
  Right font <- mkStdFont Helvetica
  writePdf "source.pdf" standardDocInfo {author = T.pack "HPDF fixture", compressed = True} (PDFRect 0 0 200 100) $ do
    page <- addPage Nothing
    drawWithPage page $ drawText $ do
      setFont (PDFFont font 12)
      textStart 10 50
      displayText (T.pack "Encrypted text")
  Right file <- F.readPdfFile "source.pdf"
  let Just (F.Ref root _) = F.dictLookup (C.pack "Root") (F.pdfTrailer file)
      Just (_, F.Plain (F.Dict catalog)) = IM.lookup root (F.pdfObjects file)
      n = maybe 1 ((+ 1) . fst) (IM.lookupMax (F.pdfObjects file))
      xmp = C.pack "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">Metadata in clear text</x:xmpmeta>"
      metadata = F.Stream (M.fromList [(C.pack "Type", F.Name (C.pack "Metadata")), (C.pack "Subtype", F.Name (C.pack "XML"))]) xmp
      catalog' = M.insert (C.pack "Metadata") (F.Ref n 0) catalog
  F.writePdfFile "source-metadata.pdf" file
    { F.pdfObjects = IM.insert n (0, metadata) (IM.insert root (0, F.Plain (F.Dict catalog')) (F.pdfObjects file)) }

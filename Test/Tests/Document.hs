{-# LANGUAGE OverloadedStrings #-}
-- | The document monad: steps, sections, pages, lazy and strict drawings
module Tests.Document (tests) where

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
tests = testGroup "Document"
  [ test "pdft" test_pdft
  , test "steps" test_steps
  , test "sectionResult" test_sectionResult
  , test "pageDrawnInSteps" test_pageDrawnInSteps
  , test "lazyAndStrictDrawings" test_lazyAndStrictDrawings
  , test "documentWithoutPage" test_documentWithoutPage
  , test "plainDocumentUnchanged" test_plainDocumentUnchanged
  ]


-- | The PDF monad transformer runs base monad effects in order and generates
-- the same document as the pure PDF monad
test_pdft :: IO()
test_pdft = do
    ref <- newIORef ([] :: [String])
    let rect = PDFRect 0 0 600 400
        doc :: PDFT IO ()
        doc = do
            liftIO $ modifyIORef ref ("start":)
            page <- addPage Nothing
            img <- createPDFJpeg jpg
            liftIO $ modifyIORef ref ("image":)
            drawWithPage page $ drawXObject img
            liftPDF pureTail
            liftIO $ modifyIORef ref ("end":)
        pureTail :: PDF ()
        pureTail = do
            page <- addPage Nothing
            drawWithPage page $ return ()
        pureDoc :: PDF ()
        pureDoc = do
            page <- addPage Nothing
            img <- createPDFJpeg jpg
            drawWithPage page $ drawXObject img
            pureTail
    bs <- renderPdfT standardDocInfo rect doc
    effects <- readIORef ref
    assertEqual ["end","image","start"] effects
    assertBool (BL.isPrefixOf "%PDF-1.5" bs)
    assertEqual (renderPdf standardDocInfo rect pureDoc) bs

-- | The step interface builds the same document as the PDF monad, and
-- rendering an intermediate builder does not alter it
test_steps :: IO()
test_steps = do
    let rect = PDFRect 0 0 600 400
        addImagePage :: PDF ()
        addImagePage = do
            page <- addPage Nothing
            img <- createPDFJpeg jpg
            drawWithPage page $ drawXObject img
        addEmptyPage :: PDF ()
        addEmptyPage = do
            page <- addPage Nothing
            drawWithPage page $ return ()
        s0 = emptyPdfBuilder standardDocInfo rect
        ((), s1) = stepPDF addImagePage s0
        snapshot = buildPdf s1
    ((), s2) <- stepPDFT (liftPDF addEmptyPage) s1
    assertEqual (renderPdf standardDocInfo rect addImagePage) snapshot
    assertEqual (renderPdf standardDocInfo rect (addImagePage >> addEmptyPage))
                (buildPdf s2)
    -- the snapshot did not alter s1
    assertEqual snapshot (buildPdf s1)

-- Embedded TrueType fonts --------------------------------------------------
--
-- These need a TrueType font on the machine: $HPDF_TEST_TTF, or one of the
-- usual system paths. Without one they are reported pending.

-- 'newSection' and 'newSectionWithPage' give the result of their contents.
test_sectionResult :: IO ()
test_sectionResult = do
  ref <- newIORef (0 :: Int, 0 :: Int)
  _ <- renderPdfT standardDocInfo (PDFRect 0 0 300 200) $ do
    page <- addPage Nothing
    a <- newSection (T.pack "a") Nothing Nothing $
      newSection (T.pack "b") Nothing Nothing (return 1)
    b <- newSectionWithPage (T.pack "c") Nothing Nothing page (return 2)
    liftIO (writeIORef ref (a, b))
  readIORef ref >>= assertEqual (1, 2)

-- A page can be drawn in more than one step: the content is in the order
-- of the drawings, also for a page with many drawings.
-- See Note [Memory of the drawings] in Graphics.PDF.Document
test_pageDrawnInSteps :: IO ()
test_pageDrawnInSteps = do
  let steps = 3000 :: Int
      pdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        first <- addPage Nothing
        second <- addPage Nothing
        mapM_ (\i -> drawWithPage (if even i then first else second) (moveto (fromIntegral i :+ 0))) [1 .. steps]
      moves = [ fst n | l <- C.lines pdf, [x, _, m] <- [C.words l], m == C.pack "m", Just n <- [C.readInt x] ]
  -- the two pages are two streams: the odd steps and the even steps, each in order
  assertEqual steps (length moves)
  assertEqual ([2, 4 .. steps] ++ [1, 3 .. steps]) moves

-- 'drawWithPage' is lazy: a drawing can use a value that is known only
-- after it, here the total number of pages. 'drawWithPage'' evaluates the
-- drawing immediately, and writes the same content.
-- See Note [Memory of the drawings] in Graphics.PDF.Document
test_lazyAndStrictDrawings :: IO ()
test_lazyAndStrictDrawings = do
  let rect = PDFRect 0 0 300 200
      -- each page shows its number and the total, and says if a page follows
      pagesWith :: Int -> Int -> PDF Int
      pagesWith total k = do
        page <- addPage Nothing
        more <- drawWithPage page $ do
          moveto (fromIntegral k :+ fromIntegral total)
          return (k < 3)
        if more then pagesWith total (k + 1) else return k
      -- the total is the result of the document that uses it
      (n, builder) = stepPDF (pagesWith n 1) (emptyPdfBuilder standardDocInfo {compressed = False} rect)
      lazyPdf = BL.toStrict (buildPdf builder)
      number t = read (C.unpack t) :: Double
      marks pdf = [ (number a, number b) | l <- C.lines pdf, [a, b, m] <- [C.words l], m == C.pack "m" ]
  assertEqual 3 n
  assertEqual [(1, 3), (2, 3), (3, 3)] (marks lazyPdf)
  -- the same document with the strict function, when the total is known
  let strictPdf = BL.toStrict $ renderPdf standardDocInfo {compressed = False} rect $
        mapM_ (\k -> addPage Nothing >>= \page -> drawWithPage' page (moveto (fromIntegral k :+ 3))) [1 :: Int, 2, 3]
  assertEqual lazyPdf strictPdf
  -- the strict function gives the result of the drawing
  let (r, _) = stepPDF (addPage Nothing >>= \page -> drawWithPage' page (return (42 :: Int))) (emptyPdfBuilder standardDocInfo rect)
  assertEqual 42 r

-- A document without a page is a problem, and not an exception when the
-- bytes are read.
test_documentWithoutPage :: IO ()
test_documentWithoutPage =
  assertEqual (Left ["the document has no page"]) (renderPdfE standardDocInfo (PDFRect 0 0 300 200) (return ()))

-- A document that does not ask for PDF/A is written as before: PDF 1.5,
-- the Info dictionary in the trailer, no /ID.
test_plainDocumentUnchanged :: IO ()
test_plainDocumentUnchanged = do
  let pdf = BL.toStrict $ renderPdf standardDocInfo (PDFRect 0 0 300 200) (addPage Nothing >> return ())
  assertBool (C.pack "%PDF-1.5\n" `BS.isPrefixOf` pdf)
  assertBool (C.pack "/Info <<" `BS.isInfixOf` pdf)
  assertBool (not (C.pack "/ID [" `BS.isInfixOf` pdf))
  assertBool (not (C.pack "/Metadata" `BS.isInfixOf` pdf))

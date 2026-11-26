{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
-- | PDF/A: structure, settings, fonts, colours and annotations
module Tests.PDFA (htf_thisModulesTests) where

import Test.Framework
import Tests.Common
import Graphics.PDF
import Graphics.PDF.Archive
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Control.Exception (SomeException, catch, evaluate)
import Data.Either (isRight)
import System.Directory (doesFileExist, getTemporaryDirectory)

exampleExtension :: XMPExtension
exampleExtension = XMPExtension (T.pack "Example schema") (T.pack "http://example.org/ns/") (T.pack "ex")
  [XMPProperty (T.pack "Level") (T.pack "Text") (T.pack "external") (T.pack "A level") (T.pack "BASIC")]

pdfaDoc :: AnyFont -> PDF ()
pdfaDoc font = do
  setPDFA (pdfa3b fakeIcc) { pdfaTitle = T.pack "Facture 42"
                           , pdfaExtensions = [exampleExtension, facturXExtension (T.pack "BASIC")] }
  _ <- embedAssociatedFile (associatedFile (T.pack "factur-x.xml") "text/xml" (BL.fromStrict (C.pack "<x/>")))
         { afRelationship = Alternative, afDescription = T.pack "Factur-X invoice"
         , afCreationDate = Just "D:20261106000000Z", afModDate = Just "D:20261106000000Z" }
  img <- createPDFRawImageFromByteString 1 1 True NoFilter (BL.pack [255, 0, 0])
  page <- addPage Nothing
  drawWithPage page $ do
    drawText $ text (PDFFont font 12) 10 100 (T.pack "Ça déçoit.")
    drawXObject img

test_pdfaStructure :: IO ()
test_pdfaStructure = withTestFont $ \_ d -> do
  let info = standardDocInfo {compressed = False, author = T.pack "Salmon"}
      Right pdf = fmap BL.toStrict . renderPdfE info (PDFRect 0 0 300 200) $
        mkTrueTypeFont d >>= pdfaDoc
      has s = assertBool' s (C.pack s `BS.isInfixOf` pdf)
      assertBool' s b = if b then return () else assertFailure ("missing: " ++ s)
  -- header with a binary comment
  assertBool (C.pack "%PDF-1.7\n%\xE2\xE3\xCF\xD3\n" `BS.isPrefixOf` pdf)
  mapM_ has
    [ "/Type /Metadata", "/Subtype /XML", "<pdfaid:part>3</pdfaid:part>"
    , "<pdfaid:conformance>B</pdfaid:conformance>"
    , "<ex:Level>BASIC</ex:Level>", "<pdfaSchema:prefix>ex</pdfaSchema:prefix>"
    , "<fx:ConformanceLevel>BASIC</fx:ConformanceLevel>"
    , "<fx:DocumentFileName>factur-x.xml</fx:DocumentFileName>"
    , "<pdfaSchema:prefix>fx</pdfaSchema:prefix>"
    , "<dc:creator><rdf:Seq><rdf:li>Salmon</rdf:li></rdf:Seq></dc:creator>"
    , "/S /GTS_PDFA1", "/OutputIntents", "/DestOutputProfile"
    , "/Type /EmbeddedFile", "/Subtype /text#2Fxml", "/AFRelationship /Alternative"
    , "/UF (factur-x.xml)", "/EmbeddedFiles", "/AF [", "/Title (Facture 42)"
    , "/Author (Salmon)", "/ID [<", "/Interpolate false"
    , "/Size 4", "/CreationDate (D:20261106000000Z)", "/ModDate (D:20261106000000Z)" ]
  assertBool (not (C.pack "/Interpolate true" `BS.isInfixOf` pdf))
  -- the metadata stream is not compressed
  assertBool (not (C.pack "/Filter" `BS.isInfixOf` C.takeWhile (/= 's') (snd (BS.breakSubstring (C.pack "/Subtype /XML") pdf))))

-- The document identifier comes from the document: the same document built
-- step by step has the same one, another document has another one.
test_pdfaDocumentId :: IO ()
test_pdfaDocumentId = withTestFont $ \_ d -> do
  let rect = PDFRect 0 0 300 200
      idOf = fmap (C.take 40 . snd . BS.breakSubstring (C.pack "/ID [<") . BL.toStrict)
      doc = mkTrueTypeFont d >>= pdfaDoc
      whole = renderPdfE standardDocInfo rect doc
      (font, b1) = stepPDF (mkTrueTypeFont d) (emptyPdfBuilder standardDocInfo rect)
      (_, b2) = stepPDF (pdfaDoc font) b1
      other = renderPdfE standardDocInfo {author = T.pack "Another"} rect doc
  assertBool (isRight whole)
  assertEqual whole (buildPdfE b2)
  assertEqual (fmap C.length (idOf whole)) (Right 40)
  assertBool (idOf whole /= idOf other)

-- The settings and the contents that cannot give a PDF/A document.
test_pdfaSettingsAreChecked :: IO ()
test_pdfaSettingsAreChecked = do
  let rect = PDFRect 0 0 300 200
      render settings body = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} rect $ do
        setPDFA settings
        page <- addPage Nothing
        body page
      nothing _ = return ()
      base = pdfa3b fakeIcc
      file mime = \_ -> () <$ embedAssociatedFile (associatedFile (T.pack "a.xml") mime (BL.fromStrict (C.pack "<x/>")))
      extension prefix prop = XMPExtension (T.pack "Schema") (T.pack "http://example.org/ns/") (T.pack prefix)
        [XMPProperty (T.pack prop) (T.pack "Text") (T.pack "external") (T.pack "A level") (T.pack "BASIC")]
      refused = not . isRight
  assertBool (isRight (render base nothing))
  assertBool (isRight (render base {pdfaPart = 2} nothing))
  assertBool (refused (render base {pdfaPart = 1} nothing))
  assertBool (refused (render base {pdfaConformance = "A"} nothing))
  assertBool (isRight (render base (file "text/xml")))
  assertBool (refused (render base {pdfaPart = 2} (file "text/xml")))
  assertBool (refused (render base (file "")))
  assertBool (isRight (render base {pdfaExtensions = [extension "ex" "Level"]} nothing))
  assertBool (refused (render base {pdfaExtensions = [extension "a b" "Level"]} nothing))
  assertBool (refused (render base {pdfaExtensions = [extension "ex" "p q"]} nothing))
  -- a file without a MIME type in a document that is not PDF/A: no /Subtype
  let plain = BL.toStrict . renderPdf standardDocInfo {compressed = False} rect $
        addPage Nothing >>= file ""
  assertBool (C.pack "/Type /EmbeddedFile" `BS.isInfixOf` plain)
  assertBool (not (C.pack "/Subtype /\n" `BS.isInfixOf` plain))
  -- a character that XML cannot have is in no part of the document
  let Right controls = render base {pdfaTitle = T.pack "Ti\SOHtle"} nothing
  assertBool (C.pack "<rdf:li xml:lang=\"x-default\">Title</rdf:li>" `BS.isInfixOf` controls)
  assertBool (C.pack "/Title (Title)" `BS.isInfixOf` controls)

-- The annotations of a PDF/A document: a link has the Print flag, a text
-- annotation is a problem. The other documents are not changed.
test_pdfaAnnotations :: IO ()
test_pdfaAnnotations = do
  let rect = PDFRect 0 0 300 200
      render pdfa annotation = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} rect $ do
        _ <- pdfa
        page <- addPage Nothing
        drawWithPage page (annotation page)
      link page = newAnnotation (PDFLink (T.pack "link") [10, 10, 50, 50] page 0 0 False)
      note _ = newAnnotation (TextAnnotation (T.pack "note") [10, 10, 50, 50] Note)
      archive = setPDFA (pdfa3b fakeIcc)
      flagged = either (const False) (C.pack "/F 4\n" `BS.isInfixOf`)
  assertBool (flagged (render archive link))
  assertBool (isRight (render (return ()) link))
  assertBool (not (flagged (render (return ()) link)))
  assertBool (isRight (render (return ()) note))
  case render archive note of
    Left [problem] -> assertBool ("annotation" `elem` words problem)
    other -> assertFailure ("expected one problem, got " ++ either show (const "a document") other)

-- The profile of the output intent of a PDF/A document decides the device
-- colours that the document can use, in a drawing and in a text object.
-- See Note [Device colours in a PDF/A document] in Graphics.PDF
test_pdfaColoursAndProfiles :: IO ()
test_pdfaColoursAndProfiles = withTestFont $ \_ d -> do
  let rect = PDFRect 0 0 300 200
      render space drawing = renderPdfE standardDocInfo rect $ do
        setPDFA (pdfa3b (fakeIccOf space))
        font <- mkTrueTypeFont d
        page <- addPage Nothing
        drawWithPage page (drawing font)
      problems space drawing = either id (const []) (render space drawing)
      shape colour _ = fillColor colour >> strokeColor colour
      textIn colour font = drawText $ do
        setFont (PDFFont font 12)
        fillColor colour
        textStart 10 10
        displayText (T.pack "text")
      gray = Gray 0.5
      rgb = Rgb 1 0 0
      hsv = Hsv 120 1 1
      cmyk = Cmyk 0 1 1 0
      -- profile, colour, name of the device colour space that is a problem
      table = [ ("RGB ", gray, Nothing), ("RGB ", rgb, Nothing), ("RGB ", hsv, Nothing), ("RGB ", cmyk, Just "DeviceCMYK")
              , ("CMYK", gray, Nothing), ("CMYK", rgb, Just "DeviceRGB"), ("CMYK", hsv, Just "DeviceRGB"), ("CMYK", cmyk, Nothing)
              , ("GRAY", gray, Nothing), ("GRAY", rgb, Just "DeviceRGB"), ("GRAY", cmyk, Just "DeviceCMYK") ]
  mapM_ (\(space, colour, expected) ->
          mapM_ (\drawing -> case (problems space drawing, expected) of
                    ([], Nothing) -> return ()
                    ([problem], Just name) -> assertBoolVerbose problem (name `elem` words problem)
                    (other, _) -> assertFailure (space ++ ": " ++ show other))
                [shape colour, textIn colour])
        table
  -- a content with no colour operator is permitted with each profile
  mapM_ (\space -> assertEqual [] (problems space (\font -> drawText (text (PDFFont font 12) 10 10 (T.pack "text")))))
        ["RGB ", "CMYK", "GRAY"]
  -- the two colours that a gray profile does not permit are two problems
  assertEqual 2 (length (problems "GRAY" (\_ -> fillColor rgb >> strokeColor cmyk)))

test_pdfaRefusesStandardFonts :: IO ()
test_pdfaRefusesStandardFonts = do
  Right helvetica <- mkStdFont Helvetica
  case renderPdfE standardDocInfo (PDFRect 0 0 300 200) (pdfaDoc helvetica) of
    Left [problem] -> assertBool ("Helvetica" `elem` words problem)
    other -> assertFailure ("expected one problem, got " ++ either show (const "a document") other)

-- The functions that cannot return the problems of a document raise an
-- exception, and write no file.
test_pdfaProblemsAreNotIgnored :: IO ()
test_pdfaProblemsAreNotIgnored = do
  Right helvetica <- mkStdFont Helvetica
  tmp <- getTemporaryDirectory
  let path = tmp ++ "/hpdf-test-not-pdfa.pdf"
      rect = PDFRect 0 0 300 200
      raises :: IO a -> IO Bool
      raises act = (act >> return False) `catch` \e -> return (length (show (e :: SomeException)) >= 0)
  assertBool =<< raises (writePdf path standardDocInfo rect (pdfaDoc helvetica))
  assertBool . not =<< doesFileExist path
  assertBool =<< raises (evaluate (BL.length (renderPdf standardDocInfo rect (pdfaDoc helvetica))))
  let (_, builder) = stepPDF (pdfaDoc helvetica) (emptyPdfBuilder standardDocInfo rect)
  assertBool (not (isRight (buildPdfE builder)))
  assertBool =<< raises (evaluate (BL.length (buildPdf builder)))

-- The font check looks at the font, not at its name: a standard font that
-- is only loaded, and not used for text, is not a problem.
test_pdfaFontCheckIsNotByName :: IO ()
test_pdfaFontCheckIsNotByName = withTestFont $ \_ d -> do
  Right _ <- mkStdFont Helvetica
  assertBool (isRight (renderPdfE standardDocInfo (PDFRect 0 0 300 200) (mkTrueTypeFont d >>= pdfaDoc)))

-- An embedded Type 1 font that has the name of a standard font is not a
-- problem.
test_pdfaAcceptsEmbeddedType1 :: IO ()
test_pdfaAcceptsEmbeddedType1 = do
  -- the two names have the same length: the PFB segments keep their size
  let rename b = case BS.breakSubstring (C.pack "Test-Icons") b of
        (before, rest) | BS.null rest -> before
                       | otherwise -> BS.concat [before, C.pack "Times-Bold", rename (BS.drop 10 rest)]
  fs <- readType1Fixture rename
  let render = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        setPDFA (pdfa3b fakeIcc)
        font <- mkType1Font fs
        page <- addPage Nothing
        drawWithPage page $ drawText $ text (PDFFont font 12) 10 10 (T.pack "\xE000\xE001")
  case render of
    Left problems -> assertFailure (unlines problems)
    Right pdf -> do
      assertBool (C.pack "/BaseFont /Times-Bold" `BS.isInfixOf` pdf)
      assertBool (C.pack "/FontFile " `BS.isInfixOf` pdf)

-- The output intent is an RGB, CMYK or gray profile, with its number of
-- components. An image, a shading and a pattern are checked as the colours
-- are.
test_pdfaDeviceColours :: IO ()
test_pdfaDeviceColours = do
  let rect = PDFRect 0 0 300 200
      render icc = fmap BL.toStrict . renderPdfE standardDocInfo {compressed = False} rect $ do
        setPDFA (pdfa3b icc)
        _ <- addPage Nothing
        return ()
      components n = either (const False) (C.pack ("/N " ++ show (n :: Int) ++ "\n") `BS.isInfixOf`)
  assertBool (components 3 (render (fakeIccOf "RGB ")))
  assertBool (components 4 (render (fakeIccOf "CMYK")))
  assertBool (components 1 (render (fakeIccOf "GRAY")))
  assertBool (not (isRight (render (fakeIccOf "Lab "))))
  assertBool (not (isRight (render (BL.fromStrict (C.pack "too short")))))
  -- the header of a CMYK JPEG image of 1 x 1
  let Right cmyk = readJpegDataURL "data:image/jpeg;base64,/9j/wAAUCAABAAEEAREAAhEAAxEABBEA/9k="
      problems space content = either id (const []) $ renderPdfE standardDocInfo rect $ do
        setPDFA (pdfa3b (fakeIccOf space))
        page <- addPage Nothing
        content page
      cmykImage page = createPDFJpeg cmyk >>= \img -> drawWithPage page (drawXObject img)
      rgbImage page = do
        img <- createPDFRawImageFromByteString 1 1 False NoFilter (BL.pack [255, 0, 0])
        drawWithPage page (drawXObject img)
      -- a shading has the colour space of its function
      shading page = drawWithPage page $
        paintWithShading (AxialShading 0 0 100 100 (ColorFunction1 RGBSpace (Interpolated1 1 (1, 0, 0) (0, 0, 1))))
                         (addShape (Rectangle (0 :+ 0) (100 :+ 100)))
      cmykShading page = drawWithPage page $
        paintWithShading (AxialShading 0 0 100 100 (ColorFunction1 CMYKSpace (Interpolated1 1 (0, 1, 1, 0) (1, 0, 0, 0))))
                         (addShape (Rectangle (0 :+ 0) (100 :+ 100)))
      expect name found = case found of
        [problem] -> assertBoolVerbose problem (name `elem` words problem)
        other -> assertFailure ("expected one problem with " ++ name ++ ", got " ++ show other)
  -- a document that is not PDF/A has no such rule
  assertBool (isRight (renderPdfE standardDocInfo rect (addPage Nothing >>= cmykImage)))
  expect "DeviceCMYK" (problems "RGB " cmykImage)
  assertEqual [] (problems "CMYK" cmykImage)
  expect "DeviceCMYK" (problems "GRAY" cmykImage)
  assertEqual [] (problems "RGB " rgbImage)
  expect "DeviceRGB" (problems "CMYK" rgbImage)
  assertEqual [] (problems "RGB " shading)
  expect "DeviceRGB" (problems "CMYK" shading)
  expect "DeviceRGB" (problems "GRAY" shading)
  expect "DeviceCMYK" (problems "RGB " cmykShading)
  assertEqual [] (problems "CMYK" cmykShading)
  -- an uncoloured pattern has the device colour space of its colour
  let patternWith colour page = do
        p <- createUncoloredTiling 0 0 10 10 10 10 ConstantSpacing (stroke (Rectangle (0 :+ 0) (5 :+ 5)))
        drawWithPage page $ do
          setUncoloredFillPattern p colour
          setUncoloredStrokePattern p colour
          fillAndStroke (Rectangle (0 :+ 0) (100 :+ 100))
  assertEqual [] (problems "CMYK" (patternWith (Cmyk 0 1 1 0)))
  expect "DeviceRGB" (problems "CMYK" (patternWith (Rgb 1 0 0)))
  expect "DeviceCMYK" (problems "RGB " (patternWith (Cmyk 0 1 1 0)))
  assertEqual [] (problems "GRAY" (patternWith (Gray 0.5)))
  let patternPdf colour = BL.toStrict $ renderPdf standardDocInfo {compressed = False} rect $
        addPage Nothing >>= patternWith colour
      written colour x = assertBoolVerbose x (C.pack x `BS.isInfixOf` patternPdf colour)
  mapM_ (written (Cmyk 0 1 0.5 0.25)) ["[/Pattern /DeviceCMYK ]", "\n0 1 0.5 0.25  /", " scn", " SCN"]
  mapM_ (written (Gray 0.5)) ["[/Pattern /DeviceGray ]", "\n0.5  /"]
  mapM_ (written (Rgb 1 0 0.5)) ["[/Pattern /DeviceRGB ]", "\n1 0 0.5  /"]

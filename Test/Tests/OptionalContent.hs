{-# LANGUAGE OverloadedStrings #-}
-- | Optional content (layers) of the builder.
-- See Note [Optional content] in Graphics.PDF.OptionalContent
module Tests.OptionalContent (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import Graphics.PDF
import Graphics.PDF.Archive (pdfa3b, setPDFA)
import qualified Graphics.PDF.File as F
import Graphics.PDF.Encryption (encryptPdf)
import Graphics.PDF.Signature (Signature(..), addSignature, signature, signPdf)
import Control.Monad (forM_, replicateM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

tests :: TestTree
tests = testGroup "OptionalContent"
  [ test "layerObjects" test_layerObjects
  , test "noUsageNoVersion" test_noUsageNoVersion
  , test "layersSetByUsage" test_layersSetByUsage
  , test "conditions" test_conditions
  , test "panelAndConfigurations" test_panelAndConfigurations
  , test "contentInLayers" test_contentInLayers
  , test "layerProblems" test_layerProblems
  , test "pdfaLayers" test_pdfaLayers
  , test "layerThatCutsAPath" test_layerThatCutsAPath
  , test "xobjectsInLayers" test_xobjectsInLayers
  , test "encryptedAndSigned" test_encryptedAndSigned
  ]

-- | The document, read back. With object streams or with a table.
render :: Bool -> PDF () -> IO F.PdfFile
render os doc = case renderPdfE standardDocInfo {objectStreams = os} (PDFRect 0 0 200 200) doc of
  Left ps -> assertFailure ("the document has problems: " ++ show ps)
  Right b -> either assertFailure return (F.parsePdfFile (BL.toStrict b))

-- | The problems of a document, to which a page is added
problems :: PDF () -> [String]
problems doc = either id (const []) (renderPdfE standardDocInfo (PDFRect 0 0 200 200) (doc >> addPage Nothing >> return ()))

has_problem :: String -> PDF () -> IO ()
has_problem p doc = assertBoolVerbose (show (p, problems doc)) (any (p `List.isInfixOf`) (problems doc))

catalog :: F.PdfFile -> F.Dict
catalog f = as_dict f (maybe F.Null id (M.lookup "Root" (F.pdfTrailer f)))

as_dict :: F.PdfFile -> F.Value -> F.Dict
as_dict f v = case F.resolveValue f v of
  F.Dict d -> d
  _ -> M.empty

look :: F.PdfFile -> BS.ByteString -> F.Dict -> F.Value
look f k d = maybe F.Null (F.resolveValue f) (F.dictLookup k d)

oc_properties :: F.PdfFile -> F.Dict
oc_properties f = as_dict f (look f "OCProperties" (catalog f))

default_config :: F.PdfFile -> F.Dict
default_config f = as_dict f (look f "D" (oc_properties f))

-- | The object number of the layer with this name
layer :: F.PdfFile -> BS.ByteString -> IO Int
layer f name = case [ n | (n, (_, F.Plain (F.Dict d))) <- IM.toList (F.pdfObjects f)
                        , M.lookup "Type" d == Just (F.Name "OCG"), M.lookup "Name" d == Just (F.String name) ] of
  [n] -> return n
  ns -> assertFailure ("layer " ++ show name ++ ": " ++ show ns)

object_dict :: F.PdfFile -> Int -> F.Dict
object_dict f n = case F.getObject f n of
  Just (F.Plain (F.Dict d)) -> d
  Just (F.Stream d _) -> d
  _ -> M.empty

refs :: [Int] -> F.Value
refs = F.Array . map (`F.Ref` 0)

-- | The first page: its decoded content and its /Properties resources
first_page :: F.PdfFile -> IO (BS.ByteString, F.Dict)
first_page f = case F.pdfPages f of
  (p, _) : _ -> do
    let page = object_dict f p
    content <- case F.dictLookup "Contents" page of
      Just (F.Ref c _) | Just (F.Stream d bytes) <- F.getObject f c -> either assertFailure return (F.decodeStream maxBound d bytes)
      other -> assertFailure ("contents: " ++ show other)
    let resources = as_dict f (look f "Resources" page)
    return (content, as_dict f (look f "Properties" resources))
  [] -> assertFailure "no page"

-- | The name of a /Properties entry that refers to this object
property_name :: F.Dict -> Int -> IO BS.ByteString
property_name properties n = case [ k | (k, F.Ref m _) <- M.toList properties, m == n ] of
  [k] -> return k
  ks -> assertFailure ("properties " ++ show properties ++ ": " ++ show ks)

text_string :: T.Text -> F.Value
text_string t = F.String (BS.pack [0xFE, 0xFF] <> TE.encodeUtf16BE t)

test_layerObjects :: IO ()
test_layerObjects = forM_ [False, True] $ \os -> do
  f <- render os $ do
    grid <- createLayer "Grid" defaultLayerOptions
    draft <- createLayer "Draft" defaultLayerOptions {layerVisible = False, layerPrint = Just True}
    fixed <- createLayer "Fixed" defaultLayerOptions {layerLocked = True}
    page <- addPage Nothing
    drawWithPage page $ do
      inLayer grid $ stroke (Line 0 0 100 100)
      inLayer draft $ fill (Rectangle (10 :+ 10) (20 :+ 20))
      inLayer fixed $ stroke (Line 0 100 100 0)
  [grid, draft, fixed] <- mapM (layer f) ["Grid", "Draft", "Fixed"]
  assertEqual (refs [grid, draft, fixed]) (look f "OCGs" (oc_properties f))
  let d = default_config f
  assertEqual (F.String "Default") (look f "Name" d)
  assertEqual (refs [draft]) (look f "OFF" d)
  assertEqual (refs [grid, draft, fixed]) (look f "Order" d)
  assertEqual (refs [fixed]) (look f "Locked" d)
  assertEqual F.Null (look f "RBGroups" d)
  assertEqual F.Null (look f "Configs" (oc_properties f))
  assertEqual (F.Array [F.Dict (M.fromList [ ("Event", F.Name "Print"), ("Category", F.Array [F.Name "Print"])
                                          , ("OCGs", refs [draft]) ])])
              (look f "AS" d)
  -- /Locked is PDF 1.6
  assertEqual (F.Name "1.6") (look f "Version" (catalog f))
  assertEqual (F.Dict (M.fromList [("Print", F.Dict (M.fromList [("PrintState", F.Name "ON")]))]))
              (look f "Usage" (object_dict f draft))
  assertEqual F.Null (look f "Usage" (object_dict f grid))
  (content, properties) <- first_page f
  names <- mapM (property_name properties) [grid, draft, fixed]
  forM_ names $ \n -> assertBoolVerbose (show (n, content)) (("/OC /" <> n <> " BDC") `BS.isInfixOf` content)
  assertEqual 3 (count "EMC" content)

count :: BS.ByteString -> BS.ByteString -> Int
count needle = length . occurrences needle

test_noUsageNoVersion :: IO ()
test_noUsageNoVersion = do
  f <- render True $ do
    l <- createLayer "Plain" defaultLayerOptions
    page <- addPage Nothing
    drawWithPage page (inLayer l (stroke (Line 0 0 1 1)))
  assertEqual F.Null (look f "AS" (default_config f))
  assertEqual F.Null (look f "OFF" (default_config f))
  assertEqual F.Null (look f "Locked" (default_config f))
  assertEqual F.Null (look f "Version" (catalog f))
  -- a document with no layer has no /OCProperties
  g <- render True (addPage Nothing >> return ())
  assertEqual F.Null (look g "OCProperties" (catalog g))

test_layersSetByUsage :: IO ()
test_layersSetByUsage = forM_ [False, True] $ \os -> do
  f <- render os $ do
    _ <- createLayer "Detail" defaultLayerOptions {layerZoom = Just (0.5, Just 2)}
    _ <- createLayer "Far" defaultLayerOptions {layerZoom = Just (0, Nothing)}
    _ <- createLayer "Screen" defaultLayerOptions {layerExport = Just False}
    _ <- createLayer "Deutsch" defaultLayerOptions {layerLanguage = Just (LayerLanguage "de-CH" True)}
    _ <- createLayer "\201bauche" defaultLayerOptions {layerLanguage = Just (LayerLanguage "fr" False), layerPrint = Just False}
    _ <- createLayer "None" defaultLayerOptions
    addPage Nothing >> return ()
  [detail, far, screen, deutsch, none] <- mapM (layer f) ["Detail", "Far", "Screen", "Deutsch", "None"]
  [ebauche] <- case [ n | (n, (_, F.Plain (F.Dict d))) <- IM.toList (F.pdfObjects f)
                        , M.lookup "Name" d == Just (text_string "\201bauche") ] of
    [n] -> return [n]
    ns -> assertFailure (show ns)
  let usage n = look f "Usage" (object_dict f n)
      dict = F.Dict . M.fromList
      number v = case v of
        F.Integer i -> Just (fromInteger i)
        F.Real m k -> Just (F.realToRational m k)
        _ -> Nothing
      zoom n = case usage n of
        F.Dict u | F.Dict z <- maybe F.Null id (M.lookup "Zoom" u) -> (number =<< M.lookup "min" z, number =<< M.lookup "max" z)
        _ -> (Nothing, Nothing)
  assertEqual (Just 0.5, Just 2) (zoom detail)
  assertEqual (Just 0, Nothing) (zoom far)
  assertEqual (dict [("Export", dict [("ExportState", F.Name "OFF")])]) (usage screen)
  assertEqual (dict [("Language", dict [("Lang", F.String "de-CH"), ("Preferred", F.Name "ON")])]) (usage deutsch)
  assertEqual (dict [ ("Language", dict [("Lang", F.String "fr"), ("Preferred", F.Name "OFF")])
                    , ("Print", dict [("PrintState", F.Name "OFF")]) ]) (usage ebauche)
  assertEqual F.Null (usage none)
  -- See Note [Layers set by usage]
  assertEqual (F.Array
    [ dict [("Event", F.Name "View"), ("Category", F.Array [F.Name "Zoom", F.Name "Language"]), ("OCGs", refs [detail, far, deutsch, ebauche])]
    , dict [("Event", F.Name "Print"), ("Category", F.Array [F.Name "Print"]), ("OCGs", refs [ebauche])]
    , dict [("Event", F.Name "Export"), ("Category", F.Array [F.Name "Export"]), ("OCGs", refs [screen])]
    ]) (look f "AS" (default_config f))

test_conditions :: IO ()
test_conditions = forM_ [False, True] $ \os -> do
  f <- render os $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    c <- createLayer "C" defaultLayerOptions
    conditions <- mapM createLayerCondition
      [ LayerAnd [LayerOn a, LayerOn b]
      , LayerOr [LayerOn a, LayerOn b, LayerOn a]
      , LayerOr [LayerNot (LayerOn a), LayerNot (LayerOn c)]
      , LayerAnd [LayerNot (LayerOn b), LayerNot (LayerOn c)]
      , LayerOn c
      , LayerNot (LayerOn c)
      , LayerOr [LayerOn a, LayerAnd [LayerOn b, LayerNot (LayerOn c)]]
      ]
    page <- addPage Nothing
    drawWithPage page $ mapM_ (\x -> inLayer x (stroke (Line 0 0 1 1))) conditions
  [a, b, c] <- mapM (layer f) ["A", "B", "C"]
  (_, properties) <- first_page f
  let ocmds = [ (n, d) | (n, (_, F.Plain (F.Dict d))) <- IM.toList (F.pdfObjects f), M.lookup "Type" d == Just (F.Name "OCMD") ]
      ocmd ls p = M.fromList ([("Type", F.Name "OCMD"), ("OCGs", refs ls)] ++ [ ("P", F.Name x) | Just x <- [p] ])
      r n = F.Ref n 0
  assertEqual 7 (length ocmds)
  assertEqual
    [ ocmd [a, b] (Just "AllOn"), ocmd [a, b] (Just "AnyOn"), ocmd [a, c] (Just "AnyOff"), ocmd [b, c] (Just "AllOff")
    , ocmd [c] (Just "AnyOn"), ocmd [c] (Just "AllOff")
    , M.insert "VE" (F.Array [F.Name "Or", r a, F.Array [F.Name "And", r b, F.Array [F.Name "Not", r c]]]) (ocmd [a, b, c] Nothing)
    ] (map snd ocmds)
  -- the content uses the conditions
  mapM_ (property_name properties . fst) ocmds
  -- /VE is PDF 1.6
  assertEqual (F.Name "1.6") (look f "Version" (catalog f))

test_panelAndConfigurations :: IO ()
test_panelAndConfigurations = forM_ [False, True] $ \os -> do
  f <- render os $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    c <- createLayer "C" defaultLayerOptions
    d <- createLayer "D" defaultLayerOptions {layerVisible = False}
    _ <- createLayer "Small" defaultLayerOptions {layerZoom = Just (2, Nothing)}
    setLayerPanel [LayerEntry a [LayerEntry b []], LayerLabel "Maps" [LayerEntry c [], LayerEntry d []], LayerLabel "Empty" []]
    addRadioGroup [c, d, d]
    addLayerConfig (layerConfig "Print") {configVisible = [a, c], configLocked = [a], configRadioGroups = [[a, b, b]]}
    addLayerConfig (layerConfig "Auto") {configPanel = Just [LayerEntry d []], configUsage = True}
    addPage Nothing >> return ()
  [a, b, c, d, small] <- mapM (layer f) ["A", "B", "C", "D", "Small"]
  let r n = F.Ref n 0
      dict = F.Dict . M.fromList
      order = F.Array [r a, F.Array [r b], F.Array [F.String "Maps", r c, r d], F.Array [F.String "Empty"]]
      automatic = F.Array [dict [("Event", F.Name "View"), ("Category", F.Array [F.Name "Zoom"]), ("OCGs", refs [small])]]
  assertEqual order (look f "Order" (default_config f))
  assertEqual (F.Array [refs [c, d]]) (look f "RBGroups" (default_config f))
  assertEqual (refs [d]) (look f "OFF" (default_config f))
  case look f "Configs" (oc_properties f) of
    F.Array [print_config, auto] -> do
      assertEqual (dict [ ("Name", F.String "Print"), ("BaseState", F.Name "OFF"), ("ON", refs [a, c])
                        , ("Order", order), ("RBGroups", F.Array [refs [a, b]]), ("Locked", refs [a]) ]) print_config
      assertEqual (dict [ ("Name", F.String "Auto"), ("BaseState", F.Name "OFF"), ("Order", F.Array [r d])
                        , ("AS", automatic) ]) auto
    other -> assertFailure (show other)
  assertEqual automatic (look f "AS" (default_config f))
  assertEqual (F.Name "1.6") (look f "Version" (catalog f))

test_contentInLayers :: IO ()
test_contentInLayers = forM_ [False, True] $ \os -> do
  f <- render os $ do
    outer <- createLayer "Outer" defaultLayerOptions
    inner <- createLayer "Inner" defaultLayerOptions
    form <- createPDFXForm 0 0 10 10 (inLayer inner (stroke (Line 0 0 10 10)))
    page <- addPage Nothing
    drawWithPage page $ do
      inLayer outer $ do
        inLayer inner $ stroke (Line 0 0 5 5)
        stroke (Line 1 1 2 2)
      inLayer outer $ drawXObject form
      newAnnotation (layerAnnotation inner (TextAnnotation "note" [0, 0, 10, 10] Note))
  [outer, inner] <- mapM (layer f) ["Outer", "Inner"]
  (content, properties) <- first_page f
  -- the same layer two times: one name
  assertEqual 2 (M.size properties)
  [o, i] <- mapM (property_name properties) [outer, inner]
  let at s = BS.length (fst (BS.breakSubstring s content))
  -- nested sequences: outer, inner, EMC, EMC, then outer again
  assertBoolVerbose (show content) $
    at ("/OC /" <> o <> " BDC") < at ("/OC /" <> i <> " BDC")
  assertEqual 3 (count "BDC" content)
  assertEqual 3 (count "EMC" content)
  -- the form has its own /Properties
  let forms = [ d | (_, (_, F.Stream d _)) <- IM.toList (F.pdfObjects f), M.lookup "Subtype" d == Just (F.Name "Form") ]
  case forms of
    [d] -> assertEqual [F.Ref inner 0] (M.elems (as_dict f (look f "Properties" (as_dict f (look f "Resources" d)))))
    _ -> assertFailure (show forms)
  -- the annotation is in the layer
  let annots = [ d | (_, (_, F.Plain (F.Dict d))) <- IM.toList (F.pdfObjects f), M.lookup "Type" d == Just (F.Name "Annot") ]
  assertEqual [Just (F.Ref inner 0)] (map (M.lookup "OC") annots)

test_layerProblems :: IO ()
test_layerProblems = do
  has_problem "has a zoom from" $ createLayer "Z" defaultLayerOptions {layerZoom = Just (-1, Nothing)} >> return ()
  has_problem "has a zoom from" $ createLayer "Z" defaultLayerOptions {layerZoom = Just (2, Just 1)} >> return ()
  has_problem "has a zoom from" $ createLayer "Z" defaultLayerOptions {layerZoom = Just (0 / 0, Nothing)} >> return ()
  has_problem "has a zoom from" $ createLayer "Z" defaultLayerOptions {layerZoom = Just (1, Just (1 / 0))} >> return ()
  has_problem "with no condition" $ do
    a <- createLayer "A" defaultLayerOptions
    _ <- createLayerCondition (LayerOr [LayerOn a, LayerAnd []])
    return ()
  has_problem "more than one visible layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    addRadioGroup [a, b]
  has_problem "more than one visible layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    addLayerConfig (layerConfig "Both") {configVisible = [a, b], configRadioGroups = [[a, b]]}
  has_problem "more than one time" $ do
    a <- createLayer "A" defaultLayerOptions
    setLayerPanel [LayerEntry a [LayerEntry a []]]
  has_problem "more than one time" $ do
    a <- createLayer "A" defaultLayerOptions
    addLayerConfig (layerConfig "C") {configPanel = Just [LayerEntry a [], LayerLabel "x" [LayerEntry a []]]}
  -- See Note [Labels of the layer panel] in Graphics.PDF.OptionalContent
  has_problem "immediately after the layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    setLayerPanel [LayerEntry a [], LayerLabel "L" [LayerEntry b []]]
  has_problem "immediately after the layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    addLayerConfig (layerConfig "C") {configPanel = Just [LayerLabel "M" [LayerEntry a [], LayerLabel "L" [LayerEntry b []]]]}
  has_problem "as the first child of the layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    setLayerPanel [LayerEntry a [LayerLabel "L" [LayerEntry b []]]]
  has_problem "as the first child of the layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    c <- createLayer "C" defaultLayerOptions
    d <- createLayer "D" defaultLayerOptions
    setLayerPanel [LayerEntry a [LayerEntry b [LayerLabel "L" [LayerEntry c []], LayerEntry d []]]]
  has_problem "as the first item of the group" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    c <- createLayer "C" defaultLayerOptions
    setLayerPanel [LayerLabel "L1" [LayerLabel "L2" [LayerEntry a []], LayerEntry b [LayerEntry c []]]]
  -- correct: a group with a label after a layer with children, in the
  -- children of a layer, and after another group
  assertEqual [] $ problems $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    c <- createLayer "C" defaultLayerOptions
    d <- createLayer "D" defaultLayerOptions
    setLayerPanel [LayerEntry a [LayerEntry b [LayerEntry c []], LayerLabel "L" [LayerEntry d []], LayerLabel "M" []]]
  has_problem "have the name" $ do
    _ <- createLayer "A" defaultLayerOptions
    addLayerConfig (layerConfig "C")
    addLayerConfig (layerConfig "C")
  has_problem "an empty name" $ createLayer "A" defaultLayerOptions >> addLayerConfig (layerConfig "")
  has_problem "\"Default\"" $ createLayer "A" defaultLayerOptions >> addLayerConfig (layerConfig "Default")
  -- the maximum is not included: this layer is never visible
  has_problem "has a zoom from" $ createLayer "Z" defaultLayerOptions {layerZoom = Just (1, Just 1)} >> return ()
  -- these are correct: a hidden layer in a radio group, a valid zoom, a
  -- layer two times in a group
  assertEqual [] $ problems $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions {layerVisible = False, layerZoom = Just (0, Just 0.5)}
    addRadioGroup [a, b, a]
    addLayerConfig (layerConfig "C") {configVisible = [a], configRadioGroups = [[a, a, b]]}
  -- a layer or a condition of another document: an object number that is
  -- not a layer of this document
  let ((foreign_layer, foreign_condition), _) =
        stepPDF other_document (emptyPdfBuilder standardDocInfo (PDFRect 0 0 200 200))
      another = "of another document"
  has_problem another $ setLayerPanel [LayerEntry foreign_layer []]
  has_problem another $ createLayer "A" defaultLayerOptions >>= \a -> addRadioGroup [a, foreign_layer]
  has_problem another $ addLayerConfig (layerConfig "C") {configVisible = [foreign_layer]}
  has_problem another $ addLayerConfig (layerConfig "C") {configLocked = [foreign_layer]}
  has_problem another $ createLayerCondition (LayerOn foreign_layer) >> return ()
  has_problem another $ do
    page <- addPage Nothing
    drawWithPage page (inLayer foreign_layer (stroke (Line 0 0 1 1)))
  has_problem another $ do
    page <- addPage Nothing
    drawWithPage page (inLayer foreign_condition (stroke (Line 0 0 1 1)))
  has_problem another $ do
    page <- addPage Nothing
    drawWithPage page (newAnnotation (layerAnnotation foreign_condition (TextAnnotation "note" [0, 0, 10, 10] Note)))
  -- a form XObject: its resources are checked too
  has_problem another $ do
    _ <- createPDFXForm 0 0 10 10 (inLayer foreign_layer (stroke (Line 0 0 1 1)))
    return ()

-- | A layer and a condition of another document
other_document :: PDF (Layer, LayerCondition)
other_document = do
  _ <- createLayer "X" defaultLayerOptions
  l <- createLayer "Y" defaultLayerOptions
  c <- createLayerCondition (LayerOn l)
  return (l, c)

-- | A PDF/A document with no text: no font is needed
pdfa_doc :: PDF () -> PDF ()
pdfa_doc layers = do
  setPDFA (pdfa3b fakeIcc)
  layers
  page <- addPage Nothing
  drawWithPage page (stroke (Line 0 0 1 1))

test_pdfaLayers :: IO ()
test_pdfaLayers = do
  -- a correct PDF/A document with layers
  f <- render True $ pdfa_doc $ do
    a <- createLayer "A" defaultLayerOptions {layerLocked = True}
    b <- createLayer "B" defaultLayerOptions {layerVisible = False}
    addLayerConfig (layerConfig "Other") {configVisible = [b]}
    addRadioGroup [a, b]
  -- PDF/A is 1.7: no /Version
  assertEqual F.Null (look f "Version" (catalog f))
  assertEqual F.Null (look f "AS" (default_config f))
  has_problem "PDF/A: the layer \"P\" has a usage" $ pdfa_doc $
    createLayer "P" defaultLayerOptions {layerPrint = Just True} >> return ()
  has_problem "applies the usage" $ pdfa_doc $
    createLayer "A" defaultLayerOptions >> addLayerConfig (layerConfig "C") {configUsage = True}
  has_problem "PDF/A: the layer panel does not have all the layers" $ pdfa_doc $ do
    a <- createLayer "A" defaultLayerOptions
    _ <- createLayer "B" defaultLayerOptions
    setLayerPanel [LayerEntry a []]
  has_problem "of the configuration \"C\" does not have all the layers" $ pdfa_doc $ do
    a <- createLayer "A" defaultLayerOptions
    _ <- createLayer "B" defaultLayerOptions
    addLayerConfig (layerConfig "C") {configPanel = Just [LayerEntry a []]}

-- | A layer must not cut a path. See Note [Optional content] in
-- Graphics.PDF.OptionalContent
test_layerThatCutsAPath :: IO ()
test_layerThatCutsAPath = do
  let on_page draw = do
        l <- createLayer "A" defaultLayerOptions
        page <- addPage Nothing
        drawWithPage page (draw l)
  -- the path starts before the layer, and is painted in it or after it
  has_problem "the layer cuts the path" $ on_page $ \l -> do
    beginPath (0 :+ 0)
    inLayer l (addLineToPath (10 :+ 10))
    strokePath
  has_problem "the layer cuts the path" $ on_page $ \l -> do
    beginPath (0 :+ 0)
    addLineToPath (10 :+ 10)
    inLayer l strokePath
  -- the path starts in the layer, and is painted after it
  has_problem "the layer cuts the path" $ on_page $ \l -> do
    inLayer l (addShape (Rectangle (0 :+ 0) (10 :+ 10)))
    fillPath
  has_problem "the layer cuts the path" $ on_page $ \l -> do
    inLayer l (beginPath (0 :+ 0) >> addBezierCubic (1 :+ 1) (2 :+ 2) (3 :+ 3) >> closePath)
    fillAndStrokePath
  -- each operator of a path opens it
  forM_ [ beginPath (0 :+ 0), addLineToPath (1 :+ 1), addBezierCubic (1 :+ 1) (2 :+ 2) (3 :+ 3), closePath ] $ \op ->
    has_problem "the layer cuts the path" $ on_page $ \l -> inLayer l op >> strokePath
  -- correct: each path is painted in its layer, nested layers, a clip
  -- path, and a path after a layer
  assertEqual [] $ problems $ on_page $ \l -> do
    inLayer l (beginPath (0 :+ 0) >> addLineToPath (10 :+ 10) >> closePath >> fillPathEO)
    inLayer l (inLayer l (stroke (Line 0 0 1 1)))
    inLayer l (withNewContext (addShape (Rectangle (0 :+ 0) (5 :+ 5)) >> setAsClipPathEO >> fill (Circle 2 2 2)))
    inLayer l (addShape (Rectangle (0 :+ 0) (5 :+ 5)) >> setAsClipPath)
    beginPath (0 :+ 0)
    addLineToPath (1 :+ 1)
    strokePath
  -- the Q of withNewContext ends a path that is not painted: a layer after
  -- it, in another drawing of the page, is correct
  assertEqual [] $ problems $ do
    l <- createLayer "A" defaultLayerOptions
    page <- addPage Nothing
    drawWithPage page (withNewContext (addShape (Rectangle (0 :+ 0) (5 :+ 5))))
    drawWithPage page (inLayer l (stroke (Line 0 0 1 1)))
  -- a path that is open across two drawings of a page
  has_problem "the layer cuts the path" $ do
    l <- createLayer "A" defaultLayerOptions
    page <- addPage Nothing
    drawWithPage page (beginPath (0 :+ 0))
    drawWithPage page (inLayer l (addLineToPath (1 :+ 1) >> strokePath))

-- | 'layerXObject' writes /OC in the dictionary of an image or a form
test_xobjectsInLayers :: IO ()
test_xobjectsInLayers = do
  forM_ [False, True] $ \os -> do
    f <- render os $ do
      logo <- createLayer "Logo" defaultLayerOptions {layerVisible = False}
      form <- createPDFXForm 0 0 10 10 (stroke (Line 0 0 10 10))
      img <- createPDFRawImageFromByteString 1 1 True NoFilter (BL.pack [255, 0, 0])
      layerXObject logo form
      layerXObject logo img
      page <- addPage Nothing
      drawWithPage page (drawXObject form >> drawXObject img)
    logo <- layer f "Logo"
    let xobjects = [ (s, M.lookup "OC" d) | (_, (_, F.Stream d _)) <- IM.toList (F.pdfObjects f)
                                         , Just (F.Name s) <- [M.lookup "Subtype" d], s `elem` ["Form", "Image"] ]
    assertEqual [("Form", Just (F.Ref logo 0)), ("Image", Just (F.Ref logo 0))] (List.sortOn fst xobjects)
    -- the content does not change
    (content, properties) <- first_page f
    assertEqual 0 (count "BDC" content)
    assertEqual M.empty properties
  -- a layer of another document, and a form of another document
  let ((foreign_layer, foreign_form), _) =
        stepPDF (do replicateM_ 50 (createLayer "X" defaultLayerOptions)
                    l <- createLayer "Y" defaultLayerOptions
                    form <- createPDFXForm 0 0 10 10 (return ())
                    return (l, form))
                (emptyPdfBuilder standardDocInfo (PDFRect 0 0 200 200))
  has_problem "of another document" $ do
    form <- createPDFXForm 0 0 10 10 (return ())
    layerXObject foreign_layer form
  has_problem "is not an XObject of this document" $ do
    l <- createLayer "A" defaultLayerOptions
    layerXObject l foreign_form
  -- one layer for an XObject: a condition for more than one
  has_problem "is already in a layer" $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    form <- createPDFXForm 0 0 10 10 (return ())
    layerXObject a form
    layerXObject b form
  assertEqual [] $ problems $ do
    a <- createLayer "A" defaultLayerOptions
    b <- createLayer "B" defaultLayerOptions
    both <- createLayerCondition (LayerAnd [LayerOn a, LayerOn b])
    form <- createPDFXForm 0 0 10 10 (return ())
    layerXObject both form

-- | A document with layers, encrypted, then signed: the layers are read
-- back with the password, and the signature is in place
test_encryptedAndSigned :: IO ()
test_encryptedAndSigned = do
  let unsigned = renderPdf standardDocInfo (PDFRect 0 0 300 200) $ do
        secret <- createLayer "Secret layer" defaultLayerOptions {layerVisible = False, layerPrint = Just True}
        other <- createLayerCondition (LayerNot (LayerOn secret))
        page <- addPage Nothing
        drawWithPage page $ do
          inLayer secret (stroke (Line 0 0 10 10))
          inLayer other (stroke (Line 0 10 10 0))
        addSignature page signature {sigName = "A. Signer", sigSize = 64}
  encrypted <- encryptPdf (F.encryption "user" "owner") unsigned >>= either assertFailure return
  -- the name of the layer is a string: it is encrypted
  assertBool (not ("Secret layer" `BS.isInfixOf` BL.toStrict encrypted))
  signed <- signPdf (\_ -> return (BS.pack [0xDE, 0xAD])) encrypted >>= either assertFailure return
  assertEqual (BL.length encrypted) (BL.length signed)
  f <- either assertFailure return (F.parsePdfFileWithPassword F.defaultLimits "user" (BL.toStrict signed))
  secret <- layer f "Secret layer"
  let d = default_config f
  assertEqual (refs [secret]) (look f "OFF" d)
  assertEqual (refs [secret]) (look f "OCGs" (oc_properties f))
  assertEqual (F.Array [F.Dict (M.fromList [ ("Event", F.Name "Print"), ("Category", F.Array [F.Name "Print"])
                                          , ("OCGs", refs [secret]) ])]) (look f "AS" d)
  (content, properties) <- first_page f
  name <- property_name properties secret
  assertBool (("/OC /" <> name <> " BDC") `BS.isInfixOf` content)
  case [ m | (_, F.Ref m _) <- M.toList properties, m /= secret ] of
    [condition] -> assertEqual (M.fromList [("Type", F.Name "OCMD"), ("OCGs", refs [secret]), ("P", F.Name "AllOff")])
                               (object_dict f condition)
    other -> assertFailure (show other)
  assertEqual [BS.pack (0xDE : 0xAD : replicate 62 0)]
              [ s | (_, F.Plain (F.Dict sig)) <- IM.elems (F.pdfObjects f), F.isSignatureDict sig, Just (F.String s) <- [M.lookup "Contents" sig] ]

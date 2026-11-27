{-# LANGUAGE OverloadedStrings #-}
-- | The layers of a file that is read: list, change, put a page in a
-- layer, remove a layer. See Note [Layers of a file] in
-- Graphics.PDF.File.Layers
module Tests.FileLayers (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import Graphics.PDF
import qualified Graphics.PDF.File as F
import Control.Monad (forM_)
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.List as List
import qualified Data.Map.Strict as M

tests :: TestTree
tests = testGroup "FileLayers"
  [ test "layersOfTheBuilder" test_layersOfTheBuilder
  , test "writeWithNoChange" test_writeWithNoChange
  , test "changeOfTheLayers" test_changeOfTheLayers
  , test "statesOfAFile" test_statesOfAFile
  , test "addLayer" test_addLayer
  , test "pageInALayer" test_pageInALayer
  , test "removeContent" test_removeContent
  , test "removeOtherUses" test_removeOtherUses
  , test "removeRefusals" test_removeRefusals
  , test "removeAnUnusedLayer" test_removeAnUnusedLayer
  , test "textFilters" test_textFilters
  , test "reviewCases" test_reviewCases
  , test "removeFromConfigs" test_removeFromConfigs
  ]

------------------------------------------------------------------------
-- Files

ref :: Int -> F.Value
ref n = F.Ref n 0

dict :: [(BS.ByteString, F.Value)] -> F.Value
dict = F.Dict . M.fromList

stream :: [(BS.ByteString, F.Value)] -> BS.ByteString -> F.Object
stream d s = F.Stream (M.fromList (("Length", F.Integer (fromIntegral (BS.length s))) : d)) s

-- | A file with one page, the layers L (10, off) and M (11), the
-- conditions C (12: L or M), D (13: L) and E (14: not L and not M), and
-- the annotations 40 (in L) and 41 (in M). The content of the page is
-- these streams (20, 21, ...).
hand_file :: [BS.ByteString] -> [(Int, F.Object)] -> F.PdfFile
hand_file contents extra = F.PdfFile (1, 7) (M.fromList [("Root", ref 1)])
  (IM.fromList [ (n, (0, o)) | (n, o) <- base ++ zip [20 ..] (map (stream []) contents) ++ extra ]) Nothing
  where
    plain = F.Plain . dict
    content_value = case contents of
      [_] -> ref 20
      _ -> F.Array (map ref (take (length contents) [20 ..]))
    base =
      [ (1, plain [ ("Type", F.Name "Catalog"), ("Pages", ref 2)
                  , ("OCProperties", dict [ ("OCGs", F.Array [ref 10, ref 11])
                                          , ("D", dict [("Order", F.Array [ref 10, ref 11]), ("OFF", F.Array [ref 10])]) ]) ])
      , (2, plain [("Type", F.Name "Pages"), ("Kids", F.Array [ref 3]), ("Count", F.Integer 1)])
      , (3, plain [ ("Type", F.Name "Page"), ("Parent", ref 2), ("Contents", content_value)
                  , ("MediaBox", F.Array (map F.Integer [0, 0, 100, 100]))
                  , ("Resources", dict [ ("Properties", dict [("L", ref 10), ("M", ref 11), ("C", ref 12), ("D", ref 13), ("E", ref 14)])
                                       , ("XObject", dict [("X", ref 30), ("Y", ref 31)]) ])
                  , ("Annots", F.Array [ref 40, ref 41]) ])
      , (10, plain [("Type", F.Name "OCG"), ("Name", F.String "L")])
      , (11, plain [("Type", F.Name "OCG"), ("Name", F.String "M")])
      , (12, plain [("Type", F.Name "OCMD"), ("OCGs", F.Array [ref 10, ref 11]), ("P", F.Name "AnyOn")])
      , (13, plain [("Type", F.Name "OCMD"), ("OCGs", ref 10)])
      , (14, plain [("Type", F.Name "OCMD"), ("OCGs", F.Array [ref 10, ref 11]), ("P", F.Name "AllOff")])
      , (40, plain [("Type", F.Name "Annot"), ("Subtype", F.Name "Text"), ("OC", ref 10)])
      , (41, plain [("Type", F.Name "Annot"), ("Subtype", F.Name "Text"), ("OC", ref 11)])
      ]

object :: F.PdfFile -> Int -> F.Dict
object f n = case F.getObject f n of
  Just (F.Plain (F.Dict d)) -> d
  Just (F.Stream d _) -> d
  _ -> M.empty

-- | The decoded data of a stream
stream_data :: F.PdfFile -> Int -> IO BS.ByteString
stream_data f n = case F.getObject f n of
  Just (F.Stream d s) -> either assertFailure return (F.decodeStream maxBound d s)
  other -> assertFailure ("not a stream: " ++ show other)

-- | The content of the first page, as one
page_content :: F.PdfFile -> IO BS.ByteString
page_content f = case F.pdfPages f of
  (p, _) : _ -> case M.lookup "Contents" (object f p) of
    Just (F.Ref n _) -> stream_data f n
    Just (F.Array xs) -> BS.intercalate "\n" <$> mapM (\x -> case x of
                                                         F.Ref n _ -> stream_data f n
                                                         _ -> assertFailure "contents") xs
    other -> assertFailure ("contents: " ++ show other)
  [] -> assertFailure "no page"

-- | The content of the page after the removal of L
removed :: [BS.ByteString] -> IO BS.ByteString
removed contents = do
  f <- either assertFailure return (F.removeLayer F.defaultLimits 10 (hand_file contents []))
  page_content f

right :: Either String a -> IO a
right = either assertFailure return

-- | A document of the builder, read back
builder_file :: PDF () -> IO F.PdfFile
builder_file doc = case renderPdfE standardDocInfo (PDFRect 0 0 200 200) doc of
  Left ps -> assertFailure (show ps)
  Right b -> right (F.parsePdfFile (BL.toStrict b))

layered_doc :: PDF ()
layered_doc = do
  grid <- createLayer "Grid" defaultLayerOptions {layerLocked = True}
  draft <- createLayer "Draft" defaultLayerOptions {layerVisible = False, layerPrint = Just True}
  detail <- createLayer "D\233tail" defaultLayerOptions {layerZoom = Just (0.5, Just 4), layerExport = Just False}
  en <- createLayer "English" defaultLayerOptions {layerLanguage = Just (LayerLanguage "en" True)}
  fr <- createLayer "French" defaultLayerOptions {layerVisible = False, layerLanguage = Just (LayerLanguage "fr" False)}
  setLayerPanel [LayerLabel "Languages" [LayerEntry en [], LayerEntry fr []], LayerEntry grid [LayerEntry detail []], LayerEntry draft []]
  addRadioGroup [en, fr]
  addLayerConfig (layerConfig "French") {configVisible = [grid, fr], configLocked = [grid], configRadioGroups = [[en, fr]]}
  addLayerConfig (layerConfig "Auto") {configVisible = [grid, detail], configPanel = Just [LayerEntry detail []], configUsage = True}
  page <- addPage Nothing
  drawWithPage page $ do
    inLayer grid (stroke (Line 0 0 100 100))
    inLayer draft (stroke (Line 0 100 100 0))
    inLayer detail (stroke (Line 50 0 50 100))
    inLayer en (stroke (Line 0 50 100 50))
    inLayer fr (stroke (Line 10 50 90 50))

------------------------------------------------------------------------
-- Reading and writing

test_layersOfTheBuilder :: IO ()
test_layersOfTheBuilder = forM_ [False, True] $ \os -> do
  let Right bytes = renderPdfE standardDocInfo {objectStreams = os} (PDFRect 0 0 200 200) layered_doc
  f <- right (F.parsePdfFile (BL.toStrict bytes))
  layers <- right (F.readLayers f)
  let by_name = M.fromList [ (t, n) | (n, t, _) <- F.fileLayers layers ]
      n t = by_name M.! t
  assertEqual ["Grid", "Draft", "D\233tail", "English", "French"] [ t | (_, t, _) <- F.fileLayers layers ]
  assertEqual
    [ defaultLayerOptions {layerLocked = True}
    , defaultLayerOptions {layerVisible = False, layerPrint = Just True}
    , defaultLayerOptions {layerZoom = Just (0.5, Just 4), layerExport = Just False}
    , defaultLayerOptions {layerLanguage = Just (LayerLanguage "en" True)}
    , defaultLayerOptions {layerVisible = False, layerLanguage = Just (LayerLanguage "fr" False)} ]
    [ o | (_, _, o) <- F.fileLayers layers ]
  assertEqual (Just [ LayerLabel "Languages" [LayerEntry (n "English") [], LayerEntry (n "French") []]
                    , LayerEntry (n "Grid") [LayerEntry (n "D\233tail") []], LayerEntry (n "Draft") [] ])
              (F.filePanel layers)
  assertEqual [[n "English", n "French"]] (F.fileRadioGroups layers)
  let panel = F.filePanel layers
  assertEqual [ (layerConfig "French") { configVisible = [n "Grid", n "French"], configLocked = [n "Grid"]
                                       , configPanel = panel, configRadioGroups = [[n "English", n "French"]] }
              , (layerConfig "Auto") { configVisible = [n "Grid", n "D\233tail"], configPanel = Just [LayerEntry (n "D\233tail") []]
                                     , configUsage = True } ]
              (F.fileConfigs layers)
  -- a file with no layer
  g <- builder_file (addPage Nothing >> return ())
  assertEqual (Right (F.FileLayers [] Nothing [] [])) (F.readLayers g)
  assertBool (either (const True) (const False) (F.writeLayers (F.FileLayers [] Nothing [] []) g))

test_writeWithNoChange :: IO ()
test_writeWithNoChange = do
  f <- builder_file layered_doc
  layers <- right (F.readLayers f)
  assertEqual (Right f) (F.writeLayers layers f)
  let g = hand_file ["0 g"] []
  layers' <- right (F.readLayers g)
  assertEqual (Right g) (F.writeLayers layers' g)

-- | The objects that are not the same in two files
changed :: F.PdfFile -> F.PdfFile -> [Int]
changed a b = [ n | n <- List.nub (IM.keys (F.pdfObjects a) ++ IM.keys (F.pdfObjects b))
                  , IM.lookup n (F.pdfObjects a) /= IM.lookup n (F.pdfObjects b) ]

test_changeOfTheLayers :: IO ()
test_changeOfTheLayers = do
  let Right bytes = renderPdfE standardDocInfo (PDFRect 0 0 200 200) layered_doc
  src <- right (F.parsePdfSource F.defaultLimits "" (BL.toStrict bytes))
  let f = F.sourceFile src
      Right (F.Ref cat _) = maybe (Left "root") Right (M.lookup "Root" (F.pdfTrailer f))
  layers <- right (F.readLayers f)
  let [grid, draft, detail, en, fr] = [ n | (n, _, _) <- F.fileLayers layers ]
      set_options m g l = l {F.fileLayers = [ if k == m then (k, t, g o) else x | x@(k, t, o) <- F.fileLayers l ]}
  -- a state: only /D changes, in the catalog
  hidden <- right (F.writeLayers (set_options grid (\o -> o {layerVisible = False}) layers) f)
  assertEqual [cat] (changed f hidden)
  again <- right (F.readLayers hidden)
  assertEqual [False, False, True, True, False] [ layerVisible o | (_, _, o) <- F.fileLayers again ]
  -- the update writes only the catalog
  update <- F.updatePdfFile src hidden >>= right
  updated <- right (F.parsePdfFile (BL.toStrict update))
  assertEqual (F.pdfObjects hidden) (F.pdfObjects updated)
  assertBool (bytes `BL.isPrefixOf` update)
  -- a name and a usage: the OCG, and /D for /AS
  renamed <- right (F.writeLayers (set_options draft (\o -> o {layerPrint = Nothing, layerExport = Just True})
                                     layers {F.fileLayers = [ if k == en then (k, "Anglais", o) else x | x@(k, _, o) <- F.fileLayers layers ]}) f)
  assertEqual (List.sort [cat, draft, en]) (List.sort (changed f renamed))
  after <- right (F.readLayers renamed)
  assertEqual (Just "Anglais") (lookup en [ (k, t) | (k, t, _) <- F.fileLayers after ])
  assertEqual (Just (defaultLayerOptions {layerVisible = False, layerExport = Just True}))
              (lookup draft [ (k, o) | (k, _, o) <- F.fileLayers after ])
  -- /AS: the Print category is gone, Export has the draft
  let d = case M.lookup "OCProperties" (object renamed cat) of
        Just (F.Dict ocp) | Just (F.Dict dd) <- M.lookup "D" ocp -> dd
        _ -> M.empty
      events = [ (e, cs, os) | Just (F.Array as) <- [M.lookup "AS" d], F.Dict a <- as
                             , Just (F.Name e) <- [M.lookup "Event" a], Just (F.Array cs) <- [M.lookup "Category" a]
                             , Just (F.Array os) <- [M.lookup "OCGs" a] ]
  assertEqual [ ("View", [F.Name "Zoom", F.Name "Language"], map ref [detail, en, fr])
              , ("Export", [F.Name "Export"], map ref [draft, detail]) ] events
  -- the panel, the radio groups, a configuration, the locked layers
  let new_panel = [LayerLabel "" [LayerEntry grid []], LayerEntry fr [LayerEntry en []], LayerLabel "Other" [LayerEntry draft [], LayerEntry detail []]]
  rearranged <- right (F.writeLayers layers { F.filePanel = Just new_panel
                                            , F.fileRadioGroups = []
                                            , F.fileConfigs = take 1 (F.fileConfigs layers) } f)
  check <- right (F.readLayers rearranged)
  assertEqual (Just new_panel) (F.filePanel check)
  assertEqual [] (F.fileRadioGroups check)
  assertEqual 1 (length (F.fileConfigs check))
  assertEqual (1, 6) (F.pdfVersion rearranged)
  -- refusals
  assertBool (isLeft (F.writeLayers layers {F.fileLayers = drop 1 (F.fileLayers layers)} f))
  assertBool (isLeft (F.writeLayers layers {F.fileRadioGroups = [[grid, 999]]} f))
  assertBool (isLeft (F.writeLayers (set_options grid (\o -> o {layerZoom = Just (2, Just 1)}) layers) f))
  -- a group after a layer with no children is its children: refused
  assertBool (isLeft (F.writeLayers layers {F.filePanel = Just [LayerEntry fr [], LayerLabel "" [LayerEntry grid []]]} f))
  assertBool (isLeft (F.writeLayers layers {F.fileConfigs = [(layerConfig "C") {configPanel = Just [LayerEntry fr [], LayerEntry fr []]}]} f))
  where
    isLeft = either (const True) (const False)

-- | /BaseState, /ON, a loop in /Order, references to objects that are
-- not layers
test_statesOfAFile :: IO ()
test_statesOfAFile = do
  let with_d d extra = (hand_file ["0 g"] extra)
        { F.pdfObjects = IM.adjust (\(g, _) -> (g, F.Plain (dict [ ("Type", F.Name "Catalog"), ("Pages", ref 2)
                                                                , ("OCProperties", dict [("OCGs", F.Array [ref 10, ref 11, ref 12, ref 99]), ("D", d)]) ])))
                                  1 (F.pdfObjects (hand_file ["0 g"] extra)) }
      states f = either (const []) (\l -> [ (n, layerVisible o) | (n, _, o) <- F.fileLayers l ]) (F.readLayers f)
  -- the OCMD 12 and the missing object 99 are not layers
  assertEqual [(10, False), (11, True)] (states (with_d (dict [("BaseState", F.Name "OFF"), ("ON", F.Array [ref 11, ref 12])]) []))
  assertEqual [(10, True), (11, True)] (states (with_d (dict [("BaseState", F.Name "Unchanged")]) []))
  assertEqual [(10, True), (11, False)] (states (with_d (dict [("OFF", F.Array [ref 11, F.Integer 3])]) []))
  -- the default values of the usage: a zoom with no minimum, a language
  -- with no /Preferred; a name in PDFDocEncoding
  let usage = hand_file ["0 g"]
        [ (11, F.Plain (dict [ ("Type", F.Name "OCG"), ("Name", F.String "Fran\231ais")
                             , ("Usage", dict [("Zoom", dict [("max", F.Integer 2)]), ("Language", dict [("Lang", F.String "fr")])]) ])) ]
  usage_layers <- right (F.readLayers usage)
  assertEqual [("Fran\231ais", defaultLayerOptions {layerZoom = Just (0, Just 2), layerLanguage = Just (LayerLanguage "fr" False)})]
              [ (t, o) | (11, t, o) <- F.fileLayers usage_layers ]
  -- a change of another layer keeps the bytes of the name
  changed_usage <- right (F.writeLayers usage_layers {F.fileLayers = [ if n == 10 then (n, t, o {layerPrint = Just True}) else x
                                                                     | x@(n, t, o) <- F.fileLayers usage_layers ]} usage)
  -- a change of its usage too: the name keeps its bytes, the other
  -- categories of /AS stay
  let with_as = changed_usage { F.pdfObjects = IM.adjust (\(g, o) -> (g, case o of
          F.Plain (F.Dict c) -> F.Plain (F.Dict (M.adjust (\(F.Dict ocp) -> F.Dict (M.adjust (\(F.Dict d) -> F.Dict (M.insert "AS"
                                  (F.Array [dict [("Event", F.Name "View"), ("Category", F.Array [F.Name "User", F.Name "Zoom"]), ("OCGs", F.Array [ref 11])]]) d)) "D" ocp)) "OCProperties" c))
          _ -> o)) 1 (F.pdfObjects changed_usage) }
  as_layers <- right (F.readLayers with_as)
  rewritten <- right (F.writeLayers as_layers {F.fileLayers = [ if n == 11 then (n, t, o {layerZoom = Nothing}) else x
                                                              | x@(n, t, o) <- F.fileLayers as_layers ]} with_as)
  assertEqual (Just (F.String "Fran\231ais")) (M.lookup "Name" (object rewritten 11))
  let d = case M.lookup "OCProperties" (object rewritten 1) of
        Just (F.Dict ocp) | Just (F.Dict dd) <- M.lookup "D" ocp -> dd
        _ -> M.empty
  assertEqual (Just (F.Array [ dict [("Event", F.Name "View"), ("Category", F.Array [F.Name "User"]), ("OCGs", F.Array [ref 11])]
                             , dict [("Event", F.Name "View"), ("Category", F.Array [F.Name "Language"]), ("OCGs", F.Array [ref 11])]
                             , dict [("Event", F.Name "Print"), ("Category", F.Array [F.Name "Print"]), ("OCGs", F.Array [ref 10])] ]))
              (M.lookup "AS" d)
  -- an /Order with a loop of references, and an array of the panel in an object
  let looped = with_d (dict [("Order", ref 50)])
                 [ (50, F.Plain (F.Array [ref 10, ref 51]))
                 , (51, F.Plain (F.Array [F.String "Group", ref 11, ref 50, ref 51])) ]
  layers <- right (F.readLayers looped)
  assertEqual (Just [LayerEntry 10 [LayerLabel "Group" [LayerEntry 11 []]]]) (F.filePanel layers)

test_addLayer :: IO ()
test_addLayer = do
  -- a file with no layer: /OCProperties is made
  g <- builder_file (addPage Nothing >> return ())
  (n, g1) <- right (F.addLayer "First" defaultLayerOptions {layerVisible = False} g)
  layers <- right (F.readLayers g1)
  assertEqual [(n, "First", defaultLayerOptions {layerVisible = False})] (F.fileLayers layers)
  assertEqual (Just [LayerEntry n []]) (F.filePanel layers)
  (m, g2) <- right (F.addLayer "Second" defaultLayerOptions {layerLocked = True, layerPrint = Just False} g1)
  layers2 <- right (F.readLayers g2)
  assertEqual [(n, "First", defaultLayerOptions {layerVisible = False}), (m, "Second", defaultLayerOptions {layerLocked = True, layerPrint = Just False})]
              (F.fileLayers layers2)
  assertEqual (Just [LayerEntry n [], LayerEntry m []]) (F.filePanel layers2)
  assertEqual (1, 6) (F.pdfVersion g2)
  -- a file with layers whose /D has /BaseState /OFF: the other states do not change
  let h = hand_file ["0 g"] []
  (k, h1) <- right (F.addLayer "\8364 layer" defaultLayerOptions h)
  layers3 <- right (F.readLayers h1)
  assertEqual [(10, False), (11, True), (k, True)] [ (x, layerVisible o) | (x, _, o) <- F.fileLayers layers3 ]
  assertEqual ["L", "M", "\8364 layer"] [ t | (_, t, _) <- F.fileLayers layers3 ]
  assertBool (either (const True) (const False) (F.addLayer "Z" defaultLayerOptions {layerZoom = Just (-1, Nothing)} h))

test_pageInALayer :: IO ()
test_pageInALayer = do
  -- the resources come from the /Pages node, and are shared
  let shared = dict [("Properties", dict [("M", ref 11)]), ("Font", dict [("F1", ref 60)])]
      base = hand_file ["0 0 m 1 1 l S", "2 2 m 3 3 l S"] [(60, F.Plain (dict [("Type", F.Name "Font")]))]
      f = base { F.pdfObjects = IM.adjust (\(g, _) -> (g, F.Plain (dict [("Type", F.Name "Pages"), ("Kids", F.Array [ref 3]), ("Count", F.Integer 1), ("Resources", shared)]))) 2
                                 (IM.adjust (\(g, o) -> (g, case o of
                                                              F.Plain (F.Dict d) -> F.Plain (F.Dict (M.delete "Resources" d))
                                                              _ -> o)) 3 (F.pdfObjects base)) }
  g <- right (F.putPageInLayer 10 0 f)
  let page = object g 3
  content <- page_content g
  assertEqual "/OC /HPDFoc1 BDC\n\n0 0 m 1 1 l S\n2 2 m 3 3 l S\n\nEMC\n" content
  assertEqual (Just (dict [("Properties", dict [("M", ref 11), ("HPDFoc1", ref 10)]), ("Font", dict [("F1", ref 60)])]))
              (M.lookup "Resources" page)
  -- the /Pages node does not change
  assertEqual (IM.lookup 2 (F.pdfObjects f)) (IM.lookup 2 (F.pdfObjects g))
  -- a layer that has a name in the resources: the same name
  h <- right (F.putPageInLayer 11 0 f)
  content_h <- page_content h
  assertBool ("/OC /M BDC" `BS.isPrefixOf` content_h)
  -- the annotations: the one that has no /OC
  let annotated = f { F.pdfObjects = IM.insert 41 (0, F.Plain (dict [("Type", F.Name "Annot"), ("Subtype", F.Name "Text")])) (F.pdfObjects f) }
  a <- right (F.putAnnotationsInLayer 11 0 annotated)
  assertEqual (Just (ref 10)) (M.lookup "OC" (object a 40))
  assertEqual (Just (ref 11)) (M.lookup "OC" (object a 41))
  -- refusals
  assertBool (either (const True) (const False) (F.putPageInLayer 12 0 f))
  assertBool (either (const True) (const False) (F.putPageInLayer 10 1 f))

------------------------------------------------------------------------
-- Removal

test_removeContent :: IO ()
test_removeContent = do
  -- the graphics state and the colours stay; the paths, the q ... Q
  -- blocks and the marked content in the sequence go
  removed ["q 1 0 0 rg /OC /L BDC 0 1 0 rg 0 0 10 10 re f q 2 w Q /Tag BMC 0 0 m 1 1 l S EMC EMC 5 5 m 9 9 l S Q"]
    >>= assertEqual "q 1 0 0 rg \n0 1 0 rg 5 5 m 9 9 l S Q"
  -- a q whose Q is after the sequence stays, with its graphics state and
  -- its clipping path (Illustrator); a block that ends in the sequence
  -- goes; a Q of a q before the sequence stays
  removed ["/OC /L BDC q 0 0 10 10 re W n /GS0 gs q 1 0 0 1 5 5 cm 0 0 m 1 1 l f Q EMC Q"]
    >>= assertEqual "\nq\n0 0 10 10 re\nW\nn\n/GS0 gs Q"
  removed ["q /OC /L BDC 1 g Q EMC 0 g"] >>= assertEqual "q \n1 g\nQ 0 g"
  removed ["/OC /L BDC q EMC Q"] >>= assertEqual "\nq Q"
  -- a text object: its text state stays
  removed ["/OC /L BDC BT /F1 12 Tf 1 0 0 1 5 5 Tm (Hi) Tj [(a) 5 (b)] TJ ET EMC BT (x) Tj ET"]
    >>= assertEqual "\n/F1 12 Tf BT (x) Tj ET"
  -- a sequence in a text object with no text: its position stays
  removed ["BT /OC /L BDC 1 0 0 1 2 2 Tm EMC (a) Tj ET"]
    >>= assertEqual "BT \n1 0 0 1 2 2 Tm (a) Tj ET"
  -- nested sequences
  removed ["/OC /M BDC /OC /L BDC 0 0 m 1 1 l S EMC 2 2 m 3 3 l S EMC"]
    >>= assertEqual "/OC /M BDC  2 2 m 3 3 l S EMC"
  removed ["/OC /L BDC /OC /M BDC 0 0 m 1 1 l S EMC EMC 0 g"]
    >>= assertEqual " 0 g"
  -- a sequence that crosses two streams: one stream
  removed ["0 g /OC /L BDC 0 0 m", "1 1 l S EMC 1 g"] >>= assertEqual "0 g  1 g"
  -- the other stream of the page is no longer in the file
  crossed <- right (F.removeLayer F.defaultLimits 10 (hand_file ["0 g /OC /L BDC 0 0 m", "1 1 l S EMC 1 g"] []))
  assertEqual Nothing (F.getObject crossed 21)
  -- an XObject that is not hidden, in the sequence: its Do goes
  removed ["/OC /L BDC /Y Do EMC 0 g"] >>= assertEqual " 0 g"
  -- an inline image whose data has EI after a byte that is not white space
  removed ["/OC /L BDC BI /W 2 /H 1 /BPC 8 /CS /G ID \255EI\n EI EMC 0 g"] >>= assertEqual " 0 g"
  -- an inline image with EI in its data, in the sequence and out of it
  removed ["/OC /L BDC BI /W 2 /H 1 /BPC 8 /CS /G ID \0EI\1 EI EMC BI /W 1 /H 1 /BPC 8 /CS /G ID \255EI\n EI 0 g"]
    >>= assertEqual " BI /W 1 /H 1 /BPC 8 /CS /G ID \255EI\n EI 0 g"
  removed ["/OC /L BDC BI /W 2 /H 1 /BPC 8 /CS /G /L 4 ID EI\0\1 EI EMC 0 g"] >>= assertEqual " 0 g"
  -- a clipping path stays, with n
  removed ["/OC /L BDC 0 0 10 10 re W n 1 0 0 1 5 5 cm 0 0 m 1 1 l S 0 0 5 5 re W* f EMC"]
    >>= assertEqual "\n0 0 10 10 re\nW\nn\n1 0 0 1 5 5 cm\n0 0 5 5 re\nW*\nn"
  -- the conditions: D (L) is hidden; C (L or M) and E (not L and not M)
  -- are not: they depend on M
  removed ["/OC /D BDC 0 g EMC /OC /C BDC 1 g EMC /OC /E BDC 2 g EMC"]
    >>= assertEqual "\n0 g /OC /C BDC 1 g EMC /OC /E BDC 2 g EMC"
  -- no change: the stream is the same object
  let same = hand_file ["/OC /M BDC 0 0 m 1 1 l S EMC"] []
  out <- right (F.removeLayer F.defaultLimits 10 same)
  assertEqual (IM.lookup 20 (F.pdfObjects same)) (IM.lookup 20 (F.pdfObjects out))

test_removeOtherUses :: IO ()
test_removeOtherUses = do
  let x_form oc content = stream ([("Type", F.Name "XObject"), ("Subtype", F.Name "Form"), ("BBox", F.Array (map F.Integer [0, 0, 1, 1]))] ++ oc) content
      inner_form = stream [ ("Type", F.Name "XObject"), ("Subtype", F.Name "Form"), ("BBox", F.Array (map F.Integer [0, 0, 1, 1]))
                          , ("Resources", dict [("Properties", dict [("P", ref 10)])]) ] "/OC /P BDC 3 g EMC 4 g"
      f = hand_file ["/X Do /Y Do /OC /C BDC 1 g EMC"]
            [ (30, x_form [("OC", ref 10)] "0 0 m 1 1 l S")
            , (31, x_form [("Resources", dict [("Properties", dict [("L", ref 10)]), ("XObject", dict [("Z", ref 32)])])]
                          "/OC /L BDC 0 g EMC /Z Do")
            , (32, inner_form) ]
  g <- right (F.removeLayer F.defaultLimits 10 f)
  -- the Do of the XObject in L goes
  page_content g >>= assertEqual " /Y Do /OC /C BDC 1 g EMC"
  -- a form, and a form in a form
  stream_data g 31 >>= assertEqual "\n0 g /Z Do"
  stream_data g 32 >>= assertEqual "\n3 g 4 g"
  -- the annotation in L leaves /Annots
  assertEqual (Just (F.Array [ref 41])) (M.lookup "Annots" (object g 3))
  -- the layer: no OCG, not in /OCProperties, not in /Properties
  assertEqual Nothing (F.getObject g 10)
  layers <- right (F.readLayers g)
  assertEqual [11] [ n | (n, _, _) <- F.fileLayers layers ]
  assertEqual (Just [LayerEntry 11 []]) (F.filePanel layers)
  case M.lookup "Resources" (object g 3) of
    Just (F.Dict r) -> assertEqual (Just (dict [("M", ref 11), ("C", ref 12), ("E", ref 14)])) (M.lookup "Properties" r)
    other -> assertFailure (show other)
  assertEqual (Just (dict [])) (M.lookup "Properties" =<< resources_of (object g 31))
  -- the conditions: C is M, E is not M
  assertEqual [("OCGs", F.Array [ref 11]), ("P", F.Name "AnyOn"), ("Type", F.Name "OCMD")] (M.toList (object g 12))
  assertEqual [("OCGs", F.Array [ref 11]), ("P", F.Name "AllOff"), ("Type", F.Name "OCMD")] (M.toList (object g 14))
  -- a condition with /VE
  let ve = hand_file ["0 g"] [(15, F.Plain (dict [ ("Type", F.Name "OCMD"), ("OCGs", F.Array [ref 10, ref 11])
                                                 , ("VE", F.Array [F.Name "Or", F.Array [F.Name "And", ref 10, ref 11], F.Array [F.Name "Not", ref 10]]) ]))]
  h <- right (F.removeLayer F.defaultLimits 10 ve)
  -- (L and M) or not L, with L off: true
  assertEqual [("OCGs", F.Array []), ("Type", F.Name "OCMD")] (M.toList (object h 15))
  -- a condition that stays a visibility expression: PDF 1.6
  let ve2 = (hand_file ["0 g"] [(15, F.Plain (dict [ ("Type", F.Name "OCMD")
                                                   , ("VE", F.Array [F.Name "Or", F.Array [F.Name "And", ref 11, ref 16], F.Array [F.Name "Not", ref 16], ref 10]) ]))
                                , (16, F.Plain (dict [("Type", F.Name "OCG"), ("Name", F.String "N")]))])
              {F.pdfVersion = (1, 4)}
  h2 <- right (F.removeLayer F.defaultLimits 10 ve2)
  assertEqual (Just (F.Array [F.Name "Or", F.Array [F.Name "And", ref 11, ref 16], F.Array [F.Name "Not", ref 16]])) (M.lookup "VE" (object h2 15))
  assertEqual (1, 6) (F.pdfVersion h2)
  -- an annotation appearance
  let ap = hand_file ["0 g"] [ (41, F.Plain (dict [("Type", F.Name "Annot"), ("Subtype", F.Name "Square"), ("AP", dict [("N", ref 33)])]))
                             , (33, x_form [("Resources", dict [("Properties", dict [("L", ref 10)])])] "/OC /L BDC 0 0 m 1 1 l S EMC 1 g") ]
  k <- right (F.removeLayer F.defaultLimits 10 ap)
  stream_data k 33 >>= assertEqual " 1 g"
  where
    resources_of d = case M.lookup "Resources" d of
      Just (F.Dict r) -> Just r
      _ -> Nothing

test_removeRefusals :: IO ()
test_removeRefusals = do
  let refused contents = assertBool (either (const True) (const False) (F.removeLayer F.defaultLimits 10 (hand_file contents [])))
  refused ["BT /OC /L BDC (a) Tj EMC ET"]
  refused ["/OC /L BDC 0 g"]
  refused ["/OC /L BDC BT EMC ET"]
  refused ["BT /OC /L BDC BT ET EMC ET"]
  refused ["BT /OC /L BDC ET EMC"]
  refused ["/OC /L BDC 0 0 m EMC S"]
  refused ["/OC /L BDC } EMC"]
  -- a filter that the reader does not decode
  let lzw = (hand_file ["0 g"] []) { F.pdfObjects = IM.insert 20 (0, stream [("Filter", F.Name "LZWDecode")] "abc") (F.pdfObjects (hand_file ["0 g"] [])) }
  assertBool (either (const True) (const False) (F.removeLayer F.defaultLimits 10 lzw))
  -- not a layer
  assertBool (either (const True) (const False) (F.removeLayer F.defaultLimits 12 (hand_file ["0 g"] [])))
  -- a limit of the decoded size
  assertBool (either (const True) (const False) (F.removeLayer F.defaultLimits {F.limitStream = 2} 10 (hand_file ["0 0 m"] [])))

-- | A document of the builder with text, images, a pattern and a form:
-- the removal of a layer that no content uses reads all the streams, and
-- changes none of them. The removal of the layer that the content uses
-- leaves no marked content in the streams.
test_removeAnUnusedLayer :: IO ()
test_removeAnUnusedLayer = forM_ [False, True] $ \os -> do
  Right helvetica <- mkStdFont Helvetica
  let doc = do
        used <- createLayer "Used" defaultLayerOptions
        _ <- createLayer "Unused" defaultLayerOptions
        img <- createPDFRawImageFromByteString 1 1 True NoFilter (BL.pack [255, 0, 0])
        jpeg <- createPDFJpeg jpg
        tile <- createColoredTiling 0 0 10 10 10 10 ConstantSpacing (inLayer used (stroke (Line 0 0 10 10)) >> fill (Circle 5 5 2))
        form <- createPDFXForm 0 0 10 10 (inLayer used (drawText (text (PDFFont helvetica 5) 0 0 "form")) >> stroke (Line 0 10 10 0))
        page <- addPage Nothing
        drawWithPage page $ do
          drawText (text (PDFFont helvetica 10) 10 10 "Text (with) \\ escapes")
          inLayer used $ withNewContext (setColoredFillPattern tile >> fill (Rectangle (0 :+ 0) (50 :+ 50)))
          drawXObject img
          inLayer used (drawXObject jpeg)
          drawXObject form
      Right bytes = renderPdfE standardDocInfo {objectStreams = os} (PDFRect 0 0 200 200) doc
  f <- right (F.parsePdfFile (BL.toStrict bytes))
  layers <- right (F.readLayers f)
  let [used, unused] = [ n | (n, _, _) <- F.fileLayers layers ]
      Just (F.Ref cat _) = M.lookup "Root" (F.pdfTrailer f)
      streams g = [ (n, o) | (n, (_, o@(F.Stream _ _))) <- IM.toList (F.pdfObjects g) ]
  g <- right (F.removeLayer F.defaultLimits unused f)
  assertEqual (streams f) (streams g)
  assertEqual (List.sort [cat, unused]) (List.sort (changed f g))
  h <- right (F.removeLayer F.defaultLimits used f)
  contents <- mapM (\(n, _) -> stream_data h n) [ x | x@(_, F.Stream d _) <- streams h, M.lookup "Subtype" d /= Just (F.Name "Image") ]
  assertEqual [] [ c | c <- contents, "BDC" `BS.isInfixOf` c ]
  -- the text, the image and the form stay; the JPEG goes
  page <- page_content h
  assertBool ("<5465787420287769746829205C2065736361706573> Tj" `BS.isInfixOf` page)
  assertEqual 2 (length (filter (== "Do") (BS.split 32 (BS.map (\w -> if w == 10 then 32 else w) page))))

-- | The text filters of the content streams of old files.
-- See Note [Filters of the reader] in Graphics.PDF.File.Filter
test_textFilters :: IO ()
test_textFilters = do
  let decode fs s = F.decodeStream 1000 (M.fromList [("Filter", F.Array (map F.Name fs))]) s
  assertEqual (Right "ab") (decode ["ASCIIHexDecode"] "61 6\n2>")
  assertEqual (Right "a`") (decode ["ASCIIHexDecode"] "616>")
  assertEqual (Right "Man ") (decode ["ASCII85Decode"] "9jqo^~>")
  assertEqual (Right "Man") (decode ["ASCII85Decode"] "9jq\no~>")
  assertEqual (Right "\0\0\0\0Man ") (decode ["ASCII85Decode"] "z9jqo^~>")
  assertBool (isLeft (decode ["ASCII85Decode"] "9jqo^9~>"))
  assertBool (isLeft (decode ["ASCII85Decode"] "s8W-\"~>"))
  assertBool (isLeft (decode ["ASCIIHexDecode"] "6g>"))
  assertBool (isLeft (decode ["LZWDecode"] "abc"))
  assertBool (isLeft (F.decodeStream 3 (M.fromList [("Filter", F.Name "ASCIIHexDecode")]) "61626364>"))
  -- a list of filters, with the parameters of each one
  let deflated = BL.toStrict (Zlib.compress "0 0 m 1 1 l S")
      hex = BS.concatMap (\w -> BS.pack (map (hex_digit . fromIntegral) [w `div` 16, w `mod` 16])) deflated <> ">"
      hex_digit d = fromIntegral (fromEnum (if d < (10 :: Int) then toEnum (48 + d) else toEnum (87 + d) :: Char))
  assertEqual (Right "0 0 m 1 1 l S")
    (F.decodeStream 1000 (M.fromList [("Filter", F.Array [F.Name "ASCIIHexDecode", F.Name "FlateDecode"]), ("DecodeParms", F.Array [F.Null, F.Null])]) hex)
  -- the removal of a layer in such a stream
  let f = hand_file ["0 g"] []
      g = f {F.pdfObjects = IM.insert 20 (0, stream [("Filter", F.Array [F.Name "ASCIIHexDecode", F.Name "FlateDecode"])]
                                                    (hex_of "/OC /L BDC 0 0 m 1 1 l S EMC 1 g")) (F.pdfObjects f)}
      hex_of s = let d = BL.toStrict (Zlib.compress (BL.fromStrict s))
                 in BS.concatMap (\w -> BS.pack (map (hex_digit . fromIntegral) [w `div` 16, w `mod` 16])) d <> ">"
  h <- right (F.removeLayer F.defaultLimits 10 g)
  page_content h >>= assertEqual " 1 g"
  where
    isLeft = either (const True) (const False)

-- | The hand-made file with another /OCProperties
with_ocp :: F.Value -> [BS.ByteString] -> [(Int, F.Object)] -> F.PdfFile
with_ocp ocp contents extra = hand_file contents
  ((1, F.Plain (dict [("Type", F.Name "Catalog"), ("Pages", ref 2), ("OCProperties", ocp)])) : extra)

-- | The defects that the reviews found
test_reviewCases :: IO ()
test_reviewCases = do
  -- a content stream that another page uses: a new stream for the page
  let two_pages = hand_file ["0 0 m 1 1 l S", "/OC /L BDC 2 2 m 3 3 l S EMC", "4 4 m 5 5 l S"]
        [ (2, F.Plain (dict [("Type", F.Name "Pages"), ("Kids", F.Array [ref 3, ref 5]), ("Count", F.Integer 2)]))
        , (3, F.Plain (dict [ ("Type", F.Name "Page"), ("Parent", ref 2), ("Contents", F.Array [ref 20, ref 21])
                            , ("Resources", dict [("Properties", dict [("L", ref 10)])]) ]))
        , (5, F.Plain (dict [ ("Type", F.Name "Page"), ("Parent", ref 2), ("Contents", F.Array [ref 20, ref 22])
                            , ("Resources", dict [("Properties", dict [("L", ref 10)])]) ])) ]
  shared <- right (F.removeLayer F.defaultLimits 10 two_pages)
  assertEqual (IM.lookup 20 (F.pdfObjects two_pages)) (IM.lookup 20 (F.pdfObjects shared))
  assertEqual (IM.lookup 22 (F.pdfObjects two_pages)) (IM.lookup 22 (F.pdfObjects shared))
  case M.lookup "Contents" (object shared 3) of
    Just (F.Ref n _) -> do
      assertBool (n `notElem` [20, 21, 22])
      stream_data shared n >>= assertEqual "0 0 m 1 1 l S\n"
    other -> assertFailure (show other)
  assertEqual Nothing (F.getObject shared 21)
  -- an inline image whose /L is too large: the EI is found
  removed ["/OC /L BDC BI /W 1 /H 1 /BPC 8 /CS /G /L 99999999999999999999 ID x EI EMC 0 g"] >>= assertEqual " 0 g"
  -- the text rendering modes that clip
  let refused contents = assertBool (either (const True) (const False) (F.removeLayer F.defaultLimits 10 (hand_file contents [])))
  refused ["q /OC /L BDC BT 7 Tr /F1 9 Tf (X) Tj ET EMC Q"]
  refused ["4 Tr /OC /L BDC BT (X) Tj ET EMC"]
  removed ["q 7 Tr Q /OC /L BDC BT (X) Tj ET EMC"] >>= assertEqual "q 7 Tr Q "
  -- a path that starts before the sequence
  removed ["0 0 m /OC /L BDC 10 10 l S EMC 1 0 0 rg"] >>= assertEqual "0 0 m \nn 1 0 0 rg"
  -- the spacings of "
  removed ["/OC /L BDC BT /F1 12 Tf 5 7 (x) \" ET EMC"] >>= assertEqual "\n/F1 12 Tf\n5 Tw 7 Tc"
  -- a condition with a reference to a missing object, or to an object of
  -- another generation: they are ignored
  missing <- right (F.removeLayer F.defaultLimits 10 (hand_file ["/OC /D BDC 0 g EMC"]
               [(13, F.Plain (dict [("Type", F.Name "OCMD"), ("OCGs", F.Array [ref 10, ref 99])]))]))
  page_content missing >>= assertEqual "\n0 g"
  other_generation <- right (F.removeLayer F.defaultLimits 10 (hand_file ["/OC /D BDC 0 g EMC"]
                        [(13, F.Plain (dict [("Type", F.Name "OCMD"), ("OCGs", F.Array [F.Ref 10 5])]))]))
  page_content other_generation >>= assertEqual "/OC /D BDC 0 g EMC"
  -- a reference to an object that is not an OCG is ignored too
  not_ocg <- right (F.removeLayer F.defaultLimits 10 (hand_file ["/OC /D BDC 0 g EMC"]
               [(13, F.Plain (dict [("Type", F.Name "OCMD"), ("OCGs", F.Array [ref 10, ref 41])]))]))
  page_content not_ocg >>= assertEqual "\n0 g"
  -- an /OC with another generation is not the layer
  generation <- right (F.removeLayer F.defaultLimits 10 (hand_file ["0 g"]
                  [(40, F.Plain (dict [("Type", F.Name "Annot"), ("Subtype", F.Name "Text"), ("OC", F.Ref 10 7)]))]))
  assertEqual (Just (F.Array [ref 40, ref 41])) (M.lookup "Annots" (object generation 3))
  -- a visibility expression of shared arrays: a limit of the nodes
  let chain = [ (n, F.Plain (F.Array [F.Name "Or", ref (n + 1), ref (n + 1)])) | n <- [50 .. 88] ]
              ++ [(89, F.Plain (F.Array [F.Name "Or", ref 10, ref 11]))]
  _ <- right (F.removeLayer F.defaultLimits 10 (hand_file ["/OC /D BDC 0 g EMC"]
               ((13, F.Plain (dict [("Type", F.Name "OCMD"), ("OCGs", F.Array [ref 10]), ("VE", ref 50)])) : chain)))
  -- an annotation whose appearance is in the layer
  let form = stream [("Type", F.Name "XObject"), ("Subtype", F.Name "Form"), ("BBox", F.Array (map F.Integer [0, 0, 1, 1])), ("OC", ref 10)] "0 g"
  ap <- right (F.removeLayer F.defaultLimits 10 (hand_file ["0 g"]
          [ (41, F.Plain (dict [("Type", F.Name "Annot"), ("Subtype", F.Name "Square"), ("AP", dict [("N", ref 33)])]))
          , (33, form) ]))
  assertEqual (Just (F.Array [])) (M.lookup "Annots" (object ap 3))
  -- a /Usage that two OCGs share
  let usage = hand_file ["0 g"]
        [ (10, F.Plain (dict [("Type", F.Name "OCG"), ("Name", F.String "L"), ("Usage", ref 60)]))
        , (11, F.Plain (dict [("Type", F.Name "OCG"), ("Name", F.String "M"), ("Usage", ref 60)]))
        , (60, F.Plain (dict [("Print", dict [("PrintState", F.Name "ON")])])) ]
  usage_layers <- right (F.readLayers usage)
  usage' <- right (F.writeLayers usage_layers {F.fileLayers = [ if n == 10 then (n, t, o {layerPrint = Just False}) else x
                                                              | x@(n, t, o) <- F.fileLayers usage_layers ]} usage)
  after <- right (F.readLayers usage')
  assertEqual [(10, Just False), (11, Just True)] [ (n, layerPrint o) | (n, _, o) <- F.fileLayers after ]
  assertEqual (IM.lookup 60 (F.pdfObjects usage)) (IM.lookup 60 (F.pdfObjects usage'))
  -- configurations in an array object: the removal of the second one
  let config name = F.Plain (dict [("Name", F.String name), ("BaseState", F.Name "OFF"), ("ON", F.Array [ref 10]), ("ListMode", F.Name "VisiblePages")])
      configs = with_ocp (dict [("OCGs", F.Array [ref 10, ref 11]), ("D", dict []), ("Configs", ref 70)]) ["0 g"]
        [ (70, F.Plain (F.Array [ref 71, F.Null, ref 72, ref 73])), (71, config "A"), (72, config "B"), (73, config "C") ]
  config_layers <- right (F.readLayers configs)
  assertEqual ["A", "B", "C"] (map configName (F.fileConfigs config_layers))
  without_b <- right (F.writeLayers config_layers {F.fileConfigs = [ c | c <- F.fileConfigs config_layers, configName c /= "B" ]} configs)
  assertEqual [70] (changed configs without_b)
  assertEqual (Just (0, F.Plain (F.Array [ref 71, ref 73]))) (IM.lookup 70 (F.pdfObjects without_b))
  -- a changed configuration that moves: it is found by its name, not by
  -- its place
  let moved = case F.fileConfigs config_layers of
        [a, _, c] -> [c {configVisible = []}, a]
        other -> other
  moved_file <- right (F.writeLayers config_layers {F.fileConfigs = moved} configs)
  assertEqual [70, 73] (List.sort (changed configs moved_file))
  -- a new name: the configuration is found by its place, and keeps its keys
  renamed <- right (F.writeLayers config_layers {F.fileConfigs = [ if configName c == "B" then c {configName = "B2"} else c
                                                                 | c <- F.fileConfigs config_layers ]} configs)
  assertEqual (List.sort [70, 72]) (List.sort (changed configs renamed))
  assertEqual (Just (F.Name "VisiblePages")) (M.lookup "ListMode" (object renamed 72))
  -- a configuration whose /BaseState is /Unchanged keeps it
  let unchanged = with_ocp (dict [ ("OCGs", F.Array [ref 10, ref 11]), ("D", dict [])
                                 , ("Configs", F.Array [dict [("Name", F.String "U"), ("BaseState", F.Name "Unchanged"), ("OFF", F.Array [ref 11])]]) ])
                    ["0 g"] []
  unchanged_layers <- right (F.readLayers unchanged)
  renamed_u <- right (F.writeLayers unchanged_layers {F.fileConfigs = map (\c -> c {configName = "V"}) (F.fileConfigs unchanged_layers)} unchanged)
  case M.lookup "OCProperties" (object renamed_u 1) of
    Just (F.Dict ocp) | Just (F.Array [F.Dict c]) <- M.lookup "Configs" ocp ->
      assertEqual [("BaseState", F.Name "Unchanged"), ("Name", F.String "V"), ("OFF", F.Array [ref 11])] (M.toList c)
    other -> assertFailure (show other)
  -- /DecodeParms with no filter
  assertEqual (Right "abc") (F.decodeStream 10 (M.fromList [("DecodeParms", dict [("Predictor", F.Integer 12)])]) "abc")
  assertBool (either (const True) (const False) (F.decodeStream 3 (M.fromList [("Filter", F.Name "ASCII85Decode")]) "9jqo^9jqo^~>"))

-- | The removal of a layer from configurations: a /Configs with a value
-- that is not a dictionary does not move the other ones
test_removeFromConfigs :: IO ()
test_removeFromConfigs = do
  let f = with_ocp (dict [ ("OCGs", F.Array [ref 10, ref 11]), ("D", dict [("Order", F.Array [ref 10, ref 11])])
                         , ("Configs", F.Array [ F.Null
                                               , dict [("Name", F.String "A"), ("BaseState", F.Name "OFF"), ("ON", F.Array [ref 10])
                                                      , ("Order", F.Array [ref 11, ref 10])] ]) ])
             ["/OC /L BDC 0 g EMC"] []
  g <- right (F.removeLayer F.defaultLimits 10 f)
  case M.lookup "OCProperties" (object g 1) of
    Just (F.Dict ocp) -> do
      assertEqual (Just (F.Array [ref 11])) (M.lookup "OCGs" ocp)
      assertEqual (Just (F.Array [ F.Null
                                 , dict [("Name", F.String "A"), ("BaseState", F.Name "OFF"), ("ON", F.Array []), ("Order", F.Array [ref 11])] ]))
                  (M.lookup "Configs" ocp)
    other -> assertFailure (show other)

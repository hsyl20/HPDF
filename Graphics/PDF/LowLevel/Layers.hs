---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- The catalog entries of the layers of a document, and the problems of
-- the layers. See Note [Optional content] in Graphics.PDF.OptionalContent
---------------------------------------------------------
module Graphics.PDF.LowLevel.Layers
  ( optionalContentEntries
  , streamLayerProblems
  ) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.LowLevel.OptionalContent
import Graphics.PDF.Draw
import Graphics.PDF.Archive (pdfTextString)

ref :: Int -> PDFReference PDFDictionary
ref = PDFReference

-- | The elements of a list in their order, with no duplicate.
nub_ints :: [Int] -> [Int]
nub_ints = go IS.empty
  where
    go _ [] = []
    go seen (x : xs)
      | IS.member x seen = go seen xs
      | otherwise = x : go (IS.insert x seen) xs

-- | The problems of the layers in the streams: the problems that the
-- streams recorded (a layer that cuts a path), and the layers and
-- conditions that the document did not create (they come from another
-- document). The streams are those of the builder before 'createStreams'
-- writes them.
streamLayerProblems :: PdfBuilder -> [String]
streamLayerProblems s =
     nub_strings (concat [ reverse (streamProblems st) | (_, (st, _)) <- IM.elems (streams s) ])
  ++ [ "a layer or a layer condition of another document is used (object " ++ show n ++ ")"
     | n <- IS.toList used, not (IS.member n known) ]
  where
    nub_strings = M.keys . M.fromList . map (\p -> (p, ()))
    known = ocKnown (optionalContent s)
    used = IS.fromList [ n | (_, (st, _)) <- IM.elems (streams s), OptionalContentRef n <- M.keys (layerNames st) ]

-- | The entries of the catalog for the layers (/OCProperties, and
-- /Version when the document uses PDF 1.6), and the problems of the
-- layers. The argument tells if the document is PDF/A.
-- See Note [Optional content] and Note [Optional content in PDF/A] in
-- Graphics.PDF.OptionalContent
optionalContentEntries :: Bool -> OptionalContentState -> ([String], [(PDFName, AnyPdfObject)])
optionalContentEntries pdfa oc
  -- the problems can be layers of another document
  | null layers = (problems, [])
  | otherwise =
      ( problems
      , entry "OCProperties" properties
        : [ entry "Version" (PDFName "1.6") | needs_1_6, not pdfa ] )
 where
  layers = reverse (ocLayers oc)
  numbers = [ n | (n, _, _) <- layers ]
  known = IS.fromList numbers
  options = IM.fromList [ (n, o) | (n, _, o) <- layers ]
  names = IM.fromList [ (n, t) | (n, t, _) <- layers ]
  layer_name n = maybe (show n) show (IM.lookup n names)
  visible n = maybe True layerVisible (IM.lookup n options)
  with f = [ n | (n, _, o) <- layers, f o ]
  locked = with layerLocked
  panel = fromMaybe [ LayerEntry n [] | n <- numbers ] (ocPanel oc)
  groups = map nub_ints (reverse (ocRadioGroups oc))
  configs = reverse (ocConfigs oc)
  needs_1_6 = not (null locked) || any (not . null . configLocked) configs || ocVisibilityExpressions oc

  properties = dictFromList $
       [ entry "OCGs" (map ref numbers)
       , entry "D" default_config ]
    ++ non_empty "Configs" (map config configs)

  default_config = dictFromList $
       [ entry "Name" (pdfTextString (T.pack "Default")) ]
    ++ non_empty "OFF" [ ref n | n <- numbers, not (visible n) ]
    ++ [ entry "Order" (order panel) ]
    ++ non_empty "RBGroups" (map (map ref) groups)
    ++ non_empty "Locked" (map ref locked)
    ++ non_empty "AS" automatic_states

  config c = dictFromList $
       [ entry "Name" (pdfTextString (configName c))
       , entry "BaseState" (PDFName "OFF") ]
    ++ non_empty "ON" (map ref (nub_ints (configVisible c)))
    ++ [ entry "Order" (order (fromMaybe panel (configPanel c))) ]
    ++ non_empty "RBGroups" (map (map ref . nub_ints) (configRadioGroups c))
    ++ non_empty "Locked" (map ref (nub_ints (configLocked c)))
    ++ (if configUsage c then non_empty "AS" automatic_states else [])

  -- See Note [Layers set by usage] in Graphics.PDF.OptionalContent
  automatic_states =
    [ dictFromList
        [ entry "Event" (PDFName event)
        , entry "Category" (map PDFName categories)
        , entry "OCGs" (map ref ocgs) ]
    | (event, categories, ocgs) <- usageApplications [ (n, o) | (n, _, o) <- layers ] ]

  order = concatMap item
  item (LayerEntry n []) = [AnyPdfObject (ref n)]
  item (LayerEntry n kids) = [AnyPdfObject (ref n), AnyPdfObject (order kids)]
  item (LayerLabel t kids)
    | T.null t = [AnyPdfObject (order kids)]
    | otherwise = [AnyPdfObject (pdfTextString t : order kids)]

  non_empty :: PdfObject a => String -> [a] -> [(PDFName, AnyPdfObject)]
  non_empty k xs = [ entry k xs | not (null xs) ]

  panel_layers p = concatMap (foldr (:) []) p
  config_layers c = configVisible c ++ configLocked c ++ concat (configRadioGroups c)
                    ++ maybe [] panel_layers (configPanel c)

  problems = concat
    [ [ "a layer of another document is used in the layers of this document (object " ++ show n ++ ")"
      | n <- IS.toList (IS.fromList (panel_layers panel ++ concat groups ++ concatMap config_layers configs))
      , not (IS.member n known) ]
    , panel_problems "the layer panel" panel
    , concat [ panel_problems ("the layer panel of the configuration " ++ show (configName c)) p
             | c <- configs, Just p <- [configPanel c] ]
    , [ "a radio group has more than one visible layer: " ++ unwords (map layer_name vs)
      | g <- groups, let vs = filter visible g, length vs > 1 ]
    , [ "a radio group of the configuration " ++ show (configName c) ++ " has more than one visible layer: "
        ++ unwords (map layer_name vs)
      | c <- configs, let on = IS.fromList (configVisible c), g <- configRadioGroups c
      , let vs = filter (`IS.member` on) (nub_ints g), length vs > 1 ]
    , [ "two layer configurations have the name " ++ show n | n <- duplicates (map configName configs) ]
    , [ "a layer configuration has an empty name" | any (T.null . configName) configs ]
    , [ "a layer configuration has the name \"Default\", which is the name of the default configuration"
      | any ((== T.pack "Default") . configName) configs ]
    , if not pdfa then [] else concat
        [ [ "PDF/A: the layer " ++ layer_name n ++ " has a usage (zoom, print, export or language), which needs /AS: PDF/A forbids /AS"
          | n <- with layerHasUsage ]
        , [ "PDF/A: the layer configuration " ++ show (configName c) ++ " applies the usage of the layers (/AS), which PDF/A forbids"
          | c <- configs, configUsage c ]
        , [ "PDF/A: the layer panel does not have all the layers" | missing panel ]
        , [ "PDF/A: the layer panel of the configuration " ++ show (configName c) ++ " does not have all the layers"
          | c <- configs, Just p <- [configPanel c], missing p ]
        ]
    ]
  panel_problems = panelProblems layer_name
  missing p = let present = IS.fromList (panel_layers p) in any (not . (`IS.member` present)) numbers

  duplicates :: Ord a => [a] -> [a]
  duplicates xs = M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [ (x, 1) | x <- xs ]))

---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- The layers (optional content) of a PDF file that is read: list them,
-- change them, put a page in a layer. The removal of a layer is in
-- "Graphics.PDF.File.LayerRemoval".
---------------------------------------------------------
module Graphics.PDF.File.Layers
  ( FileLayers(..)
  , readLayers
  , writeLayers
  , addLayer
  , putPageInLayer
  , putAnnotationsInLayer
    -- * Values of a file, for the removal of a layer
  , key
  , look
  , asDict
  , asArray
  , references
  , exists
  , isOcg
  , reference
  , objectDict
  , modifyObject
  , modifyDict
  , modifyArray
  , ocProperties
  , orderValues
  , layerCheck
  , pageResources
  , resourcesOwner
  , contentReferences
  , nubInts
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Text as T
import Graphics.PDF.File.Object
import Graphics.PDF.File.Text (decodeTextString, encodeTextString)
import Graphics.PDF.File.Write (largestNumber)
import Graphics.PDF.LowLevel.OptionalContent

-- Note [Layers of a file]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- The layers of a file are in the catalog (PDF 32000-1, 8.11.4):
--
--   /OCProperties << /OCGs [12 0 R 13 0 R]
--                    /D << /BaseState /ON /OFF [13 0 R] /Order [12 0 R 13 0 R] >>
--                    /Configs [<< /Name (Print) ... >>] >>
--
-- 'readLayers' gives each layer (the object number of its OCG, its name
-- and its options), the panel of /D, its radio groups, and the other
-- configurations. A layer is visible in /D when it is in /ON, or when it
-- is not in /OFF and /BaseState is not /OFF. A reference in these arrays
-- to an object that is not an OCG of /OCGs is ignored, as the standard
-- says. A panel (/Order) is read with a set of the arrays that are read,
-- thus a loop of references ends, and a group with no label is a
-- 'LayerLabel' with an empty label.
--
-- 'writeLayers' writes only the parts that change: an OCG whose name or
-- usage changes, /D when the states, the panel, the radio groups or the
-- usage change, and the configurations that change. Thus
--
--   writeLayers (readLayers f) f == f
--
-- and an incremental update ('Graphics.PDF.File.updatePdfFile') writes
-- only the objects that change. In the objects that it writes, it keeps
-- the keys that it does not know (/Intent, /CreatorInfo, /ListMode, the
-- other categories of /AS ...). It writes the states of /D as /OFF with
-- no /BaseState. It makes the usage application dictionaries (/AS) of the
-- four categories again, as the builder does (Note [Layers set by usage]
-- in Graphics.PDF.OptionalContent), and keeps the other ones.
--
-- Some objects can be shared, and 'writeLayers' does not change a shared
-- object for one of its users: a new /Usage is a direct dictionary in the
-- OCG. A configuration is found by its value, then by its name, and not
-- by its place in /Configs. A configuration whose /BaseState is
-- /Unchanged keeps its states when they do not change; 'LayerConfig' has
-- no such state, thus its states are written as /OFF and /ON when they
-- change.
--
-- The builder writes the panel of /D in a configuration that has no panel
-- ('configPanel' is Nothing). Thus 'readLayers' gives this panel for it.
--
-- A new panel must follow the rules of the builder (Note [Labels of the
-- layer panel] in Graphics.PDF.OptionalContent).
--
-- 'addLayer' adds an OCG, makes /OCProperties when the file has none, and
-- adds the layer at the end of the panel of /D when /D has a panel. A /D
-- with no panel stays with no panel: a panel with only the new layer
-- would hide the other layers of the panel of the viewer.

-- Note [Page in a layer]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- 'putPageInLayer' puts all the content of a page in a layer. Two new
-- streams go around the streams of the page, which do not change:
--
--   /Contents [20 0 R 4 0 R 21 0 R]      20: /OC /HPDFoc1 BDC   21: EMC
--
-- The name of the layer is in /Properties of the resources of the page.
-- These resources can be shared with other pages, or come from a /Pages
-- node. Thus the page gets its own resources: a copy of the resources
-- that it has, with the name of the layer.
--
-- 'putAnnotationsInLayer' sets /OC on the annotations of the page that
-- have no /OC.

-- | The layers of a file. See Note [Layers of a file]
data FileLayers = FileLayers
  { fileLayers :: [(Int, T.Text, LayerOptions)]
    -- ^ The object number, the name and the options of each layer, in the
    -- order of /OCGs
  , filePanel :: Maybe [LayerItem Int]
    -- ^ The panel of the default configuration (/Order), if it has one
  , fileRadioGroups :: [[Int]]
    -- ^ The radio groups of the default configuration
  , fileConfigs :: [LayerConfig Int]
    -- ^ The other configurations
  } deriving (Eq, Show)

------------------------------------------------------------------------
-- Values

key :: String -> S.ByteString
key = C.pack

look :: String -> Dict -> Maybe Value
look k = dictLookup (key k)

asDict :: PdfFile -> Value -> Maybe Dict
asDict f v = case resolveValue f v of
  Dict d -> Just d
  _ -> Nothing

asArray :: PdfFile -> Value -> Maybe [Value]
asArray f v = case resolveValue f v of
  Array a -> Just a
  _ -> Nothing

-- | The object numbers of the references of an array to objects of the
-- file. A reference whose generation is not the one of the object is a
-- reference to null (PDF 32000-1, 7.3.10).
references :: PdfFile -> Maybe Value -> [Int]
references f v = [ n | Just a <- [v >>= asArray f], Ref n g <- a, exists f n g ]

-- | The object exists, with this generation
exists :: PdfFile -> Int -> Int -> Bool
exists f n g = fmap fst (IM.lookup n (pdfObjects f)) == Just g

-- | The object is an optional content group
isOcg :: PdfFile -> Int -> Bool
isOcg f n = case IM.lookup n (pdfObjects f) of
  Just (_, Plain (Dict d)) -> look "Type" d == Just (Name (key "OCG"))
  _ -> False

number :: Value -> Maybe Double
number v = case v of
  Integer i -> Just (fromIntegral i)
  Real m e -> Just (fromRational (realToRational m e))
  _ -> Nothing

-- | A number as a value: an integer when it is one, or else a real with
-- the digits that it needs (6 at most after the point)
number_value :: Double -> Value
number_value x
  | x == fromIntegral r && abs x < 1e15 = Integer r
  | otherwise =
      let m = round (x * 1e6) :: Integer
          strip (a, e) = if e > 1 && a `mod` 10 == 0 then strip (a `div` 10, e - 1) else (a, e)
          (m', e') = strip (m, 6 :: Int)
      in Real m' e'
  where r = round x :: Integer

on_off :: Value -> Maybe Bool
on_off (Name n)
  | n == key "ON" = Just True
  | n == key "OFF" = Just False
on_off _ = Nothing

state_name :: Bool -> Value
state_name b = Name (key (if b then "ON" else "OFF"))

text :: T.Text -> Value
text = String . encodeTextString

generation :: PdfFile -> Int -> Int
generation f n = maybe 0 fst (IM.lookup n (pdfObjects f))

reference :: PdfFile -> Int -> Value
reference f n = Ref n (generation f n)

-- | Change the dictionary of an object (a dictionary or a stream)
modifyObject :: Int -> (Dict -> Dict) -> PdfFile -> PdfFile
modifyObject n g f = f {pdfObjects = IM.adjust change n (pdfObjects f)}
  where
    change (gen, Plain (Dict d)) = (gen, Plain (Dict (g d)))
    change (gen, Stream d s) = (gen, Stream (g d) s)
    change other = other

-- | Change a dictionary that is a value: in its object when the value is
-- a reference to it, or else in the value
modifyDict :: PdfFile -> Value -> (Dict -> Dict) -> (Value, PdfFile)
modifyDict f v g = case v of
  Ref n gen | Just (gen', Plain (Dict _)) <- IM.lookup n (pdfObjects f), gen == gen' -> (v, modifyObject n g f)
  Dict d -> (Dict (g d), f)
  _ -> (Dict (g M.empty), f)

-- | Change an array that is a value, as 'modifyDict' does
modifyArray :: PdfFile -> Value -> ([Value] -> [Value]) -> (Value, PdfFile)
modifyArray f v g = case v of
  Ref n gen | Just (gen', Plain (Array a)) <- IM.lookup n (pdfObjects f), gen == gen' ->
    (v, f {pdfObjects = IM.insert n (gen, Plain (Array (g a))) (pdfObjects f)})
  Array a -> (Array (g a), f)
  _ -> (Array (g []), f)

-- | Set or remove a key: an empty array removes it
set_list :: String -> [Value] -> Dict -> Dict
set_list k [] = M.delete (key k)
set_list k xs = M.insert (key k) (Array xs)

catalog_number :: PdfFile -> Either String Int
catalog_number f = case M.lookup (key "Root") (pdfTrailer f) of
  Just (Ref n _) | Just (_, Plain (Dict _)) <- IM.lookup n (pdfObjects f) -> Right n
  _ -> Left "the file has no catalog"

objectDict :: PdfFile -> Int -> Maybe Dict
objectDict f n = case IM.lookup n (pdfObjects f) of
  Just (_, Plain (Dict d)) -> Just d
  Just (_, Stream d _) -> Just d
  _ -> Nothing

-- | The value of /OCProperties in the catalog
ocProperties :: PdfFile -> Either String (Int, Maybe Value)
ocProperties f = do
  n <- catalog_number f
  return (n, objectDict f n >>= look "OCProperties")

------------------------------------------------------------------------
-- Reading

-- | The layers of a file. A file with no /OCProperties has no layer.
-- See Note [Layers of a file]
readLayers :: PdfFile -> Either String FileLayers
readLayers f = do
  (_, ocp_value) <- ocProperties f
  case ocp_value of
    Nothing -> Right (FileLayers [] Nothing [] [])
    Just v -> case asDict f v of
      Nothing -> Left "/OCProperties is not a dictionary"
      Just ocp -> Right (read_layers f ocp)

read_layers :: PdfFile -> Dict -> FileLayers
read_layers f ocp = FileLayers
  { fileLayers = [ (n, layer_name n, options n) | n <- ocgs ]
  , filePanel = panel_of d
  , fileRadioGroups = groups_of d
  , fileConfigs = map config [ c | Just cs <- [look "Configs" ocp >>= asArray f], v <- cs, Just c <- [asDict f v] ]
  }
  where
    ocgs = nubInts (filter (isOcg f) (references f (look "OCGs" ocp)))
    known = IS.fromList ocgs
    d = fromMaybe M.empty (look "D" ocp >>= asDict f)
    d_visible = visible_set d
    d_locked = IS.fromList (references f (look "Locked" d))
    layer_name n = case objectDict f n >>= look "Name" of
      Just (String s) -> decodeTextString s
      _ -> T.empty
    options n = (usage_options f (fromMaybe M.empty (objectDict f n)))
      { layerVisible = IS.member n d_visible, layerLocked = IS.member n d_locked }
    visible_set c =
      let base = look "BaseState" c /= Just (Name (key "OFF"))
          on = IS.fromList (references f (look "ON" c))
          off = IS.fromList (references f (look "OFF" c))
      in IS.fromList [ n | n <- ocgs, not (IS.member n off), base || IS.member n on ]
    panel_of c = parse_order f known <$> look "Order" c
    groups_of c = [ g | Just gs <- [look "RBGroups" c >>= asArray f], v <- gs
                      , let g = filter (`IS.member` known) (references f (Just v)), not (null g) ]
    config c = LayerConfig
      { configName = case look "Name" c of
          Just (String s) -> decodeTextString s
          _ -> T.empty
      , configVisible = filter (`IS.member` visible_set c) ocgs
      , configLocked = filter (`IS.member` known) (references f (look "Locked" c))
      , configPanel = panel_of c
      , configRadioGroups = groups_of c
      , configUsage = any (has_our_category f) (usage_dicts f c)
      }

-- | The usage options of an OCG; the state is not in the OCG
usage_options :: PdfFile -> Dict -> LayerOptions
usage_options f ocg = defaultLayerOptions
  { layerZoom = sub "Zoom" >>= \z -> Just (fromMaybe 0 (look "min" z >>= number), look "max" z >>= number)
  , layerPrint = sub "Print" >>= look "PrintState" >>= on_off
  , layerExport = sub "Export" >>= look "ExportState" >>= on_off
  , layerLanguage = sub "Language" >>= \l -> case look "Lang" l of
      Just (String s) -> Just (LayerLanguage (decodeTextString s) ((look "Preferred" l >>= on_off) == Just True))
      _ -> Nothing
  }
  where
    usage = fromMaybe M.empty (look "Usage" ocg >>= asDict f)
    sub k = look k usage >>= asDict f

-- | The usage application dictionaries (/AS) of a configuration
usage_dicts :: PdfFile -> Dict -> [Dict]
usage_dicts f c = [ a | Just as <- [look "AS" c >>= asArray f], v <- as, Just a <- [asDict f v] ]

-- | The four categories of the usage that HPDF writes
our_categories :: [S.ByteString]
our_categories = map key ["Zoom", "Print", "Export", "Language"]

has_our_category :: PdfFile -> Dict -> Bool
has_our_category f a = or [ n `elem` our_categories | Just cs <- [look "Category" a >>= asArray f], Name n <- cs ]

-- | A panel (/Order). See Note [Layers of a file]
parse_order :: PdfFile -> IS.IntSet -> Value -> [LayerItem Int]
parse_order f known v = fst (items 256 seen0 (fromMaybe [] start))
  where
    (start, seen0) = array_of IS.empty v
    -- the elements of an array, and the arrays that are read
    array_of seen x = case x of
      Array xs -> (Just xs, seen)
      Ref n gen
        | IS.member n seen -> (Nothing, seen)
        | Just (gen', Plain (Array xs)) <- IM.lookup n (pdfObjects f), gen == gen' -> (Just xs, IS.insert n seen)
      _ -> (Nothing, seen)
    is_array x = case x of
      Array _ -> True
      Ref n gen | Just (gen', Plain (Array _)) <- IM.lookup n (pdfObjects f) -> gen == gen'
      _ -> False
    items :: Int -> IS.IntSet -> [Value] -> ([LayerItem Int], IS.IntSet)
    items depth seen vs
      | depth <= 0 = ([], seen)
      | otherwise = case vs of
          [] -> ([], seen)
          Ref n g : rest | IS.member n known, exists f n g -> case rest of
            r : rest' | is_array r ->
              let (kids, seen1) = array_of seen r
                  (children, seen2) = maybe ([], seen1) (children_items (depth - 1) seen1) kids
                  (others, seen3) = items depth seen2 rest'
              in (LayerEntry n children : others, seen3)
            _ -> let (others, seen1) = items depth seen rest in (LayerEntry n [] : others, seen1)
          x : rest | is_array x ->
            let (xs, seen1) = array_of seen x
                (group, seen2) = maybe ([], seen1) (group_items (depth - 1) seen1) xs
                (others, seen3) = items depth seen2 rest
            in (group ++ others, seen3)
          _ : rest -> items depth seen rest
    -- an array: a group with a label, or a group with no label
    group_items depth seen ys = case ys of
      String s : rest -> let (g, seen1) = items depth seen rest in ([LayerLabel (decodeTextString s) g], seen1)
      _ -> let (g, seen1) = items depth seen ys in ([LayerLabel T.empty g], seen1)
    -- the children of a layer: its items, or a group with a label
    children_items depth seen ys = case ys of
      String _ : _ -> group_items depth seen ys
      _ -> items depth seen ys

------------------------------------------------------------------------
-- Writing

-- | The values of a panel (/Order). An empty label is a group with no
-- label
orderValues :: PdfFile -> [LayerItem Int] -> [Value]
orderValues f = concatMap item
  where
    item (LayerEntry n []) = [reference f n]
    item (LayerEntry n kids) = [reference f n, Array (orderValues f kids)]
    item (LayerLabel t kids)
      | T.null t = [Array (orderValues f kids)]
      | otherwise = [Array (text t : orderValues f kids)]

-- | The usage application dictionaries of these layers
our_usage_dicts :: PdfFile -> [(Int, LayerOptions)] -> [Value]
our_usage_dicts f layers =
  [ Dict (M.fromList [ (key "Event", Name (key event))
                     , (key "Category", Array (map (Name . key) categories))
                     , (key "OCGs", Array (map (reference f) ocgs)) ])
  | (event, categories, ocgs) <- usageApplications layers ]

-- | The usage application dictionaries of a configuration without the
-- four categories of HPDF
other_usage_dicts :: PdfFile -> Dict -> [Value]
other_usage_dicts f c =
  [ Dict (M.insert (key "Category") (Array others) a)
  | a <- usage_dicts f c
  , let others = [ x | Just cs <- [look "Category" a >>= asArray f], x <- cs, x `notElem` map Name our_categories ]
  , not (null others) ]

-- | The parts of the options that are in the OCG
usage_of :: LayerOptions -> (Maybe (Double, Maybe Double), Maybe Bool, Maybe Bool, Maybe LayerLanguage)
usage_of o = (layerZoom o, layerPrint o, layerExport o, layerLanguage o)

-- | Change the layers of a file: the names, the options, the panel, the
-- radio groups and the configurations. The layers must be the layers of
-- the file ('addLayer' adds one, 'removeLayer' removes one).
-- See Note [Layers of a file]
writeLayers :: FileLayers -> PdfFile -> Either String PdfFile
writeLayers new f = do
  old <- readLayers f
  (cat, ocp_value) <- ocProperties f
  ocp_v <- maybe (Left "the file has no layers: addLayer adds one") Right ocp_value
  let numbers l = [ n | (n, _, _) <- fileLayers l ]
      new_numbers = numbers new
      known = IS.fromList new_numbers
  unless (IS.fromList (numbers old) == known && length new_numbers == IS.size known) $
    Left "writeLayers does not add or remove a layer: use addLayer and removeLayer"
  let used = concatMap (foldr (:) []) (fromMaybe [] (filePanel new)) ++ concat (fileRadioGroups new)
             ++ concat [ configVisible c ++ configLocked c ++ concat (configRadioGroups c)
                         ++ concatMap (foldr (:) []) (fromMaybe [] (configPanel c)) | c <- fileConfigs new ]
  case [ n | n <- used, not (IS.member n known) ] of
    n : _ -> Left ("the object " ++ show n ++ " is not a layer of the file")
    [] -> return ()
  mapM_ Left (mapMaybe (\(_, t, o) -> zoomProblem t o) (fileLayers new))
  -- a panel that changes: the rules of the builder. See Note [Labels of
  -- the layer panel] in Graphics.PDF.OptionalContent
  let layer_name n = maybe (show n) show (lookup n [ (k, t) | (k, t, _) <- fileLayers new ])
      old_panels = map (Just . configPanel) (fileConfigs old) ++ repeat Nothing
  mapM_ Left $ concat $
       [ panelProblems layer_name "the layer panel" p | Just p <- [filePanel new], filePanel new /= filePanel old ]
    ++ [ panelProblems layer_name ("the layer panel of the configuration " ++ show (configName c)) p
       | (c, old_panel) <- zip (fileConfigs new) old_panels, Just p <- [configPanel c], old_panel /= Just (configPanel c) ]
  let old_layers = IM.fromList [ (n, (t, o)) | (n, t, o) <- fileLayers old ]
      changed_ocgs = [ (n, t, o) | (n, t, o) <- fileLayers new, Just (t0, o0) <- [IM.lookup n old_layers]
                                 , t /= t0 || usage_of o /= usage_of o0 ]
      usage_changed = or [ usage_of o /= usage_of o0 | (n, _, o) <- fileLayers new, Just (_, o0) <- [IM.lookup n old_layers] ]
      state_of l = [ (n, layerVisible o, layerLocked o) | (n, _, o) <- fileLayers l ]
      d_changed = usage_changed || state_of new /= state_of old || filePanel new /= filePanel old
                  || fileRadioGroups new /= fileRadioGroups old
      new_usage = [ (n, o) | (n, _, o) <- fileLayers new ]
      -- 1. the OCGs
      f1 = List.foldl' (write_ocg) f changed_ocgs
      -- 2. /OCGs, when the order changes
      (ocp1, f2)
        | new_numbers /= numbers old = modifyDict f1 ocp_v (M.insert (key "OCGs") (Array (map (reference f1) new_numbers)))
        | otherwise = (ocp_v, f1)
      -- 3. /D
      (ocp2, f3)
        | d_changed = write_default new new_usage usage_changed f2 ocp1
        | otherwise = (ocp1, f2)
      -- 4. /Configs
      (ocp3, f4) = write_configs new old new_usage usage_changed f3 ocp2
      f5 = modifyObject cat (M.insert (key "OCProperties") ocp3) f4
      locked = any (\(_, _, o) -> layerLocked o) (fileLayers new) || any (not . null . configLocked) (fileConfigs new)
  return $ if f5 == f then f else f5 {pdfVersion = maximum ([pdfVersion f5, (1, 5)] ++ [ (1, 6) | locked ])}

-- | Write the name and the usage of an OCG
write_ocg :: PdfFile -> (Int, T.Text, LayerOptions) -> PdfFile
write_ocg f (n, name, o) = case objectDict f n of
  Nothing -> f
  Just ocg ->
    let usage = fromMaybe M.empty (look "Usage" ocg >>= asDict f)
        usage' = set_usage usage
        -- the name keeps its bytes when its text does not change
        set_name d = case look "Name" d of
          Just (String s) | decodeTextString s == name -> d
          _ -> M.insert (key "Name") (text name) d
        -- a new usage is a direct dictionary: other OCGs can share the
        -- old one
        set_usage_key d
          | usage' == usage = d
          | M.null usage' = M.delete (key "Usage") d
          | otherwise = M.insert (key "Usage") (Dict usage') d
    in modifyObject n (set_usage_key . set_name) f
  where
    sub_dict u k = fromMaybe M.empty (look k u >>= asDict f)
    set k v = maybe (M.delete (key k)) (M.insert (key k)) v
    set_usage u = set "Zoom" ((\(lo, hi) -> Dict (M.fromList ((key "min", number_value lo) : [ (key "max", number_value h) | Just h <- [hi] ]))) <$> layerZoom o)
                . set "Print" ((\p -> Dict (M.insert (key "PrintState") (state_name p) (sub_dict u "Print"))) <$> layerPrint o)
                . set "Export" ((\e -> Dict (M.insert (key "ExportState") (state_name e) (sub_dict u "Export"))) <$> layerExport o)
                . set "Language" ((\l -> Dict (M.fromList [ (key "Lang", text (languageTag l))
                                                          , (key "Preferred", state_name (languagePreferred l)) ])) <$> layerLanguage o)
                $ u

-- | Write /D: the states, the panel, the radio groups, and /AS when the
-- usage changes
write_default :: FileLayers -> [(Int, LayerOptions)] -> Bool -> PdfFile -> Value -> (Value, PdfFile)
write_default new usage usage_changed f ocp_v =
  let ocp = fromMaybe M.empty (asDict f ocp_v)
      d_v = fromMaybe (Dict M.empty) (look "D" ocp)
      (d_v', f1) = modifyDict f d_v $ \d ->
          let as | usage_changed = other_usage_dicts f d ++ our_usage_dicts f usage
                 | otherwise = [ v | Just as0 <- [look "AS" d >>= asArray f], v <- as0 ]
          in set_list "AS" as
           . set_list "Locked" [ reference f n | (n, _, o) <- fileLayers new, layerLocked o ]
           . set_list "RBGroups" [ Array (map (reference f) g) | g <- fileRadioGroups new ]
           . maybe (M.delete (key "Order")) (M.insert (key "Order") . Array . orderValues f) (filePanel new)
           . set_list "OFF" [ reference f n | (n, _, o) <- fileLayers new, not (layerVisible o) ]
           . M.delete (key "ON")
           . M.delete (key "BaseState")
           $ d
  in modifyDict f1 ocp_v (M.insert (key "D") d_v')

-- | Write the configurations that change
write_configs :: FileLayers -> FileLayers -> [(Int, LayerOptions)] -> Bool -> PdfFile -> Value -> (Value, PdfFile)
write_configs new old usage usage_changed f ocp_v
  | fileConfigs new == fileConfigs old && not (usage_changed && any configUsage (fileConfigs new)) = (ocp_v, f)
  | otherwise =
      let ocp = fromMaybe M.empty (asDict f ocp_v)
          configs_v = fromMaybe (Array []) (look "Configs" ocp)
          old_values = fromMaybe [] (asArray f configs_v)
          -- the values of the configurations that 'readLayers' read, with
          -- their configurations: a configuration is found by its value,
          -- then by its name, and not by its place
          old_pairs = zip3 [0 :: Int ..] [ v | v <- old_values, isJust (asDict f v) ] (fileConfigs old)
          unchanged c = not (usage_changed && configUsage c)
          -- the old configuration of a new one, and the old ones that are
          -- left: Left for the same one; Right for the one that has the
          -- same name, or else the same place
          old_of i c unused =
                (\(x, rest) -> (Left x, rest)) <$> take_first (\(_, _, o) -> o == c && unchanged c) unused
            <|> (\(x, rest) -> (Right x, rest)) <$> (take_first (\(_, _, o) -> configName o == configName c) unused
                                                     <|> take_first (\(j, _, _) -> j == i) unused)
          -- the value of a new configuration
          value_of (g, unused) (i, c) = case old_of i c unused of
            Just (Left (_, v, _), rest) -> ((g, rest), v)
            Just (Right (_, v, o), rest) -> let (v', g') = modifyDict g v (write_config g c (Just o)) in ((g', rest), v')
            Nothing -> let (v', g') = modifyDict g (Dict M.empty) (write_config g c Nothing) in ((g', unused), v')
          ((f1, _), values) = List.mapAccumL value_of (f, old_pairs) (zip [0 ..] (fileConfigs new))
      in if values == old_values then (ocp_v, f1)
         else case configs_v of
           -- an array that is an object: the object changes
           Ref _ _ | not (null values) -> (ocp_v, snd (modifyArray f1 configs_v (const values)))
           _ -> modifyDict f1 ocp_v (set_list "Configs" values)
  where
    -- the first element that has the property, and the other elements
    take_first p xs = case break p xs of
      (before, x : after) -> Just (x, before ++ after)
      _ -> Nothing
    -- a configuration whose /BaseState is /Unchanged keeps its states
    -- when they do not change
    write_config g c old_config d =
      let base_unchanged = look "BaseState" d == Just (Name (key "Unchanged"))
                           && fmap configVisible old_config == Just (configVisible c)
          states
            | base_unchanged = id
            | otherwise = set_list "ON" (map (reference g) (nubInts (configVisible c)))
                        . M.delete (key "OFF")
                        . M.insert (key "BaseState") (Name (key "OFF"))
      in set_list "AS" (other_usage_dicts g d ++ (if configUsage c then our_usage_dicts g usage else []))
         . set_list "Locked" (map (reference g) (nubInts (configLocked c)))
         . set_list "RBGroups" [ Array (map (reference g) (nubInts grp)) | grp <- configRadioGroups c ]
         . maybe (M.delete (key "Order")) (M.insert (key "Order") . Array . orderValues g) (configPanel c)
         . states
         . (\x -> case look "Name" x of
               Just (String s) | decodeTextString s == configName c -> x
               _ -> M.insert (key "Name") (text (configName c)) x)
         $ d

nubInts :: [Int] -> [Int]
nubInts = go IS.empty
  where
    go _ [] = []
    go seen (x : xs)
      | IS.member x seen = go seen xs
      | otherwise = x : go (IS.insert x seen) xs

------------------------------------------------------------------------
-- A new layer

-- | Add a layer to a file: its object number, and the file. The layer is
-- at the end of the panel of the default configuration, when it has one.
-- See Note [Layers of a file]
addLayer :: T.Text -> LayerOptions -> PdfFile -> Either String (Int, PdfFile)
addLayer name options f = do
  mapM_ Left (zoomProblem name options)
  (cat, ocp_value) <- ocProperties f
  let n = largestNumber f + 1
      ocg = Dict (M.fromList [(key "Type", Name (key "OCG")), (key "Name", text name)])
      f1 = f {pdfObjects = IM.insert n (0, Plain ocg) (pdfObjects f)}
      new_ref = Ref n 0
  f2 <- case ocp_value of
    Nothing ->
      let ocp = Dict (M.fromList
            [ (key "OCGs", Array [new_ref])
            , (key "D", Dict (M.fromList [(key "Name", text (T.pack "Default")), (key "Order", Array [])])) ])
      in Right (modifyObject cat (M.insert (key "OCProperties") ocp) f1)
    Just v -> case asDict f1 v of
      Nothing -> Left "/OCProperties is not a dictionary"
      Just ocp ->
        let ocgs_v = fromMaybe (Array []) (look "OCGs" ocp)
            (ocgs_v', f1a) = modifyArray f1 ocgs_v (++ [new_ref])
            (v', f1b) = modifyDict f1a v (M.insert (key "OCGs") ocgs_v')
        in Right (modifyObject cat (M.insert (key "OCProperties") v') f1b)
  layers <- readLayers f2
  let layers' = layers
        { fileLayers = [ if m == n then (n, name, options) else l | l@(m, _, _) <- fileLayers layers ]
        , filePanel = fmap (++ [LayerEntry n []]) (filePanel layers)
        }
  f3 <- writeLayers layers' f2
  return (n, f3)

------------------------------------------------------------------------
-- Pages

-- | The object number of a page (0 is the first page)
page_number :: PdfFile -> Int -> Either String Int
page_number f i = case drop i (pdfPages f) of
  (n, _) : _ | i >= 0 -> Right n
  _ -> Left ("the file has no page " ++ show i)

-- | The object whose dictionary has the resources of a page: the page, or
-- a /Pages node above it
resourcesOwner :: PdfFile -> Int -> Maybe Int
resourcesOwner f = go (64 :: Int) IS.empty
  where
    go depth seen n
      | depth <= 0 || IS.member n seen = Nothing
      | otherwise = case objectDict f n of
          Just d | isJust (look "Resources" d) -> Just n
                 | Just (Ref p _) <- look "Parent" d -> go (depth - 1) (IS.insert n seen) p
          _ -> Nothing

pageResources :: PdfFile -> Int -> Dict
pageResources f n = fromMaybe M.empty (resourcesOwner f n >>= objectDict f >>= look "Resources" >>= asDict f)

-- | The streams of /Contents of a page
contentReferences :: PdfFile -> Dict -> [Value]
contentReferences f page = case look "Contents" page of
  Just r@(Ref n _) -> case IM.lookup n (pdfObjects f) of
    Just (_, Plain (Array xs)) -> xs
    _ -> [r]
  Just (Array xs) -> xs
  _ -> []

layerCheck :: PdfFile -> Int -> Either String ()
layerCheck f layer = do
  layers <- readLayers f
  unless (any (\(n, _, _) -> n == layer) (fileLayers layers)) $
    Left ("the object " ++ show layer ++ " is not a layer of the file")

-- | Put all the content of a page in a layer. The first argument is the
-- object number of the layer, the second the index of the page (0 is the
-- first page). See Note [Page in a layer]
putPageInLayer :: Int -> Int -> PdfFile -> Either String PdfFile
putPageInLayer layer i f = do
  layerCheck f layer
  p <- page_number f i
  page <- maybe (Left "a page is not a dictionary") Right (objectDict f p)
  let contents = contentReferences f page
  if null contents then return f else do
    let resources = pageResources f p
        properties = fromMaybe M.empty (look "Properties" resources >>= asDict f)
        name = case [ k | (k, Ref target _) <- M.toList properties, target == layer ] of
          k : _ -> k
          [] -> head' [ k | j <- [1 :: Int ..], let k = key ("HPDFoc" ++ show j), not (M.member k properties) ]
        resources' = M.insert (key "Properties") (Dict (M.insert name (reference f layer) properties)) resources
        m = largestNumber f + 1
        stream bytes = Stream (M.fromList [(key "Length", Integer (fromIntegral (S.length bytes)))]) bytes
        begin = stream (C.pack "/OC /" <> name <> C.pack " BDC\n")
        end = stream (C.pack "\nEMC\n")
        page' = M.insert (key "Contents") (Array ([Ref m 0] ++ contents ++ [Ref (m + 1) 0]))
              . M.insert (key "Resources") (Dict resources')
              $ page
        objects = IM.insert m (0, begin) . IM.insert (m + 1) (0, end) $ pdfObjects f
    return (modifyObject p (const page') f {pdfObjects = objects, pdfVersion = max (1, 5) (pdfVersion f)})
  where
    head' (x : _) = x
    head' [] = key "HPDFoc"

-- | Put the annotations of a page that have no /OC in a layer. The
-- arguments are those of 'putPageInLayer'. See Note [Page in a layer]
putAnnotationsInLayer :: Int -> Int -> PdfFile -> Either String PdfFile
putAnnotationsInLayer layer i f = do
  layerCheck f layer
  p <- page_number f i
  page <- maybe (Left "a page is not a dictionary") Right (objectDict f p)
  case look "Annots" page of
    Nothing -> return f
    Just annots_v -> do
      let oc = reference f layer
          add d = if isJust (look "OC" d) then d else M.insert (key "OC") oc d
          -- an annotation: in its object, or in the array
          annotation g v = case v of
            Ref a _ -> (modifyObject a add g, v)
            Dict d -> (g, Dict (add d))
            _ -> (g, v)
          (annots_v', f1) = case annots_v of
            Array xs -> let (g, ys) = List.mapAccumL annotation f xs in (Array ys, g)
            Ref _ _ -> let (g, ys) = List.mapAccumL annotation f (fromMaybe [] (asArray f annots_v))
                       in modifyArray g annots_v (const ys)
            _ -> (annots_v, f)
      return (modifyObject p (M.insert (key "Annots") annots_v') f1 {pdfVersion = max (1, 5) (pdfVersion f1)})


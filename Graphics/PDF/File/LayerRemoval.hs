---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- The removal of a layer of a PDF file that is read, with the content
-- that is visible only with it.
---------------------------------------------------------
module Graphics.PDF.File.LayerRemoval
  ( removeLayer
  ) where

import qualified Codec.Compression.Zlib as Z
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.State.Strict (StateT, execStateT, gets, lift, modify')
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Graphics.PDF.File.Content
import Graphics.PDF.File.Filter (decodeStream)
import Graphics.PDF.File.Layers
import Graphics.PDF.File.Object
import Graphics.PDF.File.Parse (Limits(..))
import Graphics.PDF.File.Write (largestNumber)
import Graphics.PDF.LowLevel.OptionalContent

-- Note [Removal of a layer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'removeLayer' removes the content that is visible only when the layer
-- is visible, then the layer:
--
-- * A target (/OC) is hidden when it is the layer, or a condition (/OCMD)
--   that is false when the layer is off, for all the states of the other
--   layers. A condition is evaluated with three values (on, off,
--   unknown): the layer is off, the other layers are unknown.
-- * In each content stream: the marked-content sequences of a hidden
--   target, and the Do of an XObject whose /OC is a hidden target, are
--   removed (Note [Removal of hidden content] in Graphics.PDF.File.Content).
--   The streams are those of the pages (the streams of /Contents are read
--   as one, because a sequence can cross them), of the form XObjects and
--   their soft masks, of the tiling patterns, of the glyphs of Type 3
--   fonts, and of the appearances of the annotations. Each stream is read
--   one time. A stream that changes is compressed again with Flate. The
--   new content of a page goes in its first stream, or in a new stream
--   when another page uses the first stream.
-- * An annotation whose /OC is a hidden target, or whose normal
--   appearance is a form with such an /OC, leaves /Annots.
-- * A condition that has the layer, and that is not hidden, is written
--   again with the layer replaced by "off". When it is then always true,
--   its /OCGs is empty, which makes it visible. The references of a
--   condition to objects that are not OCGs are ignored (PDF 32000-1,
--   8.11.2.2), and a visibility expression has a limit of 10000 nodes.
-- * The entries of /Properties that refer to a hidden target are
--   removed, the references to the layer leave /OCProperties (its
--   children in a panel take its place), and the OCG is removed.
--
-- The marked content of the structure tree (/MCID) in removed content is
-- no longer in the content.


data Tri = On | Off | Unknown deriving (Eq, Show)

and3, or3 :: [Tri] -> Tri
and3 xs
  | Off `elem` xs = Off
  | all (== On) xs = On
  | otherwise = Unknown
or3 xs
  | On `elem` xs = On
  | all (== Off) xs = Off
  | otherwise = Unknown

not3 :: Tri -> Tri
not3 On = Off
not3 Off = On
not3 Unknown = Unknown

-- | The condition of an OCMD, as an expression. Nothing: the OCMD has no
-- layer, and it has no effect (it is visible)
ocmd_expression :: PdfFile -> Dict -> Maybe (LayerExpr Int)
ocmd_expression f d = case look "VE" d of
  Just ve | Just (e, _) <- visibility (64 :: Int) ve (10000 :: Int) -> Just e
  _ -> case ocgs of
    [] -> Nothing
    _ -> Just $ case look "P" d of
      Just (Name p)
        | p == key "AllOn" -> LayerAnd (map LayerOn ocgs)
        | p == key "AnyOff" -> LayerOr (map (LayerNot . LayerOn) ocgs)
        | p == key "AllOff" -> LayerAnd (map (LayerNot . LayerOn) ocgs)
      _ -> LayerOr (map LayerOn ocgs)
  where
    -- the references to OCGs: the other ones are ignored (PDF 32000-1,
    -- 8.11.2.2)
    ocgs = case look "OCGs" d of
      Just (Ref n g) | exists f n g, isOcg f n -> [n]
      v -> filter (isOcg f) (references f v)
    -- a visibility expression, with a depth and a number of nodes: the
    -- arrays can be shared, and a shared array is read each time
    visibility depth v budget
      | depth <= 0 || budget <= 0 = Nothing
      | otherwise = case resolve_array v of
          Just (Name op : xs)
            | op == key "Not", [x] <- xs -> (\(e, b) -> (LayerNot e, b)) <$> visibility (depth - 1) x (budget - 1)
            | op == key "And", not (null xs) -> (\(es, b) -> (LayerAnd es, b)) <$> operands (depth - 1) xs (budget - 1)
            | op == key "Or", not (null xs) -> (\(es, b) -> (LayerOr es, b)) <$> operands (depth - 1) xs (budget - 1)
          Just _ -> Nothing
          Nothing -> case v of
            Ref n g | exists f n g, isOcg f n -> Just (LayerOn n, budget - 1)
            _ -> Nothing
    operands _ [] budget = Just ([], budget)
    operands depth (x : xs) budget = do
      (e, b1) <- visibility depth x budget
      (es, b2) <- operands depth xs b1
      return (e : es, b2)
    resolve_array v = case v of
      Array xs -> Just xs
      Ref n gen | Just (gen', Plain (Array xs)) <- IM.lookup n (pdfObjects f), gen == gen' -> Just xs
      _ -> Nothing

-- | The value of an expression when the layer is off and the other layers
-- are not known
evaluate :: Int -> LayerExpr Int -> Tri
evaluate layer e = case e of
  LayerOn n -> if n == layer then Off else Unknown
  LayerNot x -> not3 (evaluate layer x)
  LayerAnd xs -> and3 (map (evaluate layer) xs)
  LayerOr xs -> or3 (map (evaluate layer) xs)

-- | The expression with the layer off: a constant, or an expression
-- without the layer
without_layer :: Int -> LayerExpr Int -> Either Bool (LayerExpr Int)
without_layer layer e = case e of
  LayerOn n -> if n == layer then Left False else Right (LayerOn n)
  LayerNot x -> either (Left . not) (Right . LayerNot) (without_layer layer x)
  LayerAnd xs -> combine False LayerAnd (map (without_layer layer) xs)
  LayerOr xs -> combine True LayerOr (map (without_layer layer) xs)
  where
    -- 'absorbing' is the constant that decides the result (False for And)
    combine absorbing make rs
      | Left absorbing `elem` rs = Left absorbing
      | otherwise = case [ x | Right x <- rs ] of
          [] -> Left (not absorbing)
          [x] -> Right x
          xs -> Right (make xs)

-- | The OCMD of an expression, with the keys of the old OCMD that are not
-- the condition
ocmd_dict :: PdfFile -> Dict -> LayerExpr Int -> Dict
ocmd_dict f old e =
  let base = foldr M.delete old (map key ["OCGs", "P", "VE"])
      ocgs = Array (map (reference f) (nubInts (foldr (:) [] e)))
  in case conditionPolicy e of
       Just p -> M.insert (key "OCGs") ocgs (M.insert (key "P") (Name (key p)) base)
       Nothing -> M.insert (key "OCGs") ocgs (M.insert (key "VE") (ve e) base)
  where
    ve x = case x of
      LayerOn n -> reference f n
      LayerNot y -> Array [Name (key "Not"), ve y]
      LayerAnd ys -> Array (Name (key "And") : map ve ys)
      LayerOr ys -> Array (Name (key "Or") : map ve ys)

-- | The state of the removal: the file, and the streams that are read
data Removal = Removal
  { removal_file :: !PdfFile
  , removal_seen :: !IS.IntSet
  }

type Remove = StateT Removal (Either String)

-- | Remove a layer and the content that is visible only when it is
-- visible. The first argument gives the limits of the decoded streams
-- ('limitStream') and of the depth of the values ('limitDepth'). See Note
-- [Removal of a layer]
removeLayer :: Limits -> Int -> PdfFile -> Either String PdfFile
removeLayer limits layer f0 = do
  layerCheck f0 layer
  layers <- readLayers f0
  let ocmds = IM.fromList [ (n, d) | (n, (_, Plain (Dict d))) <- IM.toList (pdfObjects f0)
                                   , look "Type" d == Just (Name (key "OCMD")) ]
      ocmd_state d = maybe On (evaluate layer) (ocmd_expression f0 d)
      hidden_ocmds = IM.keysSet (IM.filter ((== Off) . ocmd_state) ocmds)
      hidden_target v = case v of
        Ref n g -> exists f0 n g && (n == layer || IS.member n hidden_ocmds)
        Dict d | look "Type" d == Just (Name (key "OCMD")) -> ocmd_state d == Off
        _ -> False
      pages = map fst (pdfPages f0)
      -- how many pages use each content stream
      content_uses = IM.fromListWith (+) [ (n, 1 :: Int) | p <- pages, Just d <- [objectDict f0 p]
                                                         , Ref n _ <- contentReferences f0 d ]
  Removal f1 _ <- execStateT (mapM_ (clean_page limits hidden_target content_uses) pages) (Removal f0 IS.empty)
  -- the conditions that have the layer, and that are not hidden
  let f2 = List.foldl' (\g (n, d) -> case ocmd_expression g d of
                      Just e | layer `elem` foldr (:) [] e, not (IS.member n hidden_ocmds) ->
                        case without_layer layer e of
                          Right e' -> modifyObject n (const (ocmd_dict g d e')) g
                          Left _ -> modifyObject n (M.insert (key "OCGs") (Array []) . flip (foldr M.delete) (map key ["P", "VE"])) g
                      _ -> g)
                  f1 (IM.toList ocmds)
      uses_ve = or [ M.member (key "VE") d | (n, _) <- IM.toList ocmds, Just d <- [objectDict f2 n] ]
  -- the layer leaves /OCProperties
  let without l = l
        { fileLayers = [ x | x@(n, _, _) <- fileLayers l, n /= layer ]
        , filePanel = remove_item <$> filePanel l
        , fileRadioGroups = filter (not . null) (map (filter (/= layer)) (fileRadioGroups l))
        , fileConfigs = [ c { configVisible = filter (/= layer) (configVisible c)
                            , configLocked = filter (/= layer) (configLocked c)
                            , configPanel = remove_item <$> configPanel c
                            , configRadioGroups = filter (not . null) (map (filter (/= layer)) (configRadioGroups c)) }
                        | c <- fileConfigs l ]
        }
      remove_item = concatMap item
      item (LayerEntry n kids)
        | n == layer = remove_item kids
        | otherwise = [LayerEntry n (remove_item kids)]
      item (LayerLabel t kids) = [LayerLabel t (remove_item kids)]
  f3 <- remove_from_properties layer (without layers) f2
  let f4 = f3 {pdfObjects = IM.delete layer (pdfObjects f3)}
  return f4 {pdfVersion = maximum ([pdfVersion f4] ++ [ (1, 6) | uses_ve ])}

-- | Write /OCProperties without the layer: the OCG is no longer in /OCGs,
-- in the arrays of /D and of the configurations, and in /AS
remove_from_properties :: Int -> FileLayers -> PdfFile -> Either String PdfFile
remove_from_properties layer new f = do
  (cat, ocp_value) <- ocProperties f
  case ocp_value of
    Nothing -> return f
    Just ocp_v -> do
      let drop_ref =filter (\v -> case v of Ref n _ -> n /= layer; _ -> True)
          (ocgs_v, f1) = case asDict f ocp_v >>= look "OCGs" of
            Just v -> modifyArray f v drop_ref
            Nothing -> (Array [], f)
          (ocp1, f2) = modifyDict f1 ocp_v (M.insert (key "OCGs") ocgs_v)
          -- the configurations, /D: their arrays without the layer
          clean_config g c = M.mapWithKey (clean_key g) c
          clean_key g k v
            | k `elem` map key ["ON", "OFF", "Locked"] = Array (drop_ref (fromMaybe [] (asArray g v)))
            | k == key "RBGroups" = Array [ Array (drop_ref (fromMaybe [] (asArray g x))) | x <- fromMaybe [] (asArray g v) ]
            | k == key "AS" = Array [ Dict (M.adjust (\o -> Array (drop_ref (fromMaybe [] (asArray g o)))) (key "OCGs") a)
                                    | x <- fromMaybe [] (asArray g v), Just a <- [asDict g x] ]
            | otherwise = v
          d_v = fromMaybe (Dict M.empty) (asDict f2 ocp1 >>= look "D")
          (d_v', f3) = modifyDict f2 d_v $ \d ->
            maybe (M.delete (key "Order")) (M.insert (key "Order") . Array . orderValues f2) (filePanel new)
              (clean_config f2 d)
          (ocp2, f4) = modifyDict f3 ocp1 (M.insert (key "D") d_v')
          configs = fromMaybe [] (asDict f4 ocp2 >>= look "Configs" >>= asArray f4)
          -- the configurations that 'readLayers' read are the dictionaries
          -- of /Configs, in their order; the other values stay
          config_value (g, cs) v = case (asDict g v, cs) of
            (Just _, c : rest) ->
              let (v', g') = modifyDict g v (\d -> maybe (M.delete (key "Order")) (M.insert (key "Order") . Array . orderValues g)
                                                     (configPanel c) (clean_config g d))
              in ((g', rest), v')
            _ -> ((g, cs), v)
          ((f5, _), config_values) = List.mapAccumL config_value (f4, fileConfigs new) configs
          (ocp3, f6)
            | null configs = (ocp2, f5)
            | otherwise = modifyDict f5 ocp2 (M.insert (key "Configs") (Array config_values))
      return (modifyObject cat (M.insert (key "OCProperties") ocp3) f6)

-- | Remove the hidden content of a page, of its resources and of its
-- annotations. See Note [Removal of a layer]
clean_page :: Limits -> (Value -> Bool) -> IM.IntMap Int -> Int -> Remove ()
clean_page limits hidden_target content_uses p = do
  f <- gets removal_file
  case objectDict f p of
    Nothing -> return ()
    Just page -> do
      let resources = pageResources f p
          contents = [ n | Ref n _ <- contentReferences f page ]
      -- the content: its streams, read as one
      datas <- forM contents $ \n -> decoded limits f n
      let joined = S.intercalate (C.pack "\n") (map snd datas)
      result <- lift (remove_hidden limits hidden_target f resources joined)
      case (result, contents) of
        (Just new, first : others) -> do
          g <- gets removal_file
          -- the first stream gets the content, when no other page uses
          -- it: else a new stream
          let shared = IM.findWithDefault 0 first content_uses > 1
              target = if shared then largestNumber g + 1 else first
              g1 | shared = g {pdfObjects = IM.insert target (0, Stream M.empty S.empty) (pdfObjects g)}
                 | otherwise = g
              g2 = modifyObject p (M.insert (key "Contents") (reference g1 target)) (replace_data target new g1)
              -- the other streams, when no other page uses them
              others_alone = [ n | n <- others, IM.lookup n content_uses == Just 1, n /= first ]
          set_file g2 {pdfObjects = foldr IM.delete (pdfObjects g2) others_alone}
        _ -> return ()
      -- the annotations
      clean_annotations limits hidden_target p
      -- the resources, then their /Properties
      clean_resources limits hidden_target resources
      forM_ (resourcesOwner f p) (clean_properties hidden_target)

-- | Remove the annotations whose /OC is hidden, and clean the appearances
-- of the others
clean_annotations :: Limits -> (Value -> Bool) -> Int -> Remove ()
clean_annotations limits hidden_target p = do
  f <- gets removal_file
  forM_ (objectDict f p >>= look "Annots") $ \annots_v -> do
    let elements = fromMaybe [] (asArray f annots_v)
        annotation v = asDict f v
        -- the annotation, or its normal appearance (a form with /OC)
        hidden v = case annotation v of
          Just a -> maybe False hidden_target (look "OC" a) || hidden_appearance a
          Nothing -> False
        hidden_appearance a = case look "AP" a >>= asDict f >>= look "N" of
          Just (Ref n _) -> hidden_form n
          Just other | Just states <- asDict f other, not (M.null states) ->
            and [ case s of
                    Ref n _ -> hidden_form n
                    _ -> False
                | s <- M.elems states ]
          _ -> False
        hidden_form n = case IM.lookup n (pdfObjects f) of
          Just (_, Stream d _) -> maybe False hidden_target (look "OC" d)
          _ -> False
        kept = filter (not . hidden) elements
    when (length kept /= length elements) $ do
      let (annots_v', g) = modifyArray f annots_v (const kept)
      set_file (modifyObject p (M.insert (key "Annots") annots_v') g)
    forM_ kept $ \v -> forM_ (annotation v >>= look "AP" >>= asDict f) $ \ap ->
      forM_ (M.elems ap) $ \appearance -> case appearance of
        Ref n _ -> clean_stream limits hidden_target M.empty n
        _ -> forM_ (asDict f appearance) $ \states ->
               forM_ [ n | Ref n _ <- M.elems states ] (clean_stream limits hidden_target M.empty)

-- | Clean the streams that resources use: the form XObjects, the soft
-- masks of the graphics states, the tiling patterns, the glyphs of the
-- Type 3 fonts
clean_resources :: Limits -> (Value -> Bool) -> Dict -> Remove ()
clean_resources limits hidden_target resources = do
  f <- gets removal_file
  let entries k = maybe [] M.elems (look k resources >>= asDict f)
      stream_dict n = case IM.lookup n (pdfObjects f) of
        Just (_, Stream d _) -> Just d
        _ -> Nothing
  forM_ (entries "XObject") $ \v -> case v of
    Ref n _ | Just d <- stream_dict n, look "Subtype" d == Just (Name (key "Form")) -> clean_stream limits hidden_target resources n
    _ -> return ()
  forM_ (entries "Pattern") $ \v -> case v of
    Ref n _ | Just d <- stream_dict n, look "PatternType" d == Just (Integer 1) -> clean_stream limits hidden_target resources n
    _ -> return ()
  forM_ (entries "ExtGState") $ \v -> forM_ (asDict f v >>= look "SMask" >>= asDict f >>= look "G") $ \g -> case g of
    Ref n _ -> clean_stream limits hidden_target resources n
    _ -> return ()
  forM_ (entries "Font") $ \v -> forM_ (asDict f v) $ \font ->
    when (look "Subtype" font == Just (Name (key "Type3"))) $ do
      let own = look "Resources" font >>= asDict f
          font_resources = fromMaybe resources own
      forM_ [ n | Just procs <- [look "CharProcs" font >>= asDict f], Ref n _ <- M.elems procs ] $
        clean_stream_resources limits hidden_target font_resources False
      -- the resources of the font, one time for all its glyphs
      forM_ own (clean_resources limits hidden_target)

-- | Clean a stream with its own resources, or with these resources when
-- it has none. A stream is cleaned one time.
clean_stream :: Limits -> (Value -> Bool) -> Dict -> Int -> Remove ()
clean_stream limits hidden_target inherited n = do
  f <- gets removal_file
  let own = case IM.lookup n (pdfObjects f) of
        Just (_, Stream d _) -> look "Resources" d >>= asDict f
        _ -> Nothing
  clean_stream_resources limits hidden_target (fromMaybe inherited own) (isJust own) n
  forM_ own (const (clean_properties hidden_target n))

-- | Clean a stream with these resources. The last argument tells if the
-- resources are the own resources of the stream: then the streams that
-- they use are cleaned too. Else the caller cleans them, one time.
clean_stream_resources :: Limits -> (Value -> Bool) -> Dict -> Bool -> Int -> Remove ()
clean_stream_resources limits hidden_target resources own n = do
  seen <- gets removal_seen
  unless (IS.member n seen) $ do
    modify' (\r -> r {removal_seen = IS.insert n (removal_seen r)})
    f <- gets removal_file
    case IM.lookup n (pdfObjects f) of
      Just (_, Stream _ _) -> do
        (_, content) <- decoded limits f n
        result <- lift (remove_hidden limits hidden_target f resources content)
        forM_ result $ \new -> modify' (\r -> r {removal_file = replace_data n new (removal_file r)})
        when own (clean_resources limits hidden_target resources)
      _ -> return ()

-- | The content without its hidden content, or Nothing
remove_hidden :: Limits -> (Value -> Bool) -> PdfFile -> Dict -> S.ByteString -> Either String (Maybe S.ByteString)
remove_hidden limits hidden_target f resources content = do
  ops <- contentOperations (limitDepth limits) content
  let properties = fromMaybe M.empty (look "Properties" resources >>= asDict f)
      xobjects = fromMaybe M.empty (look "XObject" resources >>= asDict f)
      hidden = Hidden
        { hiddenSequence = \operands -> case operands of
            [Name tag, Name n] | tag == key "OC" -> maybe False hidden_target (M.lookup n properties)
            [Name tag, d@(Dict _)] | tag == key "OC" -> hidden_target d
            _ -> False
        , hiddenXObject = \n -> case M.lookup n xobjects of
            Just (Ref x _) | Just (_, Stream d _) <- IM.lookup x (pdfObjects f) -> maybe False hidden_target (look "OC" d)
            _ -> False
        }
  removeHiddenContent hidden content ops

-- | The decoded data of a stream
decoded :: Limits -> PdfFile -> Int -> Remove (Int, S.ByteString)
decoded limits f n = case IM.lookup n (pdfObjects f) of
  Just (_, Stream d bytes) -> case decodeStream (limitStream limits) d bytes of
    Right s -> return (n, s)
    Left e -> lift (Left ("the content stream " ++ show n ++ " cannot be decoded: " ++ e))
  _ -> lift (Left ("the object " ++ show n ++ " is not a stream"))

-- | Replace the data of a stream, compressed with Flate
replace_data :: Int -> S.ByteString -> PdfFile -> PdfFile
replace_data n new f = f {pdfObjects = IM.adjust change n (pdfObjects f)}
  where
    compressed = L.toStrict (Z.compress (L.fromStrict new))
    change (gen, Stream d _) =
      let d' = M.insert (key "Filter") (Name (key "FlateDecode"))
             . M.insert (key "Length") (Integer (fromIntegral (S.length compressed)))
             . flip (foldr M.delete) (map key ["DecodeParms", "DL"])
             $ d
      in (gen, Stream d' compressed)
    change other = other

-- | Remove the entries of /Properties of the resources of an object that
-- refer to a hidden target
clean_properties :: (Value -> Bool) -> Int -> Remove ()
clean_properties hidden_target owner = do
  f <- gets removal_file
  forM_ (objectDict f owner >>= look "Resources") $ \resources_v -> do
    let clean_resources_dict g rd = case look "Properties" rd of
          Just props_v ->
            let (props_v', g') = modifyDict g props_v (M.filter (not . hidden_target))
            in (M.insert (key "Properties") props_v' rd, g')
          Nothing -> (rd, g)
    case resources_v of
      Ref r _ -> case objectDict f r of
        Just rd -> let (rd', g) = clean_resources_dict f rd in set_file (modifyObject r (const rd') g)
        Nothing -> return ()
      Dict rd -> let (rd', g) = clean_resources_dict f rd in set_file (modifyObject owner (M.insert (key "Resources") (Dict rd')) g)
      _ -> return ()

set_file :: PdfFile -> Remove ()
set_file g = modify' (\r -> r {removal_file = g})

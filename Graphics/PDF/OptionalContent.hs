{-# LANGUAGE FlexibleContexts #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Optional content (layers): content that the user, or the viewer, can
-- show or hide.
--
-- > grid <- createLayer "Grid" defaultLayerOptions
-- > draft <- createLayer "Draft" defaultLayerOptions { layerVisible = False, layerPrint = Just True }
-- > page <- addPage Nothing
-- > drawWithPage page $ do
-- >   inLayer grid $ stroke (Line 0 0 100 100)
-- >   inLayer draft $ drawText (text font 10 10 "Draft")
--
-- The second layer is not visible on the screen, and it is printed (see
-- Note [Layers set by usage]).
---------------------------------------------------------
module Graphics.PDF.OptionalContent
  ( -- * Layers
    Layer
  , createLayer
  , LayerOptions(..)
  , defaultLayerOptions
  , LayerLanguage(..)
    -- * Conditions on layers
  , LayerCondition
  , createLayerCondition
  , LayerExpr(..)
    -- * Content in a layer
  , OptionalContent
  , inLayer
  , LayerAnnotation
  , layerAnnotation
  , layerXObject
    -- * Panel and configurations
  , LayerItem(..)
  , setLayerPanel
  , addRadioGroup
  , LayerConfig(..)
  , layerConfig
  , addLayerConfig
  ) where

import Control.Monad (unless, when)
import Control.Monad.State (gets)
import Control.Monad.Writer (tell)
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List (nub)
import Data.Maybe (isJust)
import qualified Data.Text as T
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.LowLevel.Serializer
import Graphics.PDF.LowLevel.OptionalContent
import Graphics.PDF.Draw
import Graphics.PDF.Pages (addObject, updateObject)
import Graphics.PDF.Archive (pdfTextString)

-- Note [Optional content]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- A layer is an optional content group: an object with a name
-- (PDF 32000-1, 8.11.2).
--
--   12 0 obj << /Type /OCG /Name (Grid) >>
--
-- 'inLayer' puts content in a layer. The resources of the stream give a
-- name to the layer, and the content is a marked-content sequence with
-- this name:
--
--   /Resources << /Properties << /O7a 12 0 R >> >>
--   /OC /O7a BDC ... EMC
--
-- 'inLayer' takes a 'Draw' action and writes BDC before it and EMC after
-- it. Thus a sequence cannot cross a q/Q pair or a text object. It can
-- cut a path ('beginPath' ... 'strokePath'), which the standard forbids
-- (PDF 32000-1, 8.2 and 14.6). The path operators of Graphics.PDF.Shapes
-- record that a path is open ('pathOpen'), and a path that is open when
-- the sequence starts or ends is a problem of the document. A painting
-- operator, or the Q of 'withNewContext', ends the path. Only the
-- operators of Graphics.PDF.Shapes are known: a path that the caller
-- writes with other functions is not checked.
--
-- 'layerXObject' puts an XObject in one layer: a second call for the same
-- XObject is a problem. A condition puts it in more than one layer.
--
-- 'inLayer' does not apply to an annotation, which is not in the
-- content: 'layerAnnotation' writes /OC in the annotation.
--
-- An image or a form XObject is in a layer where it is drawn ('inLayer'
-- around 'drawXObject'), or everywhere: 'layerXObject' writes /OC in its
-- dictionary (PDF 32000-1, 8.11.3.3). The dictionary is written when the
-- document is saved, thus 'layerXObject' changes the pending stream.
--
-- A layer or a condition is an object number. One that comes from
-- another document is a problem of the document.
--
-- A condition ('createLayerCondition') is an optional content membership
-- dictionary (/OCMD). It is used as a layer is. See Note [Conditions on
-- layers].
--
-- The catalog lists the layers, and gives the default configuration (/D)
-- and the other configurations (/Configs):
--
--   /OCProperties << /OCGs [12 0 R 13 0 R]
--                    /D << /Name (Default) /OFF [13 0 R] /Order [12 0 R 13 0 R] >>
--                 >>
--
-- /Order is the layer panel of the viewer. When the document has no
-- panel ('setLayerPanel'), the panel has all the layers in the order of
-- their creation.
--
-- /Locked (PDF 1.6) and /VE (PDF 1.6) add /Version /1.6 to the catalog.
-- The header of the file stays at 1.5. A PDF/A document is already 1.7.

-- Note [Labels of the layer panel]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- In /Order, an array that comes immediately after a layer has the
-- children of this layer (PDF 32000-1, 8.11.4.3). A group with a label is
-- also an array, whose first element is a string. Thus two panels are
-- problems of the document:
--
-- * a group with a label immediately after a layer with no children:
--
--     [12 0 R [(Languages) 13 0 R 14 0 R]]
--
--   The group is the children of layer 12 (poppler and okular show it
--   so, and lose the label). Put the group before the layer, or after a
--   layer with children.
--
-- * a group with a label as the first item of a nested array: the first
--   child of a layer, or the first item of a group with a label:
--
--     [12 0 R [[(Languages) 13 0 R] 14 0 R]]
--     [[(Maps) [(Roads) 13 0 R] 14 0 R]]
--
--   poppler loses a part of the array: the first group, or the label of
--   the array and the layers of the first group.
--
-- A group with a label can be the first item of the panel, or come after
-- another group, or after a layer with children, also in a nested array:
--
--   [[(Languages) 13 0 R 14 0 R] 12 0 R [15 0 R [16 0 R] [(Maps) 17 0 R]]]

-- Note [Layers set by usage]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The usage dictionary of a layer (/Usage) tells why the layer exists:
--
--   /Zoom << /min 0.5 /max 2 >>         visible from 50 % to 200 % (not included)
--   /Print << /PrintState /ON >>        visible when printed
--   /Export << /ExportState /OFF >>     not visible when exported
--   /Language << /Lang (de-CH) /Preferred /ON >>
--
-- /Usage alone does nothing. The usage application dictionaries (/AS) of
-- a configuration tell the viewer which categories to apply for each
-- event (PDF 32000-1, 8.11.4.4). HPDF writes one dictionary for each
-- event:
--
--   << /Event /View /Category [/Zoom /Language] /OCGs [...] >>
--   << /Event /Print /Category [/Print] /OCGs [...] >>
--   << /Event /Export /Category [/Export] /OCGs [...] >>
--
-- A category is written only when a layer has it, and /OCGs has the
-- layers that have one of the categories. For a layer, the viewer uses
-- only the categories that the layer has, and the layer is visible when
-- all of them say so.
--
-- The language is not in the /Print event. Ghostscript shows each layer
-- of a /Language category, thus with it a printed document has all the
-- languages, one on the other.
--
-- The language rule (8.11.4.4): the viewer shows a layer whose /Lang is
-- the same as its own language. When no layer is the same, but a layer
-- agrees in part (en-US and en), the viewer shows the layers that agree
-- in part and whose /Preferred is /ON.
--
-- A zoom with a maximum equal to its minimum is a problem: the layer is
-- never visible.
--
-- The default configuration always applies the usage. Another
-- configuration applies it when 'configUsage' is true.
--
-- Acrobat applies all the categories. Ghostscript 10 applies /Print
-- (-dPrinted), and shows a layer that has /Zoom or /Language. poppler
-- applies none of them: it uses the states of /D.

-- Note [Conditions on layers]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A condition is an /OCMD. The four simple forms use /P (PDF 1.5):
--
--   LayerAnd [LayerOn a, LayerOn b]                     /P /AllOn
--   LayerOr [LayerOn a, LayerOn b]                      /P /AnyOn
--   LayerOr [LayerNot (LayerOn a), LayerNot (LayerOn b)]  /P /AnyOff
--   LayerAnd [LayerNot (LayerOn a), LayerNot (LayerOn b)] /P /AllOff
--
-- 'LayerOn' a is /P /AnyOn, and 'LayerNot' ('LayerOn' a) is /P /AllOff,
-- with one layer. Another condition is a visibility expression (/VE,
-- PDF 1.6):
--
--   LayerOr [LayerOn a, LayerAnd [LayerOn b, LayerNot (LayerOn c)]]
--   /VE [/Or 12 0 R [/And 13 0 R [/Not 14 0 R]]]
--
-- /OCGs always has all the layers of the condition. A PDF 1.5 reader uses
-- it with the default /P (/AnyOn) in place of /VE.
--
-- An 'LayerAnd' or an 'LayerOr' with no condition is a problem of the
-- document.

-- Note [Optional content in PDF/A]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ISO 19005-2, 6.9 permits optional content with these rules:
--
-- * each layer has a /Name: always written;
-- * each configuration has a /Name, and the names are unique: /D is
--   "Default", and the name of another configuration must be unique, not
--   empty, and not "Default" (also for documents that are not PDF/A);
-- * /Order, when it is present, has all the layers: a panel that does not
--   have all the layers is a problem;
-- * no configuration has /AS: a layer with a usage, or a configuration
--   with 'configUsage', is a problem, because the usage does nothing
--   without /AS.

-- | A layer of the document (an optional content group).
newtype Layer = Layer Int deriving (Eq, Ord, Show)

-- | A condition on layers (an optional content membership dictionary).
-- See Note [Conditions on layers]
newtype LayerCondition = LayerCondition Int deriving (Eq, Ord, Show)

-- | A layer, or a condition on layers.
class OptionalContent a where
  optional_content_number :: a -> Int

instance OptionalContent Layer where
  optional_content_number (Layer n) = n

instance OptionalContent LayerCondition where
  optional_content_number (LayerCondition n) = n

-- | Create a layer with a name. The name is shown in the layer panel of
-- the viewer.
createLayer :: Monad m => T.Text -> LayerOptions -> PDFT m Layer
createLayer name options = do
  mapM_ add_problem (zoomProblem name options)
  PDFReference n <- addObject (layer_dictionary name options)
  modify_oc $ \oc -> oc {ocLayers = (n, name, options) : ocLayers oc, ocKnown = IS.insert n (ocKnown oc)}
  return (Layer n)

-- | The OCG dictionary of a layer. See Note [Layers set by usage]
layer_dictionary :: T.Text -> LayerOptions -> PDFDictionary
layer_dictionary name o = dictFromList $
     [ entry "Type" (PDFName "OCG")
     , entry "Name" (pdfTextString name) ]
  ++ [ entry "Usage" (dictFromList usage) | not (null usage) ]
 where
  usage = concat
    [ [ entry "Zoom" (dictFromList (entry "min" lo : [ entry "max" hi | Just hi <- [mhi] ]))
      | Just (lo, mhi) <- [layerZoom o] ]
    , [ entry "Print" (dictFromList [entry "PrintState" (state p)]) | Just p <- [layerPrint o] ]
    , [ entry "Export" (dictFromList [entry "ExportState" (state e)]) | Just e <- [layerExport o] ]
    , [ entry "Language" (dictFromList [ entry "Lang" (pdfTextString (languageTag l))
                                       , entry "Preferred" (state (languagePreferred l)) ])
      | Just l <- [layerLanguage o] ]
    ]
  state b = PDFName (if b then "ON" else "OFF")

-- | Create a condition on layers. Content in the condition ('inLayer') is
-- visible when the condition is true. See Note [Conditions on layers]
createLayerCondition :: Monad m => LayerExpr Layer -> PDFT m LayerCondition
createLayerCondition expr = do
  let e = fmap optional_content_number expr
  when (has_empty_list e) $
    add_problem "a layer condition has a LayerAnd or a LayerOr with no condition"
  known <- gets (ocKnown . optionalContent)
  mapM_ (\n -> add_problem ("a layer condition uses a layer of another document (object " ++ show n ++ ")"))
    (nub [ n | n <- foldr (:) [] e, not (IS.member n known) ])
  let (policy, expression) = membership e
  when (isJust expression) $
    modify_oc $ \oc -> oc {ocVisibilityExpressions = True}
  PDFReference n <- addObject . dictFromList $
       [ entry "Type" (PDFName "OCMD")
       , entry "OCGs" (map ref (nub (foldr (:) [] e))) ]
    ++ [ entry "P" (PDFName p) | Just p <- [policy] ]
    ++ [ entry "VE" v | Just v <- [expression] ]
  modify_oc $ \oc -> oc {ocKnown = IS.insert n (ocKnown oc)}
  return (LayerCondition n)
 where
  has_empty_list x = case x of
    LayerOn _ -> False
    LayerNot y -> has_empty_list y
    LayerAnd ys -> null ys || any has_empty_list ys
    LayerOr ys -> null ys || any has_empty_list ys

-- | The /P policy or the /VE expression of a condition.
-- See Note [Conditions on layers]
membership :: LayerExpr Int -> (Maybe String, Maybe AnyPdfObject)
membership e = case conditionPolicy e of
  Just p -> (Just p, Nothing)
  Nothing -> (Nothing, Just (visibility e))
 where
  visibility x = case x of
    LayerOn n -> AnyPdfObject (ref n)
    LayerNot y -> AnyPdfObject [AnyPdfObject (PDFName "Not"), visibility y]
    LayerAnd ys -> AnyPdfObject (AnyPdfObject (PDFName "And") : map visibility ys)
    LayerOr ys -> AnyPdfObject (AnyPdfObject (PDFName "Or") : map visibility ys)

-- | Draw in a layer, or in a condition on layers: the content is visible
-- when the layer is visible, or when the condition is true. Calls can be
-- nested: the content is then visible when all the layers are visible.
--
-- A layer does not save the graphics state: a colour that the content
-- sets stays after it, also when the layer is hidden. Use
-- 'withNewContext' in the layer to save it. A layer that cuts a path
-- ('beginPath' ... 'strokePath') is a problem of the document. Use
-- 'layerAnnotation' for an annotation.
--
-- An image or a form XObject goes in a layer with 'inLayer' around
-- 'drawXObject', or everywhere with 'layerXObject'. See Note [Optional content]
inLayer :: OptionalContent a => a -> Draw b -> Draw b
inLayer o m = do
  name <- registerResource "Properties" layerNames (\names s -> s {layerNames = names})
            (OptionalContentRef (optional_content_number o))
  open_before <- gets pathOpen
  tell . mconcat $ [serialize "\n/OC /", serialize name, serialize " BDC"]
  a <- m
  open_after <- gets pathOpen
  tell . serialize $ "\nEMC"
  -- See Note [Optional content]
  when (open_before || open_after) $
    modifyStrict $ \s -> s {streamProblems =
      ("a layer (inLayer) starts or ends while a path is open: the layer cuts the path, "
       ++ "or the path is not painted. The painting operator of a path must be in the same layer as the path")
      : streamProblems s}
  return a

-- | An annotation in a layer, or in a condition on layers. Use it with
-- 'Graphics.PDF.Annotation.newAnnotation'.
data LayerAnnotation a = LayerAnnotation !Int a

-- | Put an image or a form XObject in a layer, or in a condition on
-- layers: each time that it is drawn, it is visible when the layer is
-- visible. This writes /OC in its dictionary, and the content that draws
-- it does not change. See Note [Optional content]
layerXObject :: (Monad m, OptionalContent o, PDFXObject a) => o -> PDFReference a -> PDFT m ()
layerXObject o (PDFReference r) = do
  let n = optional_content_number o
  known <- gets (ocKnown . optionalContent)
  unless (IS.member n known) $
    add_problem ("an XObject is in a layer or a layer condition of another document (object " ++ show n ++ ")")
  pending <- gets (IM.lookup r . streams)
  case pending of
    -- a stream with no page: an XObject, whose dictionary is otherRsrcs
    Just (Nothing, (st, w))
      | pdfDictMember (PDFName "OC") (otherRsrcs st) ->
          add_problem ("the XObject " ++ show r ++ " is already in a layer: use a layer condition "
                       ++ "(createLayerCondition) to put it in more than one layer")
      | otherwise ->
          let st' = st {otherRsrcs = insertInPdfDict (PDFName "OC") (AnyPdfObject (ref n)) (otherRsrcs st)}
          in modifyStrict $ \s -> s {streams = IM.insert r (Nothing, (st', w)) (streams s)}
    _ -> add_problem ("the object " ++ show r ++ " is not an XObject of this document: it cannot be put in a layer")

-- | Put an annotation in a layer, or in a condition on layers.
layerAnnotation :: (OptionalContent o, AnnotationObject a) => o -> a -> LayerAnnotation a
layerAnnotation o = LayerAnnotation (optional_content_number o)

instance AnnotationObject a => PdfObject (LayerAnnotation a) where
  toPDF = toPDF . dictFromList . annotationDict

instance AnnotationObject a => PdfLengthInfo (LayerAnnotation a) where

instance AnnotationObject a => AnnotationObject (LayerAnnotation a) where
  -- The annotation adds its object, with the other effects that it needs.
  -- Then the object gets the dictionary with /OC.
  addAnnotation annotation@(LayerAnnotation n a) = do
    known <- gets (ocKnown . optionalContent)
    unless (IS.member n known) $
      add_problem ("an annotation is in a layer or a layer condition of another document (object " ++ show n ++ ")")
    PDFReference r <- addAnnotation a
    updateObject (PDFReference r) annotation
    return (PDFReference r)
  annotationType (LayerAnnotation _ a) = annotationType a
  annotationContent (LayerAnnotation _ a) = annotationContent a
  annotationRect (LayerAnnotation _ a) = annotationRect a
  annotationDict (LayerAnnotation n a) =
    filter ((/= PDFName "OC") . fst) (annotationDict a) ++ [entry "OC" (ref n)]
  annotationToGlobalCoordinates (LayerAnnotation n a) =
    LayerAnnotation n <$> annotationToGlobalCoordinates a

-- | Set the layer panel of the viewer. Without it, the panel has all the
-- layers in the order of their creation.
setLayerPanel :: Monad m => [LayerItem Layer] -> PDFT m ()
setLayerPanel items = modify_oc $ \oc -> oc {ocPanel = Just (map (fmap optional_content_number) items)}

-- | Add a radio group: when the user shows a layer of the group, the
-- viewer hides the other layers of the group. One layer at most of the
-- group can be visible when the document opens.
addRadioGroup :: Monad m => [Layer] -> PDFT m ()
addRadioGroup ls = modify_oc $ \oc -> oc {ocRadioGroups = map optional_content_number ls : ocRadioGroups oc}

-- | Add an alternative configuration of the layers.
addLayerConfig :: Monad m => LayerConfig Layer -> PDFT m ()
addLayerConfig c = modify_oc $ \oc -> oc {ocConfigs = fmap optional_content_number c : ocConfigs oc}

modify_oc :: Monad m => (OptionalContentState -> OptionalContentState) -> PDFT m ()
modify_oc f = modifyStrict $ \s -> s {optionalContent = f (optionalContent s)}

add_problem :: Monad m => String -> PDFT m ()
add_problem p = modifyStrict $ \s -> s {documentProblems = documentProblems s ++ [p]}

ref :: Int -> PDFReference PDFDictionary
ref = PDFReference

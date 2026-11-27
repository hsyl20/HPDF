{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- The types of optional content (layers). The type parameter @l@ is the
-- type of a layer: 'Graphics.PDF.OptionalContent.Layer' for the users of
-- the builder, an object number in the state of the builder.
---------------------------------------------------------
module Graphics.PDF.LowLevel.OptionalContent
  ( LayerOptions(..)
  , defaultLayerOptions
  , LayerLanguage(..)
  , LayerItem(..)
  , LayerExpr(..)
  , LayerConfig(..)
  , layerConfig
  , layerHasUsage
  , zoomProblem
  , usageApplications
  , conditionPolicy
  , panelProblems
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- | The options of a layer.
--
-- The usage options (zoom, print, export, language) tell a viewer to
-- change the state of the layer by itself. See Note [Layers set by usage]
-- in Graphics.PDF.OptionalContent.
data LayerOptions = LayerOptions
  { layerVisible :: !Bool
    -- ^ The layer is visible when the document opens
  , layerLocked :: !Bool
    -- ^ The user cannot change the state of the layer (PDF 1.6)
  , layerZoom :: !(Maybe (Double, Maybe Double))
    -- ^ The layer is visible from a minimum magnification, up to a
    -- maximum magnification that is not included ('Nothing': no maximum).
    -- 1 is 100 %.
  , layerPrint :: !(Maybe Bool)
    -- ^ The layer is visible, or not, when the document is printed
  , layerExport :: !(Maybe Bool)
    -- ^ The layer is visible, or not, when the document is exported to
    -- another format
  , layerLanguage :: !(Maybe LayerLanguage)
    -- ^ The language of the content of the layer
  } deriving (Eq, Show)

-- | A layer that is visible, not locked, and that has no usage.
defaultLayerOptions :: LayerOptions
defaultLayerOptions = LayerOptions
  { layerVisible = True
  , layerLocked = False
  , layerZoom = Nothing
  , layerPrint = Nothing
  , layerExport = Nothing
  , layerLanguage = Nothing
  }

-- | The language of a layer. The viewer shows the layers whose language is
-- the same as its own language. When no layer has the same language, it
-- shows the preferred layers whose language agrees in part with its
-- language (@en@ and @en-US@).
data LayerLanguage = LayerLanguage
  { languageTag :: !T.Text
    -- ^ A language tag (BCP 47), for example @de-CH@
  , languagePreferred :: !Bool
    -- ^ The layer is visible when its language agrees in part with the
    -- language of the viewer, and no layer has the same language
  } deriving (Eq, Show)

-- | An item of the layer panel of a viewer (@/Order@).
data LayerItem l
  = LayerEntry l [LayerItem l]
    -- ^ A layer, with the items under it
  | LayerLabel T.Text [LayerItem l]
    -- ^ A label that is not a layer, with the items under it. An empty
    -- label is a group with no label
  deriving (Eq, Show, Functor, Foldable)

-- | A condition on the states of layers.
data LayerExpr l
  = LayerOn l
    -- ^ The layer is visible
  | LayerNot (LayerExpr l)
    -- ^ The condition is false
  | LayerAnd [LayerExpr l]
    -- ^ All the conditions are true (one condition at least)
  | LayerOr [LayerExpr l]
    -- ^ One of the conditions is true (one condition at least)
  deriving (Eq, Show, Functor, Foldable)

-- | An alternative configuration of the layers (@/Configs@). A viewer can
-- show the document with it in place of the default configuration.
data LayerConfig l = LayerConfig
  { configName :: !T.Text
    -- ^ The name of the configuration, which must be unique
  , configVisible :: [l]
    -- ^ The layers that are visible. The other layers are not visible.
  , configLocked :: [l]
    -- ^ The layers that the user cannot change (PDF 1.6)
  , configPanel :: Maybe [LayerItem l]
    -- ^ The layer panel ('Nothing': the panel of the default configuration)
  , configRadioGroups :: [[l]]
    -- ^ The groups of layers in which one layer at most is visible
  , configUsage :: !Bool
    -- ^ The viewer changes the states of the layers from their usage
    -- (zoom, print, export, language)
  } deriving (Eq, Show, Functor)

-- | A configuration with a name, no visible layer, no locked layer, the
-- default panel, no radio group, and no usage.
layerConfig :: T.Text -> LayerConfig l
layerConfig name = LayerConfig
  { configName = name
  , configVisible = []
  , configLocked = []
  , configPanel = Nothing
  , configRadioGroups = []
  , configUsage = False
  }

-- | Whether the options have a usage (zoom, print, export, language).
layerHasUsage :: LayerOptions -> Bool
layerHasUsage o = or
  [ maybe False (const True) (layerZoom o)
  , maybe False (const True) (layerPrint o)
  , maybe False (const True) (layerExport o)
  , maybe False (const True) (layerLanguage o)
  ]

-- | The problem of the zoom of a layer, if it has one: a minimum below 0,
-- or a maximum that is not above the minimum (the layer is visible up to
-- the maximum, which is not included).
zoomProblem :: T.Text -> LayerOptions -> Maybe String
zoomProblem name o = case layerZoom o of
  Just (lo, hi)
    | not (finite lo) || lo < 0 || maybe False (\h -> not (finite h) || h <= lo) hi ->
        Just ( "the layer " ++ show name ++ " has a zoom from " ++ show lo ++ " to "
               ++ maybe "no maximum" show hi
               ++ "; the minimum must be 0 or more, and the maximum must be above the minimum" )
  _ -> Nothing
 where
  finite x = not (isNaN x || isInfinite x)

-- | The usage application dictionaries (/AS) for the usage of these
-- layers: the event, the categories, and the layers of each dictionary.
-- See Note [Layers set by usage] in Graphics.PDF.OptionalContent
usageApplications :: [(l, LayerOptions)] -> [(String, [String], [l])]
usageApplications layers =
  [ (event, map fst used, [ l | (l, o) <- layers, any (\(_, has) -> has o) used ])
  | (event, categories) <- events
  , let used = [ c | c@(_, has) <- categories, any (has . snd) layers ]
  , not (null used) ]
 where
  events =
    [ ("View", [("Zoom", isJust' . layerZoom), ("Language", isJust' . layerLanguage)])
    , ("Print", [("Print", isJust' . layerPrint)])
    , ("Export", [("Export", isJust' . layerExport)])
    ]
  isJust' :: Maybe a -> Bool
  isJust' = maybe False (const True)

-- | The /P policy of a condition when the condition has one of its forms,
-- or Nothing when it needs a visibility expression (/VE).
-- See Note [Conditions on layers] in Graphics.PDF.OptionalContent
conditionPolicy :: LayerExpr l -> Maybe String
conditionPolicy e = case e of
  LayerOn _ -> Just "AnyOn"
  LayerNot (LayerOn _) -> Just "AllOff"
  LayerAnd xs | not (null xs), all is_on xs -> Just "AllOn"
  LayerOr xs | not (null xs), all is_on xs -> Just "AnyOn"
  LayerOr xs | not (null xs), all is_off xs -> Just "AnyOff"
  LayerAnd xs | not (null xs), all is_off xs -> Just "AllOff"
  _ -> Nothing
 where
  is_on (LayerOn _) = True
  is_on _ = False
  is_off (LayerNot (LayerOn _)) = True
  is_off _ = False

-- | The problems of a layer panel: a layer two times, and the groups that
-- viewers do not show as they are written. The arguments are the name of
-- a layer and the name of the panel.
-- See Note [Labels of the layer panel] in Graphics.PDF.OptionalContent
panelProblems :: Ord l => (l -> String) -> String -> [LayerItem l] -> [String]
panelProblems layer_name what panel =
     [ what ++ " has the layer " ++ layer_name n ++ " more than one time"
     | n <- M.keys (M.filter (> (1 :: Int)) (M.fromListWith (+) [ (x, 1) | x <- concatMap (foldr (:) []) panel ])) ]
  ++ label_problems panel
 where
  group t = if T.null t then "a group with no label" else "the label " ++ show t
  label_problems items = concat
    [ [ what ++ " has " ++ group t ++ " immediately after the layer " ++ layer_name n
        ++ ", which has no children: a viewer shows the group as the children of the layer"
      | (LayerEntry n [], LayerLabel t _) <- zip items (drop 1 items) ]
    , [ what ++ " has " ++ group t ++ " as the first child of the layer " ++ layer_name n
        ++ ": poppler does not show the group"
      | LayerEntry n (LayerLabel t _ : _) <- items ]
    , [ what ++ " has " ++ group t ++ " as the first item of the group " ++ show l
        ++ ": poppler does not show the group " ++ show l
      | LayerLabel l (LayerLabel t _ : _) <- items ]
    , concatMap label_problems [ kids | LayerEntry _ kids <- items ]
    , concatMap label_problems [ kids | LayerLabel _ kids <- items ]
    ]

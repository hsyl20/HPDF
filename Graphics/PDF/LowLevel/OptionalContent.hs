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
  ) where

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
    -- ^ A label that is not a layer, with the items under it
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

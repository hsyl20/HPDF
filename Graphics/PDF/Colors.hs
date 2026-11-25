---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Colors for a PDF document
---------------------------------------------------------
module Graphics.PDF.Colors(
  -- * Colors
  -- ** Types
    Color(..)
  -- ** Functions
  , setRGBColorSpace
  , fillColor
  , strokeColor
  , setStrokeAlpha
  , setFillAlpha
  , hsvToRgb
  -- ** Some colors
  , black
  , white
  , red
  , blue
  , green
 ) where
     
import Graphics.PDF.Draw
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Resources
import Control.Monad.Writer
import Graphics.PDF.LowLevel.Serializer
import Data.Binary.Builder (Builder)
            
black :: Color
black = Rgb 0 0 0  

white :: Color
white = Rgb 1 1 1

red :: Color
red = Rgb 1 0 0

green :: Color
green = Rgb 0 1 0

blue :: Color
blue = Rgb 0 0 1
            

       
-- | Set alpha value for transparency
setStrokeAlpha :: Double -> Draw ()
setStrokeAlpha alpha = do
    newName <-
        registerResource "ExtGState"
            strokeAlphas (\newMap s -> s { strokeAlphas = newMap })
            (StrokeAlpha alpha)
    tell . mconcat $ [ serialize "\n/"
                     , serialize newName
                     , serialize " gs"
                     ]

-- | Set alpha value for transparency
setFillAlpha :: Double -> Draw ()
setFillAlpha alpha = do
    newName <-
        registerResource "ExtGState"
            fillAlphas (\newMap s -> s { fillAlphas = newMap })
            (FillAlpha alpha)
    tell . mconcat $ [ serialize "\n/"
                     , serialize newName
                     , serialize " gs"
                     ]
    
-- | Init the PDF color space to RGB.
setRGBColorSpace :: Draw ()
setRGBColorSpace = tell . serialize $ "\n/DeviceRGB CS\n/DeviceRGB cs\n"



-- | Select the filling color
fillColor :: MonadPath m => Color -- ^ Filling color
          -> m ()
fillColor = colour_operator "rg" "g" "k"

-- | Select the drawing color
strokeColor :: MonadPath m => Color -- ^ Drawing color
            -> m ()
strokeColor = colour_operator "RG" "G" "K"

-- | Write a colour with the operator of its device colour space.
-- See Note [Device colours] in Graphics.PDF.Draw
colour_operator :: MonadPath m => String -- ^ Operator for an RGB colour
                -> String -- ^ Operator for a gray
                -> String -- ^ Operator for a CMYK colour
                -> Color
                -> m ()
colour_operator rgb gray cmyk colour = case colour of
    Rgb r g b -> tell (operator [r, g, b] rgb)
    Hsv h s v -> let (r, g, b) = hsvToRgb (h, s, v) in tell (operator [r, g, b] rgb)
    Gray g -> tell (operator [g] gray)
    Cmyk c m y k -> recordCmykColour >> tell (operator [c, m, y, k] cmyk)
  where
    operator :: [PDFFloat] -> String -> Builder
    operator components name = mconcat $
      serialize "\n" : concatMap (\x -> [toPDF x, serialize ' ']) components ++ [serialize name]

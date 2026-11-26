---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- PDF Patterns
---------------------------------------------------------

module Graphics.PDF.Pattern(
   -- * Pattern
    TilingType(..)
  , PDFColoredPattern
  , PDFUncoloredPattern
  , createColoredTiling
  , createUncoloredTiling
  , setColoredFillPattern
  , setColoredStrokePattern
  , setUncoloredFillPattern
  , setUncoloredStrokePattern
 ) where

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Resources
import Graphics.PDF.Pages(recordBound,createContent)
import Control.Monad.State
import Control.Monad.Writer
import Graphics.PDF.LowLevel.Serializer

data PaintType = ColoredTiling
               | UncoloredTiling
               deriving(Eq,Enum)
               
-- | Tiling type
data TilingType = ConstantSpacing
                | NoDistortion
                | ConstantSpacingAndFaster
                deriving(Eq,Enum)
  
-- | Create a colored tiling pattern
createColoredTiling :: Monad m => PDFFloat -- ^ Left
                    -> PDFFloat -- ^ Bottom
                    -> PDFFloat -- ^ Right
                    -> PDFFloat -- ^ Top
                    -> PDFFloat -- ^ Horizontal step
                    -> PDFFloat -- ^ Vertical step
                    -> TilingType
                    -> Draw a -- ^ Drawing commands
                    -> PDFT m (PDFReference PDFColoredPattern)
createColoredTiling  xa ya xb yb hstep vstep  tt d =  createTilingPattern xa ya xb yb hstep vstep ColoredTiling tt d  >>= return . PDFReference
 
-- | Create an uncolored tiling pattern
createUncoloredTiling :: Monad m => PDFFloat -- ^ Left
                      -> PDFFloat -- ^ Bottom
                      -> PDFFloat -- ^ Right
                      -> PDFFloat -- ^ Top
                      -> PDFFloat -- ^ Horizontal step
                      -> PDFFloat -- ^ Vertical step
                      -> TilingType
                      -> Draw a -- ^ Drawing commands
                      -> PDFT m (PDFReference PDFUncoloredPattern)
createUncoloredTiling  xa ya xb yb hstep vstep  tt d =  createTilingPattern xa ya xb yb hstep vstep UncoloredTiling tt d >>= return . PDFReference  

-- | Create a PDF tiling pattern
createTilingPattern :: Monad m => PDFFloat -- ^ Left
                    -> PDFFloat -- ^ Bottom
                    -> PDFFloat -- ^ Right
                    -> PDFFloat -- ^ Top
                    -> PDFFloat -- ^ Horizontal step
                    -> PDFFloat -- ^ Vertical step
                    -> PaintType
                    -> TilingType
                    -> Draw a -- ^ Drawing commands
                    -> PDFT m Int
createTilingPattern xa ya xb yb hstep vstep pt tt d = 
    let a' = do modifyStrict $ \s -> s  {otherRsrcs = dictFromList $
                                             [ entry "Type" (PDFName $ "Pattern")
                                             , entry "PatternType" (PDFInteger $ 1)
                                             , entry "PaintType" (PDFInteger $ (fromEnum pt) + 1)
                                             , entry "TilingType" (PDFInteger $ (fromEnum tt) + 1)
                                             , entry "Matrix" (map PDFInteger $ [1,0,0,1,0,0])
                                             , entry "BBox" [xa,ya,xb,yb]
                                             , entry "XStep" hstep
                                             , entry "YStep" vstep
                                             ]
                                         }
                d
   in do
       PDFReference s <- createContent a' Nothing  
       recordBound s (xb-xa) (yb-ya)
       return s


registerPattern :: PDFReference s -> Draw String
registerPattern (PDFReference a) =
    registerResource "Pattern"
        patterns (\newMap s -> s { patterns = newMap })
        (PDFReference a)

-- | Set the fill pattern
setColoredFillPattern :: PDFReference PDFColoredPattern -> Draw ()
setColoredFillPattern ref = do
    newName <- registerPattern ref
    tell . serialize $ ("\n/Pattern cs")
    tell . mconcat $ [ serialize "\n/"
                     , serialize newName
                     , serialize " scn"
                     ]
     
-- | Set the stroke pattern
setColoredStrokePattern :: PDFReference PDFColoredPattern -> Draw ()
setColoredStrokePattern ref = do
    newName <- registerPattern ref
    tell . serialize $ ("\n/Pattern CS")
    tell . mconcat $
                  [ serialize "\n/"
                  , serialize newName
                  , serialize " SCN"
                  ]
  
  

-- | Set the fill pattern. The pattern is painted with the colour, in the
-- device colour space of the colour.
setUncoloredFillPattern :: PDFReference PDFUncoloredPattern -> Color -> Draw ()
setUncoloredFillPattern = set_uncoloured_pattern " cs" " scn"

-- | Set the stroke pattern. The pattern is painted with the colour, in the
-- device colour space of the colour.
setUncoloredStrokePattern :: PDFReference PDFUncoloredPattern -> Color -> Draw ()
setUncoloredStrokePattern = set_uncoloured_pattern " CS" " SCN"

-- | Select the colour space of an uncoloured pattern, then the colour and
-- the pattern:
--
-- > /Oa cs                 the space [/Pattern /DeviceCMYK]
-- > 0 1 1 0  /Ob scn       the colour and the pattern
--
-- See Note [Device colours] in Graphics.PDF.Draw
set_uncoloured_pattern :: String -- ^ Operator that selects the colour space
                       -> String -- ^ Operator that selects the colour and the pattern
                       -> PDFReference PDFUncoloredPattern
                       -> Color
                       -> Draw ()
set_uncoloured_pattern space_operator colour_operator ref col = do
    let (space, components) = case col of
          Gray g -> (PatternGray, [g])
          Cmyk c m y k -> (PatternCMYK, [c, m, y, k])
          _ -> let (r, g, b) = getRgbColor col in (PatternRGB, [r, g, b])
    colorMap <- gets colorSpaces
    (newColorName,_) <- setResource "ColorSpace" space colorMap
    newName <- registerPattern ref
    tell . mconcat $ [ serialize "\n/"
                     , serialize newColorName
                     , serialize space_operator
                     ]
    tell . mconcat $ serialize '\n' : concatMap (\x -> [toPDF x, serialize ' ']) components
                     ++ [ serialize " /"
                        , serialize newName
                        , serialize colour_operator
                        ]

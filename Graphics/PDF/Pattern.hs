---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
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
createColoredTiling :: PDFFloat -- ^ Left
                    -> PDFFloat -- ^ Bottom
                    -> PDFFloat -- ^ Right
                    -> PDFFloat -- ^ Top
                    -> PDFFloat -- ^ Horizontal step
                    -> PDFFloat -- ^ Vertical step
                    -> TilingType
                    -> Draw a -- ^ Drawing commands
                    -> PDF (PDFReference PDFColoredPattern)
createColoredTiling  xa ya xb yb hstep vstep  tt d =  createTilingPattern xa ya xb yb hstep vstep ColoredTiling tt d  >>= return . PDFReference
 
-- | Create an uncolored tiling pattern
createUncoloredTiling :: PDFFloat -- ^ Left
                      -> PDFFloat -- ^ Bottom
                      -> PDFFloat -- ^ Right
                      -> PDFFloat -- ^ Top
                      -> PDFFloat -- ^ Horizontal step
                      -> PDFFloat -- ^ Vertical step
                      -> TilingType
                      -> Draw a -- ^ Drawing commands
                      -> PDF (PDFReference PDFUncoloredPattern)
createUncoloredTiling  xa ya xb yb hstep vstep  tt d =  createTilingPattern xa ya xb yb hstep vstep UncoloredTiling tt d >>= return . PDFReference  

-- | Create a PDF tiling pattern
createTilingPattern :: PDFFloat -- ^ Left
                    -> PDFFloat -- ^ Bottom
                    -> PDFFloat -- ^ Right
                    -> PDFFloat -- ^ Top
                    -> PDFFloat -- ^ Horizontal step
                    -> PDFFloat -- ^ Vertical step
                    -> PaintType
                    -> TilingType
                    -> Draw a -- ^ Drawing commands
                    -> PDF Int
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
  
  

-- | Set the fill pattern
setUncoloredFillPattern :: PDFReference PDFUncoloredPattern -> Color -> Draw ()
setUncoloredFillPattern ref col = do
       let (r,g,b) = getRgbColor col
       colorMap <- gets colorSpaces
       (newColorName,_) <- setResource "ColorSpace" PatternRGB colorMap
       newName <- registerPattern ref
       tell . mconcat $[ serialize "\n/"
                       , serialize newColorName
                       , serialize " cs"
                       ]
       tell . mconcat $[ serialize '\n'
                       , toPDF r
                       , serialize ' '
                       , toPDF g
                       , serialize ' '
                       , toPDF b
                       , serialize ' '
                       , serialize " /"
                       , serialize newName
                       , serialize " scn"
                       ]

-- | Set the stroke pattern
setUncoloredStrokePattern :: PDFReference PDFUncoloredPattern -> Color -> Draw ()
setUncoloredStrokePattern ref col = do
    let (r,g,b) = getRgbColor col
    colorMap <- gets colorSpaces
    (newColorName,_) <- setResource "ColorSpace" PatternRGB colorMap
    newName <- registerPattern ref
    tell . mconcat $[ serialize "\n/" 
                    , serialize newColorName
                    , serialize " CS"
                    ]
    tell . mconcat $   [ serialize '\n'
                       , toPDF r
                       , serialize ' '
                       , toPDF g
                       , serialize ' '
                       , toPDF b
                       , serialize ' '
                       , serialize " /"
                       , serialize newName
                       , serialize " SCN"
                       ]
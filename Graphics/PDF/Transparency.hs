---------------------------------------------------------
-- |
-- Copyright   : (c) 2023, haskell@henning-thielemann.de
-- License     : BSD-style
--
-- Maintainer  : haskell@henning-thielemann.de
-- Stability   : experimental
-- Portability : portable
--
-- PDF transparency
---------------------------------------------------------
module Graphics.PDF.Transparency(
  -- * Transparency
  SoftMask,
  createSoftMask,
  createTransparencyGroup,
  paintWithTransparency,
  ) where

import qualified Graphics.PDF.Draw as Draw
import Graphics.PDF.Document (createPDFXFormExtra)
import Graphics.PDF.Draw (PDF, Draw, SoftMask(SoftMask))
import Graphics.PDF.Shapes (Rectangle)
import Graphics.PDF.LowLevel.Serializer (serialize)
import Graphics.PDF.LowLevel.Types

import Control.Monad.Writer (tell)
import Control.Monad (void)



createSoftMask ::
       Rectangle -- ^ Bounding box
    -> Draw a -- ^ Content of the soft mask
    -> PDF SoftMask
createSoftMask bbox =
    fmap SoftMask .
    createTransparencyGroup Draw.GraySpace bbox

createTransparencyGroup ::
       Draw.ColorSpace a e
    -> Rectangle -- ^ Bounding box
    -> Draw b -- ^ Painting
    -> PDF (PDFReference Draw.PDFXForm)
createTransparencyGroup space bbox img =
    createPDFXFormExtra bbox img $
        dictFromList $
            entry "Group" (dictFromList $
                entry "Type" (PDFName "Group") :
                entry "S" (PDFName "Transparency") :
                entry "I" True :
                entry "CS" (Draw.colorSpaceName space) :
                []) :
            []

{- |
If the Draw Monad paints overlapping geometric primitives or text,
the result will certainly not be what you want.
Text ignores soft masks.
Each primitive other than text is painted with the soft mask
over the previous geometric objects.
It is very likely, that in this case you want
to generate a transparency group for your drawing.
-}
paintWithTransparency ::
       SoftMask -- ^ Soft mask
    -> Draw a -- ^ Shape to paint
    -> Draw ()
paintWithTransparency softMask d =
    Draw.withNewContext $ do
        newName <-
            Draw.registerResource "ExtGState"
                Draw.softMasks (\newMap s -> s { Draw.softMasks = newMap })
                softMask
        tell . mconcat $
            [ serialize "\n/"
            , serialize newName
            , serialize " gs"
            ]
        void d

{-
https://github.com/pdf-association/pdf20examples/issues/9
-}

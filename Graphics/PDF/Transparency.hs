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
  paintWithTransparency,
  ) where

import qualified Graphics.PDF.Draw as Draw
import Graphics.PDF.Document (createPDFXFormExtra)
import Graphics.PDF.Draw (PDF, Draw)
import Graphics.PDF.Resources (addResource)
import Graphics.PDF.Shapes (Rectangle)
import Graphics.PDF.LowLevel.Serializer (serialize)
import Graphics.PDF.LowLevel.Types

import Control.Monad.Writer (tell)
import Control.Monad (void)


type SoftMask = PDFDictionary

createSoftMask ::
       Rectangle -- ^ Bounding box
    -> Draw a -- ^ Content of the soft mask
    -> PDF SoftMask
createSoftMask bbox mask = do
    ref <-
        createPDFXFormExtra bbox mask $
            dictFromList $
                entry "Group" (dictFromList $
                    entry "Type" (PDFName "Group") :
                    entry "S" (PDFName "Transparency") :
                    entry "I" True :
                    entry "CS" (PDFName "DeviceGray") :
                    []) :
                []

    return $
        dictFromList $
            entry "Type" (PDFName "ExtGState") :
            entry "SMask"
                (dictFromList $
                    entry "Type" (PDFName "Mask") :
                    entry "S" (PDFName "Luminosity") :
                    entry "G" ref :
                    []) :
            []

paintWithTransparency ::
       SoftMask -- ^ Soft mask
    -> Draw a -- ^ Shape to paint
    -> Draw ()
paintWithTransparency extGState d =
    Draw.withNewContext $ do
        newName <- Draw.supplyName
        modifyStrict $ \s -> s {
            Draw.rsrc =
                addResource
                    (PDFName "ExtGState") (PDFName newName)
                    (AnyPdfObject extGState) (Draw.rsrc s)
            }
        tell . mconcat $
            [ serialize "\n/"
            , serialize newName
            , serialize " gs"
            ]
        void d

{-
https://github.com/pdf-association/pdf20examples/issues/9
-}

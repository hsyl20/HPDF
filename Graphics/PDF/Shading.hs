---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF shading
---------------------------------------------------------
module Graphics.PDF.Shading(
  -- * Shading
  -- ** Type
    PDFShading(..)
  , paintWithShading
  , applyShading
  , createFunction1Object
  , createFunction2Object
 ) where

import qualified Graphics.PDF.Expression as Expr
import Graphics.PDF.Draw as Draw
import Graphics.PDF.Pages(addObject)
import Graphics.PDF.Shapes(setAsClipPath)
import Graphics.PDF.Expression(PDFExpression)
import Graphics.PDF.LowLevel.Serializer(serialize)
import Graphics.PDF.LowLevel.Types(PDFFloat)

import qualified Data.Array as Array

import Control.Monad.Writer


type ExprFloat = PDFExpression PDFFloat

createFunction1Object ::
    (ColorTuple a, Expr.Result e) =>
    Function1 a e -> PDF (FunctionObject (PDFFloat -> a) (ExprFloat -> e))
createFunction1Object func =
    let domain = domain1Dict func in
    case func of
        Calculator1 f ->
            fmap FunctionStream $ addObject $
            Draw.rsrcFromCalculator domain f
        Interpolated1 n x y ->
            fmap FunctionObject $ addObject $
            Draw.rsrcFromInterpolated domain n x y
        Sampled1 arr ->
            fmap FunctionStream $ addObject $
            Draw.rsrcFromSampled domain (\bnds -> [Array.rangeSize bnds]) arr

createFunction2Object ::
    (ColorTuple a, Expr.Result e) =>
    Function2 a e ->
    PDF (FunctionObject
            (PDFFloat -> PDFFloat -> a) (ExprFloat -> ExprFloat -> e))
createFunction2Object func =
    let domain = domain2Dict func in
    fmap FunctionStream $ addObject $
    case func of
        Calculator2 f -> rsrcFromCalculator domain f
        Sampled2 arr ->
            rsrcFromSampled
                domain
                (\((lx,ly), (ux,uy)) ->
                    [Array.rangeSize (lx,ux), Array.rangeSize (ly,uy)])
                arr


-- | Fill clipping region with a shading
applyShading :: PDFShading -> Draw ()
applyShading shade = do
    newName <-
        registerResource "Shading"
            shadings (\newMap s -> s { shadings = newMap })
            shade
    tell . mconcat $[ serialize "\n/" 
                    , serialize newName
                    , serialize " sh"
                    ]
    
paintWithShading :: PDFShading -- ^ Shading
                 -> Draw a -- ^ Shape to paint
                 -> Draw ()
paintWithShading shade d = do
    withNewContext $ do
      _ <- d
      setAsClipPath
      applyShading shade
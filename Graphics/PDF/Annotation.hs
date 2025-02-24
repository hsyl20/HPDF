---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Annotations
---------------------------------------------------------

module Graphics.PDF.Annotation(
   -- * Annotations
   -- ** Types
     TextAnnotation(..)
   , URLLink(..)
   , PDFLink(..)
   , TextIcon(..)
   -- ** Functions
   , newAnnotation
   , toAsciiString
 ) where

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Action
import Graphics.PDF.Pages
import Control.Monad.State(gets)
import qualified Data.Text as T
import Network.URI 

--import Debug.Trace

data TextIcon = Note
              | Paragraph
              | NewParagraph
              | Key
              | Comment
              | Help
              | Insert
              deriving(Eq,Show)
    
                  
data TextAnnotation = TextAnnotation 
   T.Text -- Content
   [PDFFloat] -- Rect
   TextIcon
data URLLink = URLLink 
  T.Text -- Content
  [PDFFloat] -- Rect
  URI -- URL
  Bool -- Border
data PDFLink = PDFLink 
  T.Text -- Content
  [PDFFloat] -- Rect
  (PDFReference PDFPage) -- Page
  PDFFloat -- x
  PDFFloat -- y
  Bool -- Border
--data Screen = Screen (PDFReference Rendition) PDFString [PDFFloat] (PDFReference PDFPage) (Maybe (PDFReference ControlMedia)) (Maybe (PDFReference ControlMedia)) 

--det :: Matrix -> PDFFloat
--det (Matrix a b c d _ _) = a*d - b*c
--
--inverse :: Matrix -> Matrix
--inverse m@(Matrix a b c d e f) = (Matrix (d/de) (-b/de) (-c/de) (a/de) 0 0) * (Matrix 1 0 0 1 (-e) (-f))
--      where
--         de = det m

applyMatrixToRectangle :: Matrix -> [PDFFloat] -> [PDFFloat]
applyMatrixToRectangle m [xa,ya,xb,yb] = 
    let (xa',ya') = m `applyTo` (xa,ya)
        (xa'',yb') = m `applyTo` (xa,yb)
        (xb',ya'') = m `applyTo` (xb,ya)
        (xb'',yb'') = m `applyTo` (xb,yb)
        x1 = minimum [xa',xa'',xb',xb'']
        x2 = maximum [xa',xa'',xb',xb'']
        y1 = minimum [ya',ya'',yb',yb'']
        y2 = maximum [ya',ya'',yb',yb'']
    in
    [x1,y1,x2,y2]
 where
     applyTo (Matrix a b c d e f) (x,y) = (a*x+c*y+e,b*x+d*y+f)
     
applyMatrixToRectangle _ a = a

    

-- | Get the border shqpe depending on the style
getBorder :: Bool -> [PDFInteger]
getBorder False = [0,0,0]
getBorder True = [0,0,1]

standardAnnotationDict :: AnnotationObject a => a -> [(PDFName,AnyPdfObject)]
standardAnnotationDict a = [entry "Type" (PDFName $ "Annot")
                         , entry "Subtype" (annotationType a)
                         , entry "Rect" (annotationRect a)
                         , entry "Contents" (annotationContent a)
                         ]

--instance PdfObject Screen where
--   toPDF a@(Screen _ _ _ p play stop) = toPDF . dictFromList $
--        standardAnnotationDict a ++ [entry "P" p]
--                                    ++ (maybe [] (\x -> [entry "A" x]) play)
--                                    ++ (maybe [] (\x -> [entry "AA" (otherActions x)]) stop)
--         where
--             otherActions x = dictFromList $ [entry "D" x]
--
--instance AnnotationObject Screen where
--  addAnnotation (Screen video s rect p _ _) = do
--      r <- supply
--      playAction <- addObject $ ControlMedia Play r video
--      stopAction <- addObject $ ControlMedia Stop r video
--      updateObject (PDFReference r) $ Screen video s rect p (Just playAction) (Just playAction)
--      return $ PDFReference r
--  annotationType _ = PDFName "Screen"
--  annotationContent (Screen _ s _ _ _ _) = s
--  annotationRect (Screen _ _ r _ _ _) = r
                             
instance PdfObject TextAnnotation where
      toPDF a@(TextAnnotation _ _ i) = toPDF . dictFromList $
           standardAnnotationDict a ++ [entry "Name" (PDFName $ show i)]

instance PdfLengthInfo TextAnnotation where

instance AnnotationObject TextAnnotation where
    addAnnotation = addObject
    annotationType _ = PDFName "Text"
    annotationContent (TextAnnotation s _ _) = AnyPdfObject (toPDFString s)
    annotationRect (TextAnnotation _ r _) = r
    annotationToGlobalCoordinates (TextAnnotation a r b) = do
        gr <- transformAnnotRect r
        return $ TextAnnotation a gr b
    
instance PdfObject URLLink where
    toPDF a@(URLLink _ _ url border) = toPDF . dictFromList $
           standardAnnotationDict a ++ 
            [ entry "A" (GoToURL url)
            , entry "Border" (getBorder border)
            ]

instance PdfLengthInfo URLLink where
           
instance AnnotationObject URLLink where
    addAnnotation = addObject
    annotationType _ = PDFName "Link"
    annotationContent (URLLink s _ _ _) = AnyPdfObject (toPDFString s)
    annotationRect (URLLink _ r _ _) = r
    annotationToGlobalCoordinates (URLLink a r b c) = do
        gr <- transformAnnotRect r
        return $ URLLink a gr b c
        
instance PdfObject PDFLink where
    toPDF a@(PDFLink _ _ page x y border) = toPDF . dictFromList $
               standardAnnotationDict a ++ 
                [entry "Dest" dest
                ,entry "Border" (getBorder border)]
     where
         dest =  [ AnyPdfObject page
                 , AnyPdfObject (PDFName "XYZ")
                 , AnyPdfObject x
                 , AnyPdfObject y
                 , AnyPdfObject (PDFInteger 0)] 
                                                        
instance PdfLengthInfo PDFLink where

instance AnnotationObject PDFLink where
    addAnnotation = addObject
    annotationType _ = PDFName "Link"
    annotationContent (PDFLink s _ _ _ _ _) = AnyPdfObject (toPDFString s)
    annotationRect (PDFLink _ r _ _ _ _) = r
    annotationToGlobalCoordinates (PDFLink a r b c d e) = do
        gr <- transformAnnotRect r
        return $ PDFLink a gr b c d e
        
transformAnnotRect :: [PDFFloat] -> Draw [PDFFloat]
transformAnnotRect r = do
    l <- gets matrix
    let m = foldr (*) identity l
    return $ m `applyMatrixToRectangle` r
    
-- | Create a new annotation object
newAnnotation :: (PdfObject a, AnnotationObject a) => a -> Draw ()
newAnnotation annot = do
    annot' <- annotationToGlobalCoordinates annot
    modifyStrict $ \s -> s {annots = (AnyAnnotation annot'):(annots s)}
    return ()

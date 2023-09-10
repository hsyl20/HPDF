{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF API for Haskell
---------------------------------------------------------
-- #hide
module Graphics.PDF.Draw(
 -- * Draw monad
   Draw
 , PDFStream(..)
 , withNewContext
 , DrawState(..)
 , DrawEnvironment(..)
 , readDrawST 
 , writeDrawST 
 , modifyDrawST 
 , DrawTuple()
 , penPosition
 , supplyName
 , emptyDrawing
-- , writeCmd
 , runDrawing
 , setResource
 , registerResource
 , emptyEnvironment
 , PDFXForm
 , PDFXObject(..)
 , AnyPdfXForm
 , pdfDictMember
 -- PDF types
 , PDF(..)
 , PDFPage(..)
 , PDFPages(..)
 , PdfState(..)
 , PDFCatalog(..)
 , Pages(..)
 , PDFDocumentPageMode(..)
 , PDFDocumentPageLayout(..)
 , PDFViewerPreferences(..)
 , PDFDocumentInfo(..)
 -- ** Page transitions
 , PDFTransition(..)
 , PDFTransStyle(..)
 , PDFTransDirection(..)
 , PDFTransDimension(..)
 , PDFTransDirection2(..)
 -- ** Outlines
 , PDFOutline(..)
 , OutlineStyle(..)
 , PDFOutlineEntry(..)
 , Destination(..)
 , Outline
 , OutlineLoc(..)
 , Tree(..)
 , OutlineCtx(..)
 , AnnotationObject(..)
 , Color(..)
 , hsvToRgb
 , OutlineData
 , AnyAnnotation(..)
 , AnnotationStyle(..)
 , PDFShading(..)
 , ColorSpace(..)
 , colorSpaceName
 , Formula(..)
 , calculator1
 , calculator2
 , ColorFunction1(..)
 , ColorFunction2(..)
 , Function1(..)
 , Function2(..)
 , SoftMask(..)
 , getRgbColor
 , emptyDrawState
 , Matrix(..)
 , identity
 , applyMatrix
 , currentMatrix
 , multiplyCurrentMatrixWith
 , PDFGlobals(..)
 ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import qualified Data.Map.Strict as M
import qualified Data.IntMap as IM
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.Array as Array
import Data.Array (Array)

import Control.Monad.ST
import Data.STRef

import Data.Ord (comparing)

import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.State

import qualified Graphics.PDF.Expression as Expr
import Graphics.PDF.Expression (PDFExpression)
import Graphics.PDF.Coordinates
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.LowLevel.Serializer
import Graphics.PDF.Resources
import Graphics.PDF.Data.PDFTree(PDFTree)
import qualified Data.Text as T
import Graphics.PDF.Fonts.Font(PDFFont(..))

import Text.Printf (printf)

data AnnotationStyle = AnnotationStyle !(Maybe Color)

class AnnotationObject a where
    addAnnotation :: a -> PDF (PDFReference a)
    annotationType :: a -> PDFName
    annotationContent :: a -> AnyPdfObject
    annotationRect :: a -> [PDFFloat]
    annotationToGlobalCoordinates :: a -> Draw a
    annotationToGlobalCoordinates = return
    
data AnyAnnotation = forall a.(PdfObject a,AnnotationObject a) => AnyAnnotation a

instance PdfObject AnyAnnotation where
    toPDF (AnyAnnotation a) = toPDF a
instance PdfLengthInfo AnyAnnotation where

instance AnnotationObject AnyAnnotation where
    addAnnotation (AnyAnnotation a) = do
        PDFReference r <- addAnnotation a
        return (PDFReference r)
    annotationType (AnyAnnotation a) = annotationType a
    annotationContent (AnyAnnotation a) = annotationContent a
    annotationRect (AnyAnnotation a) = annotationRect a
    

-- | A PDF color
data Color = Rgb !Double !Double !Double
           | Hsv !Double !Double !Double
           deriving(Eq,Ord)

data DrawState = DrawState {
                   supplyNames :: [String]
                ,  rsrc :: PDFResource
                ,  strokeAlphas :: M.Map StrokeAlpha String
                ,  fillAlphas :: M.Map FillAlpha String
                ,  theFonts :: M.Map PDFFont String
                ,  xobjects :: M.Map (PDFReference AnyPdfXForm) String
                ,  otherRsrcs :: PDFDictionary
                ,  annots :: [AnyAnnotation]
                ,  patterns :: M.Map (PDFReference AnyPdfPattern) String
                ,  colorSpaces :: M.Map PDFColorSpace String
                ,  shadings :: M.Map PDFShading String
                ,  softMasks :: M.Map SoftMask String
                ,  matrix :: [Matrix]
                }
data DrawEnvironment = DrawEnvironment {
                        streamId :: Int
                     ,  xobjectBoundD :: IM.IntMap (PDFFloat,PDFFloat)
                     }   

data DrawTuple s
   = DrawTuple {  drawEnvironment    :: DrawEnvironment
               ,  drawStateRef  :: STRef s DrawState
               ,  builderRef :: STRef s BU.Builder
               ,  penPosition :: STRef s Point
               }
    
emptyEnvironment :: DrawEnvironment
emptyEnvironment = DrawEnvironment 0 IM.empty

class PDFGlobals m where
    bounds :: PDFXObject a => PDFReference a -> m (PDFFloat,PDFFloat)
    
-- | The drawing monad
newtype Draw a = Draw {unDraw :: forall s. DrawTuple s -> ST s a }

instance Applicative Draw where
    pure x = Draw $ \_env -> return x
    df <*> af = Draw $ \env -> do
       f <- unDraw df env
       a <- unDraw af env
       return $ f a


instance Monad Draw where
    m >>= f  = Draw $ \env -> do
                          a <- unDraw m env
                          unDraw (f a) env
    return x = Draw $ \_env -> return x

instance MonadReader DrawEnvironment Draw where
   ask       = Draw $ \env -> return (drawEnvironment env)
   local f m = Draw $ \env -> let drawenv' = f (drawEnvironment env)
                                  env' = env { drawEnvironment = drawenv' }
                               in unDraw m env' 

instance MonadState DrawState Draw where
    get    = Draw $ \env -> readSTRef  (drawStateRef env)
    put st = Draw $ \env -> writeSTRef (drawStateRef env) st

instance MonadWriter BU.Builder Draw where
    tell bu  = Draw $ \env -> modifySTRef (builderRef env) (`mappend` bu)
    listen m = Draw $ \env -> do
                 a <- unDraw m env
                 w <- readSTRef (builderRef env)
                 return (a,w)
    pass   m = Draw $ \env -> do
                 (a, f) <- unDraw m env
                 modifySTRef (builderRef env) f
                 return a

instance Functor Draw where
     fmap f = \m -> do { a <- m; return (f a) }

instance MonadPath Draw

readDrawST :: (forall s. DrawTuple s -> STRef s a) -> Draw a
readDrawST   f   = Draw $ \env -> readSTRef   (f env) 

writeDrawST :: (forall s. DrawTuple s -> STRef s a) -> a -> Draw ()
writeDrawST  f x = Draw $ \env -> writeSTRef  (f env) x 

modifyDrawST :: (forall s. DrawTuple s -> STRef s a) -> (a -> a) -> Draw ()
modifyDrawST f g = Draw $ \env -> modifySTRef (f env) g

-- | A PDF stream object
data PDFStream =
    PDFStream
        !BU.Builder
        !Bool
        !(Either (PDFReference MaybeLength) PDFLength)
        !PDFDictionary

instance PdfObject PDFStream where
  toPDF (PDFStream s c l d) = 
      mconcat   $ [ toPDF dict
                  , serialize "\nstream"
                  , newline
                  , s
                  , newline
                  , serialize "endstream"]
   where
      compressedStream False = []
      compressedStream True = if not (pdfDictMember (PDFName "Filter") d) then [entry "Filter" [PDFName $ "FlateDecode"]] else []
      lenDict = dictFromList $ [either (entry "Length") (entry "Length") l] ++ compressedStream c
      dict = pdfDictUnion lenDict d

instance PdfLengthInfo PDFStream where 
  pdfLengthInfo (PDFStream s _ el _) =
      either
        (\ref -> Just (PDFLength . B.length . BU.toLazyByteString $ s, ref))
        (const Nothing)
        el

-- | An empty drawing
emptyDrawing :: Draw ()
emptyDrawing = return ()
  
-- | is member of the dictionary
pdfDictMember :: PDFName -> PDFDictionary -> Bool
pdfDictMember k (PDFDictionary d)  = M.member k d

-- | Get a new resource name
supplyName :: Draw String
supplyName = do
    xs <- gets supplyNames -- infinite list
    modifyStrict $ \s -> s {supplyNames = tail xs}
    return (head xs)
    
emptyDrawState :: Int -> DrawState
emptyDrawState ref = 
    let names = (map (("O" ++ (show ref)) ++ ) $ [replicate k ['a'..'z'] | k <- [1..]] >>= sequence) in
    DrawState names emptyRsrc M.empty M.empty M.empty M.empty emptyDictionary []  M.empty M.empty M.empty M.empty [identity]
  
-- | Execute the drawing commands to get a new state and an uncompressed PDF stream
runDrawing :: Draw a -> DrawEnvironment -> DrawState -> (a,DrawState,BU.Builder)
runDrawing drawing environment drawState 
    = runST $ do
        dRef <- newSTRef drawState
        bRef <- newSTRef mempty
        posRef <- newSTRef 0
        let tuple = DrawTuple { drawEnvironment = environment
                              , drawStateRef    = dRef
                              , builderRef      = bRef
                              , penPosition     = posRef
                              } 
        a <- unDraw drawing tuple
        drawSt <- readSTRef (drawStateRef tuple)
        builder <- readSTRef (builderRef tuple)
        return (a, drawSt, builder)
     
pushMatrixStack :: Matrix -> Draw ()
pushMatrixStack m = do
    modifyStrict $ \s -> s {matrix = m : matrix s}
    
popMatrixStack :: Draw ()
popMatrixStack = do
    modifyStrict $ \s -> s {matrix = tail (matrix s)}
    

multiplyCurrentMatrixWith :: Matrix -> Draw ()
multiplyCurrentMatrixWith m' = modifyStrict $ \s -> s {matrix = let (m:l) = matrix s in (m' * m ):l}

    
currentMatrix :: Draw Matrix
currentMatrix = gets matrix >>= return . head
      
-- | Draw in a new drawing context without perturbing the previous context
-- that is restored after the draw       
withNewContext :: Draw a -> Draw a
withNewContext m = do
    tell . serialize $ "\nq"
    pushMatrixStack identity
    a <- m
    popMatrixStack
    tell . serialize $ "\nQ"
    return a
    
-- | Set a resource in the resource dictionary
setResource :: (Ord a, PdfResourceObject a) => String -- ^ Dict name
            -> a -- ^ Resource value
            -> M.Map a String -- ^ Old cache value
            -> Draw (String,M.Map a String) -- ^ New cache value
setResource dict values oldCache = do
    case M.lookup values oldCache of
        Nothing -> do
             newName <- supplyName
             modifyStrict $ \s -> s { rsrc = addResource (PDFName dict) (PDFName newName) (toRsrc values) (rsrc s)}
             return (newName,M.insert values newName oldCache)
        Just n -> return (n,oldCache)

-- ToDo: setter and getter could be replaced by an Accessor or a Lens
registerResource ::
    (Ord a, PdfResourceObject a) =>
    String ->
    (DrawState -> M.Map a String) ->
    (M.Map a String -> DrawState -> DrawState) ->
    a -> Draw String
registerResource dict getMap setMap resource = do
    oldMap <- gets getMap
    (newName,newMap) <- setResource dict resource oldMap
    modifyStrict $ setMap newMap
    return newName


instance PDFGlobals Draw where
    bounds (PDFReference r) = getBoundInDraw r
    
instance PDFGlobals PDF where
    bounds (PDFReference r) = getBoundInPDF r
    
-- | A PDF Xobject which can be drawn
class PDFXObject a where
    drawXObject :: PDFReference a -> Draw ()
    
    privateDrawXObject :: PDFReference a -> Draw ()
    privateDrawXObject (PDFReference r) = do
        newName <-
            registerResource "XObject"
                xobjects (\newMap s -> s { xobjects = newMap })
                (PDFReference r)
        tell . mconcat  $ [ serialize "\n/" 
                          , serialize newName
                          , serialize " Do"
                          ]
    drawXObject = privateDrawXObject
    
-- | An XObject
data AnyPdfXForm = forall a. (PDFXObject a,PdfObject a) => AnyPdfXForm a
instance PdfObject AnyPdfXForm where
    toPDF (AnyPdfXForm a) = toPDF a
instance PdfLengthInfo AnyPdfXForm where

instance PDFXObject AnyPdfXForm

data PDFXForm
instance PDFXObject PDFXForm
instance PdfObject PDFXForm where
    toPDF _ = noPdfObject
instance PdfLengthInfo PDFXForm where

instance PdfResourceObject (PDFReference PDFXForm) where
    toRsrc = AnyPdfObject
    
instance PdfResourceObject (PDFReference AnyPdfXForm) where
    toRsrc = AnyPdfObject
    

-- | Get the bounds for an xobject
getBoundInDraw :: Int -- ^ Reference
         -> Draw (PDFFloat,PDFFloat)  
getBoundInDraw ref = do
    theBounds <- asks xobjectBoundD
    return $ IM.findWithDefault (0.0,0.0) ref theBounds
 
-- | Get the bounds for an xobject
getBoundInPDF :: Int -- ^ Reference
              -> PDF (PDFFloat,PDFFloat)  
getBoundInPDF ref = do
    theBounds <- gets xobjectBound
    return $ IM.findWithDefault (0.0,0.0) ref theBounds
   
-----------
--
-- PDF types
--
------------

-- | The PDF Catalog
data PDFCatalog = PDFCatalog 
                   !(Maybe (PDFReference PDFOutline))
                   !(PDFReference PDFPages)
                   !PDFDocumentPageMode
                   !PDFDocumentPageLayout
                   !PDFViewerPreferences

-- | The PDF state
data PdfState = PdfState { supplySrc :: !Int -- ^ Supply of unique identifiers
                         , objects :: !(IM.IntMap AnyPdfObject) -- ^ Dictionary of PDF objects
                         , pages :: !Pages -- ^ Pages
                         , streams :: !(IM.IntMap ((Maybe (PDFReference PDFPage)),(DrawState,BU.Builder))) -- ^ Draw commands
                         , catalog :: !(PDFReference PDFCatalog) -- ^ Reference to the PDF catalog
                         , defaultRect :: !PDFRect -- ^ Default page size
                         , docInfo :: !PDFDocumentInfo -- ^ Document infos
                         , outline :: Maybe Outline -- ^ Root outline
                         , currentPage :: Maybe (PDFReference PDFPage) -- ^ Reference to the current page used to create outlines
                         , xobjectBound :: !(IM.IntMap (PDFFloat,PDFFloat)) -- ^ Width and height of xobjects
                         , firstOutline :: [Bool] -- ^ Used to improve the outline API
                         }
                         
-- | A PDF Page object
#ifndef __HADDOCK__
data PDFPage = PDFPage 
          !(Maybe (PDFReference PDFPages)) --  Reference to parent
          !(PDFRect) -- Media box
          !(PDFReference PDFStream) -- Reference to content
          !(Maybe (PDFReference PDFResource)) -- Reference to resources
          !(Maybe PDFFloat) -- Optional duration
          !(Maybe PDFTransition) -- Optional transition
          ![AnyPdfObject] -- Annotation array
#else
data PDFPage
#endif

instance Show PDFPage where
    show _ = "PDFPage"
    
-- | List of all pages
newtype Pages = Pages (PDFTree PDFPage)

-- | PDF Pages
#ifndef __HADDOCK__
data PDFPages = PDFPages 
              !Int
              !(Maybe (PDFReference PDFPages)) -- Reference to parent 
              [Either (PDFReference PDFPages) (PDFReference PDFPage)]
#else
data PDFPages
#endif

-- | A PDF Transition
data PDFTransition = PDFTransition !PDFFloat !PDFTransStyle  
  deriving(Eq)


-- | Dimension of a transition
data PDFTransDimension = Horizontal | Vertical 
 deriving(Eq)


instance Show PDFTransDimension where
    show Horizontal = "H"
    show Vertical = "V"

-- | Direction of a transition
data PDFTransDirection = Inward | Outward deriving(Eq)

instance Show PDFTransDirection where
    show Inward = "I"
    show Outward = "O"

-- | Direction of a transition
data PDFTransDirection2 = LeftToRight
                        | BottomToTop -- ^ Wipe only
                        | RightToLeft -- ^ Wipe only
                        | TopToBottom
                        | TopLeftToBottomRight -- ^ Glitter only
                        deriving(Eq)

-- | The PDF Monad
newtype PDF a = PDF {unPDF :: State PdfState a}
#ifndef __HADDOCK__
  deriving (Functor, Applicative, Monad, MonadState PdfState)
#else
instance Functor PDF
instance Monad PDF
instance MonadState PdfState PDF
#endif

-- | Transition style
data PDFTransStyle = Split PDFTransDimension PDFTransDirection
                   | Blinds PDFTransDimension 
                   | Box  PDFTransDirection
                   | Wipe PDFTransDirection2
                   | Dissolve 
                   | Glitter PDFTransDirection2
                   deriving(Eq)

-- | Document metadata
data PDFDocumentInfo = PDFDocumentInfo {
                     author :: T.Text
                   , subject :: T.Text
                   , pageMode :: PDFDocumentPageMode
                   , pageLayout :: PDFDocumentPageLayout
                   , viewerPreferences :: PDFViewerPreferences
                   , compressed :: Bool
                   }


-- | Document page mode
data PDFDocumentPageMode = UseNone
                       | UseOutlines
                       | UseThumbs
                       | FullScreen
                       deriving(Eq,Show)

-- | Document page layout
data PDFDocumentPageLayout = SinglePage
                           | OneColumn
                           | TwoColumnLeft
                           | TwoColumnRight
                           | TwoPageLeft
                           | TwoPageRight
                           deriving(Eq,Show)

-- | Viewer preferences
data PDFViewerPreferences = PDFViewerPreferences { hideToolbar :: Bool -- ^ To hide the toolbar
                          , hideMenuBar :: Bool -- ^ To hide the menubar
                          , hideWindowUI :: Bool -- ^ To hide the window
                          , fitWindow :: Bool -- ^ Fit window to screen
                          , centerWindow :: Bool -- ^ Center window on screen
                          , displayDoctitle :: Bool -- ^ Display the docu,ent title
                          , nonFullScreenPageMode :: PDFDocumentPageMode -- ^ Display mode when exiting the full screen mode
                          }

data PDFOutline = PDFOutline !(PDFReference PDFOutlineEntry) !(PDFReference PDFOutlineEntry)

instance PdfObject PDFOutline where
 toPDF (PDFOutline first lasto) = toPDF $ dictFromList $ [
    entry "Type" (PDFName $ "Outlines")
  , entry "First" first
  , entry "Last" lasto
  ]

instance PdfLengthInfo PDFOutline where

data OutlineStyle = NormalOutline
                  | ItalicOutline
                  | BoldOutline
                  deriving(Eq)

data PDFOutlineEntry = PDFOutlineEntry !PDFString 
                              !(PDFReference PDFOutlineEntry) -- Parent
                              !(Maybe (PDFReference PDFOutlineEntry)) -- Prev
                              !(Maybe (PDFReference PDFOutlineEntry)) -- Next
                              !(Maybe (PDFReference PDFOutlineEntry)) -- First
                              !(Maybe (PDFReference PDFOutlineEntry)) -- Last
                              Int -- Count of descendent (negative)
                              Destination
                              Color --
                              OutlineStyle 

data Destination = Destination !(PDFReference PDFPage) deriving(Eq,Show)

-- Outline types without a position pointer. The true outline is the derivative
type OutlineData = (PDFString,Maybe Color, Maybe OutlineStyle,Destination)
type Outline = OutlineLoc OutlineData

data Tree a = Node a [Tree a]

data OutlineCtx a = Top | Child { value :: a
                                , parent :: OutlineCtx a 
                                , lefts :: [Tree a]
                                , rights :: [Tree a]
                                }
                                

data OutlineLoc  a = OutlineLoc (Tree a) (OutlineCtx a)

instance PdfObject PDFViewerPreferences where
  toPDF (PDFViewerPreferences ht hm hwui fw cw ddt nfspm ) = toPDF $ dictFromList $
   [ entry "HideToolbar" ht
   , entry "HideMenubar" hm
   , entry "HideWindowUI" hwui
   , entry "FitWindow" fw
   , entry "CenterWindow" cw
   , entry "DisplayDocTitle" ddt
   , entry "NonFullScreenPageMode" (PDFName . show $ nfspm)
   ]

instance PdfLengthInfo PDFViewerPreferences where


instance Show PDFTransStyle where
   show (Split _ _) = "Split"
   show (Blinds _) = "Blinds"
   show (Box _) = "Box"
   show (Wipe _) = "Wipe"
   show (Dissolve) = "Dissolve"
   show (Glitter _) = "Glitter"

instance PdfObject PDFTransition where
 toPDF (PDFTransition d t) = toPDF $ dictFromList $
   [ entry "Type" (PDFName "Trans")
   , entry "S" (PDFName (show t))
   , entry "D" d
   ] ++ optionalDm t ++ optionalM t ++ optionalDi t
  where
    optionalDm (Split a _) = [ entry "Dm" (PDFName (show a))]
    optionalDm (Blinds a) = [ entry "Dm" (PDFName (show a))]
    optionalDm _ = []
    optionalM (Split _ a) = [ entry "M" (PDFName (show a))]
    optionalM (Box a) = [ entry "M" (PDFName (show a))]
    optionalM _ = []    
    optionalDi (Wipe a) = [ entry "Di" (floatDirection a)]
    optionalDi (Glitter a)  = [ entry "Di" (floatDirection a)]
    optionalDi _ = []  

instance PdfLengthInfo PDFTransition where

-- PDF Pages

instance PdfObject PDFPages where
 toPDF (PDFPages c Nothing l) = toPDF $ dictFromList $
  [ entry "Type" (PDFName "Pages")
  , entry "Kids" l
  , entry "Count" (PDFInteger $ c)
  ]
 toPDF (PDFPages c (Just theParent) l) = toPDF $ dictFromList $
  [ entry "Type" (PDFName "Pages")
  , entry "Parent" theParent
  , entry "Kids" l
  , entry "Count" (PDFInteger $ c)
  ]

instance PdfLengthInfo PDFPages where


instance PdfObject PDFPage where
 toPDF (PDFPage (Just theParent) box content theRsrc d t theAnnots) = toPDF $ dictFromList $
  [ entry "Type" (PDFName "Page")
  , entry "Parent" theParent
  , entry "MediaBox" box
  , entry "Contents" content
  , case theRsrc of
      Just res -> entry "Resources" res
      Nothing -> entry "Resources" emptyDictionary
  ] ++ (maybe [] (\x -> [entry "Dur" x]) d)
  ++ (maybe [] (\x -> [entry "Trans" x]) t)
  ++ ((\x -> if null x then [] else [entry "Annots" x]) theAnnots)
 toPDF (PDFPage Nothing _ _ _ _ _ _) = noPdfObject

instance PdfLengthInfo PDFPage where

-- Main objects in a PDF document

instance PdfObject PDFCatalog where
 toPDF (PDFCatalog outlines lPages pgMode pgLayout viewerPrefs) = toPDF $ dictFromList $
   [ entry "Type" (PDFName "Catalog")
   , entry "Pages" lPages
   , entry "PageMode" (PDFName . show $ pgMode)
   , entry "PageLayout" (PDFName . show $ pgLayout)
   , entry "ViewerPreferences" viewerPrefs
   ] ++ (maybe [] (\x -> [entry "Outlines" x]) outlines)

instance PdfLengthInfo PDFCatalog where

instance PdfObject OutlineStyle where
   toPDF NormalOutline = toPDF (PDFInteger 0)
   toPDF ItalicOutline = toPDF (PDFInteger 1)
   toPDF BoldOutline = toPDF (PDFInteger 2)

instance PdfLengthInfo OutlineStyle where

instance PdfObject PDFOutlineEntry where
 toPDF (PDFOutlineEntry title theParent prev next first theLast count dest color style) = 
     toPDF $ dictFromList $ [
        entry "Title" title
        , entry "Parent" theParent
        ]
      ++
      maybe [] (\x -> [entry "Prev" x]) prev
      ++
      maybe [] (\x -> [entry "Next" x]) next
      ++
      maybe [] (\x -> [entry "First" x]) first
      ++
      maybe [] (\x -> [entry "Last" x]) theLast
      ++
      [ entry "Count" (PDFInteger count)
      , entry "Dest" dest
      , entry "C" color
      , entry "F" style
      ]

instance PdfLengthInfo PDFOutlineEntry where


instance PdfObject Destination where
  toPDF (Destination r) = toPDF                [ AnyPdfObject r
                                               , AnyPdfObject . PDFName $ "Fit"
                                               ]

instance PdfLengthInfo Destination where

                                              
instance PdfObject Color where
   toPDF (Rgb r g b) = toPDF [r,g,b]
   toPDF (Hsv h s v) = let (r,g,b) = hsvToRgb (h,s,v)
    in toPDF [r,g,b]

instance PdfLengthInfo Color where

-- Degree for a transition direction
floatDirection :: PDFTransDirection2 -> PDFFloat
floatDirection LeftToRight = 0
floatDirection BottomToTop = 90
floatDirection RightToLeft = 180 
floatDirection TopToBottom = 270
floatDirection TopLeftToBottomRight = 315


hsvToRgb :: (Double,Double,Double) -> (Double,Double,Double)
hsvToRgb (h,s,v) =
  let hi = fromIntegral (floor (h / 60) `mod` 6 :: Int) :: Double
      f = h/60 - hi
      p = v * (1-s)
      q = v * (1 - f*s)
      t = v * (1 - (1-f)*s) in
 case hi of
      0 -> (v,t,p)
      1 -> (q,v,p)
      2 -> (p,v,t)
      3 -> (p,q,v)
      4 -> (t,p,v)
      5 -> (v,p,q)
      _ -> error "Hue value incorrect"

getRgbColor :: Color -> (PDFFloat,PDFFloat,PDFFloat) 
getRgbColor (Rgb r g b) = (r, g, b)  
getRgbColor (Hsv h s v) = let (r,g,b) = hsvToRgb (h,s,v) in (r, g, b)  

type FloatRGB = (PDFFloat, PDFFloat, PDFFloat)

class ColorTuple a where
    rgbHex :: a -> String
    colorComponents :: a -> [PDFFloat]
    colorDimensions :: f a e -> Int

instance ColorTuple PDFFloat where
    rgbHex c = printf "%02X" (byteFromFloat c)
    colorComponents c = [c]
    colorDimensions _ = 1

instance (a ~ PDFFloat, b ~ PDFFloat, c ~ PDFFloat) => ColorTuple (a,b,c) where
    rgbHex (r,g,b) =
        printf "%02X%02X%02X"
            (byteFromFloat r) (byteFromFloat g) (byteFromFloat b)
    colorComponents (r,g,b) = [r,g,b]
    colorDimensions _ = 3

byteFromFloat :: PDFFloat -> Int
byteFromFloat x = round $ min 255 $ max 0 $ x*255

pdfStreamFromLazyByteString :: C.ByteString -> PDFDictionary -> PDFStream
pdfStreamFromLazyByteString stream dict =
    PDFStream
        (BU.fromLazyByteString stream)
        False
        (Right . PDFLength . B.length $ stream)
        dict

rsrcFromSampled ::
    (ColorTuple a) =>
    PDFDictionary ->
    ((i, i) -> [Int]) -> Array i a -> AnyPdfObject
rsrcFromSampled domain computeSizes arr =
    AnyPdfObject $
    pdfStreamFromLazyByteString
        ((C.pack $ concatMap rgbHex $ Array.elems arr)
            <>
            C.pack " >")
        (pdfDictUnion domain . dictFromList $
           [entry "FunctionType" (PDFInteger 0),
            entry "Size" (computeSizes $ Array.bounds arr),
            entry "BitsPerSample" (PDFInteger 8),
            entry "Filter" (PDFName "ASCIIHexDecode")
            ])

-- | Interpolation function
rsrcFromInterpolated ::
    (ColorTuple a) =>
    PDFDictionary -> PDFFloat -> a -> a -> AnyPdfObject
rsrcFromInterpolated domain n a b =
    AnyPdfObject . pdfDictUnion domain . dictFromList $
                            [ entry "FunctionType" (PDFInteger $ 2)
                            , entry "C0" (colorComponents a)
                            , entry "C1" (colorComponents b)
                            , entry "N" n
                            ]

rsrcFromFormula ::
    (Expr.Function f) => PDFDictionary -> f -> AnyPdfObject
rsrcFromFormula domain f =
    AnyPdfObject $
    pdfStreamFromLazyByteString
        (C.cons '{' $ C.snoc (Expr.serialize f) '}')
        (pdfDictUnion domain . dictFromList $
            [entry "FunctionType" (PDFInteger 4)])


type ExprFloat = PDFExpression PDFFloat
type ExprRGB = (ExprFloat, ExprFloat, ExprFloat)


newtype Formula a = Formula a

instance (Expr.Function a) => Eq (Formula a) where
    Formula a == Formula b  =  Expr.serialize a == Expr.serialize b

instance (Expr.Function a) => Ord (Formula a) where
    compare (Formula a) (Formula b)  =  comparing Expr.serialize a b

data ColorSpace a e where
    GraySpace :: ColorSpace PDFFloat ExprFloat
    RGBSpace :: ColorSpace FloatRGB ExprRGB

deriving instance Eq (ColorSpace a e)
deriving instance Ord (ColorSpace a e)

colorSpaceName :: ColorSpace a e -> PDFName
colorSpaceName space =
    case space of
        GraySpace -> PDFName "DeviceGray"
        RGBSpace -> PDFName "DeviceRGB"

colorSpaceEntry :: ColorSpace a e -> (PDFName, AnyPdfObject)
colorSpaceEntry space = entry "ColorSpace" $ colorSpaceName space

rangeEntry :: (ColorTuple a) => f a e -> (PDFName, AnyPdfObject)
rangeEntry func =
    entry "Range" $ concat $ replicate (colorDimensions func) [0,1::Int]


data ColorFunction1 =
    forall a e.
    (ColorTuple a, Expr.Result e) =>
    ColorFunction1 (ColorSpace a e) (Function1 a e)

instance Eq ColorFunction1 where
    ColorFunction1 spaceA funcA == ColorFunction1 spaceB funcB  =
        case (spaceA, spaceB) of
            (GraySpace, GraySpace) -> funcA == funcB
            (RGBSpace, RGBSpace) -> funcA == funcB
            _ -> False

instance Ord ColorFunction1 where
    compare (ColorFunction1 spaceA funcA) (ColorFunction1 spaceB funcB) =
        case (spaceA, spaceB) of
            (GraySpace, GraySpace) -> compare funcA funcB
            (RGBSpace, RGBSpace) -> compare funcA funcB
            (GraySpace, RGBSpace) -> LT
            (RGBSpace, GraySpace) -> GT


data ColorFunction2 =
    forall a e.
    (ColorTuple a, Expr.Result e) =>
    ColorFunction2 (ColorSpace a e) (Function2 a e)

instance Eq ColorFunction2 where
    ColorFunction2 spaceA funcA == ColorFunction2 spaceB funcB  =
        case (spaceA, spaceB) of
            (GraySpace, GraySpace) -> funcA == funcB
            (RGBSpace, RGBSpace) -> funcA == funcB
            _ -> False

instance Ord ColorFunction2 where
    compare (ColorFunction2 spaceA funcA) (ColorFunction2 spaceB funcB) =
        case (spaceA, spaceB) of
            (GraySpace, GraySpace) -> compare funcA funcB
            (RGBSpace, RGBSpace) -> compare funcA funcB
            (GraySpace, RGBSpace) -> LT
            (RGBSpace, GraySpace) -> GT


-- ToDo: with custom Eq and Ord instances we can save the Formula wrapper
data Function1 a e =
      Sampled1 (Array Int a)
    | Interpolated1 PDFFloat a a
    | Calculator1 (Formula (ExprFloat -> e))
    deriving (Eq, Ord)

calculator1 :: (ExprFloat -> e) -> Function1 a e
calculator1 = Calculator1 . Formula

instance
    (ColorTuple a, Expr.Result e) =>
        PdfResourceObject (Function1 a e) where
    toRsrc func =
        let domain =
                dictFromList [
                    entry "Domain" [0,1::Int],
                    rangeEntry func
                ] in

        case func of
            Sampled1 arr ->
                rsrcFromSampled domain (\bnds -> [Array.rangeSize bnds]) arr

            Interpolated1 n x y -> rsrcFromInterpolated domain n x y

            Calculator1 (Formula f) -> rsrcFromFormula domain f


data Function2 a e =
      Sampled2 (Array (Int,Int) a)
    | Calculator2 (Formula (ExprFloat -> ExprFloat -> e))
    deriving (Eq, Ord)

calculator2 :: (ExprFloat -> ExprFloat -> e) -> Function2 a e
calculator2 = Calculator2 . Formula

instance
    (ColorTuple a, Expr.Result e) =>
        PdfResourceObject (Function2 a e) where
    toRsrc func =
        let domain =
                dictFromList [
                    entry "Domain" [0,1, 0,1::Int],
                    rangeEntry func
                ] in

        case func of
            Sampled2 arr ->
                rsrcFromSampled
                    domain
                    (\((lx,ly), (ux,uy)) -> [Array.rangeSize (lx,ux), Array.rangeSize (ly,uy)])
                    arr

            Calculator2 (Formula f) -> rsrcFromFormula domain f


-- | A shading
data PDFShading =
      FunctionalShading Matrix ColorFunction2
    | AxialShading PDFFloat PDFFloat PDFFloat PDFFloat ColorFunction1
    | RadialShading PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat PDFFloat ColorFunction1
                deriving(Eq,Ord)

matrixCoefficients :: Matrix -> [PDFFloat]
matrixCoefficients (Matrix a b c d e f) = [a,b,c,d,e,f]

instance PdfResourceObject PDFShading where
      toRsrc (FunctionalShading mat (ColorFunction2 cs func)) =
          AnyPdfObject . dictFromList $
                                 [ entry "ShadingType" (PDFInteger $ 1)
                                 , entry "Matrix" (matrixCoefficients $ mat)
                                 , colorSpaceEntry cs
                                 , entry "Function" (toRsrc func)
                                 ]
      toRsrc (AxialShading x0 y0 x1 y1 (ColorFunction1 cs func)) =
          AnyPdfObject . dictFromList $
                                 [ entry "ShadingType" (PDFInteger $ 2)
                                 , entry "Coords" [x0,y0,x1,y1]
                                 , entry "Extend" [True, True]
                                 , colorSpaceEntry cs
                                 , entry "Function" (toRsrc func)
                                 ]
      toRsrc (RadialShading x0 y0 r0 x1 y1 r1 (ColorFunction1 cs func)) =
          AnyPdfObject . dictFromList $
                                 [ entry "ShadingType" (PDFInteger $ 3)
                                 , entry "Coords" [x0,y0,r0,x1,y1,r1]
                                 , entry "Extend" [True, True]
                                 , colorSpaceEntry cs
                                 , entry "Function" (toRsrc func)
                                 ]


newtype SoftMask = SoftMask (PDFReference PDFXForm)
    deriving (Eq, Ord)

instance PdfResourceObject SoftMask where
    toRsrc (SoftMask ref) =
        AnyPdfObject $
        dictFromList $
            entry "Type" (PDFName "ExtGState") :
            entry "SMask"
                (dictFromList $
                    entry "Type" (PDFName "Mask") :
                    entry "S" (PDFName "Luminosity") :
                    entry "G" ref :
                    []) :
            []


-- | Apply a transformation matrix to the current coordinate frame
applyMatrix :: Matrix -> Draw ()
applyMatrix m@(Matrix a b c d e f)  = do
    multiplyCurrentMatrixWith m
    tell . mconcat $[ serialize '\n'
                    , toPDF a
                    , serialize ' '
                    , toPDF b
                    , serialize ' '
                    , toPDF c
                    , serialize ' '
                    , toPDF d
                    , serialize ' '
                    , toPDF e
                    , serialize ' '
                    , toPDF f
                    , serialize " cm"
                    ]

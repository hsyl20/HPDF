---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Management of the PDF structure
---------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Graphics.PDF.Document(
 -- * Document actions
 -- ** Special document objects
   PDFXForm
 -- ** Page management
 , addPage
 , addPageWithTransition
 , drawWithPage
 , drawWithPage'
 , createPDFXForm
 , createPDFXFormExtra
 -- ** Page transitions
 , PDFTransition(..)
 , PDFTransStyle(..)
 , PDFTransDirection(..)
 , PDFTransDimension(..)
 , PDFTransDirection2(..)
 -- ** Document information
 , PDFDocumentInfo(..)
 , PDFDocumentPageMode(..)
 , PDFDocumentPageLayout(..)
 , PDFViewerPreferences(..)
 , standardDocInfo
 , standardViewerPrefs
 -- * Draw monad and drawing functions
 -- ** Types
 , Draw
 , PDFXObject(drawXObject)
 , PDFGlobals(..)
 -- ** General drawing functions
 , withNewContext
 , emptyDrawing
 ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif

import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Pages
import Graphics.PDF.Shapes (Rectangle(Rectangle))
import Control.Monad.State
import qualified Data.IntMap as IM
import qualified Data.ByteString.Lazy as B
import Data.Binary.Builder (toLazyByteString)
import qualified Data.Text as T
import Data.Complex (Complex((:+)))

-- | No information for the document  
standardDocInfo :: PDFDocumentInfo          
standardDocInfo = PDFDocumentInfo T.empty T.empty UseNone SinglePage standardViewerPrefs True True

-- | Create a PDF XObject
createPDFXForm :: Monad m => PDFFloat -- ^ Left
              -> PDFFloat -- ^ Bottom
              -> PDFFloat -- ^ Right
              -> PDFFloat -- ^ Top
              -> Draw a -- ^ Drawing commands
              -> PDFT m (PDFReference PDFXForm)
createPDFXForm xa ya xb yb d =
    createPDFXFormExtra (Rectangle (xa:+ya) (xb:+yb)) d emptyDictionary

-- | Create a PDF XObject
createPDFXFormExtra :: Monad m =>
                 Rectangle -- ^ Bounding Box
              -> Draw a -- ^ Drawing commands
              -> PDFDictionary
              -> PDFT m (PDFReference PDFXForm)
createPDFXFormExtra (Rectangle (xa:+ya) (xb:+yb)) d dict =
 let a' = do
            modifyStrict $ \s -> s {
                otherRsrcs = pdfDictUnion dict $ dictFromList $
                    [ entry "Type" (PDFName $ "XObject")
                    , entry "Subtype" (PDFName $ "Form")
                    , entry "FormType" (PDFInteger $ 1)
                    , entry "Matrix" (map PDFInteger $ [1,0,0,1,0,0])
                    , entry "BBox" (map AnyPdfObject $ [xa,ya,xb,yb])
                    ]
              }
            d
 in do
     PDFReference s <- createContent a' Nothing  
     recordBound s (xb-xa) (yb-ya)
     return (PDFReference s)       

 
-- Create a new empty page
createANewPage :: Monad m => Maybe PDFRect -- ^ Page size or default document's one
               -> PDFT m (Int,PDFPage) -- ^ Reference to the new page
createANewPage rect' = do
       rect <- maybe (gets defaultRect) return rect'
       -- Get the root page reference
       -- Create a new page reference
       pageref <- supply
       -- Create a new empty content for the page
       pageContent <- createContent (return ()) (Just (PDFReference pageref :: PDFReference PDFPage))
       -- Create a new page having as parent the root page
       let page = PDFPage Nothing rect pageContent Nothing Nothing Nothing []
       return (pageref , page)
       
-- | Add a new page to a PDF document
addPage :: Monad m => Maybe PDFRect -- ^ Page size or default document's one
        -> PDFT m (PDFReference PDFPage) -- ^ Reference to the new page
addPage rect'   = do
   (pf,page) <- createANewPage rect'
   let pageref = PDFReference pf
   modifyStrict $ \s -> s {pages = recordPage pageref page (pages s), currentPage = Just pageref}
   return pageref
   
addPageWithTransition :: Monad m => Maybe PDFRect -- ^ Page size or default document's one
                      -> Maybe PDFFloat -- ^ Optional duration
                      -> Maybe PDFTransition -- ^ Optional transition
                      -> PDFT m (PDFReference PDFPage) -- ^ Reference to the new page
addPageWithTransition rect' dur t = do
    (pf,PDFPage a b c d _ _ pageAnnots) <- createANewPage rect'
    let pageref = PDFReference pf
    modifyStrict $ \s -> s {pages = recordPage pageref (PDFPage a b c d dur t pageAnnots) (pages s), currentPage = Just pageref}
    return pageref

        
-- Note [Memory of the drawings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The content of a page is compressed and written when the document is
-- rendered. Until then, the document keeps the drawings of each page.
--
-- 'drawWithPage' is lazy: it keeps a drawing as a computation that is not
-- evaluated. Thus a drawing can use a value that is known only after it,
-- for example the total number of pages:
--
--   go k = do
--     page <- addPage Nothing
--     more <- drawWithPage page (pageContent k total)   -- uses total
--     if more then go (k + 1) else return k
--   (total, builder) = stepPDF (go 1) (emptyPdfBuilder info rect)
--
-- The cost is memory. A drawing that is not evaluated keeps all the data
-- that it refers to: the text, the typesetting boxes, the closures of the
-- 'Draw' monad. This is many times the size of its content.
--
-- 'drawWithPage'' is strict: it evaluates the drawing to bytes
-- immediately, and the document keeps only these bytes. A document of 300
-- pages of text (750 KB when it is written) needs 320 MiB of memory with
-- 'drawWithPage', and 58 MiB with 'drawWithPage''. The example
-- above fails with @<<loop>>@ if it uses 'drawWithPage''.
--
-- In the two cases, a stream has the list of its drawings, the last one
-- first. Each drawing is a lazy ByteString (a list of chunks):
--
-- * A second drawing on the same page is added at the head of the list.
--   This does not depend on the number of drawings that the page has.
--   (An append of two lazy ByteStrings walks the chunks of the first one:
--   a page with n drawings would need n * n steps.)
-- * When the document is rendered, the drawings are put in order one
--   time, and their chunks go to zlib, or to the output if the document
--   is not compressed.

-- | Draw on a given page.
--
-- The drawing is lazy: it is evaluated when the document is rendered. For
-- a document with many pages, 'drawWithPage'' needs much less memory.
drawWithPage :: Monad m => PDFReference PDFPage -- ^ Page
            -> Draw a -- ^ Drawing commands
            -> PDFT m a
drawWithPage = draw_with_page False

-- | Draw on a given page, and evaluate the drawing immediately.
--
-- The document then keeps the bytes of the drawing, and not the drawing
-- with all the data that it refers to. Use it for a document with many
-- pages. The drawing cannot use a value that is known only after it.
--
-- See Note [Memory of the drawings]
drawWithPage' :: Monad m => PDFReference PDFPage -- ^ Page
             -> Draw a -- ^ Drawing commands
             -> PDFT m a
drawWithPage' = draw_with_page True

-- | See Note [Memory of the drawings]
draw_with_page :: Monad m => Bool -- ^ Evaluate the drawing immediately
               -> PDFReference PDFPage
               -> Draw a
               -> PDFT m a
draw_with_page strict page draw = do
     -- Get the page dictionary
     lPages <- gets pages
     -- Get the stream dictionary
     lStreams <- gets streams
     -- Look for the page
     let thePage = findPage page lPages
     case thePage of
       Nothing -> error "Can't find the page to draw on it"
       -- If the page is found, get its stream reference and look for the stream
       Just(PDFPage _ _ (PDFReference streamRef) _ _ _ _) -> do
          let theContent = IM.lookup streamRef lStreams
          case theContent of
            Nothing -> error "Can't find a content for the page to draw on it"
            -- If the stream is found
            Just (_,(oldState,oldDrawings)) -> do
              -- Create a new cntent and update the stream
              myBounds <- gets xobjectBound
              let (a,state',w') = runDrawing draw (emptyEnvironment {streamId = streamRef, xobjectBoundD = myBounds}) oldState
                  content = toLazyByteString w'
                  update = modifyStrict $ \s -> s {streams = IM.insert streamRef (Just page,(state',content : oldDrawings)) lStreams}
              if strict
                then do
                  -- the length is known when all the chunks are evaluated
                  let !_state = state'
                      !_size = B.length content
                  update
                else update
              return a

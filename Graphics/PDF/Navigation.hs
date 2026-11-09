---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Navigation
---------------------------------------------------------

module Graphics.PDF.Navigation(
  -- * Navigation
  -- ** Types
    OutlineStyle(..)
  -- ** Functions
  , newSection
  , newSectionWithPage
 ) where
     
import Graphics.PDF.Pages
import Graphics.PDF.Draw
import Graphics.PDF.LowLevel.Types
import Control.Monad.State(gets)
import Control.Monad(when)
import Data.Maybe(isNothing)
import qualified Data.Text as T

-- | True if we are adding the first outline to this level
isFirst :: [Bool] -> Bool
isFirst r = head r
  
-- | Start a new outline level  
startNew :: Monad m => PDFT m ()
startNew = modifyStrict $ \s -> s{firstOutline = True:(firstOutline s)}

-- | We remember there are outlines at this level
addedOutline :: Monad m => PDFT m ()
addedOutline = modifyStrict $ \s -> s{firstOutline = False:tail (firstOutline s)}

-- | Close an outline level
closeNew :: Monad m => PDFT m ()
closeNew = do
    r <- gets firstOutline
    when (not (isFirst r)) $ moveToParent
    modifyStrict $ \s -> s{firstOutline = tail (firstOutline s)}

-- | Create a new outline section pointing to the last created page
newSection :: Monad m => T.Text -- ^ Outline title
           -> Maybe Color -- ^ Outline color
           -> Maybe OutlineStyle -- ^Outline style
           -> PDFT m ()
           -> PDFT m ()
newSection myS col style p = newSectionPrivate (toPDFString myS) col style Nothing p

-- | Create a new outline section pointing to a given page
newSectionWithPage :: Monad m => T.Text -- ^ Outline title
                   -> Maybe Color -- ^ Outline color
                   -> Maybe OutlineStyle -- ^ Outline style
                   -> PDFReference PDFPage -- ^ Page reference
                   -> PDFT m ()
                   -> PDFT m ()
newSectionWithPage myS col style page p = newSectionPrivate (toPDFString myS) col style (Just page) p
    
newSectionPrivate :: Monad m => PDFString -- ^ Outline title
                  -> Maybe Color -- ^ Outline color
                  -> Maybe OutlineStyle -- ^Outline style
                  -> Maybe (PDFReference PDFPage)
                  -> PDFT m ()
                  -> PDFT m ()
newSectionPrivate myS col style page p = do
       let newlevel = do
            startNew
            p
            closeNew
       r <- gets firstOutline
       if isFirst r
        then do
            if length r > 1 
             then do
                newChild myS col style page
                addedOutline
                newlevel
             else do
                newSibling myS col style page
                newlevel
        else do
           newSibling myS col style page
           newlevel                  
                   
newSibling :: Monad m => PDFString -- ^ Outline title
           -> Maybe Color -- ^ Outline color
           -> Maybe OutlineStyle -- ^Outline style
           -> Maybe (PDFReference PDFPage)
           -> PDFT m ()
newSibling myS col style page = do
    p <- if isNothing page then gets currentPage else return page
    case p of
        Nothing -> return ()
        Just aPage -> do
            ot <- gets outline
            let myValue = (myS,col,style,Destination aPage)
            case ot of
                Nothing -> modifyStrict $ \s -> s {outline = Just $ insertDown myValue (OutlineLoc (Node myValue []) Top)}
                Just r -> modifyStrict $ \s -> s {outline = Just $ insertRight myValue r}
                
newChild :: Monad m => PDFString -- ^ Outline title
         -> Maybe Color -- ^ Outline color
         -> Maybe OutlineStyle -- ^Outline style
         -> Maybe (PDFReference PDFPage)
         -> PDFT m ()
newChild myS col style page = do
    p <- if isNothing page then gets currentPage else return page
    case p of
        Nothing -> return ()
        Just aPage -> do
            ot <- gets outline
            let myValue = (myS,col,style,Destination aPage)
            case ot of
                Nothing -> modifyStrict $ \s -> s {outline = Just $ insertDown myValue (OutlineLoc (Node myValue []) Top)}
                Just r -> modifyStrict $ \s -> s {outline = Just $ insertDown myValue r}
                
moveToParent :: Monad m => PDFT m ()
moveToParent = do
    ot <- gets outline
    case ot of
       Nothing -> return ()
       Just r -> modifyStrict $ \s -> s {outline = Just $ up r}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Generation of PDF documents
-- A PDF library with support for several pages, page transitions, outlines, annotations, compression, 
-- colors, shapes, patterns, jpegs, fonts, typesetting ... Have a look at the "Graphics.PDF.Documentation" 
-- module to see how to use it. Or, download the package and look at the test.hs file 
-- in the Test folder. That file is giving an example of each feature.
---------------------------------------------------------
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
module Graphics.PDF
  (
  -- * HPDF
  -- ** PDF Monad
    PDF 
  , runPdf
  , pdfByteString
  , pdfByteStringE
  -- ** PDF Common Types
  , PDFRect(..)
  , PDFFloat
  , PDFReference
  , PDFString
  , PDFPage
  , Pages
  -- ** Document management
  , module Graphics.PDF.Document
  -- ** Drawing
  , module Graphics.PDF.Shapes
  -- ** Colors
  , module Graphics.PDF.Colors
  -- ** Geometry
  , module Graphics.PDF.Coordinates
  , applyMatrix
  -- ** Text
  , module Graphics.PDF.Text
  -- ** Navigation
  , module Graphics.PDF.Navigation
  -- ** Annotations
  , module Graphics.PDF.Annotation
  -- ** Actions
  , module Graphics.PDF.Action
  -- ** Images
  , module Graphics.PDF.Image
  -- ** Patterns
  , module Graphics.PDF.Pattern
  -- ** Shading
  , module Graphics.PDF.Shading
  -- ** Transparency
  , module Graphics.PDF.Transparency
  , ColorSpace(..)
  , calculator1
  , calculator2
  , ColorFunction1(..)
  , ColorFunction2(..)
  , Function1(..)
  , Function2(..)
  , Global
  , Local
  , linearStitched
  , FunctionObject
  -- ** Fonts
  , module Graphics.PDF.Fonts.Font 
  , module Graphics.PDF.Fonts.StandardFont
  , module Graphics.PDF.Fonts.Type1
  , module Graphics.PDF.Fonts.TrueType
  , readType1Font
  , readType1FontWithEncoding
  , mkType1Font
  , PostscriptName
  , Encodings(..)
  , unionEncodings
  -- ** Typesetting
  , module Graphics.PDF.Typesetting
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid
#endif
 
import Graphics.PDF.Typesetting
import Graphics.PDF.Transparency
import Graphics.PDF.Shading
import Graphics.PDF.Pattern
import Graphics.PDF.Navigation
import Graphics.PDF.Text
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as B
import Data.Int
import Text.Printf(printf)
import Control.Monad.State
import Graphics.PDF.Annotation
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Colors
import Graphics.PDF.Shapes
import Graphics.PDF.Coordinates
import Graphics.PDF.Pages
import Graphics.PDF.Document hiding (createPDFXFormExtra)
import Codec.Compression.Zlib
import Graphics.PDF.Action
import Graphics.PDF.Image
import Graphics.PDF.Resources(emptyResource)
import qualified Graphics.PDF.Resources as R
import Data.Binary.Builder(Builder,fromLazyByteString, toLazyByteString)
import Graphics.PDF.LowLevel.Serializer
import Data.List(mapAccumL)
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import Graphics.PDF.Fonts.Font 
import Graphics.PDF.Fonts.StandardFont
import Graphics.PDF.Fonts.Type1
import Graphics.PDF.Fonts.TrueType
import Graphics.PDF.Fonts.Encoding (unionEncodings, Encodings(..), PostscriptName)
import Graphics.PDF.Archive
import Data.Bits (xor)
import Data.Word (Word64, Word8)
import Data.List (sortOn)
import qualified Data.ByteString as S
import Control.Monad.Writer (tell)
import qualified Data.ByteString.Lazy.Char8 as LC

-- | Create a new PDF document and return a first page
-- The page is using the document size by default
createPDF :: PDF ()
createPDF  = do
  -- Create the Proc structure
  --proc <- addObject PDFProc
  -- Create an empty resource
  --addObject $ PDFResource proc
  return ()
  
-- Create the PDF stream objects from the draw monads
createStreams :: PDF ()
createStreams = do
    ls <- gets streams >>= return . IM.toList
    modifyStrict $ \s -> s {streams = IM.empty}
    pdfa <- gets pdfaSettings
    mapM_ (addStream . maybe id (const noInterpolation) pdfa) ls
 where
    -- PDF/A forbids image interpolation (ISO 19005-2, 6.2.8)
    noInterpolation (k,(p,(st,w)))
      | pdfDictMember (PDFName "Interpolate") (otherRsrcs st) =
          (k,(p,(st {otherRsrcs = insertInPdfDict (PDFName "Interpolate") (AnyPdfObject False) (otherRsrcs st)},w)))
      | otherwise = (k,(p,(st,w)))
    addStream (k,(p,(state',w'))) = do
     -- New reference for the stream
     r <- supply
     -- Run the drawing and get the new state (resource, annotation)
     --myBounds <- gets xobjectBound
     --cp <- gets currentPage
     --let (_,state',w') = runDrawing d (emptyEnvironment {streamId = r, xobjectb = myBounds, currentp = maybe Nothing (\(PDFReference x) -> Just x) cp })
     let ref = PDFReference r :: PDFReference MaybeLength
         
     -- Pattern NEEDS a resource entry even if empty otherwise don't work with acrobat reader
     -- Image DON'T want a resource entry if empty otherwise don't work with apple reader
     resources <- if (emptyResource (rsrc state')) && (not (pdfDictMember (PDFName "PatternType") (otherRsrcs state')))
       then do
         case p of
            -- Not linked to a page
            -- otherResource are entries specific to a special stream (like an XObject) so we return empty for a page
            Nothing -> return (otherRsrcs state') 
            -- Linked to a page
            Just pageRef -> do
                 setPageAnnotations (annots state') pageRef
                 return emptyDictionary
       -- Some resource are needed by the stream
       else do
         rsrcRef <- addObject (rsrc state')
         case p of
             -- Not linked to a page
             Nothing -> do                  
                  return $ (otherRsrcs state') `pdfDictUnion` (dictFromList  $ [entry "Resources" rsrcRef])
             -- Linked to a page
             Just pageRef -> do
                  setPageAnnotations (annots state') pageRef
                  setPageResource rsrcRef pageRef
                  return emptyDictionary
              
     infos <- gets docInfo
     -- Resources to add to the stream
     -- We compress only if the stream is not using its own filter
     if (compressed infos) && (not (pdfDictMember (PDFName "Filter") resources))
       then do
         let w''' = compress . toLazyByteString $ w'
             w'' = fromLazyByteString w'''
         updateObject (PDFReference k :: PDFReference PDFStream) (PDFStream w'' True (Left ref) resources)
         updateObject ref UnknownLength
         --updateObject ref (PDFLength (B.length w'''))
       else do
         updateObject (PDFReference k :: PDFReference PDFStream) (PDFStream w' False (Left ref) resources)
         updateObject ref UnknownLength
         --updateObject ref (PDFLength (B.length . toLazyByteString $ w'))

-- | Run the font finalizers with the glyph ids each font showed in any
-- stream. A finalizer may add streams of its own (font programs, CMaps),
-- which 'createStreams' then writes like any other.
finalizeFonts :: PDF ()
finalizeFonts = do
  fs <- gets fontFinalizers
  used <- gets (M.unionsWith IS.union . map (usedGlyphs . fst . snd) . IM.elems . streams)
  modifyStrict $ \s -> s {fontFinalizers = []}
  mapM_ ($ used) (reverse fs)

-- | Save all the pages and streams in the main object dictionary. Returns
-- the catalog and the Info dictionary.
saveObjects :: PDF (PDFReference PDFCatalog, Maybe (PDFReference PDFDictionary))
saveObjects  = do
  -- Write the fonts that wait for the glyphs the document used (subsets)
  finalizeFonts
  pdfa <- gets pdfaSettings
  -- PDF/A: the fonts must be embedded, so none of the standard 14
  mapM_ (const checkEmbeddedFonts) pdfa
  -- The output intent's ICC profile is a stream like the others
  intents <- case pdfa of
    Nothing -> return []
    Just a -> do
      icc <- createContent (iccStream a) Nothing
      intent <- addObject . dictFromList $
        [ entry "Type" (PDFName "OutputIntent")
        , entry "S" (PDFName "GTS_PDFA1")
        , entry "OutputConditionIdentifier" (pdfTextString (pdfaOutputCondition a))
        , entry "Info" (pdfTextString (pdfaOutputCondition a))
        , entry "DestOutputProfile" icc
        ]
      return [entry "OutputIntents" [intent]]
  -- Save streams to the object dictionary so that they are saved in the PDF document
  createStreams
  infos <- gets docInfo
  -- Save pages to the object dictionary so that they are saved in the PDF document
  pRef <- addPages
  -- Create outlines object
  o <- gets outline
  oref <- addOutlines o
  -- XMP metadata: never compressed (PDF/A)
  metadata <- case pdfa of
    Nothing -> return []
    Just a -> do
      let xmp = xmpPacket a infos producerName
      r <- addObject $ PDFStream (fromLazyByteString xmp) False (Right (PDFLength (B.length xmp)))
             (dictFromList [entry "Type" (PDFName "Metadata"), entry "Subtype" (PDFName "XML")])
      return [entry "Metadata" r]
  -- Associated (embedded) files
  files <- gets (sortOn fst . reverse . associatedFiles)
  let fileEntries
        | null files = []
        | otherwise =
            [ entry "AF" (map snd files)
            , entry "Names" (dictFromList [entry "EmbeddedFiles" (dictFromList
                [entry "Names" (concat [[pdfTextString n, AnyPdfObject r] | (n, r) <- files])])])
            ]
  -- PDF/A: an indirect Info dictionary that agrees with the metadata
  -- (other documents keep theirs in the trailer, as before)
  info <- case pdfa of
    Nothing -> return Nothing
    Just a -> fmap Just . addObject . dictFromList $
         [ entry "Title" (pdfTextString (pdfaTitle a)) | not (T.null (pdfaTitle a)) ]
      ++ [ entry "Author" (pdfTextString (author infos)) | not (T.null (author infos)) ]
      ++ [ entry "Subject" (pdfTextString (subject infos)) | not (T.null (subject infos)) ]
      ++ [ entry "Producer" (pdfTextString producerName) ]
  -- Create the catalog
  cat <- addObject $ PDFCatalog oref pRef (pageMode infos) (pageLayout infos) (viewerPreferences infos)
           (metadata ++ intents ++ fileEntries)
  modifyStrict $ \s -> s {catalog = cat}
  return (cat, info)
 where
  iccStream a = do
    modifyStrict $ \s -> s {otherRsrcs = dictFromList [entry "N" (PDFInteger 3)]}
    tell (fromLazyByteString (pdfaOutputProfile a))

-- | Records a problem for each standard 14 font a stream uses: they are
-- not embedded, so the document cannot be PDF/A.
checkEmbeddedFonts :: PDF ()
checkEmbeddedFonts = do
  ss <- gets (IM.elems . streams)
  let used = M.keys (M.unions [ fs | (_, (st, _)) <- ss
                                    , Just (PDFDictionary fs) <- [M.lookup (PDFName "Font") (R.resources (rsrc st))] ])
      bad = [ n | PDFName n <- used, n `elem` map show [Helvetica ..] ]
  modifyStrict $ \s -> s {documentProblems = documentProblems s ++
    [ "PDF/A: the standard font " ++ n ++ " is not embedded; use an embedded (TrueType) font" | n <- bad ]}

producerName :: T.Text
producerName = T.pack "HPDF - The Haskell PDF Library"

-- | The PDFTrailer
#ifndef __HADDOCK__
data PDFTrailer = PDFTrailer 
  !Int -- Number of PDF objects in the document
  !(PDFReference PDFCatalog) -- Reference to the PDf catalog
  !TrailerInfo
#else
data PDFTrailer
#endif

-- | The trailer's document information: the Info dictionary written inline,
-- or (PDF/A) a reference to an indirect Info dictionary and the document
-- identifier.
data TrailerInfo = InlineInfo PDFDocumentInfo
                 | ArchiveInfo (PDFReference PDFDictionary) S.ByteString
    
instance PdfObject PDFTrailer where
   toPDF (PDFTrailer size root (InlineInfo infos)) = toPDF $ dictFromList $
     [ entry "Size" (PDFInteger $ size)
     , entry "Root" root
     , entry "Info" (dictFromList $ allInfos)
     ]
     where
      allInfos = [ entry "Author" (toPDFString . author $ infos)
                 , entry "Subject" (toPDFString . subject $ infos)
                 , entry "Producer" (toPDFString producerName)
                 ]
   toPDF (PDFTrailer size root (ArchiveInfo info docId)) = toPDF $ dictFromList $
     [ entry "Size" (PDFInteger $ size)
     , entry "Root" root
     , entry "Info" info
     , entry "ID" [HexString docId, HexString docId]
     ]

instance PdfLengthInfo PDFTrailer where

-- | A byte string written in hexadecimal
newtype HexString = HexString S.ByteString

instance PdfObject HexString where
  toPDF (HexString b) = serialize ("<" ++ concatMap (printf "%02X") (S.unpack b) ++ ">")

instance PdfLengthInfo HexString where

-- | 16 bytes from the document's objects (two FNV-1a 64 hashes), for the
-- trailer @/ID@: the same document gets the same identifier.
documentId :: B.ByteString -> S.ByteString
documentId bytes = S.pack (word64Bytes (fnv 14695981039346656037) ++ word64Bytes (fnv 0x84222325cbf29ce4))
 where
  fnv :: Word64 -> Word64
  fnv h0 = B.foldl' (\h w -> (h `xor` fromIntegral w) * 1099511628211) h0 bytes
  word64Bytes :: Word64 -> [Word8]
  word64Bytes w = [ fromIntegral (w `div` (256 ^ k)) | k <- [7, 6 .. 0 :: Int] ]

-- | Write PDF objects in the TOC
writeObjectsAndCreateToc :: [Builder] -- ^ List of objects each object being already converted to a bytestring
                          -> (Int,Int64,[Builder])
writeObjectsAndCreateToc l = 
   let lengths =  tail . scanl (\len obj -> len + (B.length . toLazyByteString $ obj)) 0 $ l
       createEntry x = serialize $ (printf "%010d 00000 n \n" ((fromIntegral x)::Integer) :: String)
       entries = map createEntry (init lengths) 
   in
   (length l,last lengths,entries)
-- foldr writeObject (0,0::Int64,[]) l where
-- writeObject obj (nb,len,toc) = (nb+1,len + (B.length . toLazyByteString $ obj),(serialize $ (printf "%010d 00000 n \n" ((fromIntegral len)::Integer))) : toc)
generateStreams :: PDFReference PDFCatalog -> TrailerInfo -> Int -> Int64 -> [Builder]
                -> [Builder] -> B.ByteString
generateStreams root di !nb !totalLen ens [] = 
  let entries = reverse (tail ens)  
  in
  toLazyByteString $ mconcat $ [ serialize "xref\n"
                               , serialize $ "0 " ++ show nb ++ "\n"
                               , serialize "0000000000 65535 f \n"
                               ]
                               ++
                               entries
                               ++
                               [ serialize "\ntrailer\n"
                               , toPDF $ PDFTrailer nb root di
                               , serialize "\nstartxref\n"
                               , serialize (show totalLen)
                               , serialize "\n%%EOF"
                             ]
generateStreams root di !nb !totalLen ens (obj:t) = 
     let s = toLazyByteString obj 
         createEntry x = serialize $ (printf "%010d 00000 n \n" ((fromIntegral x)::Integer) :: String) 
         newLen = B.length s + totalLen
         en = createEntry $! newLen
     in
     (s `B.append`) . generateStreams root di (nb+1) newLen (en : ens) $ t

defaultPdfSettings :: PdfState
defaultPdfSettings = 
  PdfState {
             supplySrc = 1
           , objects = IM.empty
           , pages = noPages
           , streams = IM.empty
           , catalog = PDFReference 0
           , defaultRect = PDFRect 0 0 600 400 
           , docInfo = standardDocInfo { author=T.pack "Unknown", compressed = True}
           , outline = Nothing
           , currentPage = Nothing
           , xobjectBound = IM.empty
           , firstOutline = [True]
           , fontFinalizers = []
           , pdfaSettings = Nothing
           , associatedFiles = []
           , documentProblems = []
           }

createObjectByteStrings :: PdfState -> PDF a -> B.ByteString 
createObjectByteStrings pdfState m = snd (createDocument pdfState m)

-- | The problems found while saving the document, and the document.
createDocument :: PdfState -> PDF a -> ([String], B.ByteString)
createDocument pdfState m =
      let objectEncoding (x,a) = toPDF . PDFReferencedObject (fromIntegral $! x) $ a
          ((root, info),s) = flip runState pdfState  . unPDF $ createPDF >> m >> saveObjects
          header = case pdfaSettings s of
            -- PDF/A: a comment line of four bytes above 127 after the header
            Just _ -> fromLazyByteString (LC.pack "%PDF-1.7\n%\xE2\xE3\xCF\xD3\n")
            Nothing -> serialize "%PDF-1.5\n"
          objs = objects s
          encodeAnObject im k =
            let Just o = IM.lookup k im
                mim = do
                    (l, PDFReference ref) <- pdfLengthInfo o
                    return $ IM.insert ref (AnyPdfObject (KnownLength l)) im
            in (fromMaybe im mim, objectEncoding (k,o))

          encodedObjects = snd $ mapAccumL encodeAnObject objs (IM.keys objs)
          objectContents = header : encodedObjects
          (_nb, _len, _toc) = writeObjectsAndCreateToc objectContents
          trailerInfo = case info of
            Nothing -> InlineInfo (docInfo pdfState)
            Just i -> ArchiveInfo i (documentId (toLazyByteString (mconcat encodedObjects)))
      in
      (documentProblems s, generateStreams root trailerInfo 0 0 [] objectContents)

        --mconcat$ objectContents ++
        --        [ serialize "xref\n"
        --        , serialize $ "0 " ++ show nb ++ "\n"
        --        , serialize "0000000000 65535 f \n"
        --        ]
        --        ++
        --        toc
        --        ++
        --        [ serialize "\ntrailer\n"
        --        , toPDF $ PDFTrailer nb root (docInfo pdfState)
        --        , serialize "\nstartxref\n"
        --        , serialize (show len)
        --        , serialize "\n%%EOF"
        --        ]

-- | Generate a lazy bytestring for the PDF     
pdfByteString :: PDFDocumentInfo
              -> PDFRect -- ^ Default size for a page
              -> PDF a  -- ^ PDF action 
              -> B.ByteString
pdfByteString infos rect m = createObjectByteStrings (defaultPdfSettings {defaultRect = rect, docInfo = infos} ) m

-- | Like 'pdfByteString', or the reasons the document is not what it was
-- asked to be (a PDF/A document using a font that is not embedded).
pdfByteStringE :: PDFDocumentInfo
               -> PDFRect -- ^ Default size for a page
               -> PDF a  -- ^ PDF action
               -> Either [String] B.ByteString
pdfByteStringE infos rect m =
  case createDocument (defaultPdfSettings {defaultRect = rect, docInfo = infos}) m of
    ([], bytes) -> Right bytes
    (problems, _) -> Left problems

-- | Generates a PDF document
runPdf :: String -- ^ Name of the PDF document
       -> PDFDocumentInfo
       -> PDFRect -- ^ Default size for a page
       -> PDF a  -- ^ PDF action 
       -> IO ()
runPdf filename infos rect m = do
  let bytestring = pdfByteString infos rect m 
  B.writeFile filename bytestring



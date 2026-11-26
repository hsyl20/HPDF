{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
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
  , PDFT
  , PdfBuilder
  , liftPDF
  , writePdf
  , writePdfT
  , renderPdf
  , renderPdfT
  , renderPdfE
  , renderPdfTE
  -- ** Step by step construction
  , emptyPdfBuilder
  , stepPDF
  , stepPDFT
  , buildPdf
  , buildPdfE
  -- ** Deprecated
  , runPdf
  , pdfByteString
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
import Control.Monad (when)
import Control.Monad.State
import qualified Graphics.PDF.Data.PDFTree as PT
import Data.Functor.Identity (runIdentity)
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
import Graphics.PDF.Resources(emptyResource, PDFResource(..))
import Data.Binary.Builder(Builder,fromLazyByteString, toLazyByteString)
import Graphics.PDF.LowLevel.Serializer
import Data.List(intercalate, mapAccumL)
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import Graphics.PDF.Fonts.Font 
import Graphics.PDF.Fonts.StandardFont
import Graphics.PDF.Fonts.Type1
import Graphics.PDF.Fonts.TrueType
import Graphics.PDF.Fonts.Encoding (unionEncodings, Encodings(..), PostscriptName)
import Graphics.PDF.Archive
import Graphics.PDF.LowLevel.Hash (fnv1a64OffsetBasis, fnv1a64Step)
import Control.Exception (IOException, catch, onException)
import System.Directory (removeFile)
import System.IO (IOMode(WriteMode), hClose, withBinaryFile)
import Graphics.PDF.LowLevel.Signature (setByteRange)
import qualified Graphics.PDF.File.Object as F
import qualified Graphics.PDF.File.Lexer as F (parseValue, runParser, valueWith)
import qualified Graphics.PDF.File.Write as F (renderPdfFile)
import Data.Word (Word64, Word8)
import Data.List (sortOn)
import qualified Data.Set as Set
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Control.Monad.Writer (tell)
import qualified Data.ByteString.Lazy.Char8 as LC

-- Create the PDF stream objects from the draw monads
createStreams :: Monad m => PDFT m ()
createStreams = do
    ls <- gets streams >>= return . IM.toList
    modifyStrict $ \s -> s {streams = IM.empty}
    pdfa <- gets pdfaSettings
    mapM_ (addStream pdfa . maybe id (const noInterpolation) pdfa) ls
 where
    -- PDF/A forbids image interpolation (ISO 19005-2, 6.2.8)
    noInterpolation (k,(p,(st,w)))
      | pdfDictMember (PDFName "Interpolate") (otherRsrcs st) =
          (k,(p,(st {otherRsrcs = insertInPdfDict (PDFName "Interpolate") (AnyPdfObject False) (otherRsrcs st)},w)))
      | otherwise = (k,(p,(st,w)))
    -- See Note [Annotations of a PDF/A document]
    pageAnnotations pdfa st = maybe id (const (map (AnyAnnotation . PrintedAnnotation))) pdfa (annots st)
    addStream pdfa (k,(p,(state',drawings))) = do
     -- the drawings in order: see Note [Memory of the drawings] in Graphics.PDF.Document
     let content = B.concat (reverse drawings)
     -- New reference for the stream
     r <- supply
     -- Run the drawing and get the new state (resource, annotation)
     --myBounds <- gets xobjectBound
     --cp <- gets currentPage
     --let (_,state',w') = runDrawing d (emptyEnvironment {streamId = r, xobjectb = myBounds, currentp = maybe Nothing (\(PDFReference x) -> Just x) cp })
     let ref = PDFReference r :: PDFReference MaybeLength
         
     -- Pattern NEEDS a resource entry even if empty otherwise don't work with acrobat reader
     -- Image DON'T want a resource entry if empty otherwise don't work with apple reader
     streamDict <- if (emptyResource (rsrc state')) && (not (pdfDictMember (PDFName "PatternType") (otherRsrcs state')))
       then do
         case p of
            -- Not linked to a page
            -- otherResource are entries specific to a special stream (like an XObject) so we return empty for a page
            Nothing -> return (otherRsrcs state') 
            -- Linked to a page
            Just pageRef -> do
                 setPageAnnotations (pageAnnotations pdfa state') pageRef
                 return emptyDictionary
       -- Some resource are needed by the stream
       else do
         rsrcRef <- indirectFonts (rsrc state') >>= addObject
         case p of
             -- Not linked to a page
             Nothing -> do                  
                  return $ (otherRsrcs state') `pdfDictUnion` (dictFromList  $ [entry "Resources" rsrcRef])
             -- Linked to a page
             Just pageRef -> do
                  setPageAnnotations (pageAnnotations pdfa state') pageRef
                  setPageResource rsrcRef pageRef
                  return emptyDictionary
              
     infos <- gets docInfo
     -- Resources to add to the stream
     -- We compress only if the stream is not using its own filter
     if (compressed infos) && (not (pdfDictMember (PDFName "Filter") streamDict))
       then do
         let w''' = compress content
             w'' = fromLazyByteString w'''
         updateObject (PDFReference k :: PDFReference PDFStream) (PDFStream w'' True (Left ref) streamDict)
         updateObject ref UnknownLength
         --updateObject ref (PDFLength (B.length w'''))
       else do
         updateObject (PDFReference k :: PDFReference PDFStream) (PDFStream (fromLazyByteString content) False (Left ref) streamDict)
         updateObject ref UnknownLength
         --updateObject ref (PDFLength (B.length . toLazyByteString $ w'))

-- Note [Font dictionaries are indirect objects]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A standard font or a Type 1 font gives its font dictionary as its
-- resource ('toRsrc'), because these fonts can be made before the document
-- has objects. If this dictionary was written in the resource dictionary
-- of each stream that uses the font, the document would contain it one
-- time for each stream:
--
--   /Font << /Courier << /BaseFont /Courier ... >> >>      not written
--   /Font << /Courier 4 0 R >>                             written
--
-- Thus each different font dictionary is written one time as an object,
-- and the resource dictionaries refer to it. Some tools also expect this:
-- pdffonts (poppler) reports an internal error for a font dictionary that
-- is not an indirect object.

-- | Replace the font dictionaries of a resource dictionary by references.
-- See Note [Font dictionaries are indirect objects]
indirectFonts :: Monad m => PDFResource -> PDFT m PDFResource
indirectFonts r = case M.lookup font_key (resources r) of
    Nothing -> return r
    Just (PDFDictionary fonts) -> do
      fonts' <- mapM indirect fonts
      return r {resources = M.insert font_key (PDFDictionary fonts') (resources r)}
  where
    font_key = PDFName "Font"
    indirect obj = do
      let bytes = toLazyByteString (toPDF obj)
      if not (LC.pack "<<" `B.isPrefixOf` bytes)
        then return obj
        else do
          known <- gets fontObjects
          case M.lookup bytes known of
            Just n -> return (AnyPdfObject (PDFReference n :: PDFReference PDFDictionary))
            Nothing -> do
              PDFReference n <- addObject obj
              modifyStrict $ \s -> s {fontObjects = M.insert bytes n (fontObjects s)}
              return (AnyPdfObject (PDFReference n :: PDFReference PDFDictionary))

-- Note [Annotations of a PDF/A document]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- ISO 19005-2, 6.3 has two rules for the annotations:
--
-- * each annotation has the /F key with the Print flag (4). An annotation
--   does not know that the document is PDF/A. Thus 'PrintedAnnotation' adds
--   the key to the entries that 'annotationDict' gives. The annotations of
--   a document that is not PDF/A are not changed.
-- * each annotation other than a link has an appearance stream (/AP). HPDF
--   writes none, so a text annotation is a problem of the document
--   ('checkAnnotations').

-- | An annotation with the Print flag.
-- See Note [Annotations of a PDF/A document]
newtype PrintedAnnotation = PrintedAnnotation AnyAnnotation

instance PdfObject PrintedAnnotation where
  toPDF = toPDF . dictFromList . annotationDict

instance PdfLengthInfo PrintedAnnotation where

instance AnnotationObject PrintedAnnotation where
  -- The annotation adds its object, with the other effects that it needs.
  -- Then the object gets the dictionary with the flag.
  addAnnotation printed@(PrintedAnnotation a) = do
    PDFReference r <- addAnnotation a
    updateObject (PDFReference r) printed
    return (PDFReference r)
  annotationType (PrintedAnnotation a) = annotationType a
  annotationContent (PrintedAnnotation a) = annotationContent a
  annotationRect (PrintedAnnotation a) = annotationRect a
  annotationDict (PrintedAnnotation a) =
    let entries = annotationDict a
    in if any ((== PDFName "F") . fst) entries then entries else entries ++ [entry "F" (PDFInteger 4)]

-- | Records a problem when a stream has a text annotation.
-- See Note [Annotations of a PDF/A document]
checkAnnotations :: PDF ()
checkAnnotations = do
  as <- gets (concatMap (annots . fst . snd) . IM.elems . streams)
  modifyStrict $ \s -> s {documentProblems = documentProblems s ++
    [ "PDF/A: a text annotation has no appearance stream" | any ((== PDFName "Text") . annotationType) as ]}

-- | Run the font finalizers with the glyph ids each font showed in any
-- stream. A finalizer may add streams of its own (font programs, CMaps),
-- which 'createStreams' then writes like any other.
finalizeFonts :: PDF ()
finalizeFonts = do
  fs <- gets fontFinalizers
  used <- gets (M.unionsWith IS.union . map (usedGlyphs . fst . snd) . IM.elems . streams)
  modifyStrict $ \s -> s {fontFinalizers = []}
  mapM_ ($ used) (reverse fs)

-- | Save all the pages and streams in the main object dictionary. Also
-- gives the reference of the indirect Info dictionary of a PDF/A document.
saveObjects :: Monad m => PDFT m (PDFReference PDFCatalog, Maybe (PDFReference PDFDictionary))
saveObjects  = do
  -- Write the fonts that wait for the glyphs the document used (subsets)
  liftPDF finalizeFonts
  pdfa <- gets pdfaSettings
  -- PDF/A: the fonts must be embedded, so none of the standard 14
  mapM_ (const (liftPDF checkEmbeddedFonts)) pdfa
  mapM_ (const (liftPDF checkAnnotations)) pdfa
  mapM_ (mapM_ problem . pdfaProblems) pdfa
  -- The output intent's ICC profile is a stream like the others
  intents <- case pdfa of
    Nothing -> return []
    Just a -> do
      let profile = pdfaOutputProfile a
      -- See Note [Device colours in a PDF/A document]
      -- See Note [Device colours in a PDF/A document]
      components <- case iccColorSpace profile of
        Right space | Just n <- lookup space [("RGB ", 3), ("CMYK", 4), ("GRAY", 1)] -> do
          liftPDF (checkDeviceColours space)
          return n
        Right space -> do
          problem ("PDF/A: the ICC profile of the output intent must be an RGB, CMYK or gray profile; its colour space is " ++ show space)
          return 3
        Left e -> do
          problem ("PDF/A: the ICC profile of the output intent cannot be used: " ++ e)
          return 3
      icc <- createContent (iccStream components profile) Nothing
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
  -- See Note [Keys of the EmbeddedFiles name tree]
  files <- gets (reverse . associatedFiles)
  let fileNames = [ n | (n, _, _) <- files ]
      fileSpecs = [ r | (_, _, r) <- files ]
      sorted = sortOn (textStringBytes . fst) (zip (uniqueKeys fileNames) fileSpecs)
  case pdfa of
    Nothing -> return ()
    Just a -> do
      when (pdfaPart a == 2 && not (null files)) $
        problem "PDF/A: a PDF/A-2 document cannot have embedded files; use part 3"
      mapM_ problem [ "PDF/A: the embedded file " ++ show n ++ " has no MIME type (" ++ show mime ++ ")"
                    | (n, mime, _) <- files, '/' `notElem` mime ]
  let fileEntries
        | null files = []
        | otherwise =
            [ entry "AF" fileSpecs
            , entry "Names" (dictFromList [entry "EmbeddedFiles" (dictFromList
                [entry "Names" (concat [[pdfTextString n, AnyPdfObject r] | (n, r) <- sorted])])])
            ]
  -- PDF/A: an indirect Info dictionary that agrees with the metadata
  -- (other documents keep theirs in the trailer, as before)
  info <- case pdfa of
    Nothing -> return Nothing
    Just a -> fmap Just . addObject . dictFromList $
         -- 'xmlText': the same text as in the metadata
         [ entry "Title" (pdfTextString (xmlText (pdfaTitle a))) | not (T.null (pdfaTitle a)) ]
      ++ [ entry "Author" (pdfTextString (xmlText (author infos))) | not (T.null (author infos)) ]
      ++ [ entry "Subject" (pdfTextString (xmlText (subject infos))) | not (T.null (subject infos)) ]
      ++ [ entry "Producer" (pdfTextString producerName) ]
  -- See Note [Signature of a document] in Graphics.PDF.Signature
  fields <- gets signatureFields
  let formEntries
        | null fields = []
        | otherwise =
            [ entry "AcroForm" (dictFromList
                [ entry "Fields" [ PDFReference f :: PDFReference PDFDictionary | f <- fields ]
                -- SignaturesExist and AppendOnly
                , entry "SigFlags" (PDFInteger 3)
                ]) ]
  -- Create the catalog
  cat <- addObject $ PDFCatalog oref pRef (pageMode infos) (pageLayout infos) (viewerPreferences infos)
           (metadata ++ intents ++ fileEntries ++ formEntries)
  modifyStrict $ \s -> s {catalog = cat}
  return (cat, info)
 where
  problem p = modifyStrict $ \s -> s {documentProblems = documentProblems s ++ [p]}
  iccStream :: Int -> B.ByteString -> Draw ()
  iccStream components profile = do
    modifyStrict $ \s -> s {otherRsrcs = dictFromList [entry "N" (PDFInteger components)]}
    tell (fromLazyByteString profile)

-- Note [Keys of the EmbeddedFiles name tree]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A reader does a binary search in a name tree, and compares the keys as
-- byte strings (PDF 32000-1, 7.9.6). Thus the keys must be:
--
-- * sorted by the bytes that are written ('textStringBytes'), which is not
--   the order of the texts. Example: "z.xml" is written as ASCII and
--   "\228.xml" as UTF-16BE with a byte order mark (FE FF ...), so "z.xml"
--   comes first, although 'z' > '\228' is False.
-- * different. The key of a file is its name. But two files can have the
--   same name: the name that a reader shows is in the file specification
--   (/F, /UF), and the key only finds the file. A file whose name is
--   already a key gets its name and a number as its key ('uniqueKeys').
--   Example: "a.xml", "a.xml" have the keys "a.xml", "a.xml (2)".
--
-- The /AF array keeps the order in which the files were embedded.

-- | A different key for each name, in the same order: the name, or the name
-- and a number when the name is already a key.
-- See Note [Keys of the EmbeddedFiles name tree]
uniqueKeys :: [T.Text] -> [T.Text]
uniqueKeys = snd . mapAccumL pick Set.empty
 where
  pick used n = let key = free used n (2 :: Int) n in (Set.insert key used, key)
  free used n k key
    | Set.member key used = free used n (k + 1) (n <> T.pack (" (" ++ show k ++ ")"))
    | otherwise = key

-- Note [Device colours in a PDF/A document]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A PDF/A document can use a device colour space only when the output
-- intent can give its colours a meaning (ISO 19005-2, 6.2.4.3):
--
--   profile of the output intent      device colour spaces that are permitted
--   RGB                               DeviceRGB, DeviceGray
--   CMYK                              DeviceCMYK, DeviceGray
--   gray                              DeviceGray
--
-- The caller gives the profile ('Graphics.PDF.Archive.pdfaOutputProfile'),
-- and HPDF reads its colour space in the header of the profile.
--
-- A stream uses a device colour space in two ways:
--
-- * with a colour operator ('Graphics.PDF.Colors.fillColor'): the stream
--   records it, see Note [Device colours] in Graphics.PDF.Draw;
-- * with a name in a dictionary: the colour space of an image, of a
--   shading, of a pattern or of a transparency group. These dictionaries
--   are the dictionary of the stream and its resources, and
--   'checkDeviceColours' looks for the three names in their bytes.
--
-- A content with no colour operator paints in black with DeviceGray, which
-- each profile permits.

-- | The device colour spaces that a stream uses.
-- See Note [Device colours in a PDF/A document]
streamDeviceColours :: DrawState -> DeviceColours
streamDeviceColours st = deviceColours st <> named (otherRsrcs st) <> named (rsrc st)
  where
    named :: PdfObject a => a -> DeviceColours
    named dict =
      let bytes = B.toStrict (toLazyByteString (toPDF dict))
          uses space = S.isInfixOf (S.pack (map (fromIntegral . fromEnum) space)) bytes
      in DeviceColours (uses "/DeviceGray") (uses "/DeviceRGB") (uses "/DeviceCMYK")

-- | Records a problem for each device colour space that the document uses
-- and that the profile of its output intent does not permit. The argument
-- is the colour space of the profile: @"RGB "@, @"CMYK"@ or @"GRAY"@.
--
-- See Note [Device colours in a PDF/A document]
checkDeviceColours :: String -> PDF ()
checkDeviceColours space = do
  used <- gets (mconcat . map (streamDeviceColours . fst . snd) . IM.elems . streams)
  let intent = case space of
        "RGB " -> "an RGB profile"
        "CMYK" -> "a CMYK profile"
        _ -> "a gray profile"
      problem used_space = "PDF/A: the document uses " ++ used_space ++ " (a colour, an image or a shading); the output intent is " ++ intent
  modifyStrict $ \s -> s {documentProblems = documentProblems s ++
    [ problem "DeviceRGB" | usesRgb used, space /= "RGB " ] ++
    [ problem "DeviceCMYK" | usesCmyk used, space /= "CMYK" ]}

-- | Records a problem for each font a stream uses whose program is not in
-- the document (see 'fontEmbedded'): the document cannot be PDF/A.
checkEmbeddedFonts :: PDF ()
checkEmbeddedFonts = do
  fonts <- gets (Set.toList . Set.unions . map (unembeddedFonts . fst . snd) . IM.elems . streams)
  modifyStrict $ \s -> s {documentProblems = documentProblems s ++
    [ "PDF/A: the font " ++ n ++ " is not embedded; use an embedded (TrueType) font" | n <- fonts ]}

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
documentId bytes = S.pack (word64Bytes (fnv fnv1a64OffsetBasis) ++ word64Bytes (fnv 0x84222325cbf29ce4))
 where
  fnv :: Word64 -> Word64
  fnv h0 = B.foldl' (\h w -> fnv1a64Step h (fromIntegral w)) h0 bytes
  word64Bytes :: Word64 -> [Word8]
  word64Bytes w = [ fromIntegral (w `div` (256 ^ k)) | k <- [7, 6 .. 0 :: Int] ]

-- | Write PDF objects in the TOC
writeObjectsAndCreateToc :: [Builder] -- ^ List of objects each object being already converted to a bytestring
                          -> (Int,Int64,[Builder])
writeObjectsAndCreateToc l = 
   let lengths =  drop 1 . scanl (\len obj -> len + (B.length . toLazyByteString $ obj)) 0 $ l
       createEntry x = serialize $ (printf "%010d 00000 n \n" ((fromIntegral x)::Integer) :: String)
       entries = map createEntry (init lengths) 
   in
   (length l,last lengths,entries)
-- foldr writeObject (0,0::Int64,[]) l where
-- writeObject obj (nb,len,toc) = (nb+1,len + (B.length . toLazyByteString $ obj),(serialize $ (printf "%010d 00000 n \n" ((fromIntegral len)::Integer))) : toc)
generateStreams :: PDFReference PDFCatalog -> TrailerInfo -> Int -> Int64 -> [Builder]
                -> [Builder] -> B.ByteString
generateStreams root di !nb !totalLen ens [] = 
  let entries = reverse (drop 1 ens)  
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

defaultPdfSettings :: PdfBuilder
defaultPdfSettings = 
  PdfBuilder {
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
           , fontObjects = M.empty
           , pdfaSettings = Nothing
           , associatedFiles = []
           , signatureValues = []
           , signatureFields = []
           , documentProblems = []
           }

-- | Builder of a new, empty document
emptyPdfBuilder :: PDFDocumentInfo
                -> PDFRect -- ^ Default size for a page
                -> PdfBuilder
emptyPdfBuilder infos rect = defaultPdfSettings {defaultRect = rect, docInfo = infos}

-- | Run a 'PDF' action on a document builder and return the new builder
stepPDF :: PDF a -> PdfBuilder -> (a, PdfBuilder)
stepPDF m s = runIdentity (stepPDFT m s)

-- | Run a 'PDFT' action on a document builder and return the new builder
stepPDFT :: PDFT m a -> PdfBuilder -> m (a, PdfBuilder)
stepPDFT m s = runStateT (unPDFT m) s

-- Note [Rendering a document that has problems]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A document can ask for more than HPDF can give it: for example a PDF/A
-- document that uses a standard 14 font, which is not embedded. Such
-- problems are found when the document is saved ('documentProblems'). A
-- document without a page is a problem too.
--
-- 'buildPdfE' and the other functions whose name ends in @E@ return the
-- problems as a 'Left'. The functions without the @E@ cannot return them:
-- the pure ones ('buildPdf', 'renderPdf') call 'error', and the ones in
-- 'IO' ('writePdf', 'writePdfT') raise an 'IOError' before they write the
-- file. No function writes a document that has problems.

-- | Render the document of a builder.
--
-- The builder is not modified: the document can still be extended after this
-- call, and rendered again.
--
-- Calls 'error' when the document has problems: see 'buildPdfE'.
buildPdf :: PdfBuilder -> B.ByteString
buildPdf = either (error . problemsMessage) id . buildPdfE

-- | Like 'buildPdf', or the reasons the document is not what it was asked to
-- be (for example a PDF/A document using a font that is not embedded).
buildPdfE :: PdfBuilder -> Either [String] B.ByteString
buildPdfE s
  -- the page tree of a document cannot be empty
  | Pages t <- pages s, PT.size t == 0 = Left ["the document has no page"]
  | otherwise =
      let ((root, info), s') = stepPDF saveObjects s
      in case documentProblems s' of
           [] -> Right (render_document root info s')
           ps -> Left ps

-- | Render a document that has no problems.
-- See Note [Object streams of a document]
render_document :: PDFReference PDFCatalog -> Maybe (PDFReference PDFDictionary) -> PdfBuilder -> B.ByteString
render_document root info s
  | objectStreams (docInfo s) && compressed (docInfo s)
  , Right file <- document_file root info s = F.renderPdfFile file
  | otherwise = renderDocument root info s

-- Note [Object streams of a document]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- With 'objectStreams' and 'compressed' (the default), the objects of a
-- document that are not streams are in object streams, and the document
-- has a cross-reference stream (an object stream is compressed, and a
-- document that is not compressed is for a person to read): see Note
-- [Object streams of the writer] in Graphics.PDF.File.Write.
--
-- 'document_file' makes a 'F.PdfFile' from the objects of the builder,
-- and the writer of Graphics.PDF.File writes it. Thus one writer makes
-- the object streams, also for the encryption
-- ('Graphics.PDF.Encryption.encryptPdf'). Each object is rendered alone,
-- and the lexer of Graphics.PDF.File reads it again:
--
-- * the data of a stream is not read: it is the bytes between
--   "\nstream\n" and "\nendstream" ('PDFStream');
-- * the nesting of the values has no depth limit: the builder makes the
--   values, and a deep nesting is correct (nested functions);
-- * the /Length objects of the streams are not in the file: the writer
--   gives each stream a direct /Length.
--
-- The lexer cannot read a number of more than 255 digits (Note [Limits
-- of the reader] in Graphics.PDF.File.Parse), and the builder writes a
-- real of 1e255 or more with all its digits. Such a document keeps its
-- cross-reference table.

-- | The document of a builder, as the objects of a PDF file.
-- See Note [Object streams of a document]
document_file :: PDFReference PDFCatalog -> Maybe (PDFReference PDFDictionary) -> PdfBuilder -> Either String F.PdfFile
document_file root info s = do
  objs <- IM.traverseWithKey file_object (IM.withoutKeys (objects s) length_objects)
  trailer <- F.parseValue (B.toStrict (toLazyByteString (toPDF (PDFTrailer 0 root trailer_info))))
  case trailer of
    F.Dict d -> Right (F.PdfFile version (M.delete (S8.pack "Size") d) objs Nothing)
    _ -> Left "the trailer is not a dictionary"
 where
  version = maybe (1, 5) (const (1, 7)) (pdfaSettings s)
  length_objects = IS.fromList [ n | o <- IM.elems (objects s), Just (_, PDFReference n) <- [pdfLengthInfo o] ]
  trailer_info = case info of
    Nothing -> InlineInfo (docInfo s)
    Just i -> ArchiveInfo i (documentId (toLazyByteString (mconcat (encoded_objects s))))
  file_object k o = either (\e -> Left ("object " ++ show k ++ ": " ++ e)) (Right . (,) 0) (object_of (B.toStrict (toLazyByteString (toPDF o))))
  object_of bytes = do
    (v, end) <- F.runParser (F.valueWith maxBound) bytes 0
    let start = end + S.length stream_start
        size = S.length bytes - start - S.length stream_end
    case v of
      F.Dict d
        | stream_start `S.isPrefixOf` S.drop end bytes, stream_end `S.isSuffixOf` bytes, size >= 0 ->
            Right (F.Stream (M.insert (S8.pack "Length") (F.Integer (fromIntegral size)) d) (S.take size (S.drop start bytes)))
      _ | end == S.length bytes -> Right (F.Plain v)
        | otherwise -> Left "data after the value"
  stream_start = S8.pack "\nstream\n"
  stream_end = S8.pack "\nendstream"

-- | The message of the exception raised for a document that has problems.
problemsMessage :: [String] -> String
problemsMessage ps = intercalate "\n" ("HPDF: the document cannot be rendered:" : map ("  " ++) ps)

-- | The objects of a document, each with its number, and the /Length
-- objects of the streams.
encoded_objects :: PdfBuilder -> [Builder]
encoded_objects s = snd (mapAccumL encodeAnObject objs (IM.keys objs))
 where
  objs = objects s
  objectEncoding (x,a) = toPDF . PDFReferencedObject (fromIntegral $! x) $ a
  encodeAnObject im k = case IM.lookup k im of
    -- 'k' is a key of 'objs', and no object is removed
    Nothing -> (im, mempty)
    Just o ->
      let mim = do
              (l, PDFReference ref) <- pdfLengthInfo o
              return $ IM.insert ref (AnyPdfObject (KnownLength l)) im
      in (fromMaybe im mim, objectEncoding (k,o))

-- | Render the objects of a complete document
renderDocument :: PDFReference PDFCatalog -> Maybe (PDFReference PDFDictionary) -> PdfBuilder -> B.ByteString
renderDocument root info s =
      let header = case pdfaSettings s of
            -- PDF/A: a comment line of four bytes above 127 after the header
            Just _ -> fromLazyByteString (LC.pack "%PDF-1.7\n%\xE2\xE3\xCF\xD3\n")
            Nothing -> serialize "%PDF-1.5\n"
          objs = objects s
          objectContents = header : encoded_objects s
          (_nb, _len, _toc) = writeObjectsAndCreateToc objectContents
          trailerInfo = case info of
            Nothing -> InlineInfo (docInfo s)
            Just i -> ArchiveInfo i (documentId (toLazyByteString (mconcat (encoded_objects s))))
          rendered = generateStreams root trailerInfo 0 0 [] objectContents
          -- offset of an object in the document
          offsetOf k = sum [ B.length (toLazyByteString o)
                           | (k', o) <- zip (0 : IM.keys objs) objectContents, k' < k ]
      in
      -- See Note [Layout of a signature dictionary] in Graphics.PDF.LowLevel.Signature
      foldr (setByteRange . offsetOf) rendered (signatureValues s)

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

-- | Render a PDF document as a lazy bytestring
--
-- Calls 'error' when the document has problems: see 'renderPdfE'.
renderPdf :: PDFDocumentInfo
          -> PDFRect -- ^ Default size for a page
          -> PDF a  -- ^ PDF action
          -> B.ByteString
renderPdf infos rect m = runIdentity (renderPdfT infos rect m)

-- | Like 'renderPdf', or the reasons the document is not what it was asked to
-- be (for example a PDF/A document using a font that is not embedded).
renderPdfE :: PDFDocumentInfo
           -> PDFRect -- ^ Default size for a page
           -> PDF a  -- ^ PDF action
           -> Either [String] B.ByteString
renderPdfE infos rect m = runIdentity (renderPdfTE infos rect m)

-- | Render a PDF document as a lazy bytestring, running the PDF action in
-- the base monad @m@ (see 'PDFT')
--
-- Calls 'error' when the document has problems: see 'renderPdfTE'.
renderPdfT :: Monad m
           => PDFDocumentInfo
           -> PDFRect -- ^ Default size for a page
           -> PDFT m a  -- ^ PDF action
           -> m B.ByteString
renderPdfT infos rect m = either (error . problemsMessage) id <$> renderPdfTE infos rect m

-- | Like 'renderPdfT', or the reasons the document is not what it was asked
-- to be.
renderPdfTE :: Monad m
            => PDFDocumentInfo
            -> PDFRect -- ^ Default size for a page
            -> PDFT m a  -- ^ PDF action
            -> m (Either [String] B.ByteString)
renderPdfTE infos rect m = do
  (_, s) <- stepPDFT m (emptyPdfBuilder infos rect)
  return (buildPdfE s)

-- | Write a PDF document to a file
--
-- Raises an 'IOError' before the file is written when the document has
-- problems: see 'renderPdfE'.
writePdf :: String -- ^ Name of the PDF document
         -> PDFDocumentInfo
         -> PDFRect -- ^ Default size for a page
         -> PDF a  -- ^ PDF action
         -> IO ()
writePdf filename infos rect m = case renderPdfE infos rect m of
  Left ps -> ioError (userError (problemsMessage ps))
  Right bs -> write_document filename bs

-- | Write a PDF document to a file, running the PDF action in the base
-- monad @m@ (see 'PDFT')
--
-- Raises an 'IOError' before the file is written when the document has
-- problems: see 'renderPdfE'.
writePdfT :: MonadIO m
          => String -- ^ Name of the PDF document
          -> PDFDocumentInfo
          -> PDFRect -- ^ Default size for a page
          -> PDFT m a  -- ^ PDF action
          -> m ()
writePdfT filename infos rect m = do
  r <- renderPdfTE infos rect m
  liftIO (either (ioError . userError . problemsMessage) (write_document filename) r)

-- | Write the bytes of a document to a file. The bytes are evaluated
-- while they are written, and this can raise an exception (see Note
-- [Numbers with five decimals] in Graphics.PDF.LowLevel.Serializer): the
-- file is then removed, so that no incomplete document stays.
--
-- The file is removed only if it was opened: if it cannot be opened, a
-- file that was there before is not touched.
write_document :: FilePath -> B.ByteString -> IO ()
write_document path bs =
  withBinaryFile path WriteMode $ \h ->
    B.hPut h bs `onException` (hClose h >> (removeFile path `catch` ignore))
  where
    ignore :: IOException -> IO ()
    ignore _ = return ()

{-# DEPRECATED runPdf "Use writePdf" #-}
runPdf :: String -> PDFDocumentInfo -> PDFRect -> PDF a -> IO ()
runPdf = writePdf

{-# DEPRECATED pdfByteString "Use renderPdf" #-}
pdfByteString :: PDFDocumentInfo -> PDFRect -> PDF a -> B.ByteString
pdfByteString = renderPdf


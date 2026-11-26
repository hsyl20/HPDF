{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Digital signatures.
--
-- 'addSignature' writes the signature field and reserves the space for
-- the signature. 'signPdfWith' signs the rendered document with a key and
-- its certificate:
--
-- > doc = do
-- >   page <- addPage Nothing
-- >   ...
-- >   addSignature page signature { sigName = "A. Signer", sigReason = "Approval" }
-- >
-- > main = do
-- >   Right signer <- readSigner "key.pem" "certificates.pem"
-- >   signed <- signPdfWith signer (renderPdf standardDocInfo rect doc)
-- >   either fail (B.writeFile "signed.pdf") signed
--
-- A key that the program cannot read (a smart card, an HSM, a signing
-- service) can also be used: 'signPdf' gives the bytes to sign to a
-- function of the caller, which returns the signature.
--
-- Limits:
--
-- * a rendered document has one signature, and the signature field is not
--   visible. 'addSignatureField' adds the next signatures to the file, in
--   incremental updates;
-- * a PDF\/A document ("Graphics.PDF.Archive") with a signature field
--   conforms after it is signed. Before that, the signature is empty.
---------------------------------------------------------
module Graphics.PDF.Signature
  ( -- * Signature field
    Signature(..)
  , SignatureFormat(..)
  , signature
  , addSignature
  , addSignatureField
    -- * Signing
  , Signer(..)
  , readSigner
  , parseSigner
  , signPdfWith
  , signPdf
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Control.Monad (when)
import Control.Monad.State (gets)
import Data.Bits ((.|.))
import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types (CurveName, Point(..), curveSizeBits, getCurveByName)
import Crypto.PubKey.RSA (private_pub)
import Crypto.Random (MonadRandom)
import Data.X509 ( PrivKey(..), PrivKeyEC(..), PubKey(..), PubKeyEC(..), SerializedPoint(..), SignedCertificate
                 , certPubKey, encodeSignedObject, getCertificate )
import Data.X509.Memory (readKeyFileFromMemory, readSignedObjectFromMemory)
import Graphics.PDF.LowLevel.CMS (cmsSignature)
import Graphics.PDF.Annotation (newAnnotation)
import Graphics.PDF.Archive (pdfTextString, textStringBytes)
import qualified Graphics.PDF.File as F
import Graphics.PDF.Document (drawWithPage)
import Graphics.PDF.Draw
import Graphics.PDF.LowLevel.Signature
import Graphics.PDF.LowLevel.Serializer (serialize)
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Pages (addObject)

-- Note [Signature of a document]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A signed document has three objects (PDF 32000-1, 12.7.4.5 and 12.8):
--
--   catalog   /AcroForm << /Fields [field] /SigFlags 3 >>
--   field     a widget annotation of the page: /FT /Sig /V value
--   value     the signature dictionary: /ByteRange, /Contents, /Name ...
--
-- * The field has no appearance: its rectangle is empty, as PDF/A permits
--   for an annotation that has no appearance stream.
-- * The value of /Contents is a CMS (PKCS #7) object in DER, made by the
--   caller on the bytes of the document. See Note [Layout of a signature
--   dictionary] in Graphics.PDF.LowLevel.Signature.
-- * A rendered document has one signature at most. A second signature is
--   in an incremental update of the file: see Note [Signature of a file
--   that is read].

-- | Format of the signature, the /SubFilter of the signature dictionary
data SignatureFormat
  = Pkcs7Detached -- ^ @adbe.pkcs7.detached@: a detached CMS signature
  | CadesDetached -- ^ @ETSI.CAdES.detached@: a detached CAdES signature (PAdES)
  deriving (Eq, Show)

-- | A signature of the document
data Signature = Signature
  { sigFieldName :: T.Text -- ^ Name of the signature field. It cannot be empty
  , sigName :: T.Text -- ^ Name of the signer. Not written if empty
  , sigReason :: T.Text -- ^ Reason for the signature. Not written if empty
  , sigLocation :: T.Text -- ^ Place of the signature. Not written if empty
  , sigContactInfo :: T.Text -- ^ How to contact the signer. Not written if empty
  , sigDate :: T.Text -- ^ Time of the signature as a PDF date, for example @D:20260131120000Z@. Not written if empty
  , sigFormat :: SignatureFormat
  , sigSize :: Int -- ^ Number of bytes to reserve for the signature. A signature with its certificates must fit
  } deriving (Eq, Show)

-- | A signature field named @Signature1@ for a detached CMS signature of
-- 8192 bytes at most
signature :: Signature
signature = Signature
  { sigFieldName = "Signature1"
  , sigName = ""
  , sigReason = ""
  , sigLocation = ""
  , sigContactInfo = ""
  , sigDate = ""
  , sigFormat = Pkcs7Detached
  , sigSize = 8192
  }

-- | The signature dictionary
newtype SignatureValue = SignatureValue Signature

instance PdfObject SignatureValue where
  toPDF (SignatureValue sig) = signatureValue subfilter (sigSize sig) (mconcat (map entry_bytes others))
    where
      subfilter = case sigFormat sig of
        Pkcs7Detached -> "adbe.pkcs7.detached"
        CadesDetached -> "ETSI.CAdES.detached"
      others = [ (k, pdfTextString v)
               | (k, v) <- [ ("Name", sigName sig), ("Reason", sigReason sig), ("Location", sigLocation sig)
                           , ("ContactInfo", sigContactInfo sig), ("M", sigDate sig) ]
               , not (T.null v) ]
      entry_bytes (k, v) = mconcat [toPDF (PDFName k), serialize ' ', toPDF v, serialize '\n']

instance PdfLengthInfo SignatureValue where

-- | The signature field: a widget annotation of a page
data SignatureField = SignatureField T.Text (PDFReference SignatureValue) (PDFReference PDFPage)

instance PdfObject SignatureField where
  toPDF = toPDF . dictFromList . annotationDict

instance PdfLengthInfo SignatureField where

instance AnnotationObject SignatureField where
  -- 'saveObjects' needs the reference of the field for /AcroForm
  addAnnotation f = do
    PDFReference r <- addObject f
    modifyStrict $ \s -> s {signatureFields = signatureFields s ++ [r]}
    return (PDFReference r)
  annotationType _ = PDFName "Widget"
  annotationContent (SignatureField n _ _) = pdfTextString n
  annotationRect _ = [0, 0, 0, 0]
  annotationDict f@(SignatureField n v page) =
    [ entry "Type" (PDFName "Annot")
    , entry "Subtype" (annotationType f)
    , entry "FT" (PDFName "Sig")
    , (PDFName "T", pdfTextString n)
    , entry "V" v
    , entry "Rect" (annotationRect f)
    -- Print and Locked
    , entry "F" (PDFInteger 132)
    , entry "P" page
    ]

-- | Add a signature field to a page of the document. The field is not
-- visible. The rendered document has the space for the signature, and
-- 'signPdf' writes the signature.
--
-- A document can have one signature. A second call is a problem of the
-- document: see 'Graphics.PDF.renderPdfE'.
--
-- See Note [Signature of a document]
addSignature :: Monad m => PDFReference PDFPage -> Signature -> PDFT m ()
addSignature page sig = do
    declared <- gets signatureValues
    mapM_ problem $
         [ "a document can have one signature only" | not (null declared) ]
      ++ [ "the name of a signature field cannot be empty" | T.null (sigFieldName sig) ]
      ++ [ "the size reserved for a signature must be more than 0 bytes" | sigSize sig <= 0 ]
    PDFReference v <- addObject (SignatureValue sig)
    modifyStrict $ \s -> s {signatureValues = signatureValues s ++ [v]}
    drawWithPage page (newAnnotation (SignatureField (sigFieldName sig) (PDFReference v) page))
  where
    problem :: Monad m => String -> PDFT m ()
    problem p = modifyStrict $ \s -> s {documentProblems = documentProblems s ++ [p]}

-- | Sign a rendered document that has a signature field ('addSignature').
--
-- The function of the caller gets the bytes to sign: the complete document
-- without the value of the signature. It gives a detached CMS (PKCS #7)
-- signature of these bytes in DER.
--
-- 'Left' gives the reason the document cannot be signed: it has no
-- signature field, it is already signed, or the signature is longer than
-- 'sigSize'.
signPdf :: Monad m
        => (B.ByteString -> m S.ByteString) -- ^ Signs the bytes
        -> B.ByteString -- ^ Rendered document
        -> m (Either String B.ByteString)
signPdf sign doc = case findSignature doc of
    Left e -> return (Left e)
    Right (a, b) -> do
      sig <- sign (B.take a doc `B.append` B.drop b doc)
      return (writeSignature (a, b) sig doc)

-- | A key and its certificates
data Signer = Signer
  { signerKey :: PrivKey -- ^ An RSA key, or an EC key on a named curve
  , signerCertificates :: [SignedCertificate] -- ^ The certificate of the key, then the other certificates of its chain
  }

-- | Read a signer from two PEM files: the private key (not encrypted), and
-- the certificate of the key followed by the other certificates of its
-- chain.
readSigner :: FilePath -- ^ Key
           -> FilePath -- ^ Certificates
           -> IO (Either String Signer)
readSigner key_path certs_path = parseSigner <$> S.readFile key_path <*> S.readFile certs_path

-- | Like 'readSigner', with the contents of the two PEM files.
parseSigner :: S.ByteString -- ^ Key
            -> S.ByteString -- ^ Certificates
            -> Either String Signer
parseSigner key_pem certs_pem =
    case (readKeyFileFromMemory key_pem, readSignedObjectFromMemory certs_pem) of
      ([], _) -> Left "no private key found (the key must be in PEM and not encrypted)"
      (_ : _ : _, _) -> Left "more than one private key found"
      (_, []) -> Left "no certificate found (the certificates must be in PEM)"
      ([key], certs@(cert : _))
        | key_agrees key (certPubKey (getCertificate cert)) -> Right (Signer key certs)
        | otherwise -> Left "the first certificate is not the certificate of the key"
  where
    key_agrees (PrivKeyRSA k) (PubKeyRSA p) = private_pub k == p
    key_agrees (PrivKeyRSA _) _ = False
    key_agrees (PrivKeyEC (PrivKeyEC_Named curve d)) (PubKeyEC (PubKeyEC_Named curve' (SerializedPoint q))) =
      curve == curve' && Just q == uncompressed_point curve d
    key_agrees (PrivKeyEC _) _ = False
    -- 'signPdfWith' gives the error for the other types of key
    key_agrees _ _ = True

-- | The public point of a private EC key, in the form of a certificate:
-- 0x04, then X and Y with the size of the curve.
uncompressed_point :: CurveName -> Integer -> Maybe S.ByteString
uncompressed_point name d =
    case generateQ curve d of
      Point x y -> Just (S.concat [S.singleton 4, coordinate x, coordinate y])
      PointO -> Nothing
  where
    curve = getCurveByName name
    size = (curveSizeBits curve + 7) `div` 8
    coordinate v = S.pack [ fromIntegral (v `div` (256 ^ i)) | i <- [size - 1, size - 2 .. 0] ]

-- Note [Signature of a file that is read]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A second signature of a document is in an incremental update of the
-- file (Note [Incremental update] in Graphics.PDF.File.Update): the first
-- signature signs the old bytes, which do not change.
--
-- 'addSignatureField' adds the objects of Note [Signature of a document]
-- to a 'F.PdfFile': the signature dictionary, and the field on a page.
-- The update has the new objects and the changed objects, and
-- 'signPdfWith' signs all the bytes of the file with the update.
--
-- * The page gets an /Annots array of its own, with the field: an array
--   that is an object can be the /Annots of other pages too, or the
--   /Fields of the form.
-- * The /Fields of the /AcroForm get the field, in its object when it is
--   a reference. /SigFlags gets the bits 1 and 2.
-- * A reference to an object that the file does not have is null, as
--   for 'F.resolveValue'.
-- * The name of the field is a partial name: it has no ".", and no field
--   of the form has it (the names are text: PDFDocEncoding, UTF-16BE or
--   UTF-8).
-- * A file with a signature dictionary that is not signed gives 'Left':
--   'signPdfWith' signs one signature, the one whose byte range ends at
--   the end of the file. The other one could not be signed later, as its
--   signature would change the bytes of the first one.
--
-- 'F.updatePdfFile' respects the restrictions of the file: the
-- permissions, and a DocMDP certification with no changes (Note
-- [Restrictions of an update] in Graphics.PDF.File.Update). A file that
-- the user password opened needs the permission to modify it, also when
-- it permits to fill forms and to sign them: use the owner password, or
-- 'F.updateIgnorePermissions'.

-- | Add a signature field to a page of a file that the reader read, for
-- an incremental update. The field is not visible. Then
-- 'F.updatePdfFile' writes the update, and 'signPdfWith' signs it:
--
-- > Right source <- F.readPdfSource "" "signed.pdf"
-- > let Right file = addSignatureField signature { sigFieldName = "Signature2" } 1 (F.sourceFile source)
-- > Right update <- F.updatePdfFile source file
-- > signed <- signPdfWith signer update
--
-- 'Left' for a name of a field that the file already has, an empty name
-- or a name with a ".", a size of 0, a page that the file does not have,
-- and a file with a signature field that is not signed.
--
-- See Note [Signature of a file that is read]
addSignatureField :: Signature
                  -> Int -- ^ The number of the page, from 1 ('F.pdfPages')
                  -> F.PdfFile
                  -> Either String F.PdfFile
addSignatureField sig page_number file = do
    -- See Note [Signature of a file that is read]
    let name = sigFieldName sig
    when (T.null name) $ Left "the name of a signature field cannot be empty"
    when (T.any (== '.') name) $ Left "the name of a signature field cannot have a \".\""
    when (sigSize sig <= 0) $ Left "the size reserved for a signature must be more than 0 bytes"
    when (any unsigned (IM.elems (F.pdfObjects file))) $
      Left "the file has a signature field that is not signed: sign it first (a signature is for one field)"
    catalog_ref <- case F.dictLookup "Root" (F.pdfTrailer file) of
      Just (F.Ref n _) -> Right n
      _ -> Left "the trailer has no /Root"
    root_dict <- case F.getObject file catalog_ref of
      Just (F.Plain (F.Dict d)) -> Right d
      _ -> Left "the catalog is not a dictionary"
    (page, page_g) <- case drop (page_number - 1) (F.pdfPages file) of
      p : _ | page_number >= 1 -> Right p
      _ -> Left ("the file has no page " ++ show page_number)
    page_dict <- case F.getObject file page of
      Just (F.Plain (F.Dict d)) -> Right d
      _ -> Left "the page is not a dictionary"
    let fields = case resolve <$> (F.dictLookup "AcroForm" root_dict >>= dict_of >>= F.dictLookup "Fields") of
          Just (F.Array fs) -> fs
          _ -> []
        field_name f = case resolve <$> (dict_of f >>= F.dictLookup "T") of
          Just (F.String s) -> Just (text_string s)
          _ -> Nothing
    when (any ((== Just name) . field_name) fields) $
      Left ("the file already has a field named " ++ T.unpack name)
    page_annots <- case resolve <$> F.dictLookup "Annots" page_dict of
      Just (F.Array xs) -> Right xs
      Just F.Null -> Right []
      Nothing -> Right []
      Just _ -> Left "the /Annots of the page is not an array"
    let objs0 = F.pdfObjects file
        value_n = maybe 1 ((+ 1) . fst) (IM.lookupMax objs0)
        field_n = value_n + 1
        field = F.Ref field_n 0
        page' = F.Dict (M.insert "Annots" (F.Array (page_annots ++ [field])) page_dict)
        added = IM.insert page (page_g, F.Plain page')
              $ IM.insert field_n (0, F.Plain (F.Dict (M.fromList (field_entries page page_g value_n))))
              $ IM.insert value_n (0, F.Plain (F.Dict (M.fromList value_entries))) objs0
        append v = case v of
          Nothing -> Right (F.Array [field])
          Just (F.Array xs) -> Right (F.Array (xs ++ [field]))
          Just _ -> Left "the /Fields of the form is not an array"
        flags v = case v of
          Just (F.Integer f) -> Right (F.Integer (f .|. 3))
          _ -> Right (F.Integer 3)
    objs0' <- at_path [ "AcroForm", "Fields" ] append catalog_ref added
            >>= at_path [ "AcroForm", "SigFlags" ] flags catalog_ref
    Right file { F.pdfObjects = objs0' }
  where
    resolve = F.resolveValue file
    dict_of v = case resolve v of
      F.Dict d -> Just d
      _ -> Nothing
    unsigned (_, o) = case o of
      F.Plain (F.Dict d) | F.isSignatureDict d, Just (F.String s) <- M.lookup "Contents" d -> not (S.null s) && S.all (== 0) s
      _ -> False
    subfilter = case sigFormat sig of
      Pkcs7Detached -> "adbe.pkcs7.detached"
      CadesDetached -> "ETSI.CAdES.detached"
    -- See Note [Signature of a document]
    value_entries =
      [ ("Type", F.Name "Sig"), ("Filter", F.Name "Adobe.PPKLite"), ("SubFilter", F.Name subfilter)
      , ("ByteRange", F.Array (replicate 4 (F.Integer 0))), ("Contents", F.String (S.replicate (sigSize sig) 0)) ]
      ++ [ (k, F.String (textStringBytes v))
         | (k, v) <- [ ("Name", sigName sig), ("Reason", sigReason sig), ("Location", sigLocation sig)
                     , ("ContactInfo", sigContactInfo sig), ("M", sigDate sig) ]
         , not (T.null v) ]
    field_entries page page_g value_n =
      [ ("Type", F.Name "Annot"), ("Subtype", F.Name "Widget"), ("FT", F.Name "Sig")
      , ("T", F.String (textStringBytes (sigFieldName sig))), ("V", F.Ref value_n 0)
      , ("Rect", F.Array (replicate 4 (F.Integer 0))), ("F", F.Integer 132), ("P", F.Ref page page_g) ]

-- | The text of a text string (PDF 32000-1, 7.9.2.2): UTF-16BE or UTF-8
-- with a byte order mark, else PDFDocEncoding (as ISO Latin 1, which is
-- the same for the characters of a name of a field)
text_string :: S.ByteString -> T.Text
text_string s
  | Just rest <- S.stripPrefix (S.pack [0xFE, 0xFF]) s = TE.decodeUtf16BEWith lenientDecode rest
  | Just rest <- S.stripPrefix (S.pack [0xEF, 0xBB, 0xBF]) s = TE.decodeUtf8With lenientDecode rest
  | otherwise = T.pack (map (toEnum . fromIntegral) (S.unpack s))

-- | Change the value at a path of keys from the dictionary of an object.
-- A value that is a reference to an object changes in this object. A
-- reference to an object that the file does not have is null. A
-- reference that comes again (a loop of references) gives 'Left'.
at_path :: [S.ByteString] -> (Maybe F.Value -> Either String F.Value) -> Int
        -> IM.IntMap (Int, F.Object) -> Either String (IM.IntMap (Int, F.Object))
at_path path f n objs0 = case IM.lookup n objs0 of
    Just (g, F.Plain v) -> do
      (v', objs0') <- go (IS.singleton n) objs0 (Just v) path
      Right (IM.insert n (g, F.Plain v') objs0')
    _ -> Left ("the object " ++ show n ++ " is not a dictionary")
  where
    go visited objs (Just (F.Ref r g)) ks
      | IS.member r visited = Left ("the references of the object " ++ show r ++ " make a loop")
      | Just (g', F.Plain v) <- IM.lookup r objs, g == g' = do
          (v', objs') <- go (IS.insert r visited) objs (Just v) ks
          Right (F.Ref r g, IM.insert r (g, F.Plain v') objs')
      | Just (g', _) <- IM.lookup r objs, g == g' = Left ("the object " ++ show r ++ " is a stream")
      | otherwise = go visited objs Nothing ks
    go visited objs (Just F.Null) ks = go visited objs Nothing ks
    go _ objs v [] = (\x -> (x, objs)) <$> f v
    go visited objs v (k : ks) = case v of
      Just (F.Dict d) -> do
        (x, objs') <- go visited objs (F.dictLookup k d) ks
        Right (F.Dict (M.insert k x d), objs')
      Nothing -> do
        (x, objs') <- go visited objs Nothing ks
        Right (F.Dict (M.singleton k x), objs')
      Just _ -> Left ("the /" ++ C.unpack k ++ " is not in a dictionary")

-- | Sign a rendered document that has a signature field ('addSignature')
-- with a key and its certificates. The signature is a detached CMS
-- signature with SHA-256. It has the certificates of the signer, and it
-- agrees with the two formats of 'sigFormat'.
--
-- 'Left' gives the reason the document cannot be signed: see 'signPdf'.
signPdfWith :: MonadRandom m => Signer -> B.ByteString -> m (Either String B.ByteString)
signPdfWith (Signer key certs) doc = case findSignature doc of
    Left e -> return (Left e)
    Right range@(a, b) -> do
      sig <- cmsSignature key (map encodeSignedObject certs) (B.take a doc `B.append` B.drop b doc)
      return (sig >>= \bytes -> writeSignature range bytes doc)

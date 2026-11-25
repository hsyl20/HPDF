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
-- * a document has one signature, and the signature field is not visible;
-- * a PDF\/A document ("Graphics.PDF.Archive") with a signature field
--   conforms after it is signed. Before that, the signature is empty.
---------------------------------------------------------
module Graphics.PDF.Signature
  ( -- * Signature field
    Signature(..)
  , SignatureFormat(..)
  , signature
  , addSignature
    -- * Signing
  , Signer(..)
  , readSigner
  , parseSigner
  , signPdfWith
  , signPdf
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import Control.Monad.State (gets)
import Crypto.PubKey.ECC.Generate (generateQ)
import Crypto.PubKey.ECC.Types (CurveName, Point(..), curveSizeBits, getCurveByName)
import Crypto.PubKey.RSA (private_pub)
import Crypto.Random (MonadRandom)
import Data.X509 ( PrivKey(..), PrivKeyEC(..), PubKey(..), PubKeyEC(..), SerializedPoint(..), SignedCertificate
                 , certPubKey, encodeSignedObject, getCertificate )
import Data.X509.Memory (readKeyFileFromMemory, readSignedObjectFromMemory)
import Graphics.PDF.LowLevel.CMS (cmsSignature)
import Graphics.PDF.Annotation (newAnnotation)
import Graphics.PDF.Archive (pdfTextString)
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
-- * A document has one signature at most. A second signature needs an
--   incremental update of the file, which HPDF cannot write.

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

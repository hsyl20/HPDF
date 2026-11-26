{-# LANGUAGE OverloadedStrings #-}
-- | Digital signatures
module Tests.Signature (tests) where

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Tests.Common
import Graphics.PDF
import Graphics.PDF.Archive
import Graphics.PDF.Signature
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.IORef
import qualified Graphics.PDF.File as F

tests :: TestTree
tests = testGroup "Signature"
  [ test "signature" test_signature
  , test "signatureWithKey" test_signatureWithKey
  , test "rewrittenSignature" test_rewrittenSignature
  , test "rewrittenSignatureNotAnnotation" test_rewrittenSignatureNotAnnotation
  , localOption (mkTimeout 10000000) (test "manySignatureDictionaries" test_manySignatureDictionaries)
  ]

-- | A file with one page, and other objects from the number 4
file_with :: [F.Object] -> F.PdfFile
file_with others = F.PdfFile (1, 7) (M.fromList [("Root", F.Ref 1 0)]) $ IM.fromList $
  [ (1, (0, F.Plain (F.Dict (M.fromList [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0)]))))
  , (2, (0, F.Plain (F.Dict (M.fromList [("Type", F.Name "Pages"), ("Kids", F.Array [F.Ref 3 0]), ("Count", F.Integer 1)]))))
  , (3, (0, F.Plain (F.Dict (M.fromList [("Type", F.Name "Page"), ("Parent", F.Ref 2 0)]))))
  ] ++ zip [4 ..] (map (\o -> (0, o)) others)

-- | A signature dictionary that is not signed, with n bytes for the
-- signature
unsigned_sig :: Int -> F.Object
unsigned_sig n = F.Plain (F.Dict (M.fromList
  [ ("Type", F.Name "Sig"), ("Filter", F.Name "Adobe.PPKLite"), ("SubFilter", F.Name "adbe.pkcs7.detached")
  , ("ByteRange", F.Array (replicate 4 (F.Integer 0))), ("Contents", F.String (BS.replicate n 0)) ]))

-- An annotation with a /ByteRange and a /Contents of zero bytes is not a
-- signature: the writer does not give it the layout of a signature, and
-- 'signPdf' signs the signature dictionary after it.
-- See Note [Signatures of the writer] in Graphics.PDF.File.Write
test_rewrittenSignatureNotAnnotation :: IO ()
test_rewrittenSignatureNotAnnotation = do
  let decoy = F.Plain (F.Dict (M.fromList
        [ ("Type", F.Name "Annot"), ("Subtype", F.Name "Text"), ("Filter", F.Name "X")
        , ("ByteRange", F.Array (replicate 4 (F.Integer 0))), ("Contents", F.String (BS.replicate 3 0)) ]))
  r <- signPdf (const (return (BS.pack [0xDE, 0xAD]))) (F.renderPdfFile (file_with [decoy, unsigned_sig 64]))
  case r >>= F.parsePdfFile . BL.toStrict of
    Left e -> assertFailure e
    Right f -> do
      let contents n = case F.getObject f n of
            Just (F.Plain (F.Dict d)) -> M.lookup "Contents" d
            _ -> Nothing
      assertEqual (Just (F.String (BS.replicate 3 0))) (contents 4)
      assertEqual (Just (F.String (BS.pack (0xDE : 0xAD : replicate 62 0)))) (contents 5)

-- The time of the writer is linear, also with many signature
-- dictionaries, and each one gets its byte range.
-- See Note [Signatures of the writer] in Graphics.PDF.File.Write
test_manySignatureDictionaries :: IO ()
test_manySignatureDictionaries = do
  let count = 20000
      bytes = F.renderPdfFile (file_with (replicate count (unsigned_sig 1)))
      total = fromIntegral (BL.length bytes)
  case F.parsePdfFile (BL.toStrict bytes) of
    Left e -> assertFailure e
    Right f -> do
      let ranges = [ r | (_, F.Plain (F.Dict d)) <- IM.elems (F.pdfObjects f), Just (F.Array r) <- [M.lookup "ByteRange" d] ]
      assertEqual count (length ranges)
      -- each range excludes the "<00>" of its /Contents, and ends at the end of the file
      assertBool (and [ b - a == 4 && b + c == total | [F.Integer 0, F.Integer a, F.Integer b, F.Integer c] <- ranges ])
      assertEqual count (length [ () | [F.Integer 0, F.Integer _, F.Integer _, F.Integer _] <- ranges ])

-- A document that is read and written again ('F.renderPdfFile') keeps a
-- signature field that is not signed: 'signPdf' can sign it.
-- See Note [Signatures of the writer] in Graphics.PDF.File.Write
test_rewrittenSignature :: IO ()
test_rewrittenSignature = do
  let unsigned = renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        page <- addPage Nothing
        addSignature page signature {sigName = T.pack "A. Signer", sigSize = 64}
  case F.parsePdfFile (BL.toStrict unsigned) of
    Left e -> assertFailure e
    Right file -> do
      let rewritten = F.renderPdfFile file
      ref <- newIORef BL.empty
      r <- signPdf (\bytes -> writeIORef ref bytes >> return (BS.pack [0xDE, 0xAD])) rewritten
      case r of
        Left e -> assertFailure e
        Right signed -> do
          toSign <- readIORef ref
          -- the signature excludes the /Contents only: "<" 128 zeros ">"
          assertEqual (BL.length rewritten - 130) (BL.length toSign)
          assertEqual (BL.length rewritten) (BL.length signed)
          -- the file is read again with the same objects, and the signature
          case F.parsePdfFile (BL.toStrict signed) of
            Left e -> assertFailure e
            Right file' -> do
              let contents f = [ s | (_, F.Plain (F.Dict d)) <- IM.elems (F.pdfObjects f)
                                   , Just (F.String s) <- [M.lookup "Contents" d] ]
              assertEqual [BS.pack (0xDE : 0xAD : replicate 62 0)] (contents file')
              assertEqual (IM.keys (F.pdfObjects file)) (IM.keys (F.pdfObjects file'))


-- A signature field reserves the space of the signature, and 'signPdf'
-- writes the signature of the other bytes of the document.
-- See Note [Layout of a signature dictionary] in
-- Graphics.PDF.LowLevel.Signature
test_signature :: IO ()
test_signature = do
  let rect = PDFRect 0 0 300 200
      render doc = renderPdfE standardDocInfo {compressed = False} rect doc
      signedDoc sig = do
        -- a file with text that looks like a byte range, before the signature
        _ <- embedAssociatedFile (associatedFile (T.pack "decoy.txt") "text/plain"
               (BL.fromStrict (C.pack "/ByteRange [0 5 10 20] /Contents <0000>")))
        page <- addPage Nothing
        addSignature page sig
      mark = BS.pack [0xDE, 0xAD, 0xBE, 0xEF]
  case render (signedDoc signature {sigName = T.pack "A. Signer", sigSize = 64}) of
    Left ps -> assertFailure (unlines ps)
    Right unsignedLazy -> do
      let unsigned = BL.toStrict unsignedLazy
          has x = assertBoolVerbose x (C.pack x `BS.isInfixOf` unsigned)
      has "/AcroForm <<"
      has "/SigFlags 3"
      has "/FT /Sig"
      has "/SubFilter /adbe.pkcs7.detached"
      has "/Name (A. Signer)"
      -- the real byte range is the one after "/Type /Sig"
      let sigDict = snd (BS.breakSubstring (C.pack "/Type /Sig") unsigned)
          range = case BS.breakSubstring (C.pack "/ByteRange [0 ") sigDict of
            (_, r) | BS.null r -> Nothing
                   | otherwise -> case map C.readInt (C.words (C.takeWhile (/= ']') (BS.drop 14 r))) of
                       [Just (a, _), Just (b, _), Just (c, _)] | b + c == BS.length unsigned -> Just (a, b)
                       _ -> Nothing
      case range of
        Nothing -> assertFailure "no byte range that agrees with the length of the document"
        Just (a, b) -> do
          -- /Contents is a string of 64 bytes in hexadecimal, all zeros
          assertEqual (2 * 64 + 2) (b - a)
          assertEqual (C.pack ("<" ++ replicate 128 '0' ++ ">")) (BS.take (b - a) (BS.drop a unsigned))
          ref <- newIORef BS.empty
          r <- signPdf (\bytes -> writeIORef ref (BL.toStrict bytes) >> return mark) unsignedLazy
          toSign <- readIORef ref
          assertEqual (BS.take a unsigned `BS.append` BS.drop b unsigned) toSign
          case r of
            Left e -> assertFailure e
            Right signedLazy -> do
              let signed = BL.toStrict signedLazy
              assertEqual (BS.length unsigned) (BS.length signed)
              assertEqual (C.pack ("<DEADBEEF" ++ replicate 120 '0' ++ ">")) (BS.take (b - a) (BS.drop a signed))
              assertEqual (BS.take a unsigned) (BS.take a signed)
              assertEqual (BS.drop b unsigned) (BS.drop b signed)
              -- a signed document is not signed again
              again <- signPdf (const (return mark)) signedLazy
              assertEqual (Left "the document is already signed") again
          -- a signature that is longer than the reserved space
          long <- signPdf (const (return (BS.replicate 65 1))) unsignedLazy
          assertBool (either (("sigSize" `elem`) . words) (const False) long)
          empty <- signPdf (const (return BS.empty)) unsignedLazy
          assertBool (either (const True) (const False) empty)
  -- a document without a signature field
  plain <- signPdf (const (return mark)) (renderPdf standardDocInfo rect (addPage Nothing >> return ()))
  assertBool (either (("addSignature" `elem`) . words) (const False) plain)
  none <- signPdf (const (return mark)) BL.empty
  assertBool (either (const True) (const False) none)
  -- one signature only, with a field name and a size
  let problems doc = either length (const 0) (render doc)
  assertEqual 1 (problems (addPage Nothing >>= \p -> addSignature p signature >> addSignature p signature))
  assertEqual 1 (problems (addPage Nothing >>= \p -> addSignature p signature {sigFieldName = T.empty}))
  assertEqual 1 (problems (addPage Nothing >>= \p -> addSignature p signature {sigSize = 0}))

-- A key for the tests only (EC, P-256) with its certificate, and a second
-- key. They are not secrets.
fixture_key_pem :: BS.ByteString
fixture_key_pem = C.pack $ unlines
    [ "-----BEGIN PRIVATE KEY-----"
    , "MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgLrZEHE8Z03CYNC5x"
    , "7+tb7P2xgeJ1uc/AmIWGFgCE5YihRANCAAQjkmV/ZhRh4fjFMp6vG2pqmBOaGPDt"
    , "qRbE8ygztoVi+Wx/WVTY8CJc/kECd4bqQYEsXfkvoXB9+lzKwIh+c3je"
    , "-----END PRIVATE KEY-----"
    ]

fixture_cert_pem :: BS.ByteString
fixture_cert_pem = C.pack $ unlines
    [ "-----BEGIN CERTIFICATE-----"
    , "MIIBfDCCASKgAwIBAgIULa9txngEbCZ9QX7m7L5ChVesvuYwCgYIKoZIzj0EAwIw"
    , "IzEhMB8GA1UEAwwYSFBERiB0ZXN0IChub3QgYSBzZWNyZXQpMCAXDTI2MTEyNTEz"
    , "MjgzOVoYDzIxMjYxMTAxMTMyODM5WjAjMSEwHwYDVQQDDBhIUERGIHRlc3QgKG5v"
    , "dCBhIHNlY3JldCkwWTATBgcqhkjOPQIBBggqhkjOPQMBBwNCAAQjkmV/ZhRh4fjF"
    , "Mp6vG2pqmBOaGPDtqRbE8ygztoVi+Wx/WVTY8CJc/kECd4bqQYEsXfkvoXB9+lzK"
    , "wIh+c3jeozIwMDAdBgNVHQ4EFgQUxFsQNsvKe/9/lt/1hAhXOFqPN6QwDwYDVR0T"
    , "AQH/BAUwAwEB/zAKBggqhkjOPQQDAgNIADBFAiBn11kiiy2Hdh3H/YanMtuz5Jvv"
    , "mgcOyaFG4oT1yGO68gIhAOVrJzB8eYJELIWx+XY9HXBXdg7vU8LkQvXNb0E7TsEh"
    , "-----END CERTIFICATE-----"
    ]

fixture_other_key_pem :: BS.ByteString
fixture_other_key_pem = C.pack $ unlines
    [ "-----BEGIN PRIVATE KEY-----"
    , "MIGHAgEAMBMGByqGSM49AgEGCCqGSM49AwEHBG0wawIBAQQgfhuwE7zzpILSLYTj"
    , "brP94J7ycjjFahF2YMbQwxb2khGhRANCAAStHxxb7D53o//28pm1OInaNw8vPdGi"
    , "DWXwZxIXU06oAFUiwEuxtYw+KuyOfwTpyZd5lc4m10JXTagGKVhAYquJ"
    , "-----END PRIVATE KEY-----"
    ]

-- 'signPdfWith' writes a CMS signature with the certificate of the signer.
-- CI verifies a signature with pdfsig: see .github/workflows/ci.yml
test_signatureWithKey :: IO ()
test_signatureWithKey = do
  let unsigned = renderPdf standardDocInfo {compressed = False} (PDFRect 0 0 300 200) $ do
        page <- addPage Nothing
        addSignature page signature
      isLeft = either (const True) (const False)
      -- the DER of the certificate: the base64 of the PEM file starts with it
      contents pdf = let (_, r) = BS.breakSubstring (C.pack "/Contents <") (BL.toStrict pdf)
                     in C.takeWhile (/= '>') (BS.drop 11 r)
  case parseSigner fixture_key_pem fixture_cert_pem of
    Left e -> assertFailure e
    Right signer -> do
      r <- signPdfWith signer unsigned
      case r of
        Left e -> assertFailure e
        Right signed -> do
          assertEqual (BL.length unsigned) (BL.length signed)
          -- a DER SEQUENCE with a length of two bytes, then the OID of signedData
          assertBool (C.pack "3082" `BS.isPrefixOf` contents signed)
          assertBool (C.pack "06092A864886F70D010702" `BS.isInfixOf` contents signed)
          -- the name of the certificate ("HPDF test") is in the signature
          assertBool (C.pack "4850444620746573742028" `BS.isInfixOf` contents signed)
          again <- signPdfWith signer signed
          assertEqual (Left "the document is already signed") again
      none <- signPdfWith signer (renderPdf standardDocInfo (PDFRect 0 0 300 200) (addPage Nothing >> return ()))
      assertBool (isLeft none)
  -- a certificate that is not the certificate of the key, and files that
  -- are not PEM files
  assertBool (isLeft (fmap (const ()) (parseSigner fixture_other_key_pem fixture_cert_pem)))
  assertBool (isLeft (fmap (const ()) (parseSigner fixture_cert_pem fixture_cert_pem)))
  assertBool (isLeft (fmap (const ()) (parseSigner fixture_key_pem fixture_key_pem)))
  assertBool (isLeft (fmap (const ()) (parseSigner BS.empty BS.empty)))

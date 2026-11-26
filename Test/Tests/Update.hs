{-# LANGUAGE OverloadedStrings #-}
-- | Incremental updates of a PDF file.
-- See Note [Incremental update] in Graphics.PDF.File.Update
module Tests.Update (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import qualified Graphics.PDF.File as F
import qualified Graphics.PDF as P
import Graphics.PDF.Encryption (Encryption(..), encryption, encryptPdf)
import Graphics.PDF.Signature (addSignature, addSignatureField, signPdf, signature, Signature(..))
import Control.Monad (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

tests :: TestTree
tests = testGroup "Update"
  [ test "updateKeepsTheBytes" test_updateKeepsTheBytes
  , test "updateWithNoChange" test_updateWithNoChange
  , test "updateOfACrossReferenceStream" test_updateOfACrossReferenceStream
  , test "updateOfEncryptedFiles" test_updateOfEncryptedFiles
  , test "clearMetadataInAnUpdate" test_clearMetadataInAnUpdate
  , test "idOfAnUpdate" test_idOfAnUpdate
  , test "versionOfAnUpdate" test_versionOfAnUpdate
  , test "updateRefusals" test_updateRefusals
  , test "garbageBeforeTheHeader" test_garbageBeforeTheHeader
  , test "secondSignature" test_secondSignature
  , test "secondSignatureOfAnEncryptedFile" test_secondSignatureOfAnEncryptedFile
  , test "signatureFieldOfAFile" test_signatureFieldOfAFile
  , test "pagesOfAFile" test_pagesOfAFile
  , test "permissionsOfAnUpdate" test_permissionsOfAnUpdate
  , test "certificationOfAnUpdate" test_certificationOfAnUpdate
  , test "numbersOfAnUpdate" test_numbersOfAnUpdate
  , test "infoOfAnUpdate" test_infoOfAnUpdate
  ]

-- | A document of HPDF with one page, not compressed
hpdf_doc :: BS.ByteString
hpdf_doc = BL.toStrict (P.renderPdf P.standardDocInfo {P.compressed = False} (P.PDFRect 0 0 300 200) (P.addPage Nothing >> return ()))

fixture :: FilePath -> IO BS.ByteString
fixture name = BS.readFile ("Test/encrypted/" ++ name ++ ".pdf")

source :: String -> BS.ByteString -> IO F.PdfSource
source password = either assertFailure return . F.parsePdfSource F.defaultLimits (T.pack password)

parse :: String -> BS.ByteString -> IO F.PdfFile
parse password = either assertFailure return . F.parsePdfFileWithPassword F.defaultLimits (T.pack password)

update :: F.PdfSource -> F.PdfFile -> IO BS.ByteString
update src file = F.updatePdfFile src file >>= either assertFailure (return . BL.toStrict)

-- | The first number after the objects of a file
next_number :: F.PdfFile -> Int
next_number f = maybe 1 ((+ 1) . fst) (IM.lookupMax (F.pdfObjects f))

-- | A file with more objects
with_objects :: [(Int, F.Object)] -> F.PdfFile -> F.PdfFile
with_objects os f = f { F.pdfObjects = foldr (\(n, o) -> IM.insert n (0, o)) (F.pdfObjects f) os }

-- | A file with a key in its catalog
with_catalog_key :: BS.ByteString -> F.Value -> F.PdfFile -> F.PdfFile
with_catalog_key k v f = case F.dictLookup "Root" (F.pdfTrailer f) of
  Just (F.Ref n g) | Just (F.Plain (F.Dict d)) <- F.getObject f n ->
    f { F.pdfObjects = IM.insert n (g, F.Plain (F.Dict (M.insert k v d))) (F.pdfObjects f) }
  _ -> f

catalog_key :: BS.ByteString -> F.PdfFile -> Maybe F.Value
catalog_key k f = case F.resolveValue f <$> F.dictLookup "Root" (F.pdfTrailer f) of
  Just (F.Dict d) -> F.dictLookup k d
  _ -> Nothing

-- | The new objects of the tests: a dictionary with a string, and a
-- stream, that the catalog gives
with_secret :: F.PdfFile -> F.PdfFile
with_secret f =
  let n = next_number f
  in with_catalog_key "Secret" (F.Ref n 0) $ with_objects
       [ (n, F.Plain (F.Dict (M.fromList [("Text", F.String "secret text"), ("Data", F.Ref (n + 1) 0)])))
       , (n + 1, F.Stream (M.fromList [("Length", F.Integer 13)]) "secret stream") ] f

-- | The file that the reader reads is the new file: its objects, and its
-- trailer with the references resolved (the /Info of the update is an
-- object), with no /ID (the update gives a new one)
assert_same :: F.PdfFile -> F.PdfFile -> IO ()
assert_same expected actual = do
  forM_ (IM.toList (F.pdfObjects expected)) $ \(n, x) -> assertEqual (Just x) (IM.lookup n (F.pdfObjects actual))
  assertEqual (resolved expected) (resolved actual)
  where
    resolved f = fmap (F.resolveValue f) (M.delete "ID" (F.pdfTrailer f))

-- | The bytes after the bytes of the source
appended :: BS.ByteString -> BS.ByteString -> BS.ByteString
appended old new = BS.drop (BS.length old) new

-- An update adds the new objects, the changed objects and the deleted
-- objects after the bytes of the file, and the file read again is the new
-- file. A second update adds a third section.
test_updateKeepsTheBytes :: IO ()
test_updateKeepsTheBytes = do
  src <- source "" hpdf_doc
  let old = F.sourceFile src
      new = with_secret old
      n = next_number old
  bytes <- update src new
  assertEqual hpdf_doc (BS.take (BS.length hpdf_doc) bytes)
  -- the catalog and the two new objects only, and one table
  -- the catalog, the two new objects and the /Info (an object now)
  assertEqual 4 (length (occurrences " 0 obj\n" (appended hpdf_doc bytes)))
  assertEqual 1 (length (occurrences "\nxref\n" (appended hpdf_doc bytes)))
  read1 <- parse "" bytes
  assert_same new read1
  -- a second update: a changed object and a deleted object
  src2 <- source "" bytes
  let new2 = (F.sourceFile src2)
        { F.pdfObjects = IM.insert n (0, F.Plain (F.Dict (M.fromList [("Text", F.String "changed")])))
                                  (IM.delete (n + 1) (F.pdfObjects (F.sourceFile src2))) }
  bytes2 <- update src2 new2
  assertEqual bytes (BS.take (BS.length bytes) bytes2)
  read2 <- parse "" bytes2
  assertEqual (F.pdfObjects new2) (F.pdfObjects read2)
  -- the deleted object has a free entry with the next generation, and the
  -- entry 0 gives it
  assertBool ("0000000000 00001 f \n" `BS.isInfixOf` appended bytes bytes2)
  assertBool (C.pack ("0 1\n" ++ pad10 (n + 1) ++ " 65535 f \n") `BS.isInfixOf` appended bytes bytes2)
  -- /Prev gives the previous section
  assertBool (("/Prev " <> start_xref bytes) `BS.isInfixOf` appended bytes bytes2)
  where
    pad10 x = let s = show x in replicate (10 - length s) '0' ++ s
    -- the line before the last "%%EOF"
    start_xref b = case dropWhile (/= "%%EOF") (reverse (C.lines b)) of
      (_ : x : _) -> x
      _ -> "none"

-- A new file that is the file gives the bytes of the file
test_updateWithNoChange :: IO ()
test_updateWithNoChange = do
  forM_ [("", hpdf_doc)] $ \(p, b) -> do
    src <- source p b
    bytes <- update src (F.sourceFile src)
    assertEqual b bytes
  r6 <- fixture "r6-aes-256"
  src <- source "user" r6
  bytes <- update src (F.sourceFile src)
  assertEqual r6 bytes

-- A file whose newest section is a cross-reference stream gets a
-- cross-reference stream. A new object does not take the number of the
-- object stream or of the cross-reference stream: it gets a new number.
test_updateOfACrossReferenceStream :: IO ()
test_updateOfACrossReferenceStream = do
  objstm <- fixture "objstm"
  src <- source "" objstm
  let old = F.sourceFile src
      -- the cross-reference stream of qpdf has the number after the objects
      clash = next_number old
  assertBool (IM.notMember 1 (F.pdfObjects old))
  bytes <- update src (with_objects [(clash, F.Plain (F.String "new"))] (with_catalog_key "New" (F.Ref clash 0) old))
  let added = appended objstm bytes
  assertBool ("/Type /XRef" `BS.isInfixOf` added)
  assertBool (not ("\nxref\n" `BS.isInfixOf` added))
  file <- parse "" bytes
  case catalog_key "New" file of
    Just (F.Ref m 0) -> do
      assertBool (m > clash)
      assertEqual (Just (F.Plain (F.String "new"))) (F.getObject file m)
    other -> assertFailure (show other)
  -- the objects of the object stream are still there
  forM_ (IM.toList (F.pdfObjects old)) $ \(n, (g, o)) ->
    assertBool (n == fst (catalog_number old) || IM.lookup n (F.pdfObjects file) == Just (g, o))
  -- a second update
  src2 <- source "" bytes
  bytes2 <- update src2 (with_secret (F.sourceFile src2))
  file2 <- parse "" bytes2
  assertEqual (Just (F.String "secret text")) (catalog_key "Secret" file2 >>= \r -> secret_text file2 r)
  where
    catalog_number f = case F.dictLookup "Root" (F.pdfTrailer f) of
      Just (F.Ref n g) -> (n, g)
      _ -> (0, 0)

secret_text :: F.PdfFile -> F.Value -> Maybe F.Value
secret_text f r = case F.resolveValue f r of
  F.Dict d -> F.dictLookup "Text" d
  _ -> Nothing

-- The objects of an update of an encrypted file are encrypted with the key
-- of the file: each password reads them, and their text is not in the
-- bytes. The files of qpdf have each revision, and cross-reference
-- streams (/R 4 with AES, /R 6).
test_updateOfEncryptedFiles :: IO ()
test_updateOfEncryptedFiles =
  forM_ ["r2-rc4-40", "r3-rc4-128", "r4-rc4-128", "r4-aes-128", "r5-aes-256", "r6-aes-256"] $ \name -> do
    original <- fixture name
    forM_ [("user", "owner"), ("owner", "user")] $ \(p, other) -> do
      src <- source p original
      let new = with_secret (F.sourceFile src)
      bytes <- update src new
      let added = appended original bytes
      assertBoolVerbose name (not ("secret" `BS.isInfixOf` added))
      assertBoolVerbose name ("/Encrypt" `BS.isInfixOf` added)
      file <- parse other bytes
      assertEqual (Just (F.String "secret text")) (catalog_key "Secret" file >>= secret_text file)
      -- the stream can have a new number: the reader removed the /Encrypt
      -- object, or a cross-reference stream, with the number n + 1
      case F.resolveValue file <$> catalog_key "Secret" file of
        Just (F.Dict d) | Just (F.Ref m _) <- F.dictLookup "Data" d ->
          assertEqual (Just (F.Stream (M.fromList [("Length", F.Integer 13)]) "secret stream")) (F.getObject file m)
        other_value -> assertFailure (show other_value)
      -- the first string of the /ID is a part of the key
      assertEqual (first_id (F.sourceFile src)) (first_id file)
  where
    first_id f = case F.dictLookup "ID" (F.pdfTrailer f) of
      Just (F.Array (x : _)) -> Just x
      _ -> Nothing

-- With /EncryptMetadata false, a metadata stream of an update is not
-- encrypted, and the other streams are
test_clearMetadataInAnUpdate :: IO ()
test_clearMetadataInAnUpdate = do
  original <- fixture "r6-aes-256-clear-metadata"
  src <- source "user" original
  let old = F.sourceFile src
      n = next_number old
      metadata = F.Stream (M.fromList [("Type", F.Name "Metadata"), ("Subtype", F.Name "XML"), ("Length", F.Integer 19)]) "<x:xmpmeta new=''/>"
  bytes <- update src (with_secret (with_objects [(n + 10, metadata)] old))
  assertBool ("<x:xmpmeta new=''/>" `BS.isInfixOf` appended original bytes)
  assertBool (not ("secret" `BS.isInfixOf` appended original bytes))
  file <- parse "user" bytes
  assertEqual (Just metadata) (F.getObject file (n + 10))

-- The second string of the /ID changes, and the first one stays. The
-- first string of an encrypted file cannot change.
test_idOfAnUpdate :: IO ()
test_idOfAnUpdate = do
  objstm <- fixture "objstm"
  src <- source "" objstm
  let old = F.sourceFile src
  bytes <- update src (with_secret old)
  file <- parse "" bytes
  case (F.dictLookup "ID" (F.pdfTrailer old), F.dictLookup "ID" (F.pdfTrailer file)) of
    (Just (F.Array [a, b]), Just (F.Array [a', b'])) -> do
      assertEqual a a'
      assertBool (b /= b')
      -- an /ID of the caller stays
      let given = F.Array [F.String "first", F.String "second"]
      bytes2 <- update src (with_secret old) { F.pdfTrailer = M.insert "ID" given (F.pdfTrailer old) }
      file2 <- parse "" bytes2
      assertEqual (Just given) (F.dictLookup "ID" (F.pdfTrailer file2))
    other -> assertFailure (show other)
  -- a file with no /ID gets one. The keys of a cross-reference section in
  -- the trailer of the caller are not written: the update writes its own
  srcH <- source "" hpdf_doc
  let oldH = F.sourceFile srcH
      section_keys = M.fromList [("XRefStm", F.Integer 5), ("Filter", F.Name "FlateDecode"), ("W", F.Array [])]
  assertEqual Nothing (M.lookup "ID" (F.pdfTrailer oldH))
  bytesH <- update srcH (with_secret oldH) { F.pdfTrailer = M.union section_keys (F.pdfTrailer oldH) }
  assertBool (not (any (`BS.isInfixOf` appended hpdf_doc bytesH) ["/XRefStm", "/Filter", "/W"]))
  fileH <- parse "" bytesH
  case M.lookup "ID" (F.pdfTrailer fileH) of
    Just (F.Array [F.String a, F.String b]) -> assertEqual (16, 16) (BS.length a, BS.length b)
    other -> assertFailure (show other)
  r3 <- fixture "r3-rc4-128"
  src3 <- source "owner" r3
  let old3 = F.sourceFile src3
  r <- F.updatePdfFile src3 (with_secret old3) { F.pdfTrailer = M.insert "ID" (F.Array [F.String "x", F.String "y"]) (F.pdfTrailer old3) }
  assertBool (either (("key" `elem`) . words) (const False) r)

-- A higher version is the /Version of the catalog: the header stays
test_versionOfAnUpdate :: IO ()
test_versionOfAnUpdate = do
  src <- source "" hpdf_doc
  let old = F.sourceFile src
  bytes <- update src old { F.pdfVersion = (2, 0) }
  file <- parse "" bytes
  assertEqual (F.pdfVersion old) (F.pdfVersion file)
  assertEqual (Just (F.Name "2.0")) (catalog_key "Version" file)
  -- a /Version that is already higher stays
  src2 <- source "" bytes
  bytes2 <- update src2 (with_secret (F.sourceFile src2)) { F.pdfVersion = (1, 7) }
  file2 <- parse "" bytes2
  assertEqual (Just (F.Name "2.0")) (catalog_key "Version" file2)

-- An update cannot encrypt a file, and needs a /Root. It cannot lower the
-- version.
test_updateRefusals :: IO ()
test_updateRefusals = do
  src <- source "" hpdf_doc
  let old = F.sourceFile src
      refused f = F.updatePdfFile src f >>= \r -> assertBool (either (const True) (const False) r)
  refused old { F.pdfTrailer = M.insert "Encrypt" (F.Dict M.empty) (F.pdfTrailer old) }
  refused old { F.pdfTrailer = M.delete "Root" (F.pdfTrailer old) }
  refused old { F.pdfVersion = (1, 0) }
  -- the encryption of the file is not a change of an update
  encrypted <- F.encryptPdfFile (encryption "user" "owner") old >>= either assertFailure return
  refused encrypted

-- The offsets of a file with bytes before the header count from the
-- header: the offsets of the update too. A file whose last bytes are
-- "%%EOF" (as HPDF writes it) gets an end of line before the update.
test_garbageBeforeTheHeader :: IO ()
test_garbageBeforeTheHeader = do
  let original = "garbage\n" <> hpdf_doc
  assertBool ("%%EOF" `BS.isSuffixOf` original)
  src <- source "" original
  let new = with_secret (F.sourceFile src)
  bytes <- update src new
  assertBool ((original <> "\n") `BS.isPrefixOf` bytes)
  file <- parse "" bytes
  assert_same new file

-- | A signed document of HPDF, with a signature of 64 bytes ("mark")
signed_doc :: BS.ByteString -> IO BS.ByteString
signed_doc mark = do
  let unsigned = P.renderPdf P.standardDocInfo {P.compressed = False} (P.PDFRect 0 0 300 200) $ do
        page <- P.addPage Nothing
        addSignature page signature {sigName = "First", sigSize = 64}
  signPdf (const (return mark)) unsigned >>= either assertFailure (return . BL.toStrict)

-- | The /Contents of the signature dictionaries of a file
signature_contents :: F.PdfFile -> [BS.ByteString]
signature_contents f = [ BS.take 4 s | (_, F.Plain (F.Dict d)) <- IM.elems (F.pdfObjects f)
                                     , F.dictLookup "Type" d == Just (F.Name "Sig")
                                     , Just (F.String s) <- [M.lookup "Contents" d] ]

-- A second signature is in an update: the first signature signs the same
-- bytes, and the second one signs all the bytes of the file.
-- See Note [Signature of a file that is read] in Graphics.PDF.Signature
test_secondSignature :: IO ()
test_secondSignature = do
  first <- signed_doc "\x01\x02\x03\x04"
  src <- source "" first
  file <- either assertFailure return (addSignatureField signature {sigFieldName = "Second", sigSize = 64} 1 (F.sourceFile src))
  unsigned <- update src file
  ref <- newIORef BL.empty
  signed <- signPdf (\b -> writeIORef ref b >> return "\x05\x06\x07\x08") (BL.fromStrict unsigned) >>= either assertFailure (return . BL.toStrict)
  to_sign <- readIORef ref
  -- the bytes of the first signature do not change
  assertEqual first (BS.take (BS.length first) signed)
  -- the second signature excludes its /Contents only: "<" 128 digits ">"
  assertEqual (BS.length signed - 130) (fromIntegral (BL.length to_sign))
  read_file <- parse "" signed
  assertEqual ["\x01\x02\x03\x04", "\x05\x06\x07\x08"] (signature_contents read_file)
  -- two fields, on the page
  case F.resolveValue read_file <$> catalog_key "AcroForm" read_file of
    Just (F.Dict form) -> do
      assertEqual (Just (F.Integer 3)) (F.dictLookup "SigFlags" form)
      assertEqual (Just 2) (length' <$> F.dictLookup "Fields" form)
    other -> assertFailure (show other)
  where
    length' v = case v of
      F.Array xs -> length xs
      _ -> 0

-- An encrypted and signed file gets a second signature: the update is
-- encrypted, except the /Contents of the signature.
test_secondSignatureOfAnEncryptedFile :: IO ()
test_secondSignatureOfAnEncryptedFile = do
  let unsigned = P.renderPdf P.standardDocInfo {P.compressed = False} (P.PDFRect 0 0 300 200) $ do
        page <- P.addPage Nothing
        addSignature page signature {sigSize = 64}
  encrypted <- encryptPdf (encryption "user" "owner") unsigned >>= either assertFailure return
  first <- signPdf (const (return "\x01\x02\x03\x04")) encrypted >>= either assertFailure (return . BL.toStrict)
  src <- source "owner" first
  file <- either assertFailure return
            (addSignatureField signature {sigFieldName = "Second", sigName = "Second signer", sigSize = 64} 1 (F.sourceFile src))
  unsigned2 <- update src file
  assertBool (not ("Second signer" `BS.isInfixOf` unsigned2))
  signed <- signPdf (const (return "\x05\x06\x07\x08")) (BL.fromStrict unsigned2) >>= either assertFailure (return . BL.toStrict)
  assertEqual first (BS.take (BS.length first) signed)
  read_file <- parse "user" signed
  assertEqual ["\x01\x02\x03\x04", "\x05\x06\x07\x08"] (signature_contents read_file)
  assertBool (any (\(_, o) -> case o of
                      F.Plain (F.Dict d) -> F.dictLookup "Name" d == Just (F.String "Second signer")
                      _ -> False) (IM.elems (F.pdfObjects read_file)))

-- | A file with two pages, whose page tree has a loop and a node in two
-- places. The first page has its /Annots in an object, and the
-- /AcroForm and its /Fields are objects.
two_pages :: F.PdfFile
two_pages = F.PdfFile (1, 7) (M.fromList [("Root", F.Ref 1 0)]) (IM.fromList (map (fmap (\o -> (0, o)))
  [ (1, dict [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0), ("AcroForm", F.Ref 5 0)])
  , (2, dict [("Type", F.Name "Pages"), ("Kids", F.Array [F.Ref 3 0, F.Ref 10 0]), ("Count", F.Integer 2)])
  , (3, dict [("Type", F.Name "Page"), ("Parent", F.Ref 2 0), ("Annots", F.Ref 4 0)])
  , (4, F.Plain (F.Array []))
  , (5, dict [("Fields", F.Ref 6 0), ("SigFlags", F.Integer 4)])
  , (6, F.Plain (F.Array [F.Ref 7 0]))
  , (7, dict [("FT", F.Name "Tx"), ("T", F.String "Name")])
  , (10, dict [("Type", F.Name "Pages"), ("Kids", F.Array [F.Ref 11 0, F.Ref 2 0, F.Ref 3 0]), ("Count", F.Integer 1)])
  , (11, dict [("Type", F.Name "Page"), ("Parent", F.Ref 10 0)]) ])) Nothing
  where
    dict = F.Plain . F.Dict . M.fromList

-- The pages are the leaves of the page tree, one time each, also with a
-- loop and a node in two places
test_pagesOfAFile :: IO ()
test_pagesOfAFile = do
  assertEqual [(3, 0), (11, 0)] (F.pdfPages two_pages)
  doc <- parse "" (BL.toStrict (P.renderPdf P.standardDocInfo (P.PDFRect 0 0 300 200)
                                  (mapM_ (const (P.addPage Nothing)) [1 .. 30 :: Int])))
  assertEqual 30 (length (F.pdfPages doc))
  assertEqual [] (F.pdfPages two_pages { F.pdfTrailer = M.empty })
  -- a leaf with no /Type is a page; a node with no /Kids is not
  let no_type = two_pages { F.pdfObjects = IM.insert 11 (0, F.Plain (F.Dict (M.fromList [("Parent", F.Ref 10 0)])))
                                             (F.pdfObjects two_pages) }
  assertEqual [(3, 0), (11, 0)] (F.pdfPages no_type)
  let empty_node = two_pages { F.pdfObjects = IM.insert 11 (0, F.Plain (F.Dict (M.fromList [("Type", F.Name "Pages")])))
                                                (F.pdfObjects two_pages) }
  assertEqual [(3, 0)] (F.pdfPages empty_node)

-- 'addSignatureField' changes the /Annots and the /AcroForm in their
-- objects when they are references, and puts the field on a page. It
-- refuses a name that the file has, and a page that the file does not
-- have.
test_signatureFieldOfAFile :: IO ()
test_signatureFieldOfAFile = do
  case addSignatureField signature {sigFieldName = "Signature"} 1 two_pages of
    Left e -> assertFailure e
    Right f -> do
      -- the catalog does not change, the /Fields change in their object,
      -- and the page gets an /Annots of its own: the object of its old
      -- /Annots can be the /Annots of another page
      assertEqual (F.getObject two_pages 1) (F.getObject f 1)
      assertEqual (F.getObject two_pages 4) (F.getObject f 4)
      assertEqual (Just (F.Array [F.Ref 13 0])) (key "Annots" (F.getObject f 3))
      assertEqual (Just (F.Plain (F.Array [F.Ref 7 0, F.Ref 13 0]))) (F.getObject f 6)
      assertEqual (Just (F.Integer 7)) (key "SigFlags" (F.getObject f 5))
      assertEqual (Just (F.Ref 12 0)) (key "V" (F.getObject f 13))
      assertEqual (Just (F.Ref 3 0)) (key "P" (F.getObject f 13))
  -- the second page has no /Annots: the page gets one
  case addSignatureField signature {sigFieldName = "Signature"} 2 two_pages of
    Left e -> assertFailure e
    Right f -> do
      assertEqual (Just (F.Array [F.Ref 13 0])) (key "Annots" (F.getObject f 11))
      assertEqual (Just (F.Ref 11 0)) (key "P" (F.getObject f 13))
  let refused f = assertBool (either (const True) (const False) f)
  refused (addSignatureField signature {sigFieldName = "Name"} 1 two_pages)
  refused (addSignatureField signature {sigFieldName = ""} 1 two_pages)
  refused (addSignatureField signature {sigSize = 0} 1 two_pages)
  refused (addSignatureField signature 0 two_pages)
  refused (addSignatureField signature 3 two_pages)
  refused (addSignatureField signature {sigFieldName = "A.B"} 1 two_pages)
  -- a name in UTF-16BE, or in an object, is the same name
  let with_object n o f = f { F.pdfObjects = IM.insert n (0, o) (F.pdfObjects f) }
      utf16 = BS.pack [0xFE, 0xFF, 0, 0x4E, 0, 0x61, 0, 0x6D, 0, 0x65]
  refused (addSignatureField signature {sigFieldName = "Name"} 1
             (with_object 7 (dict [("FT", F.Name "Tx"), ("T", F.String utf16)]) two_pages))
  refused (addSignatureField signature {sigFieldName = "Name"} 1
             (with_object 20 (F.Plain (F.String "Name"))
               (with_object 7 (dict [("FT", F.Name "Tx"), ("T", F.Ref 20 0)]) two_pages)))
  -- the same array for the /Annots of the page and the /Fields: the field
  -- is in it one time
  case addSignatureField signature 1 (with_object 5 (dict [("Fields", F.Ref 4 0)]) two_pages) of
    Left e -> assertFailure e
    Right f -> do
      assertEqual (Just (F.Plain (F.Array [F.Ref 13 0]))) (F.getObject f 4)
      assertEqual (Just (F.Array [F.Ref 13 0])) (key "Annots" (F.getObject f 3))
  -- a reference to an object that the file does not have is null
  case addSignatureField signature 1 (with_object 1 (dict [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0), ("AcroForm", F.Ref 30 0)]) two_pages) of
    Left e -> assertFailure e
    Right f -> assertEqual (Just (F.Dict (M.fromList [("Fields", F.Array [F.Ref 13 0]), ("SigFlags", F.Integer 3)])))
                           (key "AcroForm" (F.getObject f 1))
  -- a loop of references
  refused (addSignatureField signature 1 (with_object 5 (F.Plain (F.Ref 5 0)) two_pages))
  refused (addSignatureField signature 1 (with_object 6 (F.Plain (F.Ref 8 0)) (with_object 8 (F.Plain (F.Ref 6 0)) two_pages)))
  -- a signature field that is not signed: a signature is for one field
  case addSignatureField signature {sigFieldName = "First"} 1 two_pages of
    Left e -> assertFailure e
    Right f -> refused (addSignatureField signature {sigFieldName = "Second"} 1 f)
  where
    dict = F.Plain . F.Dict . M.fromList
    key k o = case o of
      Just (F.Plain (F.Dict d)) -> F.dictLookup k d
      _ -> Nothing

-- An update of a file that the user password opened needs the permission
-- to modify it, or 'F.updateIgnorePermissions'. The owner password gives
-- all the permissions. A file with no change gives its bytes.
-- See Note [Restrictions of an update] in Graphics.PDF.File.Update
test_permissionsOfAnUpdate :: IO ()
test_permissionsOfAnUpdate = do
  let ignore = F.defaultUpdateOptions { F.updateIgnorePermissions = True }
      is_left = either (const True) (const False)
  forM_ [("r6-aes-256-restricted", "user"), ("r6-aes-256-empty-user-restricted", ""), ("r3-rc4-128-restricted", "user")] $ \(name, user) -> do
    original <- fixture name
    src <- source user original
    let new = with_secret (F.sourceFile src)
    refused <- F.updatePdfFile src new
    assertBoolVerbose name (is_left refused)
    same <- update src (F.sourceFile src)
    assertEqual original same
    bytes <- F.updatePdfFileWith ignore src new >>= either assertFailure (return . BL.toStrict)
    file <- parse "owner" bytes
    assertEqual (Just (F.String "secret text")) (catalog_key "Secret" file >>= secret_text file)
    owner <- source "owner" original
    _ <- update owner (with_secret (F.sourceFile owner))
    return ()
  -- the permission to modify, and not the others
  let modify_only = (encryption "user" "owner") { encPermissions = F.noPermissions { F.permModify = True } }
  encrypted <- encryptPdf modify_only (BL.fromStrict hpdf_doc) >>= either assertFailure (return . BL.toStrict)
  src <- source "user" encrypted
  assertBool (F.pdfPermissions (F.sourceFile src) /= Nothing)
  _ <- update src (with_secret (F.sourceFile src))
  return ()

-- A new object does not take a free number of the file (its generation
-- is not 0) or a number after a large /Size: it gets the number after the
-- numbers of the cross-reference sections.
-- See Note [Incremental update] in Graphics.PDF.File.Update
test_numbersOfAnUpdate :: IO ()
test_numbersOfAnUpdate = do
  -- a file whose /Info is an object, the last one
  src0 <- source "" hpdf_doc
  first <- update src0 (with_secret (F.sourceFile src0))
  src <- source "" first
  let old = F.sourceFile src
      lastn = next_number old - 1
  assertEqual (Just (F.Ref lastn 0)) (M.lookup "Info" (F.pdfTrailer old))
  -- the last object is deleted: its number is free, with the generation 1
  deleted <- update src old { F.pdfObjects = IM.delete lastn (F.pdfObjects old)
                            , F.pdfTrailer = M.delete "Info" (F.pdfTrailer old) }
  src2 <- source "" deleted
  let old2 = F.sourceFile src2
      n = next_number old2
  assertBool (n <= lastn)
  bytes <- update src2 (with_catalog_key "New" (F.Ref n 0) (with_objects [(n, F.Plain (F.String "new"))] old2))
  file <- parse "" bytes
  case catalog_key "New" file of
    Just (F.Ref m 0) -> do
      assertBool (m > lastn)
      assertEqual (Just (F.Plain (F.String "new"))) (F.getObject file m)
    other -> assertFailure (show other)
  -- a /Size of 2000000000 in the trailer
  let (before, after) = BS.breakSubstring "/Size " hpdf_doc
      large = before <> "/Size 2000000000" <> C.dropWhile (\c -> c >= '0' && c <= '9') (BS.drop 6 after)
  src3 <- source "" large
  bytes3 <- update src3 (with_secret (F.sourceFile src3))
  file3 <- parse "" bytes3
  case catalog_key "Secret" file3 of
    Just (F.Ref m 0) -> assertBool (m < 100)
    other -> assertFailure (show other)

-- A trailer /Info that is a dictionary becomes an object: in an encrypted
-- file, its strings are then encrypted
test_infoOfAnUpdate :: IO ()
test_infoOfAnUpdate = do
  src <- source "" hpdf_doc
  let old = F.sourceFile src
  assertBool (case M.lookup "Info" (F.pdfTrailer old) of
                Just (F.Dict _) -> True
                _ -> False)
  -- no change: the bytes of the file
  same <- update src old
  assertEqual hpdf_doc same
  bytes <- update src (with_secret old)
  file <- parse "" bytes
  case M.lookup "Info" (F.pdfTrailer file) of
    Just r@(F.Ref _ _) -> assertEqual (M.lookup "Info" (F.pdfTrailer old)) (Just (F.resolveValue file r))
    other -> assertFailure (show other)
  r3 <- fixture "r3-rc4-128"
  src3 <- source "user" r3
  let info = F.Dict (M.fromList [("Title", F.String "PLAINTITLE")])
  bytes3 <- update src3 (F.sourceFile src3) { F.pdfTrailer = M.insert "Info" info (F.pdfTrailer (F.sourceFile src3)) }
  assertBool (not ("PLAINTITLE" `BS.isInfixOf` bytes3))
  file3 <- parse "user" bytes3
  assertEqual (Just info) (F.resolveValue file3 <$> M.lookup "Info" (F.pdfTrailer file3))

-- An update of a file that a DocMDP signature certifies with no changes
-- (/P 1) needs 'F.updateIgnoreCertification'. /P 2 permits an update.
-- See Note [Restrictions of an update] in Graphics.PDF.File.Update
test_certificationOfAnUpdate :: IO ()
test_certificationOfAnUpdate = do
  let docmdp p old =
        let n = next_number old
            transform = F.Dict (M.fromList [ ("TransformMethod", F.Name "DocMDP")
                                           , ("TransformParams", F.Dict (M.fromList [("P", p)])) ])
        in with_catalog_key "Perms" (F.Dict (M.fromList [("DocMDP", F.Ref n 0)])) $
             with_objects [ (n, F.Plain (F.Dict (M.fromList [("Type", F.Name "Sig"), ("Reference", F.Array [transform])])))
                          , (n + 1, F.Plain (F.Integer 1)) ] old
      -- a signed field whose /Lock has a /P (ISO 32000-2, 12.7.5.5)
      locked p old =
        let n = next_number old
        in with_catalog_key "AcroForm" (F.Dict (M.fromList [("Fields", F.Array [F.Ref n 0])])) $
             with_objects [ (n, F.Plain (F.Dict (M.fromList
                              [ ("FT", F.Name "Sig"), ("T", F.String "Locked"), ("V", F.Ref (n + 1) 0)
                              , ("Lock", F.Dict (M.fromList [("Type", F.Name "SigFieldLock"), ("P", p)])) ])))
                          , (n + 1, F.Plain (F.Dict (M.fromList [("Type", F.Name "Sig")]))) ] old
      cases = [ (docmdp (F.Integer 1), True), (docmdp (F.Integer 2), False)
              , (\old -> docmdp (F.Ref (next_number old + 1) 0) old, True)
              , (locked (F.Integer 1), True), (locked (F.Integer 2), False) ]
  forM_ (zip [0 :: Int ..] cases) $ \(i, (certify, forbidden)) -> do
    src0 <- source "" hpdf_doc
    certified_bytes <- update src0 (certify (F.sourceFile src0))
    src <- source "" certified_bytes
    r <- F.updatePdfFile src (with_secret (F.sourceFile src))
    assertBoolVerbose (show i) (forbidden == either (const True) (const False) r)
    let ignore = F.defaultUpdateOptions { F.updateIgnoreCertification = True }
    ignored <- F.updatePdfFileWith ignore src (with_secret (F.sourceFile src))
    assertBool (either (const False) (const True) ignored)

{-# LANGUAGE OverloadedStrings #-}
-- | The object streams and the cross-reference streams of the writer.
-- See Note [Object streams of the writer] in Graphics.PDF.File.Write
module Tests.ObjectStreams (tests) where

import Test.Tasty (TestTree, testGroup)
import Tests.Common
import qualified Graphics.PDF as P
import qualified Graphics.PDF.File as F
import Graphics.PDF.Encryption (encryptPdf)
import Graphics.PDF.Signature (Signature(..), addSignature, signature, signPdf)
import qualified Codec.Compression.Zlib as Zlib
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Graphics.PDF.Expression (PDFExpression)

tests :: TestTree
tests = testGroup "ObjectStreams"
  [ test "objectsInObjectStreams" test_objectsInObjectStreams
  , test "rowsOfTheCrossReferenceStream" test_rowsOfTheCrossReferenceStream
  , test "objectsThatStayInTheFile" test_objectsThatStayInTheFile
  , test "encryptedObjectStreams" test_encryptedObjectStreams
  , test "signedDocument" test_signedDocument
  , test "updateOfACompactFile" test_updateOfACompactFile
  , test "malformedCompactFiles" test_malformedCompactFiles
  , test "numbersOfTheNewObjects" test_numbersOfTheNewObjects
  , test "filesWithATable" test_filesWithATable
  , test "keysOfTheTrailer" test_keysOfTheTrailer
  , test "valuesOfTheBuilder" test_valuesOfTheBuilder
  ]

-- | The reader with no repair: a file whose cross-reference stream is
-- not correct gives Left
strict_parse :: BS.ByteString -> Either String F.PdfFile
strict_parse = F.parsePdfFileWith F.defaultLimits { F.limitRepair = False }

-- | A file with a page and many objects that are not streams
many_objects :: Int -> F.PdfFile
many_objects count = F.PdfFile (1, 4) (M.fromList [("Root", F.Ref 1 0), ("Info", F.Dict (M.fromList [("Title", F.String "many")]))])
  (IM.fromList $
    [ (1, (0, dict [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0)]))
    , (2, (0, dict [("Type", F.Name "Pages"), ("Kids", F.Array [F.Ref 3 0]), ("Count", F.Integer 1)]))
    , (3, (0, dict [("Type", F.Name "Page"), ("Parent", F.Ref 2 0), ("Contents", F.Ref 4 0)
                   , ("MediaBox", F.Array (map F.Integer [0, 0, 100, 100]))]))
    , (4, (0, F.Stream (M.fromList [("Length", F.Integer 11)]) "0 0 m 1 1 l"))
    ] ++
    -- values whose ends need a delimiter: numbers, names, a reference
    [ (n, (0, F.Plain v)) | (n, v) <- zip [5 .. 4 + count] (cycle [F.Integer 7, F.Name "N", F.Array [F.Ref 1 0], F.Real 5 1, F.Bool True, F.Null, F.String "s)"]) ])
  Nothing
  where
    dict = F.Plain . F.Dict . M.fromList

-- | The rows of the cross-reference stream of a file: the fields of each
-- entry, with the predictor and the compression removed here, and the
-- numbers of the /Index
xref_rows :: BS.ByteString -> IO (F.Dict, [(Int, [Integer])])
xref_rows bytes = do
  -- "startxref\n<offset>\n%%EOF\n" at the end of the file: the newest
  -- section
  let start = maybe 0 fst (C.readInt (BS.drop (10 + last (0 : occurrences "startxref\n" bytes)) bytes))
      -- "n 0 obj\n<< ... >>\nstream\n<data>\nendstream"
      (header, rest) = BS.breakSubstring "\nstream\n" (BS.drop start bytes)
      dict_bytes = snd (BS.breakSubstring "<<" header)
  d <- case F.parseValue dict_bytes of
    Right (F.Dict d) -> return d
    other -> assertFailure ("no cross-reference stream: " ++ show other)
  assertEqual (Just (F.Name "XRef")) (M.lookup "Type" d)
  let Just (F.Array ws) = M.lookup "W" d
      widths = [ fromIntegral w | F.Integer w <- ws ]
      Just (F.Array index) = M.lookup "Index" d
      Just (F.Integer len) = M.lookup "Length" d
      columns = sum widths
      decoded = BL.toStrict (Zlib.decompress (BL.fromStrict (BS.take (fromIntegral len) (BS.drop 8 rest))))
      raw_rows = chunks (columns + 1) decoded
      -- the predictor "Up": each byte plus the byte above it
      rows = drop 1 (scanl (\above r -> zipWith (+) above (BS.unpack (BS.drop 1 r))) (replicate columns 0) raw_rows)
      fields r = snd (foldl (\(xs, acc) w -> (drop w xs, acc ++ [foldl (\a x -> a * 256 + fromIntegral x) 0 (take w xs)])) (r, []) widths)
      numbers = concat [ [first .. first + count - 1] | (F.Integer first, F.Integer count) <- pairs index ]
  -- each row has the type 2 of PNG (Up)
  assertBool (all ((== 2) . BS.head) raw_rows)
  assertEqual (length numbers) (length rows)
  return (d, zip (map fromIntegral numbers) (map fields rows))
  where
    chunks n b | BS.null b = [] | otherwise = BS.take n b : chunks n (BS.drop n b)
    pairs (a : b : rest) = (a, b) : pairs rest
    pairs _ = []

-- | The dictionary and the decoded data of the first object stream of a
-- file
object_stream_data :: BS.ByteString -> IO (F.Dict, BS.ByteString)
object_stream_data bytes = do
  let before = fst (BS.breakSubstring "/Type /ObjStm" bytes)
      -- the dictionary starts after the last "obj\n" before /Type
      start = BS.length before - BS.length (snd (BS.breakSubstring "<<" (last_part before)))
      last_part b = snd (BS.breakSubstring "obj\n" (BS.drop (last_of "obj\n" b) b))
      last_of pat b = last (0 : occurrences pat b)
      (dict_bytes, rest) = BS.breakSubstring "\nstream\n" (BS.drop start bytes)
  d <- case F.parseValue dict_bytes of
    Right (F.Dict d) -> return d
    other -> assertFailure ("no object stream: " ++ show other)
  let Just (F.Integer len) = M.lookup "Length" d
  return (d, BL.toStrict (Zlib.decompress (BL.fromStrict (BS.take (fromIntegral len) (BS.drop 8 rest)))))

-- 250 objects that are not streams: three object streams (100, 100 and
-- 50 objects). The reader reads the file with no repair, with the same
-- objects. The writer gives the same bytes for the file that it reads.
test_objectsInObjectStreams :: IO ()
test_objectsInObjectStreams = do
  let file = many_objects 246
      bytes = BL.toStrict (F.renderPdfFile file)
  assertBool ("%PDF-1.5\n" `BS.isPrefixOf` bytes)
  assertEqual 3 (length (occurrences "/Type /ObjStm" bytes))
  assertEqual ["100", "100", "50"] [ C.takeWhile (/= ' ') (BS.drop (at + 3) bytes) | at <- occurrences "/N " bytes ]
  -- the first object stream: the numbers and the offsets, then the
  -- objects from /First (the catalog is the first one)
  (d, decoded) <- object_stream_data bytes
  let Just (F.Integer first) = M.lookup "First" d
      table = map (maybe 0 fst . C.readInt) (C.words (BS.take (fromIntegral first) decoded))
      objects = BS.drop (fromIntegral first) decoded
  assertEqual [1, 0, 2] (take 3 table)
  assertBool ("<< /Pages 2 0 R /Type /Catalog >>\n" `BS.isPrefixOf` objects)
  assertBool ("<< /Count 1 " `BS.isPrefixOf` BS.drop (table !! 3) objects)
  read_again <- either assertFailure return (strict_parse bytes)
  assertEqual (F.pdfObjects file) (IM.delete 251 (F.pdfObjects read_again))
  assertEqual (Just (F.Dict (M.fromList [("Title", F.String "many")]))) (F.resolveValue read_again <$> M.lookup "Info" (F.pdfTrailer read_again))
  assertEqual bytes (BL.toStrict (F.renderPdfFile read_again))

-- The cross-reference stream: an entry for each object, in a stream
-- (type 2), in the file (type 1), or free (type 0), and its own entry
test_rowsOfTheCrossReferenceStream :: IO ()
test_rowsOfTheCrossReferenceStream = do
  let file = many_objects 246
      bytes = BL.toStrict (F.renderPdfFile file)
  (d, rows) <- xref_rows bytes
  -- 1 to 250, the /Info (251), three object streams, the cross-reference stream
  assertEqual [0 .. 255] (map fst rows)
  assertEqual (Just (F.Integer 256)) (M.lookup "Size" d)
  assertEqual (Just (F.Ref 1 0)) (M.lookup "Root" d)
  assertEqual (Just (F.Ref 251 0)) (M.lookup "Info" d)
  assertEqual (Just (F.Name "FlateDecode")) (M.lookup "Filter" d)
  let row n = lookup n rows
      offset_of n = BS.length (fst (BS.breakSubstring (C.pack ("\n" ++ show n ++ " 0 obj\n")) bytes)) + 1
  assertEqual (Just [0, 0, 65535]) (row 0)
  -- the objects that are not streams: the object stream and the index
  forM_ ([1, 2, 3] ++ [5 .. 251]) $ \n -> do
    let k = length (filter (< n) ([1, 2, 3] ++ [5 .. 251]))
    assertEqual (Just [2, fromIntegral (252 + k `div` 100), fromIntegral (k `mod` 100)]) (row n)
  -- the stream, the object streams and the cross-reference stream: their offsets
  forM_ [4, 252, 253, 254, 255] $ \n ->
    assertEqual (Just [1, fromIntegral (offset_of n), 0]) (row n)


-- These objects are not in an object stream: a stream, an object whose
-- generation is not 0, a signature dictionary, and the objects of an
-- encrypted file (the writer does not have its key). An object 0 is not
-- written.
test_objectsThatStayInTheFile :: IO ()
test_objectsThatStayInTheFile = do
  let sig = F.Dict (M.fromList [ ("Type", F.Name "Sig"), ("Filter", F.Name "Adobe.PPKLite")
                               , ("ByteRange", F.Array (replicate 4 (F.Integer 0))), ("Contents", F.String (BS.replicate 8 0)) ])
      base = many_objects 3
      file = base { F.pdfObjects = IM.insert 0 (0, F.Plain (F.Integer 1)) $ IM.insert 20 (2, F.Plain (F.Integer 42))
                                   $ IM.insert 21 (0, F.Plain sig) $ IM.insert 22 (0, F.Plain (F.Ref 1 0)) (F.pdfObjects base) }
      bytes = BL.toStrict (F.renderPdfFile file)
  -- a stream, a generation that is not 0, a signature dictionary, a
  -- reference
  forM_ ["\n4 0 obj\n", "\n20 2 obj\n", "\n21 0 obj\n", "\n22 0 obj\n1 0 R\n"] $ \o -> assertBool (o `BS.isInfixOf` bytes)
  forM_ ["\n0 0 obj\n", "\n1 0 obj\n", "\n5 0 obj\n"] $ \o -> assertBool (not (o `BS.isInfixOf` bytes))
  -- the signature has its byte range: Note [Signatures of the writer]
  assertBool (not ("/ByteRange [0 0 0 0]" `BS.isInfixOf` bytes))
  -- the entry 0 is free: the object 0 is not in an object stream
  (_, rows) <- xref_rows bytes
  assertEqual (Just [0, 0, 65535]) (lookup 0 rows)
  read_again <- either assertFailure return (strict_parse bytes)
  assertEqual (IM.delete 0 (F.pdfObjects file)) (IM.filterWithKey (\k _ -> k /= 23) (IM.map unsign (F.pdfObjects read_again)))
  -- an encrypted file: a cross-reference stream, and no object stream
  encrypted <- F.encryptPdfFile (F.encryption "user" "owner") base >>= either assertFailure return
  let encrypted_bytes = BL.toStrict (F.renderPdfFile encrypted)
  assertBool ("/Type /XRef" `BS.isInfixOf` encrypted_bytes)
  assertBool (not ("/ObjStm" `BS.isInfixOf` encrypted_bytes))
  opened <- either assertFailure return (F.parsePdfFileWithPassword F.defaultLimits "user" encrypted_bytes)
  assertEqual (IM.keys (F.pdfObjects encrypted)) (IM.keys (F.pdfObjects opened))
  assertEqual (F.getObject base 7) (F.getObject opened 7)
  where
    -- the byte range that the writer gives
    unsign (g, F.Plain (F.Dict d)) | F.isSignatureDict d = (g, F.Plain (F.Dict (M.insert "ByteRange" (F.Array (replicate 4 (F.Integer 0))) d)))
    unsign o = o

-- 'renderEncryptedPdfFile' and 'encryptPdf' put the objects in object
-- streams, then encrypt the object streams: the objects in them are not
-- encrypted again.
test_encryptedObjectStreams :: IO ()
test_encryptedObjectStreams = do
  let file = many_objects 246
  bytes <- F.renderEncryptedPdfFile F.defaultRenderOptions (F.encryption "user" "owner") file >>= either assertFailure (return . BL.toStrict)
  assertEqual 3 (length (occurrences "/Type /ObjStm" bytes))
  -- the strings of the objects are not in the bytes
  assertBool (not ("(s\\))" `BS.isInfixOf` bytes))
  (_, rows) <- xref_rows bytes
  assertEqual 250 (length [ () | (_, 2 : _) <- rows ])
  forM_ ["user", "owner"] $ \password -> do
    opened <- either assertFailure return (F.parsePdfFileWithPassword F.defaultLimits password bytes)
    assertEqual (1, 7) (F.pdfVersion opened)
    -- the same objects, except the catalog (with /Extensions) and the /Info object
    assertEqual (IM.delete 1 (F.pdfObjects file)) (IM.delete 1 (IM.delete 251 (F.pdfObjects opened)))
    assertEqual (Just (F.Dict (M.fromList [("Title", F.String "many")]))) (F.resolveValue opened <$> M.lookup "Info" (F.pdfTrailer opened))
  -- without object streams: a cross-reference table
  table <- F.renderEncryptedPdfFile (F.defaultRenderOptions { F.renderObjectStreams = False }) (F.encryption "user" "owner") file
             >>= either assertFailure (return . BL.toStrict)
  assertBool ("\nxref\n" `BS.isInfixOf` table)
  assertBool (not ("/ObjStm" `BS.isInfixOf` table))
  -- a file whose numbers are too large for a cross-reference stream: a
  -- table (Note [Object streams of the writer] in Graphics.PDF.File.Write)
  let large = file { F.pdfObjects = IM.insert 8388607 (0, F.Plain (F.Integer 1)) (F.pdfObjects file) }
  large_bytes <- F.renderEncryptedPdfFile F.defaultRenderOptions (F.encryption "user" "owner") large
                   >>= either assertFailure (return . BL.toStrict)
  assertBool ("\nxref\n" `BS.isInfixOf` large_bytes)
  assertBool (not ("/XRef" `BS.isInfixOf` large_bytes))
  opened <- either assertFailure return (F.parsePdfFileWithPassword F.defaultLimits "user" table)
  assertEqual (IM.delete 1 (F.pdfObjects file)) (IM.delete 1 (IM.delete 251 (F.pdfObjects opened)))

-- A document of the builder with a signature field: the objects are in
-- object streams, and the signature dictionary is not. It is signed, also
-- after the encryption.
test_signedDocument :: IO ()
test_signedDocument = do
  let unsigned = P.renderPdf P.standardDocInfo (P.PDFRect 0 0 300 200) $ do
        page <- P.addPage Nothing
        addSignature page signature { sigName = T.pack "A. Signer", sigSize = 64 }
      raw = BL.toStrict unsigned
  assertBool ("/Type /ObjStm" `BS.isInfixOf` raw)
  assertBool ("/ByteRange [" `BS.isInfixOf` raw)
  encrypted <- encryptPdf (F.encryption "user" "owner") unsigned >>= either assertFailure return
  forM_ [(unsigned, ""), (encrypted, "user")] $ \(doc, password) -> do
    signed <- signPdf (\_ -> return (BS.pack [0xDE, 0xAD])) doc >>= either assertFailure return
    assertEqual (BL.length doc) (BL.length signed)
    file <- either assertFailure return (F.parsePdfFileWithPassword F.defaultLimits password (BL.toStrict signed))
    assertEqual [BS.pack (0xDE : 0xAD : replicate 62 0)]
                [ s | (_, F.Plain (F.Dict d)) <- IM.elems (F.pdfObjects file), F.isSignatureDict d, Just (F.String s) <- [M.lookup "Contents" d] ]

-- An incremental update of a file with object streams: a cross-reference
-- stream with /Prev, after the bytes of the file
test_updateOfACompactFile :: IO ()
test_updateOfACompactFile = do
  let doc = BL.toStrict (P.renderPdf P.standardDocInfo (P.PDFRect 0 0 300 200) (P.addPage Nothing >> return ()))
  -- an encrypted file: the object streams of the update are encrypted,
  -- and the objects in them are not encrypted again
  encrypted <- encryptPdf (F.encryption "user" "owner") (BL.fromStrict doc) >>= either assertFailure (return . BL.toStrict)
  forM_ [("plain" :: String, doc, ""), ("encrypted", encrypted, "owner")] $ \(name, bytes, password) -> do
    src <- either assertFailure return (F.parsePdfSource F.defaultLimits password bytes)
    -- the numbers below /Size are the numbers of the file: an update
    -- gives a new number to a new object with such a number
    (old_d, old_rows) <- xref_rows bytes
    let file = F.sourceFile src
        Just (F.Integer old_size) = M.lookup "Size" old_d
        Just (F.Ref root _) = M.lookup "Root" (F.pdfTrailer file)
        Just (_, F.Plain (F.Dict catalog)) = IM.lookup root (F.pdfObjects file)
        -- a reference to no object, after all the numbers of the file
        new_object = fromIntegral old_size
        -- the next number: the first number of a new object of the update
        missing = new_object + 1
        changed = file { F.pdfObjects = IM.insert root (0, F.Plain (F.Dict (M.insert "Lang" (F.String "en") catalog)))
                                          (IM.insert new_object (0, F.Plain (F.Array [F.String "s", F.Ref missing 0, F.Ref 9000000 0])) (F.pdfObjects file)) }
        update options = F.updatePdfFileWith options src changed >>= either assertFailure (return . BL.toStrict)
        read_again b = either assertFailure return (F.parsePdfSource F.defaultLimits { F.limitRepair = False } password b)
    updated <- update F.defaultUpdateOptions
    assertEqual (name, bytes) (name, BS.take (BS.length bytes) updated)
    let added = BS.drop (BS.length bytes) updated
    assertBoolVerbose name ("/Type /XRef" `BS.isInfixOf` added && "/Prev " `BS.isInfixOf` added)
    assertEqual (name, 1) (name, length (occurrences "/Type /ObjStm" added))
    again <- read_again updated
    assertEqual (name, F.pdfObjects changed) (name, F.pdfObjects (F.sourceFile again))
    -- the two objects are in the object stream of the update, whose
    -- number is after the reference to no object
    (d, rows) <- xref_rows updated
    let Just [2, container, 0] = lookup root rows
        Just [2, container', 1] = lookup new_object rows
        Just (F.Integer size) = M.lookup "Size" d
    assertEqual (name, container) (name, container')
    assertBoolVerbose name (container > fromIntegral missing && size == container + 2)
    assertEqual (name, Just [1]) (name, take 1 <$> lookup (fromIntegral container) rows)
    -- a trailer /Info dictionary becomes an object, and an object with
    -- the number of the old object stream gets a new number: both after
    -- the reference to no object
    old_container <- case [ fromIntegral c | (_, [2, c, _]) <- old_rows ] of
      c : _ -> return c
      [] -> assertFailure "no object stream in the file"
    with_info <- F.updatePdfFile src changed { F.pdfTrailer = M.insert "Info" (F.Dict (M.fromList [("Title", F.String "t")])) (F.pdfTrailer changed) }
                   >>= either assertFailure (return . BL.toStrict)
    info_again <- F.sourceFile <$> read_again with_info
    let Just (F.Ref info_n 0) = M.lookup "Info" (F.pdfTrailer info_again)
    assertBoolVerbose name (info_n > missing && IM.notMember missing (F.pdfObjects info_again))
    renumbered <- F.updatePdfFile src changed { F.pdfObjects = IM.insert old_container (0, F.Plain (F.String "renumbered")) (F.pdfObjects changed) }
                    >>= either assertFailure (return . BL.toStrict)
    renumbered_again <- F.sourceFile <$> read_again renumbered
    assertBoolVerbose name (IM.notMember missing (F.pdfObjects renumbered_again))
    assertBoolVerbose name (any (\(n, (_, o)) -> n > missing && o == F.Plain (F.String "renumbered")) (IM.toList (F.pdfObjects renumbered_again)))
    -- with no object streams: the objects are in the file
    plain <- update F.defaultUpdateOptions { F.updateObjectStreams = False }
    let added' = BS.drop (BS.length bytes) plain
    assertBoolVerbose name ("/Type /XRef" `BS.isInfixOf` added' && not ("/Type /ObjStm" `BS.isInfixOf` added'))
    again' <- read_again plain
    assertEqual (name, F.pdfObjects changed) (name, F.pdfObjects (F.sourceFile again'))
    (d', rows') <- xref_rows plain
    assertEqual (name, Just [1]) (name, take 1 <$> lookup root rows')
    -- the cross-reference stream too has a number after the reference
    -- to no object
    let Just (F.Integer size') = M.lookup "Size" d'
    assertBoolVerbose name (size' == fromIntegral missing + 2 && lookup missing rows' == Nothing)
  -- an object stream would get a number above 8,388,607: the objects of
  -- the update stay in the file
  let large = (many_objects 3) { F.pdfObjects = IM.insert 8388500 (0, F.Plain (F.Integer 1)) (F.pdfObjects (many_objects 3)) }
      large_bytes = BL.toStrict (F.renderPdfFile large)
  assertBool ("/Type /XRef" `BS.isInfixOf` large_bytes)
  src <- either assertFailure return (F.parsePdfSource F.defaultLimits "" large_bytes)
  let near = F.sourceFile src
      near' = near { F.pdfObjects = IM.insert 5 (0, F.Plain (F.Array [F.Ref 8388606 0])) (F.pdfObjects near) }
  updated <- F.updatePdfFile src near' >>= either assertFailure (return . BL.toStrict)
  let added = BS.drop (BS.length large_bytes) updated
  assertBool ("/Type /XRef" `BS.isInfixOf` added && not ("/Type /ObjStm" `BS.isInfixOf` added))
  again <- either assertFailure return (strict_parse updated)
  assertEqual (F.pdfObjects near') (F.pdfObjects again)

-- Prefixes and mutations of a file with object streams give Left or a
-- file, and no exception
test_malformedCompactFiles :: IO ()
test_malformedCompactFiles = do
  let bytes = BL.toStrict (F.renderPdfFile (many_objects 120))
      outcome b = case F.parsePdfFile b of
        Left e -> length e
        Right f -> IM.size (F.pdfObjects f) + fromIntegral (BL.length (F.renderPdfFile f))
  forM_ [0, 7 .. BS.length bytes] $ \n -> assertBool (outcome (BS.take n bytes) >= 0)
  forM_ (mutationsOf 1000 bytes) $ \m -> assertBool (outcome m >= 0)

-- The new objects get the numbers after all the numbers of the file, also
-- the numbers of the references: a reference to an object that the file
-- does not have stays a reference to no object
test_numbersOfTheNewObjects :: IO ()
test_numbersOfTheNewObjects = do
  let base = many_objects 3
      file = base { F.pdfObjects = IM.insert 5 (0, F.Plain (F.Array [F.Ref 300 0])) (F.pdfObjects base) }
      bytes = BL.toStrict (F.renderPdfFile file)
  (d, rows) <- xref_rows bytes
  -- the /Info, the object stream, the cross-reference stream
  assertEqual (Just (F.Ref 301 0)) (M.lookup "Info" d)
  assertEqual [302, 303] (filter (> 301) (map fst rows))
  read_again <- either assertFailure return (strict_parse bytes)
  assertEqual F.Null (F.resolveValue read_again (F.Ref 300 0))

-- A file with a cross-reference table, also with 'F.renderObjectStreams':
-- a PDF/A-1 file (ISO 19005-1 forbids the cross-reference streams), and
-- a file whose numbers are larger than the limit of PDF (8,388,607)
test_filesWithATable :: IO ()
test_filesWithATable = do
  let base = many_objects 3
      with_part part = base { F.pdfObjects = IM.insert 1 (0, F.Plain (F.Dict (M.fromList [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0), ("Metadata", F.Ref 9 0)])))
                                           $ IM.insert 9 (0, F.Stream (M.fromList [("Type", F.Name "Metadata"), ("Subtype", F.Name "XML"), ("Length", F.Integer (fromIntegral (BS.length (xmp part))))]) (xmp part))
                                           (F.pdfObjects base) }
      xmp part = C.pack ("<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:Description pdfaid:part='" ++ part ++ "' pdfaid:conformance='B'/></x:xmpmeta>")
      table file = do
        let bytes = BL.toStrict (F.renderPdfFile file)
        assertBool ("\nxref\n" `BS.isInfixOf` bytes)
        assertBool (not ("/XRef" `BS.isInfixOf` bytes))
        read_again <- either assertFailure return (strict_parse bytes)
        assertEqual (F.pdfObjects file) (F.pdfObjects read_again)
  table (with_part "1")
  table base { F.pdfObjects = IM.insert 8388607 (0, F.Plain (F.Integer 1)) (F.pdfObjects base), F.pdfTrailer = M.delete "Info" (F.pdfTrailer base) }
  -- a generation above 65535 (PDF 32000-1, Annex C): not truncated in a
  -- cross-reference stream
  let large_generation = BL.toStrict (F.renderPdfFile base { F.pdfObjects = IM.insert 5 (70000, F.Plain (F.Integer 1)) (F.pdfObjects base) })
  assertBool ("\nxref\n" `BS.isInfixOf` large_generation)
  assertBool ("\n5 70000 obj\n" `BS.isInfixOf` large_generation)
  -- PDF/A-2 and PDF/A-3 accept the cross-reference streams
  assertBool ("/Type /XRef" `BS.isInfixOf` BL.toStrict (F.renderPdfFile (with_part "2")))

-- The keys of a cross-reference section in the trailer of a 'F.PdfFile'
-- are not written: the writer gives its own section
test_keysOfTheTrailer :: IO ()
test_keysOfTheTrailer = do
  let base = many_objects 3
      file = base { F.pdfTrailer = M.union (F.pdfTrailer base) (M.fromList [("Prev", F.Integer 99999), ("XRefStm", F.Integer 5), ("DL", F.Integer 7)]) }
  forM_ [F.defaultRenderOptions, F.defaultRenderOptions { F.renderObjectStreams = False }] $ \options -> do
    let bytes = BL.toStrict (F.renderPdfFileWith options file)
    assertBool (not (any (`BS.isInfixOf` bytes) ["/Prev", "/XRefStm", "/DL"]))
    either assertFailure (const (return ())) (strict_parse bytes)

-- The builder makes a 'F.PdfFile' from its objects. A deep nesting of
-- values gets object streams; a number of more than 255 digits, which the
-- lexer does not read, keeps a table. See Note [Object streams of a
-- document] in Graphics.PDF
test_valuesOfTheBuilder :: IO ()
test_valuesOfTheBuilder = do
  let nested :: Int -> P.Function1 P.Local P.PDFFloat (PDFExpression P.PDFFloat)
      nested 0 = P.Interpolated1 1 0 1
      nested n = P.Stitched1 (nested (n - 1)) [(0.5, P.Interpolated1 1 0 1)]
      top :: P.Function1 P.Global P.PDFFloat (PDFExpression P.PDFFloat)
      top = P.Stitched1 (nested 200) [(0.5, P.Interpolated1 1 0 1)]
      render rect doc = either (assertFailure . unlines) (return . BL.toStrict)
                          (P.renderPdfE P.standardDocInfo (P.PDFRect 0 0 200 200) (doc >> P.addPage rect >> return ()))
  deep <- render Nothing (() <$ P.createFunction1Object top)
  assertBool ("/Type /ObjStm" `BS.isInfixOf` deep)
  file <- either assertFailure return (F.parsePdfFileWith F.defaultLimits { F.limitDepth = 1000 } deep)
  assertBool (IM.size (F.pdfObjects file) > 3)
  large <- render (Just (P.PDFRect 0 0 1e300 1e300)) (return ())
  assertBool ("\nxref\n" `BS.isInfixOf` large && not ("/Type /ObjStm" `BS.isInfixOf` large))

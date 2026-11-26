{-# LANGUAGE OverloadedStrings #-}
-- | The repair of a file whose cross-reference is not correct.
-- See Note [Repair of a file] in Graphics.PDF.File.Parse
module Tests.Repair (tests) where

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Tests.Common
import qualified Graphics.PDF.File as F
import qualified Graphics.PDF as P
import Control.Exception (evaluate)
import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (isInfixOf, isPrefixOf)

tests :: TestTree
tests = testGroup "Repair"
  [ test "wrongStartxref" test_wrongStartxref
  , test "wrongOffsets" test_wrongOffsets
  , test "noCrossReference" test_noCrossReference
  , test "objectStreams" test_objectStreams
  , test "encryptedFiles" test_encryptedFiles
  , test "updatesOfTheFile" test_updatesOfTheFile
  , test "objectsInStreams" test_objectsInStreams
  , test "garbageBeforeTheHeader" test_garbageBeforeTheHeader
  , test "noRepair" test_noRepair
  , localOption (mkTimeout 20000000) (test "linearScan" test_linearScan)
  , localOption (mkTimeout 120000000) (test "malformedRepairs" test_malformedRepairs)
  , test "limitsOfTheRepair" test_limitsOfTheRepair
  , test "errorsThatAreNotRepaired" test_errorsThatAreNotRepaired
  , test "indirectLengths" test_indirectLengths
  ]

-- Only a structure that is not correct is repaired. An error of a value,
-- of the limits or of the password is the error of the file: a repair
-- would hide it, or give an older version of an object.
-- See Note [Repair of a file] in Graphics.PDF.File.Parse
test_errorsThatAreNotRepaired :: IO ()
test_errorsThatAreNotRepaired = do
  let not_repaired name r = case r of
        Left e -> assertBoolVerbose (name ++ ": " ++ e) (not ("repair" `isInfixOf` e))
        Right _ -> assertFailure (name ++ ": the file was read")
      dict = F.Plain . F.Dict . M.fromList
      deep = iterate (\v -> F.Array [v]) F.Null !! 300
      file = F.PdfFile (1, 7) (M.fromList [("Root", F.Ref 1 0)]) (IM.fromList (map (fmap (\o -> (0, o)))
        [ (1, dict [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0), ("Deep", F.Ref 3 0)])
        , (2, dict [("Type", F.Name "Pages"), ("Kids", F.Array []), ("Count", F.Integer 0)])
        , (3, F.Plain deep) ])) Nothing
  -- a value nested too deep, in a file whose cross-reference is correct
  not_repaired "depth" (F.parsePdfFile (BL.toStrict (F.renderPdfFile file)))
  -- the newest version of an object is not correct: not the older one
  src <- source "" hpdf_doc
  let old = F.sourceFile src
      n = maybe 1 ((+ 1) . fst) (IM.lookupMax (F.pdfObjects old))
      with_string = case F.dictLookup "Root" (F.pdfTrailer old) of
        Just (F.Ref c g) | Just (F.Plain (F.Dict d)) <- F.getObject old c ->
          old { F.pdfObjects = IM.insert c (g, F.Plain (F.Dict (M.insert "Text" (F.String "abcdef") d))) (F.pdfObjects old) }
        _ -> old
  updated <- F.updatePdfFile src with_string >>= either assertFailure (return . BL.toStrict)
  assertBool (n > 0)
  let broken_value = BS.take (BS.length hpdf_doc) updated
                     <> replaceOnce "(abcdef)" "(abcdef(" (BS.drop (BS.length hpdf_doc) updated)
  not_repaired "newest version" (F.parsePdfFile broken_value)
  -- a limit of the reader, also for the object stream of a repaired file:
  -- its catalog is newer than the catalog of the file
  objstm <- fixture "objstm"
  not_repaired "limit" (F.parsePdfFileWith F.defaultLimits { F.limitStream = 10 } objstm)
  let older_catalog = "6 0 obj << /Type /Catalog /Pages 2 0 R /Old true >> endobj\n"
      broken = with_startxref "12" objstm
      (header_line, rest) = BS.breakSubstring "\n" broken
  case F.parsePdfFileWith F.defaultLimits { F.limitStream = 10 } (header_line <> "\n" <> older_catalog <> BS.drop 1 rest) of
    Left e -> assertBoolVerbose e ("limit" `isInfixOf` e)
    Right _ -> assertFailure "an object stream that a limit stops was skipped"
  -- a wrong password
  r6 <- fixture "r6-aes-256"
  not_repaired "password" (F.parsePdfFileWithPassword F.defaultLimits (T.pack "wrong") r6)
  where
    replaceOnce a b x = let (y, z) = BS.breakSubstring a x in y <> b <> BS.drop (BS.length a) z

-- A /Length that is a reference: the data of the stream ends at its
-- length, also when it has "endstream" and objects (an embedded PDF)
test_indirectLengths :: IO ()
test_indirectLengths = do
  let dict = F.Plain . F.Dict . M.fromList
      embedded = "%PDF-1.4\n2 0 obj\n<< /Length 3 >>\nstream\nabc\nendstream\nendobj\n1 0 obj\n<< /Type /Catalog /Evil true >>\nendobj\n"
      file = F.PdfFile (1, 7) (M.fromList [("Root", F.Ref 1 0)]) (IM.fromList (map (fmap (\o -> (0, o)))
        [ (1, dict [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0), ("Names", F.Ref 4 0)])
        , (2, dict [("Type", F.Name "Pages"), ("Kids", F.Array []), ("Count", F.Integer 0)])
        , (4, F.Stream (M.fromList [("Length", F.Integer (fromIntegral (BS.length embedded)))]) embedded)
        , (5, F.Plain (F.Integer (fromIntegral (BS.length embedded)))) ])) Nothing
      bytes = BL.toStrict (F.renderPdfFile file)
      -- the /Length of the stream is the object 5, after the stream (with
      -- the same number of bytes: the offsets stay correct)
      indirect = replaceOnce (C.pack ("<< /Length " ++ show (BS.length embedded) ++ " >>")) "<</Length 5 0 R>>" bytes
  assertEqual (BS.length bytes) (BS.length indirect)
  good <- source "" indirect
  assertBool (not (F.sourceRepaired good))
  repaired <- source "" (with_startxref "12" indirect)
  assertBool (F.sourceRepaired repaired)
  assertEqual (F.pdfObjects (F.sourceFile good)) (F.pdfObjects (F.sourceFile repaired))
  where
    replaceOnce a b x = let (y, z) = BS.breakSubstring a x in y <> b <> BS.drop (BS.length a) z

-- The object streams that the repair reads count in the limits: the
-- decoded data ('F.limitTotal') and the entries ('F.limitObjects').
-- See Note [Repair of a file] in Graphics.PDF.File.Parse
test_limitsOfTheRepair :: IO ()
test_limitsOfTheRepair = do
  objstm <- fixture "objstm"
  let broken = with_startxref "12" objstm
      ok limits b = either (const False) (const True) (F.parsePdfSource limits T.empty b)
      total l = F.defaultLimits { F.limitTotal = l }
      objects l = F.defaultLimits { F.limitObjects = l }
  -- the repair decodes the object stream two times: a limit that the file
  -- with its cross-reference keeps is too small for the repair
  assertBool (ok F.defaultLimits broken)
  assertBool (any (\l -> ok (total l) objstm && not (ok (total l) broken)) [0, 16 .. 8192])
  -- the entries of the object stream count: 3 objects of the file and 5
  -- objects of its object stream
  assertBool (not (ok (objects 4) broken))
  assertBool (ok (objects 8) broken)
  -- two object streams of 3 objects: 4 objects of the file, then 3 and 3
  let objstm n first = let header = C.pack (unwords [ show k ++ " " ++ show (2 * i) | (i, k) <- zip [0 :: Int ..] [first, first + 1, first + 2 :: Int] ] ++ " ")
                           body = header <> "1 2 3"
                       in C.pack (show (n :: Int) ++ " 0 obj << /Type /ObjStm /N 3 /First " ++ show (BS.length header) ++ " /Length " ++ show (BS.length body) ++ " >> stream\n")
                          <> body <> "\nendstream endobj\n"
      two = "%PDF-1.7\n1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n2 0 obj << /Type /Pages /Kids [] /Count 0 >> endobj\n"
            <> objstm 10 3 <> objstm 11 6 <> "startxref\n12\n%%EOF"
  assertBool (ok (objects 10) two)
  assertBool (not (ok (objects 7) two))

-- Files that only the repair reads (a wrong "startxref"), with changes:
-- each gives 'Left' or a file, never an exception. The file that the
-- reader gives is written again, and its pages are read.
-- See Note [Repair of a file] and Note [Limits of the reader] in
-- Graphics.PDF.File.Parse
test_malformedRepairs :: IO ()
test_malformedRepairs = do
  objstm <- fixture "objstm"
  r4 <- fixture "r4-aes-128"
  r6 <- fixture "r6-aes-256"
  r3 <- fixture "r3-rc4-128"
  let outcome password bytes = case F.parsePdfSource F.defaultLimits (T.pack password) bytes of
        Left e -> length e
        Right s -> let f = F.sourceFile s
                   in fromIntegral (BL.length (F.renderPdfFile f)) + length (F.pdfPages f) + fromEnum (F.sourceRepaired s)
  forM_ [("", hpdf_doc), ("", objstm), ("user", r4), ("user", r6), ("owner", r3)] $ \(password, original) -> do
    let broken = with_startxref "12" original
    forM_ [0, 13 .. BS.length broken] $ \n -> evaluate (outcome password (BS.take n broken)) >>= assertBool . (>= 0)
    forM_ (mutationsOf 400 broken) $ \m -> evaluate (outcome password m) >>= assertBool . (>= 0)

-- | A document of HPDF with two pages, not compressed
hpdf_doc :: BS.ByteString
hpdf_doc = BL.toStrict $ P.renderPdf P.standardDocInfo {P.compressed = False, P.author = "Repair"} (P.PDFRect 0 0 300 200) $
  mapM_ (\i -> do
           page <- P.addPage Nothing
           P.drawWithPage page $ P.stroke (P.Line 10 10 (100 * i) 100))
        [1, 2]

fixture :: FilePath -> IO BS.ByteString
fixture name = BS.readFile ("Test/encrypted/" ++ name ++ ".pdf")

source :: String -> BS.ByteString -> IO F.PdfSource
source password = either assertFailure return . F.parsePdfSource F.defaultLimits (T.pack password)

strict :: String -> BS.ByteString -> Either String F.PdfSource
strict password = F.parsePdfSource F.defaultLimits { F.limitRepair = False } (T.pack password)

is_left :: Either a b -> Bool
is_left = either (const True) (const False)

-- | The file with the number after the last "startxref" changed
with_startxref :: BS.ByteString -> BS.ByteString -> BS.ByteString
with_startxref n bytes =
  let (before, after) = breakLast "startxref" bytes
      rest = C.dropWhile (`elem` ("\r\n " :: String)) (BS.drop 9 after)
  in before <> "startxref\n" <> n <> C.dropWhile (\c -> c >= '0' && c <= '9') rest
  where
    breakLast pat b = case occurrences pat b of
      [] -> (b, "")
      ps -> BS.splitAt (last ps) b

-- | The repaired file, with the objects and the trailer of the file that
-- the reader reads with its cross-reference
assert_repaired :: String -> BS.ByteString -> BS.ByteString -> IO ()
assert_repaired password original broken = do
  good <- source password original
  assertBool (not (F.sourceRepaired good))
  assertBool (is_left (strict password broken))
  repaired <- source password broken
  assertBool (F.sourceRepaired repaired)
  assertEqual (F.pdfObjects (F.sourceFile good)) (F.pdfObjects (F.sourceFile repaired))
  assertEqual (F.pdfTrailer (F.sourceFile good)) (F.pdfTrailer (F.sourceFile repaired))

-- A "startxref" that gives a wrong offset. An update of a repaired file is
-- not possible: its /Prev would be wrong.
test_wrongStartxref :: IO ()
test_wrongStartxref = do
  forM_ ["12", "999999999", "0"] $ \n -> assert_repaired "" hpdf_doc (with_startxref n hpdf_doc)
  repaired <- source "" (with_startxref "12" hpdf_doc)
  r <- F.updatePdfFile repaired (F.sourceFile repaired)
  assertBool (is_left r)
  -- the file written again is correct
  rewritten <- source "" (BL.toStrict (F.renderPdfFile (F.sourceFile repaired)))
  assertBool (not (F.sourceRepaired rewritten))

-- Bytes inserted before an object: the offsets of the table are wrong.
-- Also one wrong entry, in a table whose offset is correct
test_wrongOffsets :: IO ()
test_wrongOffsets = do
  let (before, after) = BS.breakSubstring "\n2 0 obj" hpdf_doc
      broken = before <> "\n% a comment that moves the objects" <> after
  assertBool (not (BS.null after))
  assert_repaired "" hpdf_doc broken
  -- the entry of the object 2: the line 3 of the table, one more
  let (head_, table) = BS.breakSubstring "\nxref\n" hpdf_doc
      table_lines = C.lines table
      entry = table_lines !! 4
      offset = maybe 0 fst (C.readInt entry)
      entry' = C.pack (pad10 (offset + 1)) <> BS.drop 10 entry
      one_wrong = head_ <> C.intercalate "\n" (take 4 table_lines ++ [entry'] ++ drop 5 table_lines)
                  <> (if "\n" `BS.isSuffixOf` table then "\n" else "")
  assertEqual (BS.length hpdf_doc) (BS.length one_wrong)
  assert_repaired "" hpdf_doc one_wrong
  where
    pad10 x = let t = show x in replicate (10 - length t) '0' ++ t

-- A file with no cross-reference table and no trailer: the catalog is the
-- root, and the /Info of the trailer is lost
test_noCrossReference :: IO ()
test_noCrossReference = do
  let (objects, _) = BS.breakSubstring "\nxref" hpdf_doc
  good <- source "" hpdf_doc
  repaired <- source "" objects
  assertBool (F.sourceRepaired repaired)
  assertEqual (F.pdfObjects (F.sourceFile good)) (F.pdfObjects (F.sourceFile repaired))
  assertEqual (M.lookup "Root" (F.pdfTrailer (F.sourceFile good))) (M.lookup "Root" (F.pdfTrailer (F.sourceFile repaired)))
  assertEqual Nothing (M.lookup "Info" (F.pdfTrailer (F.sourceFile repaired)))
  -- no object at all
  assertBool (is_left (F.parsePdfFile "%PDF-1.7\nnothing here\n%%EOF"))

-- The objects of the object streams, and the trailer of a cross-reference
-- stream
test_objectStreams :: IO ()
test_objectStreams = do
  objstm <- fixture "objstm"
  assert_repaired "" objstm (with_startxref "12" objstm)
  -- the file with no cross-reference stream: the catalog is in an object
  -- stream
  let (no_xref, _) = BS.breakSubstring "/Type /XRef" objstm
      cut = BS.take (last (occurrences "\n8 0 obj" no_xref) + 1) no_xref
  good <- source "" objstm
  repaired <- source "" cut
  assertEqual (F.pdfObjects (F.sourceFile good)) (F.pdfObjects (F.sourceFile repaired))
  assertEqual (M.lookup "Root" (F.pdfTrailer (F.sourceFile good))) (M.lookup "Root" (F.pdfTrailer (F.sourceFile repaired)))

-- An encrypted file: the object streams are decrypted before their
-- numbers are read
test_encryptedFiles :: IO ()
test_encryptedFiles =
  forM_ ["r3-rc4-128", "r4-aes-128", "r6-aes-256"] $ \name -> do
    original <- fixture name
    forM_ ["user", "owner"] $ \p -> assert_repaired p original (with_startxref "12" original)

-- A file with an update: the newest objects win, also an object of an
-- object stream that comes after an object of the file
test_updatesOfTheFile :: IO ()
test_updatesOfTheFile = do
  src <- source "" hpdf_doc
  let old = F.sourceFile src
      changed = case F.dictLookup "Root" (F.pdfTrailer old) of
        Just (F.Ref n g) | Just (F.Plain (F.Dict d)) <- F.getObject old n ->
          old { F.pdfObjects = IM.insert n (g, F.Plain (F.Dict (M.insert "Changed" (F.Bool True) d))) (F.pdfObjects old) }
        _ -> old
  updated <- F.updatePdfFile src changed >>= either assertFailure (return . BL.toStrict)
  assert_repaired "" updated (with_startxref "12" updated)
  -- the update of a file with object streams is a cross-reference stream
  objstm <- fixture "objstm"
  src2 <- source "" objstm
  let old2 = F.sourceFile src2
      n = 4
  assertBool (IM.member n (F.pdfObjects old2))
  updated2 <- F.updatePdfFile src2 old2 { F.pdfObjects = IM.insert n (0, F.Plain (F.String "newer")) (F.pdfObjects old2) }
                >>= either assertFailure (return . BL.toStrict)
  assert_repaired "" updated2 (with_startxref "12" updated2)

-- "n g obj" and "trailer" in the data of a stream are not objects and not
-- trailers
test_objectsInStreams :: IO ()
test_objectsInStreams = do
  let dict = F.Plain . F.Dict . M.fromList
      decoy = "9 0 obj\n(not an object)\nendobj\ntrailer\n<< /Root 9 0 R >>\n"
      file = F.PdfFile (1, 7) (M.fromList [("Root", F.Ref 1 0)]) (IM.fromList (map (fmap (\o -> (0, o)))
        [ (1, dict [("Type", F.Name "Catalog"), ("Pages", F.Ref 2 0)])
        , (2, dict [("Type", F.Name "Pages"), ("Kids", F.Array [F.Ref 3 0]), ("Count", F.Integer 1)])
        , (3, dict [("Type", F.Name "Page"), ("Parent", F.Ref 2 0), ("Contents", F.Ref 4 0)])
        , (4, F.Stream (M.fromList [("Length", F.Integer (fromIntegral (BS.length decoy)))]) decoy) ])) Nothing
      bytes = BL.toStrict (F.renderPdfFile file)
  assert_repaired "" bytes (with_startxref "12" bytes)
  -- a number that is the end of a word ("x9") is not the number of an
  -- object
  let (before, after) = BS.breakSubstring "\n2 0 obj" bytes
      glued = before <> "\n%x9 0 obj (not an object) endobj" <> after
  glued_file <- source "" (with_startxref "12" glued)
  assertEqual Nothing (F.getObject (F.sourceFile glued_file) 9)
  -- with no trailer, the "trailer" of the stream does not give the root
  let (no_trailer, _) = BS.breakSubstring "\nxref" bytes
  cut <- source "" no_trailer
  assertEqual (Just (F.Ref 1 0)) (M.lookup "Root" (F.pdfTrailer (F.sourceFile cut)))
  -- a stream with no correct /Length ends at its "endstream"
  let no_length = replaceOnce (C.pack ("/Length " ++ show (BS.length decoy))) "/Length 99 0 R" bytes
  repaired <- source "" (with_startxref "12" no_length)
  assertEqual (Just (F.Stream (M.fromList [("Length", F.Integer (fromIntegral (BS.length decoy)))]) decoy))
              (F.getObject (F.sourceFile repaired) 4)
  assertEqual Nothing (F.getObject (F.sourceFile repaired) 9)
  where
    replaceOnce old new b = let (x, y) = BS.breakSubstring old b in x <> new <> BS.drop (BS.length old) y

-- Bytes before the header: the offsets of the scan are offsets in the file
test_garbageBeforeTheHeader :: IO ()
test_garbageBeforeTheHeader = do
  let original = "garbage before the header\n" <> hpdf_doc
  assert_repaired "" original (with_startxref "12" original)

-- With no repair ('F.limitRepair'), a wrong cross-reference gives 'Left'.
-- A file that the repair cannot read gives the error of its
-- cross-reference.
test_noRepair :: IO ()
test_noRepair = do
  let broken = with_startxref "12" hpdf_doc
  assertBool (is_left (strict "" broken))
  let no_object = "%PDF-1.7\nno object\nstartxref\n5\n%%EOF"
  case (strict "" no_object, F.parsePdfFile no_object) of
    (Left e, Left e') -> do
      assertBoolVerbose e' ((e ++ " (and the repair: ") `isPrefixOf` e')
      assertBoolVerbose e' ("no object" `isInfixOf` e')
    _ -> assertFailure "a file with no object was read"

-- The scan is linear: many streams with no "endstream", many "obj" and
-- many "trailer", after a catalog (the trailers of a file with no object
-- are not read)
test_linearScan :: IO ()
test_linearScan = do
  let catalog = "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj\n2 0 obj << /Type /Pages /Kids [] /Count 0 >> endobj\n"
      count = 100000
      files =
        [ "%PDF-1.7\n" <> BS.concat (replicate count "1 0 obj << /Length 9 0 R >> stream\nxxxx\n") <> "%%EOF"
        , "%PDF-1.7\n" <> BS.concat (replicate count "1 0 obj ") <> "%%EOF"
        , "%PDF-1.7\n" <> BS.concat (replicate count "trailer << /Root 1 0 R >>\n") <> "%%EOF"
        , "%PDF-1.7\n" <> BS.concat (replicate count "1 0 obj (x) endobj trailer << /Root 1 0 R >> ") <> "%%EOF"
        -- a value that does not end: it ends at the next object
        , "%PDF-1.7\n" <> BS.concat (replicate count "1 0 obj (") <> "%%EOF"
        -- trailers whose strings go to the end of the file (an escaped
        -- parenthesis in the string of the trailer before)
        , "%PDF-1.7\n" <> BS.concat (replicate count "trailer<</A\\(") <> ")>>%%EOF" ]
  forM_ (map (catalog <>) files) $ \f -> case F.parsePdfFile f of
    Left e -> assertBool (length e >= 0)
    Right file -> assertBool (IM.size (F.pdfObjects file) >= 0)

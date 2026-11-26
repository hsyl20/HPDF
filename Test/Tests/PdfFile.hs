{-# LANGUAGE OverloadedStrings #-}
-- | Reading and writing PDF files: the values, the cross-reference
-- sections, the object streams
module Tests.PdfFile (tests) where

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Tests.Common
import qualified Graphics.PDF.File as F
import qualified Graphics.PDF as P
import qualified Graphics.PDF.Archive as A
import Control.Exception (evaluate)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Either (isLeft)

tests :: TestTree
tests = testGroup "PdfFile"
  [ test "numbers" test_numbers
  , test "names" test_names
  , test "literalStrings" test_literalStrings
  , test "hexStrings" test_hexStrings
  , test "arraysAndDictionaries" test_arraysAndDictionaries
  , test "valuesRoundTrip" test_valuesRoundTrip
  , test "malformedValues" test_malformedValues
  , test "flatePredictor" test_flatePredictor
  , test "classicFiles" test_classicFiles
  , test "incrementalUpdate" test_incrementalUpdate
  , test "badCrossReferences" test_badCrossReferences
  , test "crossReferenceStreams" test_crossReferenceStreams
  , test "hybridFile" test_hybridFile
  , test "objectStreamErrors" test_objectStreamErrors
  , test "streamLengths" test_streamLengths
  , test "refusedFiles" test_refusedFiles
  , localOption (mkTimeout 60000000) (test "malformedFiles" test_malformedFiles)
  , test "hpdfDocuments" test_hpdfDocuments
  , test "limitsOfTheFile" test_limitsOfTheFile
  , localOption (mkTimeout 10000000) (test "largeValues" test_largeValues)
  , test "objectStreamOffsets" test_objectStreamOffsets
  , test "unusualFiles" test_unusualFiles
  , test "valuesAndObjects" test_valuesAndObjects
  , localOption (mkTimeout 10000000) (test "knownAttacks" test_knownAttacks)
  ]

value :: BS.ByteString -> Either String F.Value
value = F.parseValue

render :: F.Value -> BS.ByteString
render = BL.toStrict . B.toLazyByteString . F.renderValue

-- Note [Real numbers of a PDF file] in Graphics.PDF.File.Object
test_numbers :: IO ()
test_numbers = do
  assertEqual (Right (F.Integer 42)) (value "42")
  assertEqual (Right (F.Integer (-17))) (value "-17")
  assertEqual (Right (F.Integer 5)) (value "+5")
  assertEqual (Right (F.Integer (10 ^ (59 :: Int)))) (value (C.pack ('1' : replicate 59 '0')))
  assertEqual (Right (F.Real 50 2)) (value "0.50")
  assertEqual (Right (F.Real 5 1)) (value "+.5")
  assertEqual (Right (F.Real (-2) 3)) (value "-.002")
  assertEqual (Right (F.Real 40 1)) (value "4.")
  assertEqual (Right (F.Real 0 1)) (value "-0.0")
  mapM_ (\t -> assertBoolVerbose (C.unpack t) (isLeft (value t))) ["1.2.3", "--5", "12abc", ".", "-", "+", "1e5"]
  -- the written form keeps the digits
  mapM_ (\(v, t) -> assertEqual t (render v))
    [ (F.Real 50 2, "0.50"), (F.Real (-2) 3, "-0.002"), (F.Real 40 1, "4.0"), (F.Real 5 1, "0.5")
    , (F.Real 123456 3, "123.456"), (F.Integer (-3), "-3") ]
  assertEqual (1 / 2) (F.realToRational 50 2)

-- Note [Names and strings of the reader] in Graphics.PDF.File.Lexer
test_names :: IO ()
test_names = do
  assertEqual (Right (F.Name "A B")) (value "/A#20B")
  assertEqual (Right (F.Name "A#2")) (value "/A#2")
  assertEqual (Right (F.Name "a#zz")) (value "/a#zz")
  assertEqual (Right (F.Name "")) (value "/")
  assertEqual (Right (F.Name "\0")) (value "/#00")
  assertEqual (Right (F.Array [F.Name "A", F.Name "B"])) (value "[/A/B]")
  assertEqual "/A#20#28B#29#23" (render (F.Name "A (B)#"))

test_literalStrings :: IO ()
test_literalStrings = do
  assertEqual (Right (F.String "a(b)c")) (value "(a(b)c)")
  assertEqual (Right (F.String "a(b")) (value "(a\\(b)")
  assertEqual (Right (F.String "\n\r\t\b\f()\\")) (value "(\\n\\r\\t\\b\\f\\(\\)\\\\)")
  assertEqual (Right (F.String "\0+\253+3\255")) (value "(\\0\\53\\375\\0533\\777)")
  -- a backslash at the end of a line continues the string
  assertEqual (Right (F.String "ab")) (value "(a\\\nb)")
  assertEqual (Right (F.String "ab")) (value "(a\\\r\nb)")
  -- an end of line in a string is LF
  assertEqual (Right (F.String "a\nb\nc\nd")) (value "(a\rb\r\nc\nd)")
  -- an unknown escape is the byte
  assertEqual (Right (F.String "q")) (value "(\\q)")
  assertEqual "(a\\(b\\)\\\\)" (render (F.String "a(b)\\"))
  assertEqual "<0A41>" (render (F.String "\nA"))

test_hexStrings :: IO ()
test_hexStrings = do
  assertEqual (Right (F.String "AB")) (value "<4142>")
  assertEqual (Right (F.String "A@")) (value "<41 4\n>")
  assertEqual (Right (F.String "\171")) (value "<aB>")
  assertEqual (Right (F.String "")) (value "<>")
  assertBool (isLeft (value "<41G2>"))
  assertBool (isLeft (value "<4142"))

test_arraysAndDictionaries :: IO ()
test_arraysAndDictionaries = do
  assertEqual (Right (F.Array [F.Ref 1 0, F.Integer 2])) (value "[1 0 R 2]")
  assertEqual (Right (F.Array [F.Integer 1, F.Integer 0, F.Integer 2])) (value "[1 0 2]")
  assertEqual (Right (F.Array [F.Integer 1, F.Ref 0 2])) (value "[1 0 2 R]")
  assertEqual (Right (F.Array [])) (value "[ ]")
  assertEqual (Right (F.Array [F.Bool True, F.Bool False, F.Null])) (value "[true false null]")
  -- comments between the tokens
  assertEqual (Right (F.Dict (M.fromList [("A", F.Integer 1), ("B", F.Array [F.Ref 3 0])])))
              (value "<< % a comment\n/A 1 %\r/B [3 % c\n 0 R] >>")
  -- the first value of a key is kept
  assertEqual (Right (F.Dict (M.fromList [("A", F.Integer 1)]))) (value "<</A 1 /A 2>>")
  assertEqual (Right (F.Dict M.empty)) (value "<<>>")
  assertEqual (Just (F.Integer 1)) (F.dictLookup "A" (M.fromList [("A", F.Integer 1), ("B", F.Null)]))
  assertEqual Nothing (F.dictLookup "B" (M.fromList [("A", F.Integer 1), ("B", F.Null)]))
  assertEqual "<< /A [1 0 R (x)] /B << >> >>"
              (render (F.Dict (M.fromList [("A", F.Array [F.Ref 1 0, F.String "x"]), ("B", F.Dict M.empty)])))

-- A value that is written and read again is the same value.
-- See Note [Values of the writer] in Graphics.PDF.File.Write
test_valuesRoundTrip :: IO ()
test_valuesRoundTrip = do
  let allBytes = BS.pack [0 .. 255]
      values =
        [ F.Null, F.Bool True, F.Integer 0, F.Integer (-12345678901234567890), F.Real 0 1, F.Real (-7) 5
        , F.Real 1234 1, F.Name "", F.Name allBytes, F.Name "#", F.String "", F.String allBytes
        , F.String "((()", F.String "\r\n\r", F.String "a\\b", F.Ref 0 0, F.Ref 2147483647 65535
        , F.Array [F.Array [F.Array []], F.Name "x", F.Integer 1, F.Integer 0, F.Real 10 1]
        , F.Dict (M.fromList [(allBytes, F.Null), ("", F.Dict (M.fromList [("k", F.Ref 4 1)]))]) ]
  mapM_ (\v -> assertEqual (Right v) (value (render v))) values
  -- the written bytes of a value read again are the same bytes
  mapM_ (\v -> assertEqual (render v) (either (const "") render (value (render v)))) values

test_malformedValues :: IO ()
test_malformedValues = do
  let bad = [ "", "(abc", "<<", "<</A 1", "<</A>>", "<<1 2>>", "[1 2", ")", "]", ">>", "{", "obj"
            , "1 0", "/A extra", "<< /A 1 >", "2147483648 0 R", "1 65536 R" ]
  -- "1 0" is two integers, which are not one value
  mapM_ (\t -> assertBoolVerbose (C.unpack t) (isLeft (value t))) bad
  -- the depth of the nesting has a limit
  assertBool (isLeft (value (C.replicate 300 '[' <> C.replicate 300 ']')))
  -- the depth is the number of arrays and dictionaries in each other
  assertEqual (Right (F.Array [F.Array []])) (F.parseValueWith 2 "[[]]")
  assertBool (isLeft (F.parseValueWith 1 "[[]]"))
  assertEqual (Right (F.Integer 1)) (F.parseValueWith 1 "1")

-- Note [Filters of the reader] in Graphics.PDF.File.Filter
test_flatePredictor :: IO ()
test_flatePredictor = do
  let rows = [BS.pack [1, 0, 10, 2], BS.pack [1, 0, 20, 3], BS.pack [2, 1, 0, 0]]
      -- PNG "up" filter (2) on each line, "none" (0) on the first
      up prev row = BS.pack (zipWith (-) (BS.unpack row) (BS.unpack prev))
      encoded = BS.concat [ BS.cons f line | (f, line) <- zip (0 : repeat 2) (zipWith up (BS.replicate 4 0 : rows) rows) ]
      deflate = BL.toStrict . Zlib.compress . BL.fromStrict
      parms = F.Dict (M.fromList [("Predictor", F.Integer 12), ("Columns", F.Integer 4)])
      dict = M.fromList [("Filter", F.Name "FlateDecode"), ("DecodeParms", parms)]
  assertEqual (Right (BS.concat rows)) (F.decodeStream 1000 dict (deflate encoded))
  assertEqual (Right (BS.concat rows)) (F.decodeStream 1000 (M.delete "DecodeParms" dict) (deflate (BS.concat rows)))
  assertEqual (Right "abc") (F.decodeStream 1000 M.empty "abc")
  -- the limit of the decoded size
  assertBool (isLeft (F.decodeStream 11 (M.delete "DecodeParms" dict) (deflate (BS.concat rows))))
  -- an unknown filter byte, another filter, the TIFF predictor
  assertBool (isLeft (F.decodeStream 1000 dict (deflate (BS.cons 7 (BS.drop 1 encoded)))))
  assertBool (isLeft (F.decodeStream 1000 (M.insert "Filter" (F.Name "LZWDecode") dict) encoded))
  assertBool (isLeft (F.decodeStream 1000 (M.insert "DecodeParms" (F.Dict (M.fromList [("Predictor", F.Integer 2)])) dict)
                                     (deflate encoded)))
  assertBool (isLeft (F.decodeStream 1000 dict "not zlib data"))

-- Fixtures: small files built here, so that each variant is exact.

-- | An object: number, generation, and the bytes between "obj" and
-- "endobj"
type RawObject = (Int, Int, BS.ByteString)

-- | The objects of a simple document: a catalog, the pages, a page, and
-- the content of the page
documentObjects :: [RawObject]
documentObjects =
  [ (1, 0, "<< /Type /Catalog /Pages 2 0 R >>")
  , (2, 0, "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
  , (3, 0, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] /Contents 4 0 R >>")
  , (4, 0, "<< /Length 11 >>\nstream\n0 0 m 1 1 l\nendstream") ]

-- | Place objects from an offset: the bytes, and the offset of each one
layout :: Int -> [RawObject] -> (BS.ByteString, [(Int, Int, Int)])
layout start objs = go start objs [] []
  where
    go _ [] chunks offs = (BS.concat (reverse chunks), reverse offs)
    go at ((n, g, body) : rest) chunks offs =
      let chunk = BS.concat [C.pack (show n ++ " " ++ show g ++ " obj\n"), body, "\nendobj\n"]
      in go (at + BS.length chunk) rest (chunk : chunks) ((n, at, g) : offs)

-- | The offset of an object of 'layout'
offsetOf :: Int -> [(Int, Int, Int)] -> Int
offsetOf n offs = case [ o | (m, o, _) <- offs, m == n ] of
  o : _ -> o
  [] -> error ("no object " ++ show n)

-- | A cross-reference table, with one subsection for each entry. The
-- first argument is the end of line of the entries (two bytes).
xrefTable :: BS.ByteString -> [(Int, Int, Int)] -> [Int] -> BS.ByteString
xrefTable eol used free = BS.concat ("xref\n" : map entry (M.toList all_entries))
  where
    all_entries = M.fromList ([ (n, Right (off, g)) | (n, off, g) <- used ] ++ [ (n, Left ()) | n <- free ])
    entry (n, Right (off, g)) = C.pack (show n ++ " 1\n" ++ pad 10 off ++ " " ++ pad 5 g ++ " n") <> eol
    entry (n, Left ()) = C.pack (show n ++ " 1\n0000000000 65535 f") <> eol
    pad k x = let t = show x in replicate (k - length t) '0' ++ t

-- | A file with a cross-reference table
classicFile :: BS.ByteString -> [RawObject] -> BS.ByteString -> BS.ByteString
classicFile eol objs extra =
  let header = "%PDF-1.4\n"
      (body, offs) = layout (BS.length header) objs
      start = BS.length header + BS.length body
  in BS.concat [ header, body, xrefTable eol offs [0]
               , "trailer\n<< /Size ", C.pack (show (length objs + 1)), " /Root 1 0 R ", extra, " >>\n"
               , "startxref\n", C.pack (show start), "\n%%EOF\n" ]

-- | Add an update to a file: new objects, free objects, and a trailer
-- with /Prev
withUpdate :: BS.ByteString -> [RawObject] -> [Int] -> BS.ByteString
withUpdate file objs free =
  let (body, offs) = layout (BS.length file) objs
      start = BS.length file + BS.length body
  in BS.concat [ file, body, xrefTable " \n" offs free
               , "trailer\n<< /Size 10 /Root 1 0 R /Prev ", C.pack (show (previousStart file)), " >>\n"
               , "startxref\n", C.pack (show start), "\n%%EOF\n" ]

-- | The offset after the last "startxref" of a file
previousStart :: BS.ByteString -> Int
previousStart file =
  let (_, r) = BS.breakSubstring "startxref\n" (snd (BS.breakSubstring "startxref\n" (BS.drop (BS.length file - 60) file)))
  in maybe 0 fst (C.readInt (BS.drop 10 r))

deflate :: BS.ByteString -> BS.ByteString
deflate = BL.toStrict . Zlib.compress . BL.fromStrict

-- | An entry of a cross-reference stream
data XEntry = XFree | XFile Int Int | XIn Int Int

-- | Options of a cross-reference stream
data XOptions = XOptions
  { xWidths :: (Int, Int, Int)
  , xPredictor :: Bool
  , xFlate :: Bool
  , xIndex :: Maybe [(Int, Int)]
  , xPadding :: Int -- ^ Spaces at the end of the object stream
  }

defaultX :: XOptions
defaultX = XOptions (1, 2, 1) True True Nothing 0

-- | The body of a cross-reference stream for these entries
xrefStreamBody :: XOptions -> Int -> [(Int, XEntry)] -> BS.ByteString -> BS.ByteString
xrefStreamBody opts size entries extra =
  let (w1, w2, w3) = xWidths opts
      be w x = BS.pack [ fromIntegral (x `div` (256 ^ k)) | k <- reverse [0 .. w - 1] ]
      fields e = case e of
        XFree -> (0, 0, 0)
        XFile off g -> (1, off, g)
        XIn c i -> (2, c, i)
      row e = let (a, b, c) = fields e in BS.concat [be w1 a, be w2 b, be w3 c]
      rows = map (row . snd) entries
      columns = w1 + w2 + w3
      -- the "up" PNG filter on each line, the "none" filter on the first
      filtered = BS.concat [ BS.cons f (BS.pack (zipWith (-) (BS.unpack r) (BS.unpack p)))
                           | (f, r, p) <- zip3 (0 : repeat 2) rows (BS.replicate columns 0 : rows) ]
      raw = if xPredictor opts then filtered else BS.concat rows
      stream = if xFlate opts then deflate raw else raw
      index = maybe "" (\is -> " /Index [" <> C.pack (unwords [ show a ++ " " ++ show b | (a, b) <- is ]) <> "]") (xIndex opts)
      filters = (if xFlate opts then " /Filter /FlateDecode" else "")
             <> (if xPredictor opts then " /DecodeParms << /Predictor 12 /Columns " <> C.pack (show columns) <> " >>" else "")
  in BS.concat [ "<< /Type /XRef /Size ", C.pack (show size), " /W [", C.pack (unwords (map show [w1, w2, w3])), "]"
               , index, filters, " /Root 1 0 R ", extra, " /Length ", C.pack (show (BS.length stream)), " >>\nstream\n"
               , stream, "\nendstream" ]

-- | An object stream with these objects
objectStreamBody :: [(Int, BS.ByteString)] -> BS.ByteString
objectStreamBody = objectStreamWith 0

-- | An object stream with these objects, and spaces at the end
objectStreamWith :: Int -> [(Int, BS.ByteString)] -> BS.ByteString
objectStreamWith padding objs =
  let bodies = map snd objs
      offs = scanl (+) 0 (map ((+ 1) . BS.length) bodies)
      header = C.pack (unwords [ show n ++ " " ++ show o | ((n, _), o) <- zip objs offs ]) <> "\n"
      content = header <> BS.intercalate "\n" bodies <> BS.replicate padding 0x20
      stream = deflate content
  in BS.concat [ "<< /Type /ObjStm /N ", C.pack (show (length objs)), " /First ", C.pack (show (BS.length header))
               , " /Filter /FlateDecode /Length ", C.pack (show (BS.length stream)), " >>\nstream\n", stream, "\nendstream" ]

-- | A file with a cross-reference stream: the objects 2 and 3 are in an
-- object stream (object 5), and the stream is object 6
xrefStreamFile :: XOptions -> BS.ByteString
xrefStreamFile opts =
  let header = "%PDF-1.5\n"
      [(_, catalog), (_, pages), (_, page), (_, content)] = [ (n, b) | (n, _, b) <- documentObjects ]
      objstm = objectStreamWith (xPadding opts) [(2, pages), (3, page)]
      (body, offs) = layout (BS.length header) [(1, 0, catalog), (4, 0, content), (5, 0, objstm)]
      start = BS.length header + BS.length body
      at n = offsetOf n offs
      entries = [ (0, XFree), (1, XFile (at 1) 0), (2, XIn 5 0), (3, XIn 5 1), (4, XFile (at 4) 0)
                , (5, XFile (at 5) 0), (6, XFile start 0) ]
      selected = case xIndex opts of
        Nothing -> entries
        Just ranges -> [ e | e@(n, _) <- entries, any (\(a, b) -> n >= a && n < a + b) ranges ]
      xref = BS.concat ["6 0 obj\n", xrefStreamBody opts 7 selected "", "\nendobj\n"]
  in BS.concat [header, body, xref, "startxref\n", C.pack (show start), "\n%%EOF\n"]

-- | The objects that a document of the fixtures must give
expectedDocument :: F.PdfFile -> IO ()
expectedDocument file = do
  assertEqual [1, 2, 3, 4] (IM.keys (F.pdfObjects file))
  assertEqual (Just (F.Stream (M.fromList [("Length", F.Integer 11)]) "0 0 m 1 1 l")) (F.getObject file 4)
  assertEqual (Just (F.Plain (F.Dict (M.fromList [("Type", F.Name "Pages"), ("Kids", F.Array [F.Ref 3 0]), ("Count", F.Integer 1)]))))
              (F.getObject file 2)
  assertEqual (Just (F.Ref 1 0)) (F.dictLookup "Root" (F.pdfTrailer file))
  assertEqual Nothing (F.dictLookup "Size" (F.pdfTrailer file))

-- | Read a file, write it, read it again: the same objects, and the same
-- bytes when the second file is written
roundTrip :: BS.ByteString -> IO F.PdfFile
roundTrip bytes = case F.parsePdfFile bytes of
  Left e -> assertFailure e
  Right p1 -> do
    let b2 = BL.toStrict (F.renderPdfFile p1)
    case F.parsePdfFile b2 of
      Left e -> assertFailure ("the written file cannot be read: " ++ e)
      Right p2 -> do
        assertEqual p1 p2
        assertEqual b2 (BL.toStrict (F.renderPdfFile p2))
        assertBool ("xref\n" `BS.isInfixOf` b2)
        assertBool (not ("/ObjStm" `BS.isInfixOf` b2))
        assertBool (not ("/XRef" `BS.isInfixOf` b2))
        return p1

-- The entries of a cross-reference table can end with " \n", " \r" or
-- "\r\n".
test_classicFiles :: IO ()
test_classicFiles = do
  mapM_ (\eol -> roundTrip (classicFile eol documentObjects "") >>= expectedDocument) [" \n", " \r", "\r\n"]
  -- an object with a generation that is not 0
  let file = classicFile " \n" (take 3 documentObjects ++ [(4, 2, "<< /Length 11 >>\nstream\n0 0 m 1 1 l\nendstream")]) ""
  p <- roundTrip file
  assertEqual (Just 2) (fst <$> IM.lookup 4 (F.pdfObjects p))
  assertBool (" 00002 n \n" `BS.isInfixOf` BL.toStrict (F.renderPdfFile p))

-- Note [Order of the cross-reference sections] in Graphics.PDF.File.Parse
test_incrementalUpdate :: IO ()
test_incrementalUpdate = do
  let base = classicFile " \n" documentObjects ""
      updated = withUpdate base [(3, 0, "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 50 50] >>")] [4]
  p <- roundTrip updated
  assertEqual [1, 2, 3] (IM.keys (F.pdfObjects p))
  assertEqual (Just (F.Array [F.Integer 0, F.Integer 0, F.Integer 50, F.Integer 50]))
              (case F.getObject p 3 of
                 Just (F.Plain (F.Dict d)) -> F.dictLookup "MediaBox" d
                 _ -> Nothing)
  assertEqual Nothing (F.dictLookup "Prev" (F.pdfTrailer p))

test_badCrossReferences :: IO ()
test_badCrossReferences = do
  let base = classicFile " \n" documentObjects ""
      start = previousStart base
      -- the /Prev of the update is the update itself
      loop = let u = withUpdate base [] [] in replaceOnce (C.pack ("/Prev " ++ show start)) (C.pack ("/Prev " ++ show (previousStart u))) u
      prev_out = replaceOnce (C.pack ("/Prev " ++ show start)) "/Prev 99999999" (withUpdate base [] [])
      expectLeft name bytes = assertBoolVerbose name (isLeft (F.parsePdfFile bytes))
  assertBool (either (("loop" `elem`) . words) (const False) (F.parsePdfFile loop))
  expectLeft "prev out of the file" prev_out
  expectLeft "no startxref" (replaceOnce "startxref" "startxrfe" base)
  expectLeft "startxref out of the file" (replaceOnce (C.pack ("startxref\n" ++ show start)) "startxref\n999999" base)
  expectLeft "no header" (BS.drop 9 base)
  expectLeft "no root" (replaceOnce "/Root 1 0 R" "/Rot 1 0 R" base)
  expectLeft "wrong number at an offset" (replaceOnce "2 0 obj" "7 0 obj" base)
  expectLeft "not n or f" (replaceOnce " 00000 n" " 00000 x" base)
  -- an entry in use at the offset 0 is free.
  -- See Note [Objects at the offset 0] in Graphics.PDF.File.Parse
  let at_zero = replaceOnce "trailer" "5 1\n0000000000 00000 n \ntrailer" base
  p <- roundTrip at_zero
  expectedDocument p

replaceOnce :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
replaceOnce old new s = case BS.breakSubstring old s of
  (before, rest) | BS.null rest -> s
                 | otherwise -> BS.concat [before, new, BS.drop (BS.length old) rest]

-- Cross-reference streams and object streams (PDF 32000-1, 7.5.7 and 7.5.8)
test_crossReferenceStreams :: IO ()
test_crossReferenceStreams = do
  let variants =
        [ defaultX
        , defaultX { xPredictor = False }
        , defaultX { xPredictor = False, xFlate = False }
        , defaultX { xWidths = (1, 3, 1) }
        , defaultX { xWidths = (1, 4, 2) }
        , defaultX { xIndex = Just [(0, 3), (3, 4)] }
        , defaultX { xIndex = Just [(1, 6)] } ]
  mapM_ (\opts -> roundTrip (xrefStreamFile opts) >>= expectedDocument) variants
  -- the first byte of the data of the cross-reference stream (not
  -- compressed) is the predictor of the first line: 0 to 4
  let plain = xrefStreamFile defaultX { xFlate = False }
      (before, after) = BS.breakSubstring "stream\n" (snd (BS.breakSubstring "/Type /XRef" plain))
      at = BS.length plain - BS.length before - BS.length after + BS.length before + 7
      bad_predictor = BS.concat [BS.take at plain, BS.singleton 7, BS.drop (at + 1) plain]
  assertEqual (Just 0) (BS.indexMaybe plain at)
  assertBool (isLeft (F.parsePdfFile bad_predictor))
  -- a width that is not possible, an /Index larger than the data, a
  -- predictor with the wrong columns
  assertBool (isLeft (F.parsePdfFile (replaceOnce "/W [1 2 1]" "/W [1 2 9]" (xrefStreamFile defaultX))))
  assertBool (isLeft (F.parsePdfFile (replaceOnce "/Size 7" "/Size 70" (xrefStreamFile defaultX))))
  assertBool (isLeft (F.parsePdfFile (replaceOnce "/Columns 4" "/Columns 5" (xrefStreamFile defaultX))))

-- Note [Hybrid files] in Graphics.PDF.File.Parse
test_hybridFile :: IO ()
test_hybridFile = roundTrip (fst hybridFile) >>= expectedDocument

-- | A hybrid file, and the offset of its cross-reference stream
hybridFile :: (BS.ByteString, Int)
hybridFile =
  let header = "%PDF-1.5\n"
      [(_, catalog), (_, pages), (_, page), (_, content)] = [ (n, b) | (n, _, b) <- documentObjects ]
      objstm = objectStreamBody [(3, page)]
      (body, offs) = layout (BS.length header) [(1, 0, catalog), (2, 0, pages), (4, 0, content), (5, 0, objstm)]
      stream_at = BS.length header + BS.length body
      xref_stream = BS.concat ["6 0 obj\n", xrefStreamBody defaultX { xIndex = Just [(3, 1)] } 7 [(3, XIn 5 0)] "", "\nendobj\n"]
      xrefs_at = stream_at + BS.length xref_stream
      -- the table gives the object 3 as free
      table = xrefTable " \n" offs [0, 3]
  in ( BS.concat [ header, body, xref_stream, table
                 , "trailer\n<< /Size 7 /Root 1 0 R /XRefStm ", C.pack (show stream_at), " >>\n"
                 , "startxref\n", C.pack (show xrefs_at), "\n%%EOF\n" ]
     , stream_at )

-- | Add empty updates to a file, each with a /XRefStm
withEmptyUpdates :: Int -> Int -> BS.ByteString -> BS.ByteString
withEmptyUpdates k xrefstm file0 = iterate add file0 !! k
  where
    add file = BS.concat [ file, "xref\n0 0\ntrailer\n<< /Size 7 /Root 1 0 R /Prev ", C.pack (show (previousStart file))
                         , " /XRefStm ", C.pack (show xrefstm), " >>\nstartxref\n", C.pack (show (BS.length file))
                         , "\n%%EOF\n" ]

-- The limits of the reader hold for the whole file.
-- See Note [Limits of the reader] in Graphics.PDF.File.Parse
test_limitsOfTheFile :: IO ()
test_limitsOfTheFile = do
  let (hybrid, xrefstm) = hybridFile
      updated = withEmptyUpdates 300 xrefstm hybrid
      parse limits = F.parsePdfFileWith limits updated
      ok = either (const False) (const True)
  -- 300 sections give the same cross-reference stream: it is decoded one
  -- time (5 bytes), and not 300 times
  assertBool (ok (parse F.defaultLimits))
  assertBool (ok (parse F.defaultLimits { F.limitTotal = 1000 }))
  -- but the entries of each section count
  assertBool (isLeft (parse F.defaultLimits { F.limitObjects = 100 }))
  -- the rows of a cross-reference stream are entries: the check comes
  -- before the entries are made
  assertBool (isLeft (F.parsePdfFileWith F.defaultLimits { F.limitObjects = 3 } (xrefStreamFile defaultX)))
  assertBool (isLeft (F.parsePdfFileWith F.defaultLimits { F.limitObjects = 3 } (classicFile " \n" documentObjects "")))
  assertBool (ok (F.parsePdfFileWith F.defaultLimits { F.limitObjects = 10 } (classicFile " \n" documentObjects "")))

-- Names and strings are decoded in linear time and memory.
-- See Note [Linear decoding of names and strings] in Graphics.PDF.File.Lexer
test_largeValues :: IO ()
test_largeValues = do
  -- with a quadratic decoding, this name takes 20 s
  let name = "/" <> BS.concat (replicate 800000 "a#41")
  case F.parseValue name of
    Right (F.Name n) -> assertEqual (1600000, "aAaA") (BS.length n, BS.take 4 n)
    other -> assertFailure (take 100 (show other))
  let string = "(" <> BS.concat (replicate 200000 "0123456789\\n") <> ")"
  case F.parseValue string of
    Right (F.String t) -> assertEqual (2200000, "9\n0") (BS.length t, BS.take 3 (BS.drop 9 t))
    other -> assertFailure (take 100 (show other))
  let hex = "<" <> BS.concat (replicate 1000000 "4 1 ") <> ">"
  case F.parseValue hex of
    Right (F.String t) -> assertEqual (1000000, "AA") (BS.length t, BS.take 2 t)
    other -> assertFailure (take 100 (show other))

-- The objects of an object stream do not overlap.
-- See Note [Objects of an object stream] in Graphics.PDF.File.ObjectStream
test_objectStreamOffsets :: IO ()
test_objectStreamOffsets = do
  let pages = "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
      page = "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 100 100] /Contents 4 0 R >>"
      file header_line = file_with header_line (pages <> "\n" <> page)
      file_with header_line values =
                         let content = header_line <> values
                             objstm = BS.concat [ "<< /Type /ObjStm /N 2 /First ", C.pack (show (BS.length header_line))
                                                , " /Length ", C.pack (show (BS.length content)), " >>\nstream\n", content, "\nendstream" ]
                             [(_, catalog), _, _, (_, content4)] = [ (n, b) | (n, _, b) <- documentObjects ]
                             hdr = "%PDF-1.5\n"
                             (body, offs) = layout (BS.length hdr) [(1, 0, catalog), (4, 0, content4), (5, 0, objstm)]
                             start = BS.length hdr + BS.length body
                             entries = [ (0, XFree), (1, XFile (offsetOf 1 offs) 0), (2, XIn 5 0), (3, XIn 5 1)
                                       , (4, XFile (offsetOf 4 offs) 0), (5, XFile (offsetOf 5 offs) 0), (6, XFile start 0) ]
                             xref = BS.concat ["6 0 obj\n", xrefStreamBody defaultX 7 entries "", "\nendobj\n"]
                         in BS.concat [hdr, body, xref, "startxref\n", C.pack (show start), "\n%%EOF\n"]
      second = C.pack (show (BS.length pages + 1))
  roundTrip (file ("2 0 3 " <> second <> "\n")) >>= expectedDocument
  -- two objects at the same offset, and offsets that do not increase
  assertBool (isLeft (F.parsePdfFile (file "2 0 3 0\n")))
  assertBool (isLeft (F.parsePdfFile (file ("2 " <> second <> " 3 0\n"))))
  -- the first object cannot read the second one: it ends at the next
  -- offset. Here the object 2 is "[1 " and not [1 2 3]
  assertBool (isLeft (F.parsePdfFile (file_with "2 0 3 3\n" "[1 2 3] 4")))
  assertBool (either (const False) (const True) (F.parsePdfFile (file_with "2 0 3 8\n" "[1 2 3] 4")))

-- Files that some writers make, and that the reader reads.
test_unusualFiles :: IO ()
test_unusualFiles = do
  let content_of p = case F.parsePdfFile p of
        Right f -> case F.getObject f 4 of
          Just (F.Stream _ d) -> Right d
          other -> Left (show other)
        Left e -> Left e
      with_stream body = take 3 documentObjects ++ [(4, 0, body)]
  -- spaces after "stream", before the end of line
  mapM_ (\sp -> assertEqual (Right "0 0 m 1 1 l")
                   (content_of (classicFile " \n" (with_stream ("<< /Length 11 >>\nstream" <> sp <> "0 0 m 1 1 l\nendstream")) "")))
    [" \r\n", "  \n", "\t\n", " \r"]
  -- a length that is too large for an Int
  assertEqual (Right "0 0 m 1 1 l")
              (content_of (classicFile " \n" (with_stream "<< /Length 18446744073709551633 >>\nstream\n0 0 m 1 1 l\nendstream") ""))
  -- bytes before the header, with no /Length: the offsets count from the
  -- header, or from the start of the file
  -- See Note [Garbage before the header] in Graphics.PDF.File.Parse
  let no_length = with_stream "<< >>\nstream\n0 0 m 1 1 l\nendstream"
      from_header = "JUNK\n" <> classicFile " \n" no_length ""
      from_start = BS.drop 5 (classicFileAt 5 " \n" no_length)
  assertEqual (Right "0 0 m 1 1 l") (content_of from_header)
  assertEqual (Right "0 0 m 1 1 l") (content_of ("JUNK\n" <> from_start))

-- | Like 'classicFile', with the offsets counted from a position before
-- the file
classicFileAt :: Int -> BS.ByteString -> [RawObject] -> BS.ByteString
classicFileAt shift eol objs =
  let header = "%PDF-1.4\n"
      (body, offs) = layout (shift + BS.length header) objs
      start = shift + BS.length header + BS.length body
  in BS.concat [ BS.replicate shift 0x20, header, body, xrefTable eol offs [0]
               , "trailer\n<< /Size ", C.pack (show (length objs + 1)), " /Root 1 0 R >>\n"
               , "startxref\n", C.pack (show start), "\n%%EOF\n" ]

-- Other findings of the review of the reader
test_valuesAndObjects :: IO ()
test_valuesAndObjects = do
  -- a real with less than one decimal keeps its value when it is written
  assertEqual "5.0" (render (F.Real 5 0))
  assertEqual "50.0" (render (F.Real 5 (-1)))
  assertEqual "-25.0" (render (F.Real (-25) 0))
  -- the two numbers of a reference have no sign
  assertBool (isLeft (value "[+1 0 R]"))
  assertBool (isLeft (value "[1 +0 R]"))
  assertEqual (Right (F.Array [F.Integer 1, F.Integer (-0)])) (value "[+1 -0]")
  -- a reference with another generation gives null
  case F.parsePdfFile (classicFile " \n" documentObjects "") of
    Left e -> assertFailure e
    Right f -> do
      assertBool (F.resolveValue f (F.Ref 2 0) /= F.Null)
      assertEqual F.Null (F.resolveValue f (F.Ref 2 1))
      assertEqual F.Null (F.resolveValue f (F.Ref 9 0))
      -- an object 0 is not written
      let with_zero = f { F.pdfObjects = IM.insert 0 (0, F.Plain (F.Integer 7)) (F.pdfObjects f) }
      case F.parsePdfFile (BL.toStrict (F.renderPdfFile with_zero)) of
        Right g -> assertEqual (F.pdfObjects f) (F.pdfObjects g)
        Left e -> assertFailure e

test_objectStreamErrors :: IO ()
test_objectStreamErrors = do
  let good = xrefStreamFile defaultX { xPredictor = False, xFlate = False }
      -- the object stream with other first lines
      withObjStm f = let header = "%PDF-1.5\n"
                         [(_, catalog), _, (_, page), (_, content)] = [ (n, b) | (n, _, b) <- documentObjects ]
                         (body, offs) = layout (BS.length header) [(1, 0, catalog), (4, 0, content), (5, 0, f page)]
                         start = BS.length header + BS.length body
                         at n = offsetOf n offs
                         entries = [ (0, XFree), (1, XFile (at 1) 0), (2, XIn 5 0), (3, XIn 5 1), (4, XFile (at 4) 0)
                                   , (5, XFile (at 5) 0), (6, XFile start 0) ]
                         xref = BS.concat ["6 0 obj\n", xrefStreamBody defaultX 7 entries "", "\nendobj\n"]
                     in BS.concat [header, body, xref, "startxref\n", C.pack (show start), "\n%%EOF\n"]
      pages = "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
      plain_objstm n first content = BS.concat [ "<< /Type /ObjStm /N ", C.pack (show (n :: Int)), " /First ", C.pack (show (first :: Int))
                                               , " /Length ", C.pack (show (BS.length content)), " >>\nstream\n", content, "\nendstream" ]
      two page = let h = "2 0 3 " <> C.pack (show (BS.length pages + 1)) <> "\n"
                 in plain_objstm 2 (BS.length h) (h <> pages <> "\n" <> page)
  roundTrip (withObjStm two) >>= expectedDocument
  -- the index 1 has the object 7 and not 3
  let wrong_number page = let h = "2 0 7 " <> C.pack (show (BS.length pages + 1)) <> "\n"
                          in plain_objstm 2 (BS.length h) (h <> pages <> "\n" <> page)
      huge_n page = let h = "2 0 3 " <> C.pack (show (BS.length pages + 1)) <> "\n"
                    in plain_objstm 100000000 (BS.length h) (h <> pages <> "\n" <> page)
      with_stream _ = let h = "2 0 3 " <> C.pack (show (BS.length pages + 1)) <> "\n"
                      in plain_objstm 2 (BS.length h) (h <> pages <> "\n<< /Length 1 >>\nstream\nx\nendstream")
      not_objstm page = replaceOnce "/Type /ObjStm" "/Type /XObject" (two page)
  mapM_ (\(name, f) -> assertBoolVerbose name (isLeft (F.parsePdfFile (withObjStm f))))
    [("wrong number", wrong_number), ("huge N", huge_n), ("stream in an object stream", with_stream), ("not an object stream", not_objstm)]
  assertBool (isRight' (F.parsePdfFile good))
  where
    isRight' = either (const False) (const True)

-- Note [Length of a stream] in Graphics.PDF.File.Parse
test_streamLengths :: IO ()
test_streamLengths = do
  let withContent body extra = classicFile " \n" (take 3 documentObjects ++ [(4, 0, body)] ++ extra) ""
      content p = case F.parsePdfFile p of
        Right f -> case F.getObject f 4 of
          Just (F.Stream _ d) -> Right d
          other -> Left (show other)
        Left e -> Left e
  -- the length is an object after the stream, as HPDF writes it
  assertEqual (Right "0 0 m 1 1 l") (content (withContent "<< /Length 5 0 R >>\nstream\n0 0 m 1 1 l\nendstream" [(5, 0, "11")]))
  -- a wrong length, no length, a length that is the stream itself
  mapM_ (\d -> assertEqual (Right "0 0 m 1 1 l") (content (withContent (d <> "\nstream\n0 0 m 1 1 l\nendstream") [])))
    ["<< /Length 3 >>", "<< /Length 300 >>", "<< >>", "<< /Length 4 0 R >>", "<< /Length 9 0 R >>"]
  -- the end of line after "stream"
  mapM_ (\eol -> assertEqual (Right "0 0 m 1 1 l") (content (withContent ("<< /Length 11 >>\nstream" <> eol <> "0 0 m 1 1 l\nendstream") [])))
    ["\n", "\r\n", "\r"]
  -- the end of line before "endstream" is not data
  mapM_ (\eol -> assertEqual (Right "0 0 m 1 1 l") (content (withContent ("<< /Length 99 >>\nstream\n0 0 m 1 1 l" <> eol <> "endstream") [])))
    ["\n", "\r\n", "\r", ""]
  assertBool (isLeft (content (withContent "<< /Length 11 >>\nstream\n0 0 m 1 1 l" [])))
  -- the search for "endstream" stops at the next object of the file: the
  -- "endstream" of the object 5 is not the end of the object 4
  let missing_end = withContent "<< /Length 300 >>\nstream\n0 0 m 1 1 l" [(5, 0, "<< /Length 3 >>\nstream\nabc\nendstream")]
  assertBool ("endstream" `BS.isInfixOf` missing_end)
  assertBool (isLeft (content missing_end))
  -- the length is an object of an object stream (pdfTeX writes such
  -- files), and the data ends with an end of line that is data: without
  -- the length, the reader would remove it
  let header = "%PDF-1.5\n"
      [(_, catalog), (_, pages), (_, page), _] = [ (k, b) | (k, _, b) <- documentObjects ]
      objstm = objectStreamBody [(2, pages), (3, page), (7, "12")]
      (body, offs) = layout (BS.length header)
                       [(1, 0, catalog), (4, 0, "<< /Length 7 0 R >>\nstream\n0 0 m 1 1 l\nendstream"), (5, 0, objstm)]
      start = BS.length header + BS.length body
      entries = [ (0, XFree), (1, XFile (offsetOf 1 offs) 0), (2, XIn 5 0), (3, XIn 5 1), (4, XFile (offsetOf 4 offs) 0)
                , (5, XFile (offsetOf 5 offs) 0), (6, XFile start 0), (7, XIn 5 2) ]
      xref = BS.concat ["6 0 obj\n", xrefStreamBody defaultX 8 entries "", "\nendobj\n"]
      file = BS.concat [header, body, xref, "startxref\n", C.pack (show start), "\n%%EOF\n"]
  assertEqual (Right "0 0 m 1 1 l\n") (content file)

-- An encryption that is not complete, and the limits of the reader. See
-- Tests.Encryption for the encrypted files, and Note [Limits of the
-- reader] in Graphics.PDF.File.Parse
test_refusedFiles :: IO ()
test_refusedFiles = do
  let encrypted = classicFile " \n" documentObjects "/Encrypt << /Filter /Standard >>"
  case F.parsePdfFile encrypted of
    Left e -> assertBoolVerbose e ("encryption" `elem` words e)
    Right _ -> assertFailure "an encrypted file was read"
  let file = xrefStreamFile defaultX
      parse limits = F.parsePdfFileWith limits file
  assertBool (either (const False) (const True) (parse F.defaultLimits))
  assertBool (isLeft (parse F.defaultLimits { F.limitTotal = 10 }))
  assertBool (isLeft (parse F.defaultLimits { F.limitStream = 10 }))
  assertBool (isLeft (parse F.defaultLimits { F.limitDepth = 1 }))
  -- an object stream that expands to 10 MB from a file of 10 KB
  let bomb = xrefStreamFile defaultX { xPadding = 10000000 }
  assertBool (BS.length bomb < 20000)
  assertBool (either (const False) (const True) (F.parsePdfFileWith F.defaultLimits bomb))
  assertBool (isLeft (F.parsePdfFileWith F.defaultLimits { F.limitStream = 1000000 } bomb))
  assertBool (isLeft (F.parsePdfFileWith F.defaultLimits { F.limitTotal = 1000000 } bomb))

-- A file that is not correct gives Left, and never an exception.
-- See Note [Limits of the reader] in Graphics.PDF.File.Parse
test_malformedFiles :: IO ()
test_malformedFiles = do
  let files = [classicFile " \n" documentObjects "", xrefStreamFile defaultX, xrefStreamFile defaultX { xFlate = False }]
      -- the length of the result, so that all of it is computed
      outcome bytes = case F.parsePdfFile bytes of
        Left e -> length e
        Right f -> fromIntegral (BL.length (F.renderPdfFile f))
  mapM_ (\f -> mapM_ (\n -> evaluate (outcome (BS.take n f)) >>= assertBool . (>= 0)) [0 .. BS.length f]) files
  -- a part of the body is removed, and the end of the file is kept: the
  -- reader reads the cross-reference sections and fails later
  let tail_of f = BS.drop (BS.length f - 80) f
  mapM_ (\f -> mapM_ (\n -> evaluate (outcome (BS.take n f <> tail_of f)) >>= assertBool . (>= 0)) [0 .. BS.length f - 80]) files
  mapM_ (\f -> mapM_ (\m -> evaluate (outcome m) >>= assertBool . (>= 0)) (mutationsOf 2000 f)) files

-- HPDF documents: read, write, read again.
test_hpdfDocuments :: IO ()
test_hpdfDocuments = withTestFont $ \_ ttf -> do
  Right helvetica <- P.mkStdFont P.Helvetica
  let rect = P.PDFRect 0 0 300 200
      documents =
        [ ("text and shapes", P.standardDocInfo, do
              page <- P.addPage Nothing
              P.drawWithPage page $ do
                P.strokeColor P.red
                P.stroke (P.Rectangle (10 P.:+ 10) (100 P.:+ 100))
                P.drawText (P.text (P.PDFFont helvetica 12) 10 150 (T.pack "Hello")))
        , ("uncompressed", P.standardDocInfo { P.compressed = False }, do
              page <- P.addPage Nothing
              P.drawWithPage page (P.stroke (P.Line 0 0 10 10)))
        , ("jpeg and outline", P.standardDocInfo, do
              img <- P.createPDFJpeg jpg
              page <- P.addPage Nothing
              P.newSection (T.pack "section") Nothing Nothing $ P.drawWithPage page (P.drawXObject img))
        , ("TrueType", P.standardDocInfo, do
              font <- P.mkTrueTypeFont ttf
              page <- P.addPage Nothing
              P.drawWithPage page (P.drawText (P.text (P.PDFFont font 12) 10 150 (T.pack "Unicode \1046"))))
        , ("PDF/A and a file", P.standardDocInfo, do
              A.setPDFA (A.pdfa3b fakeIcc)
              font <- P.mkTrueTypeFont ttf
              _ <- A.embedAssociatedFile (A.associatedFile (T.pack "a.xml") "text/xml" (BL.fromStrict "<x/>"))
              page <- P.addPage Nothing
              P.drawWithPage page (P.drawText (P.text (P.PDFFont font 12) 10 150 (T.pack "PDF/A")))) ]
  mapM_ (\(name, info, doc) -> case P.renderPdfE info rect doc of
          Left ps -> assertFailure (name ++ ": " ++ unlines ps)
          Right bytes -> do
            let original = BL.toStrict bytes
            p <- roundTrip original
            -- the same number of objects as the table of HPDF ("0 n")
            let count = maybe 0 fst (C.readInt (BS.drop 8 (snd (BS.breakSubstring "\nxref\n0 " original))))
            assertEqual (name, count - 1) (name, IM.size (F.pdfObjects p))
            -- the content of the page has its operators
            let contents = [ d | (_, (_, F.Stream dict raw)) <- IM.toList (F.pdfObjects p)
                               , Right d <- [F.decodeStream 1000000 dict raw] ]
            assertBoolVerbose name (any (\d -> " re" `BS.isInfixOf` d || " l" `BS.isInfixOf` d || "Tf" `BS.isInfixOf` d || " Do" `BS.isInfixOf` d) contents))
        documents

-- The attacks of known CVEs of other PDF readers. Each file must give
-- Left, or the correct objects, in a short time (the test has a timeout).
test_knownAttacks :: IO ()
test_knownAttacks = do
  let classic = classicFile " \n" documentObjects ""
      stream_file = xrefStreamFile defaultX
      -- the reason of the refusal has these words
      refusedFor words_ name bytes = case F.parsePdfFile bytes of
        Left e -> assertBoolVerbose (name ++ ": " ++ e) (all (`elem` words e) words_)
        Right _ -> assertFailure (name ++ ": the file was read")
  -- CVE-2018-7174 (xpdf), CVE-2026-27628 (pypdf): a /Prev loop. The
  -- reader of xpdf checked the loops of the tables, but not of the
  -- cross-reference streams
  let start = previousStart stream_file
  refusedFor ["loop"] "/Prev loop of a cross-reference stream"
          (replaceOnce "/Root 1 0 R  /Length" (C.pack ("/Root 1 0 R /Prev " ++ show start ++ " /Length")) stream_file)
  refusedFor ["loop"] "/Prev loop of a table"
          (let u = withUpdate classic [] [] in replaceOnce (C.pack ("/Prev " ++ show (previousStart classic)))
                                                           (C.pack ("/Prev " ++ show (previousStart u))) u)
  -- CVE-2026-119203 (pypdf): a cross-reference table with no trailer
  refusedFor ["integer"] "a table with no trailer" (replaceOnce "trailer\n" "" classic)
  -- CVE-2017-15587 and CVE-2017-17858 (MuPDF): the numbers of a
  -- subsection overflow
  mapM_ (\header -> refusedFor ["subsection"] (C.unpack header) (replaceOnce "trailer" (header <> "0000000009 00000 n \ntrailer") classic))
    ["2147483647 2\n", "4294967296 1\n", "-1 1\n", "18446744073709551616 1\n", "5 -1\n"]
  -- CVE-2018-6544 (MuPDF): an object stream that refers to itself, or to
  -- an object stream
  refusedFor ["stream"] "an object stream in itself" (replaceOnce "/Type /ObjStm" "/Type /ObjStm /Extends 5 0 R" stream_file `seq`
                                        objectStreamLoop)
  -- CVE-2017-9209 (qpdf): values nested so deep that the stack is
  -- exhausted
  let nested value = classicFile " \n" (take 3 documentObjects ++ [(4, 0, value)]) ""
  refusedFor ["deep"] "nested arrays" (nested (C.replicate 100000 '['))
  refusedFor ["deep"] "nested dictionaries" (nested (BS.concat (replicate 100000 "<< /A ")))
  -- CVE-2025-55197 (pypdf): FlateDecode filters one after the other on a
  -- cross-reference stream, a decompression bomb
  refusedFor ["supported"] "two FlateDecode filters" (replaceOnce "/Filter /FlateDecode /DecodeParms" "/Filter [/FlateDecode /FlateDecode] /DecodeParms" stream_file)
  -- CVE-2025-62708 (pypdf): an LZWDecode bomb. The reader does not decode
  -- LZWDecode
  refusedFor ["supported"] "LZWDecode" (replaceOnce "/Filter /FlateDecode /DecodeParms" "/Filter /LZWDecode /DecodeParms" stream_file)
  -- CVE-2026-102997 (pypdf): a FlateDecode stream that is not correct,
  -- and a long recovery. The reader does not recover: Left at once
  let (before, after) = BS.breakSubstring "stream\n" (snd (BS.breakSubstring "/Type /XRef" stream_file))
      at = BS.length stream_file - BS.length after + 7
  assertBool (BS.length before > 0)
  refusedFor ["expanded:"] "a FlateDecode stream that is not correct"
          (BS.concat [BS.take at stream_file, "XX", BS.drop (at + 2) stream_file])
  -- CVE-2023-36464 (pypdf): a comment with no end of line at the end of
  -- the data made the parser loop
  mapM_ (\t -> assertBoolVerbose (C.unpack t) (isLeft (F.parseValue t))) ["<< /A 1 % no end", "[1 2 %", "%", "( % )x"]
  assertEqual (Right (F.Integer 1)) (F.parseValue "1 % a comment at the end")
  refusedFor ["integer"] "a comment after startxref" (fst (BS.breakSubstring "startxref" classic) <> "startxref % no end")
  -- CVE-2018-20481 (poppler): a reference to an object that is not in the
  -- cross-reference
  case F.parsePdfFile (replaceOnce "/Root 1 0 R" "/Root 99 0 R" classic) of
    Left e -> assertFailure e
    Right f -> do
      assertEqual Nothing (F.getObject f 99)
      assertEqual F.Null (F.resolveValue f (F.Ref 99 0))
  where
    -- the object 5 is an object stream whose entry says that it is in the
    -- object stream 5
    objectStreamLoop =
      let header = "%PDF-1.5\n"
          [(_, catalog), (_, pages), (_, page), (_, content)] = [ (n, b) | (n, _, b) <- documentObjects ]
          objstm = objectStreamBody [(2, pages), (3, page)]
          (body, offs) = layout (BS.length header) [(1, 0, catalog), (4, 0, content), (5, 0, objstm)]
          start = BS.length header + BS.length body
          entries = [ (0, XFree), (1, XFile (offsetOf 1 offs) 0), (2, XIn 5 0), (3, XIn 5 1)
                    , (4, XFile (offsetOf 4 offs) 0), (5, XIn 5 2), (6, XFile start 0) ]
          xref = BS.concat ["6 0 obj\n", xrefStreamBody defaultX 7 entries "", "\nendobj\n"]
      in BS.concat [header, body, xref, "startxref\n", C.pack (show start), "\n%%EOF\n"]

---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The bytes of the values and of a PDF file
---------------------------------------------------------
module Graphics.PDF.File.Write
  ( renderValue
  , renderDict
  , RenderOptions(..)
  , defaultRenderOptions
  , renderPdfFile
  , renderPdfFileWith
  , Packed(..)
  , packObjects
  , packInObjectStreams
  , largestNumber
  , renderPacked
  , infoObject
  , objectStreamsPossible
  , sectionKeys
  , renderObjects
  , XrefEntry(..)
  , xrefTable
  , xrefStream
  ) where

import qualified Codec.Compression.Zlib as Z
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.IntMap.Strict as IM
import Data.Int (Int64)
import qualified Data.List as List (foldl')
import Text.Printf (printf)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Graphics.PDF.File.Filter (decodeStream)
import Graphics.PDF.File.Object
import Graphics.PDF.LowLevel.Signature (signatureKeys)
import Graphics.PDF.LowLevel.Types (upperHexBytes)

-- Note [Values of the writer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The writer gives each value one form, so that a value read again is the
-- same value:
--
--   Real 50 2          "0.50"     the digits of the real
--   Name "A B"         "/A#20B"   a byte outside "!" to "~", a delimiter
--                                 or "#" is written #xx
--   String "a(b)"      "(a\(b\))" literal when all the bytes are "
--                                 " to "~"
--   String "\0\255"    "<00FF>"   hexadecimal for the other strings
--   Dict               "<< /K v /L w >>", in the order of the keys
--
-- A literal string has no end of line in it: the reader would read CR as
-- LF (Note [Names and strings of the reader] in Graphics.PDF.File.Lexer).

-- | The bytes of a value.
-- See Note [Values of the writer]
renderValue :: Value -> B.Builder
renderValue v = case v of
  Null -> B.string7 "null"
  Bool True -> B.string7 "true"
  Bool False -> B.string7 "false"
  Integer n -> B.integerDec n
  Real m e -> render_real m e
  Name n -> render_name n
  String s
    | S.all printable s -> B.char7 '(' <> B.byteString (escape_literal s) <> B.char7 ')'
    | otherwise -> B.char7 '<' <> B.byteString (upperHexBytes s) <> B.char7 '>'
  Array vs -> B.char7 '[' <> mconcat (separated (map renderValue vs)) <> B.char7 ']'
  Dict d -> renderDict d
  Ref n g -> B.intDec n <> B.char7 ' ' <> B.intDec g <> B.string7 " R"
  where
    printable w = w >= 0x20 && w <= 0x7E
    separated (x : xs@(_ : _)) = x : B.char7 ' ' : separated xs
    separated xs = xs

-- | The bytes of a dictionary
renderDict :: Dict -> B.Builder
renderDict d = B.string7 "<<"
  <> mconcat [ B.char7 ' ' <> render_name k <> B.char7 ' ' <> renderValue x | (k, x) <- M.toList d ]
  <> B.string7 " >>"

-- | A real number with its digits.
-- See Note [Real numbers of a PDF file] in Graphics.PDF.File.Object
render_real :: Integer -> Int -> B.Builder
render_real m0 e0 =
  let -- a real with less than one decimal has one: the same value
      (m, e) = if e0 >= 1 then (m0, e0) else (m0 * 10 ^ (1 - e0), 1)
      digits = show (abs m)
      padded = replicate (e + 1 - length digits) '0' ++ digits
      (whole, fraction) = splitAt (length padded - e) padded
  in B.string7 ((if m < 0 then "-" else "") ++ whole ++ "." ++ fraction)

render_name :: S.ByteString -> B.Builder
render_name n = B.char7 '/' <> S.foldr (\w b -> name_byte w <> b) mempty n
  where
    name_byte :: Word8 -> B.Builder
    name_byte w
      | w < 0x21 || w > 0x7E || w `S.elem` C.pack "()<>[]{}/%#" =
          B.char7 '#' <> B.byteString (upperHexBytes (S.singleton w))
      | otherwise = B.word8 w

-- | A backslash before each "(", ")" and "\\". In one buffer, for a large
-- string.
escape_literal :: S.ByteString -> S.ByteString
escape_literal s
  | not (S.any special s) = s
  | otherwise = fst (S.unfoldrN (2 * S.length s) step (0, Nothing))
  where
    special w = w == 0x28 || w == 0x29 || w == 0x5C
    step (i, Just w) = Just (w, (i, Nothing))
    step (i, Nothing)
      | i >= S.length s = Nothing
      | special (S.index s i) = Just (0x5C, (i + 1, Just (S.index s i)))
      | otherwise = Just (S.index s i, (i + 1, Nothing))

-- Note [Files of the writer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- With 'renderObjectStreams' (the default), the writer gives a file with
-- object streams and one cross-reference stream: see Note [Object streams
-- of the writer]. Without it, the writer gives a file with one
-- cross-reference table, and no object stream and no cross-reference
-- stream:
--
--   %PDF-1.7
--   %<four bytes above 127>
--   1 0 obj
--   << /Type /Catalog ... >>
--   endobj
--   ...
--   xref
--   0 4
--   0000000000 65535 f
--   0000000015 00000 n
--   ...
--   trailer
--   << /Root 1 0 R /Size 4 >>
--   startxref
--   ...
--   %%EOF
--
-- The objects keep their numbers and their generations; the table has a
-- subsection for each run of numbers, so that a file whose numbers have
-- large gaps does not give a large table. An object 0 is not written. The
-- /Length of a stream is the length of its data. A signed file is written
-- again with its signature, but the signature is then not valid: it signs
-- the bytes of the old file ('Graphics.PDF.File.updatePdfFile' keeps
-- them). See Note [Signatures of the writer] for a file that is not
-- signed.

-- Note [Signatures of the writer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A signature dictionary that is not signed ('isSignatureDict', with a
-- /Contents of only zero bytes) gets the layout of Note [Layout of a
-- signature dictionary] in Graphics.PDF.LowLevel.Signature, with the two
-- keys first:
--
--   5 0 obj
--   <</ByteRange [0 120 4242 377]   (spaces to a fixed width)
--   /Contents <0000...0000> /Filter /Adobe.PPKLite /Type /Sig ... >>
--
-- Thus 'Graphics.PDF.Signature.signPdfWith' can sign the file: for example
-- a document that is encrypted after it is rendered.
--
-- The writer renders the objects two times. The first rendering gives the
-- offsets and the length of the file: the length of a signature
-- dictionary does not depend on its byte range. The second rendering
-- gives each signature dictionary its byte range. The time is linear in
-- the size of the file, also with many signature dictionaries.

-- Note [Object streams of the writer]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- An object stream (PDF 32000-1, 7.5.7) contains objects that are not
-- streams, compressed together. A cross-reference stream (7.5.8) gives
-- the place of each object: an offset in the file, or an object stream
-- and an index in it. The file is smaller: the objects are compressed,
-- and an entry has 4 or 5 bytes before the compression, not 20.
--
--   %PDF-1.5
--   12 0 obj                        a stream stays an object of the file
--   << /Length 230 /Filter /FlateDecode >> stream ... endstream
--   endobj
--   20 0 obj                        the objects 1 to 11, 13 to 19
--   << /Type /ObjStm /N 18 /First 97 /Filter /FlateDecode >> stream
--   1 0 2 54 ... 19 802             (before the compression)
--   << /Type /Catalog ... >>
--   ...
--   endstream endobj
--   21 0 obj                        the cross-reference stream
--   << /Type /XRef /Root 1 0 R /Size 22 /W [1 1 2] /Index [0 22]
--      /Filter /FlateDecode /DecodeParms << /Predictor 12 /Columns 4 >> >>
--   stream ... endstream endobj
--   startxref
--   ...
--
-- These objects are not in an object stream ('packObjects'):
--
-- * the streams, and the objects whose generation is not 0 (7.5.7);
-- * the signature dictionaries: a signature gives the offset of its
--   /Contents in the file (Note [Signatures of the writer]);
-- * an object whose value is a reference ("5 0 obj 1 0 R endobj"): qpdf
--   and mutool read only one token for an object of an object stream,
--   thus the integer 1;
-- * all the objects of an encrypted file, when the writer does not have
--   the key: the object stream must be encrypted, and the objects in it
--   must not be (Note [Objects that are not encrypted] in
--   Graphics.PDF.File.Crypt). 'Graphics.PDF.File.renderEncryptedPdfFile'
--   puts the objects in object streams before the encryption.
--
-- An object stream has 100 objects at most: a reader decodes all of an
-- object stream to read one object of it. The rows of the
-- cross-reference stream use the predictor "Up" of PNG: a row is mostly
-- the row before it plus a small number, and these differences compress
-- well. A trailer /Info that is a dictionary becomes an object
-- ('infoObject'): it is a reference in a cross-reference stream (7.5.8.2).
-- The version is 1.5 at least.
--
-- The new objects (the /Info, the object streams, the cross-reference
-- stream) get the numbers after all the numbers of the file, also the
-- numbers of the references ('largestNumber'): a reference to an object
-- that the file does not have stays a reference to no object (null). A
-- reference above 8,388,607 (the largest object number) is not counted:
-- it is a reference to no object, and the new numbers stay below it.
--
-- The file has a cross-reference table, also with 'renderObjectStreams',
-- when ('objectStreamsPossible'):
--
-- * its metadata declares PDF/A-1 (pdfaid:part 1): PDF/A-1 forbids the
--   cross-reference streams (ISO 19005-1, 6.1.4);
-- * the new numbers would be above 8,388,607, the largest object number
--   of PDF 32000-1 (Annex C), or a generation is above 65535, the
--   largest generation: the readers refuse such a stream. (Such a file
--   is not correct, and the readers can refuse its table too.)
--
-- The keys of a cross-reference section in the trailer of the 'PdfFile'
-- ('sectionKeys': /Prev, /XRefStm, /W, ...) are not written: the writer
-- gives its own section.

-- | The options of the writer
data RenderOptions = RenderOptions
  { renderObjectStreams :: !Bool
    -- ^ Object streams and a cross-reference stream (PDF 1.5): a smaller
    -- file. False: a cross-reference table, for the readers of PDF 1.4.
    -- See Note [Object streams of the writer]
  } deriving (Eq, Show)

-- | Object streams and a cross-reference stream
defaultRenderOptions :: RenderOptions
defaultRenderOptions = RenderOptions { renderObjectStreams = True }

-- | The bytes of a PDF file, with 'defaultRenderOptions'.
-- See Note [Files of the writer]
renderPdfFile :: PdfFile -> L.ByteString
renderPdfFile = renderPdfFileWith defaultRenderOptions

-- | The bytes of a PDF file.
--
-- With 'renderObjectStreams', an encrypted file (an /Encrypt in the
-- trailer, for example from 'Graphics.PDF.File.encryptPdfFile') gets a
-- cross-reference stream but no object streams, and its /Info stays as it
-- is: the writer does not have its key. A PDF/A-1 file, or a file with
-- very large object numbers, gets a cross-reference table.
-- See Note [Files of the writer] and Note [Object streams of the writer]
renderPdfFileWith :: RenderOptions -> PdfFile -> L.ByteString
renderPdfFileWith options file
  | not (renderObjectStreams options && objectStreamsPossible file) = render_with_table file
  -- the writer does not have the key of the file
  | M.member (C.pack "Encrypt") (pdfTrailer file) = renderPacked (Packed file IM.empty)
  | otherwise = renderPacked (packObjects file)

-- | Can a file have object streams and a cross-reference stream? Not a
-- PDF/A-1 file, and not a file whose new numbers or generations would be
-- too large. See Note [Object streams of the writer]
objectStreamsPossible :: PdfFile -> Bool
objectStreamsPossible file = not (pdfa_1 file)
  && all ((<= 65535) . fst) (IM.elems (pdfObjects file))
  -- the /Info, the object streams and the cross-reference stream
  && largestNumber file <= 8388607 - 3 - IM.size (pdfObjects file) `div` 100

-- | Does the metadata of the catalog declare PDF/A-1?
pdfa_1 :: PdfFile -> Bool
pdfa_1 file = case resolveValue file <$> dictLookup (C.pack "Root") (pdfTrailer file) of
  Just (Dict catalog) | Just (Ref n _) <- dictLookup (C.pack "Metadata") catalog
                      , Just (Stream d raw) <- getObject file n
                      , Right xmp <- decodeStream (16 * 1024 * 1024) d raw ->
    any (`S.isInfixOf` xmp) (map C.pack ["pdfaid:part=\"1\"", "pdfaid:part='1'", "<pdfaid:part>1</pdfaid:part>"])
  _ -> False

-- | The largest number of a file: of its objects, and of the references
-- of its objects and of its trailer. 0 for a file with no object. A
-- reference above 8,388,607, the largest object number of PDF 32000-1
-- (Annex C), is not counted: no new object can get its number.
-- See Note [Object streams of the writer]
largestNumber :: PdfFile -> Int
largestNumber file = maximum (0 : fst_max (pdfObjects file) : map dict_max [pdfTrailer file] ++ map object_max (IM.elems (pdfObjects file)))
  where
    fst_max = maybe 0 fst . IM.lookupMax
    object_max (_, o) = case o of
      Plain v -> value_max v
      Stream d _ -> dict_max d
    dict_max = M.foldl' (\m v -> max m (value_max v)) 0
    value_max v = case v of
      Ref n _ | n <= 8388607 -> n
      Array vs -> List.foldl' (\m x -> max m (value_max x)) 0 vs
      Dict d -> dict_max d
      _ -> 0

-- | The keys of a cross-reference section in a trailer. The writer gives
-- its own section. See Note [Object streams of the writer]
sectionKeys :: [S.ByteString]
sectionKeys = map C.pack ["Size", "Prev", "XRefStm", "Type", "W", "Index", "Length", "Filter", "DecodeParms", "DL"]

-- | The trailer of a file, without the keys of a cross-reference section
trailer_of :: PdfFile -> Dict
trailer_of file = foldr M.delete (pdfTrailer file) sectionKeys

-- | A file with a cross-reference table.
-- See Note [Files of the writer]
render_with_table :: PdfFile -> L.ByteString
render_with_table file = header <> renderObjects (L.length header) objects xref_and_trailer
  where
    header = file_header (pdfVersion file)
    objects = numbered_objects (pdfObjects file)
    size = maybe 1 ((+ 1) . fst) (IM.lookupMax (pdfObjects file))
    xref_and_trailer offsets start_xref =
      let entries = IM.fromList ((0, FreeEntry 0 65535) : [ (n, InUse off g) | ((n, (g, _)), off) <- zip objects offsets ])
      in B.toLazyByteString $ xrefTable entries <> B.string7 "trailer\n"
           <> renderDict (M.insert (C.pack "Size") (Integer (fromIntegral size)) (trailer_of file))
           <> B.string7 ("\nstartxref\n" ++ show start_xref ++ "\n%%EOF\n")

-- | The header of a file, and a comment with four bytes above 127: the
-- file is binary
file_header :: (Int, Int) -> L.ByteString
file_header (major, minor) =
  B.toLazyByteString (B.string7 ("%PDF-" ++ show major ++ "." ++ show minor ++ "\n%")
                      <> B.word8 0xE2 <> B.word8 0xE3 <> B.word8 0xCF <> B.word8 0xD3 <> B.char7 '\n')

-- | The objects that the writer writes: their numbers start at 1, and an
-- object 0 is not written
numbered_objects :: IM.IntMap (Int, Object) -> [(Int, (Int, Object))]
numbered_objects objects = IM.toList (snd (IM.split 0 objects))

-- | The objects of a file, some of them in object streams
data Packed = Packed
  { packedFile :: !PdfFile -- ^ The objects that are not in an object stream, and the object streams
  , packedEntries :: !(IM.IntMap (Int, Int)) -- ^ The objects in object streams: the number of the object stream, and the index in it
  }

-- | Put the objects of a file in object streams.
-- See Note [Object streams of the writer]
packObjects :: PdfFile -> Packed
packObjects file0 = Packed file { pdfObjects = IM.union kept containers } entries
  where
    file = infoObject file0
    (kept, containers, entries) = packInObjectStreams (largestNumber file + 1) (pdfObjects file)

-- | Put the objects that can be in an object stream in object streams.
-- The object streams get the numbers from the first argument. The result:
-- the objects that stay outside, the object streams, and the place of
-- each object in an object stream ('packedEntries').
-- See Note [Object streams of the writer]
packInObjectStreams :: Int -> IM.IntMap (Int, Object) -> (IM.IntMap (Int, Object), IM.IntMap (Int, Object), IM.IntMap (Int, Int))
packInObjectStreams first objects = (IM.difference objects entries, containers, entries)
  where
    packable (n, (g, o)) = case o of
      Plain (Ref _ _) -> False
      Plain v -> n > 0 && g == 0 && not (signature v)
      Stream _ _ -> False
    signature v = case v of
      Dict d -> isSignatureDict d
      _ -> False
    groups = chunks [ (n, v) | x@(n, (_, Plain v)) <- IM.toList objects, packable x ]
    containers = IM.fromList [ (c, (0, object_stream group)) | (c, group) <- zip [first ..] groups ]
    entries = IM.fromList [ (n, (c, i)) | (c, group) <- zip [first ..] groups, (i, (n, _)) <- zip [0 ..] group ]
    chunks xs = case splitAt 100 xs of
      ([], _) -> []
      (group, rest) -> group : chunks rest

-- | An object stream with some objects (PDF 32000-1, 7.5.7): a line with
-- the numbers and the offsets, then the objects
object_stream :: [(Int, Value)] -> Object
object_stream group = Stream dict (L.toStrict (Z.compress (L.fromChunks (table : bodies))))
  where
    bodies = [ L.toStrict (B.toLazyByteString (renderValue v <> B.char7 '\n')) | (_, v) <- group ]
    offsets = scanl (+) 0 (map S.length bodies)
    table = C.pack (unwords [ show n ++ " " ++ show off | ((n, _), off) <- zip group offsets ] ++ "\n")
    dict = M.fromList [ (C.pack "Type", Name (C.pack "ObjStm")), (C.pack "N", Integer (fromIntegral (length group)))
                      , (C.pack "First", Integer (fromIntegral (S.length table))), (C.pack "Filter", Name (C.pack "FlateDecode")) ]

-- | A file with a cross-reference stream: the objects that are not in an
-- object stream, then the cross-reference stream.
-- See Note [Object streams of the writer]
renderPacked :: Packed -> L.ByteString
renderPacked (Packed file in_streams) = header <> renderObjects (L.length header) objects xref
  where
    header = file_header (max (1, 5) (pdfVersion file))
    objects = numbered_objects (pdfObjects file)
    number = 1 + max (largestNumber file) (maybe 0 fst (IM.lookupMax in_streams))
    xref offsets start_xref =
      let entries = IM.fromList ((0, FreeEntry 0 65535) : (number, InUse start_xref 0)
                                 : [ (n, InUse off g) | ((n, (g, _)), off) <- zip objects offsets ]
                                 ++ [ (n, InStream c i) | (n, (c, i)) <- IM.toList in_streams ])
      in B.toLazyByteString $ xrefStream number (trailer_of file) entries
           <> B.string7 ("startxref\n" ++ show start_xref ++ "\n%%EOF\n")

-- | A trailer /Info that is a dictionary becomes a new object, with the
-- number after the numbers of the file
infoObject :: PdfFile -> PdfFile
infoObject file = case M.lookup (C.pack "Info") (pdfTrailer file) of
  Just (Dict info) ->
    let n = largestNumber file + 1
    in file { pdfTrailer = M.insert (C.pack "Info") (Ref n 0) (pdfTrailer file)
            , pdfObjects = IM.insert n (0, Plain (Dict info)) (pdfObjects file) }
  _ -> file

-- | The bytes of objects at an offset of a file, then the bytes that the
-- offsets of the objects and the offset after them give (a cross-reference
-- section). The byte range of a signature dictionary that is not signed
-- ends at the end of these bytes.
-- See Note [Signatures of the writer]
renderObjects :: Int64 -> [(Int, (Int, Object))] -> ([Int64] -> Int64 -> L.ByteString) -> L.ByteString
renderObjects start objects end_of = L.concat bodies <> rest
  where
    first_bodies = [ B.toLazyByteString (render_object Nothing n g o) | (n, (g, o)) <- objects ]
    offsets = scanl (+) start (map L.length first_bodies)
    rest = end_of (init offsets) (last offsets)
    total = last offsets + L.length rest
    bodies = [ case o of
                 Plain (Dict d) | Just _ <- unsigned_signature d -> B.toLazyByteString (render_object (Just (off, total)) n g o)
                 _ -> body
             | ((n, (g, o)), off, body) <- zip3 objects offsets first_bodies ]

-- | An entry of a cross-reference section
data XrefEntry
  = InUse !Int64 !Int -- ^ The offset and the generation of an object
  | FreeEntry !Int !Int -- ^ The number of the next free entry, and the generation of the next object with this number
  | InStream !Int !Int -- ^ The number of an object stream, and the index of the object in it. Only in a cross-reference stream

-- | A cross-reference table, with a subsection for each run of numbers.
-- The entries must not be 'InStream'.
xrefTable :: IM.IntMap XrefEntry -> B.Builder
xrefTable entries = B.string7 "xref\n" <> mconcat (map subsection (runs (IM.keys entries)))
  where
    subsection ns@(first : _) = B.string7 (show first ++ " " ++ show (length ns) ++ "\n") <> mconcat (map line ns)
    subsection [] = mempty
    line n = case IM.lookup n entries of
      Just (InUse off g) -> B.string7 (printf "%010d %05d n \n" off g)
      Just (FreeEntry next g) -> B.string7 (printf "%010d %05d f \n" next g)
      Just (InStream _ _) -> mempty
      Nothing -> mempty

-- | A cross-reference stream (PDF 32000-1, 7.5.8): its number, the
-- entries of the trailer, and the entries of the section. The entries
-- must have the one of the stream. The rows use the predictor "Up" of
-- PNG, and are compressed. See Note [Object streams of the writer]
xrefStream :: Int -> Dict -> IM.IntMap XrefEntry -> B.Builder
xrefStream number trailer entries = render_object Nothing number 0 (Stream dict compressed) <> B.string7 "\n"
  where
    width = max 1 (length (takeWhile (> 0) (iterate (`div` 256) (maximum (1 : map field (IM.elems entries))))))
    field e = case e of
      InUse off _ -> off
      FreeEntry next _ -> fromIntegral next
      InStream container _ -> fromIntegral container
    big_endian :: Int -> Int64 -> [Word8]
    big_endian k x = [ fromIntegral (x `div` 256 ^ i) | i <- [k - 1, k - 2 .. 0] ]
    row e = case e of
      InUse off g -> 1 : big_endian width off ++ big_endian 2 (fromIntegral g)
      FreeEntry next g -> 0 : big_endian width (fromIntegral next) ++ big_endian 2 (fromIntegral g)
      InStream container i -> 2 : big_endian width (fromIntegral container) ++ big_endian 2 (fromIntegral i)
    rows = map row (IM.elems entries)
    columns = 3 + width
    -- the predictor "Up": the type 2 of PNG, then each byte minus the byte above it
    predicted = concat (zipWith (\above r -> 2 : zipWith (-) r above) (replicate columns 0 : rows) rows)
    compressed = L.toStrict (Z.compress (L.pack predicted))
    index = concat [ [Integer (fromIntegral first), Integer (fromIntegral (length ns))] | ns@(first : _) <- runs (IM.keys entries) ]
    size = maybe 1 ((+ 1) . fst) (IM.lookupMax entries)
    dict = M.union (M.fromList [ (C.pack "Type", Name (C.pack "XRef")), (C.pack "W", Array (map Integer [1, fromIntegral width, 2]))
                               , (C.pack "Index", Array index), (C.pack "Size", Integer (fromIntegral size))
                               , (C.pack "Filter", Name (C.pack "FlateDecode"))
                               , (C.pack "DecodeParms", Dict (M.fromList [ (C.pack "Predictor", Integer 12)
                                                                          , (C.pack "Columns", Integer (fromIntegral columns)) ])) ])
                   trailer

-- | A signature dictionary that is not signed: the number of bytes of its
-- /Contents, and its other entries.
-- See Note [Signatures of the writer]
unsigned_signature :: Dict -> Maybe (Int, Dict)
unsigned_signature d = case M.lookup (C.pack "Contents") d of
  Just (String s) | isSignatureDict d, not (S.null s), S.all (== 0) s ->
    Just (S.length s, M.delete (C.pack "ByteRange") (M.delete (C.pack "Contents") d))
  _ -> Nothing

-- | The numbers in runs of consecutive numbers. The input is sorted.
runs :: [Int] -> [[Int]]
runs = reverse . map reverse . List.foldl' add []
  where
    add (run@(prev : _) : done) n | n == prev + 1 = (n : run) : done
    add done n = [n] : done

-- | The bytes of an object. For a signature dictionary that is not
-- signed: the offset of the object and the length of the file, when they
-- are known. See Note [Signatures of the writer]
render_object :: Maybe (Int64, Int64) -> Int -> Int -> Object -> B.Builder
render_object at n g o = B.string7 obj_header <> body <> B.string7 "\nendobj\n"
  where
    obj_header = show n ++ " " ++ show g ++ " obj\n"
    -- the keys come after the object header and "<<"
    keys_at = fmap (\(offset, total) -> (offset + fromIntegral (length obj_header) + 2, total)) at
    body = case o of
      Plain (Dict d) | Just (size, others) <- unsigned_signature d ->
        B.string7 "<<" <> B.lazyByteString (signatureKeys keys_at size)
          <> mconcat [ B.char7 ' ' <> render_name k <> B.char7 ' ' <> renderValue x | (k, x) <- M.toList others ]
          <> B.string7 " >>"
      Plain v -> renderValue v
      Stream d bytes -> renderDict (M.insert (C.pack "Length") (Integer (fromIntegral (S.length bytes))) d)
        <> B.string7 "\nstream\n" <> B.byteString bytes <> B.string7 "\nendstream"

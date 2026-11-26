---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The bytes of the values and of a PDF file
---------------------------------------------------------
module Graphics.PDF.File.Write
  ( renderValue
  , renderDict
  , renderPdfFile
  , renderObjects
  , XrefEntry(..)
  , xrefTable
  , xrefStream
  ) where

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
-- The writer gives a file with one cross-reference table, and no object
-- stream and no cross-reference stream (an incremental update can have a
-- cross-reference stream: see Note [Incremental update] in
-- Graphics.PDF.File.Update):
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

-- | The bytes of a PDF file.
-- See Note [Files of the writer]
renderPdfFile :: PdfFile -> L.ByteString
renderPdfFile file = header <> renderObjects (L.length header) objects xref_and_trailer
  where
    (major, minor) = pdfVersion file
    header = B.toLazyByteString (B.string7 ("%PDF-" ++ show major ++ "." ++ show minor ++ "\n%")
                                 <> B.word8 0xE2 <> B.word8 0xE3 <> B.word8 0xCF <> B.word8 0xD3 <> B.char7 '\n')
    -- the object numbers start at 1: an object 0 is not written
    objects = IM.toList (snd (IM.split 0 (pdfObjects file)))
    size = maybe 1 ((+ 1) . fst) (IM.lookupMax (pdfObjects file))
    xref_and_trailer offsets start_xref =
      let entries = IM.fromList ((0, FreeEntry 0 65535) : [ (n, InUse off g) | ((n, (g, _)), off) <- zip objects offsets ])
      in B.toLazyByteString $ xrefTable entries <> B.string7 "trailer\n"
           <> renderDict (M.insert (C.pack "Size") (Integer (fromIntegral size)) (pdfTrailer file))
           <> B.string7 ("\nstartxref\n" ++ show start_xref ++ "\n%%EOF\n")

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

-- | A cross-reference table, with a subsection for each run of numbers
xrefTable :: IM.IntMap XrefEntry -> B.Builder
xrefTable entries = B.string7 "xref\n" <> mconcat (map subsection (runs (IM.keys entries)))
  where
    subsection ns@(first : _) = B.string7 (show first ++ " " ++ show (length ns) ++ "\n") <> mconcat (map line ns)
    subsection [] = mempty
    line n = case IM.lookup n entries of
      Just (InUse off g) -> B.string7 (printf "%010d %05d n \n" off g)
      Just (FreeEntry next g) -> B.string7 (printf "%010d %05d f \n" next g)
      Nothing -> mempty

-- | A cross-reference stream (PDF 32000-1, 7.5.8) with no filter: its
-- number, the entries of the trailer, and the entries of the section. The
-- entries must have the one of the stream.
xrefStream :: Int -> Dict -> IM.IntMap XrefEntry -> B.Builder
xrefStream number trailer entries = render_object Nothing number 0 (Stream dict rows) <> B.string7 "\n"
  where
    width = max 1 (length (takeWhile (> 0) (iterate (`div` 256) (maximum (1 : map field (IM.elems entries))))))
    field e = case e of
      InUse off _ -> off
      FreeEntry next _ -> fromIntegral next
    big_endian :: Int -> Int64 -> [Word8]
    big_endian k x = [ fromIntegral (x `div` 256 ^ i) | i <- [k - 1, k - 2 .. 0] ]
    row e = case e of
      InUse off g -> 1 : big_endian width off ++ big_endian 2 (fromIntegral g)
      FreeEntry next g -> 0 : big_endian width (fromIntegral next) ++ big_endian 2 (fromIntegral g)
    rows = S.pack (concatMap row (IM.elems entries))
    index = concat [ [Integer (fromIntegral first), Integer (fromIntegral (length ns))] | ns@(first : _) <- runs (IM.keys entries) ]
    size = maybe 1 ((+ 1) . fst) (IM.lookupMax entries)
    dict = M.union (M.fromList [ (C.pack "Type", Name (C.pack "XRef")), (C.pack "W", Array (map Integer [1, fromIntegral width, 2]))
                               , (C.pack "Index", Array index), (C.pack "Size", Integer (fromIntegral size)) ])
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

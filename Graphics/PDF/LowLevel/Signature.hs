{-# LANGUAGE BangPatterns #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The bytes of a signature dictionary, and the functions that complete
-- them in the bytes of a document
---------------------------------------------------------
module Graphics.PDF.LowLevel.Signature
  ( signatureValue
  , signatureKeys
  , setByteRange
  , findSignature
  , writeSignature
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Binary.Builder (Builder, fromLazyByteString)
import Data.Char (isDigit)
import Data.Int (Int64)

-- Note [Layout of a signature dictionary]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A signature is made on the bytes of the complete file, without the
-- value of /Contents (PDF 32000-1, 12.8.1). /ByteRange gives these bytes:
--
--   %PDF-1.5 ... /ByteRange [0 a b c]   /Contents <3082...0000> ... %%EOF
--   |------------- a bytes ------------------------|          |-- c --|
--                                                  ^ offset a ^ offset b
--
-- Thus the document is written in three steps, and no step changes the
-- length of the file:
--
-- 1. 'signatureValue' writes /ByteRange [0 0 0 0] followed by spaces, and
--    a /Contents that has only zeros. The two keys come first in the
--    dictionary, before any text of the user.
-- 2. When the document is rendered, the offsets are known:
--    'setByteRange' writes them in the reserved space.
-- 3. 'writeSignature' writes the signature in /Contents. The signature is
--    shorter than the reserved space: the zeros that stay are padding.
--
-- A rendered document that is not signed is thus a correct PDF file with
-- an empty signature: another tool can also sign it.

-- | Length of the text of a byte range: "[0 ", three numbers of ten digits
-- at most, two spaces and "]"
byte_range_width :: Int64
byte_range_width = 3 + 3 * 10 + 2 + 1

byte_range_key, contents_key :: B.ByteString
byte_range_key = C.pack "/ByteRange "
contents_key = C.pack "\n/Contents <"

pad_byte_range :: String -> B.ByteString
pad_byte_range t = C.pack (t ++ replicate (fromIntegral byte_range_width - length t) ' ')

-- | The bytes of a signature dictionary that is not signed.
-- See Note [Layout of a signature dictionary]
signatureValue :: String -- ^ Value of /SubFilter, without the "/"
               -> Int -- ^ Number of bytes to reserve for the signature
               -> Builder -- ^ The other entries
               -> Builder
signatureValue subfilter size others = mconcat
    [ lazy (C.pack ("<<\n/Type /Sig\n/Filter /Adobe.PPKLite\n/SubFilter /" ++ subfilter ++ "\n"))
    , lazy (signatureKeys Nothing (max 1 size))
    , lazy (C.pack "\n")
    , others
    , lazy (C.pack ">>")
    ]
  where
    lazy = fromLazyByteString

-- | The /ByteRange and the /Contents of a signature dictionary that is not
-- signed, with a given number of bytes for the signature. With the offset
-- of these bytes in the file and the length of the file, the byte range
-- is the final one; else it is [0 0 0 0] ('setByteRange' writes it later,
-- and finds the keys only in the first 200 bytes of the object). The
-- length of the result does not depend on the byte range.
-- See Note [Layout of a signature dictionary]
signatureKeys :: Maybe (Int64, Int64) -> Int -> B.ByteString
signatureKeys at size = B.concat
    [ byte_range_key
    , range
    , contents_key
    , C.replicate (2 * fromIntegral size) '0'
    , C.pack ">"
    ]
  where
    placeholder = pad_byte_range "[0 0 0 0]"
    range = case at of
      Nothing -> placeholder
      Just (offset, total) ->
        -- the offsets of the "<" and after the ">" of /Contents
        let a = offset + B.length byte_range_key + byte_range_width + B.length contents_key - 1
            b = a + 2 * fromIntegral size + 2
            text = pad_byte_range (concat ["[0 ", show a, " ", show b, " ", show (total - b), "]"])
        in if B.length text == byte_range_width then text else placeholder

-- | Write the byte range of the signature dictionary that starts at the
-- given offset of the document. The document is not changed if it has no
-- such dictionary at this offset.
-- See Note [Layout of a signature dictionary]
setByteRange :: Int64 -> B.ByteString -> B.ByteString
setByteRange object_offset doc =
    case B.stripPrefix byte_range_key at_key of
      Just after_key | key_offset >= 0
                     , contents_key `B.isPrefixOf` B.drop byte_range_width after_key
                     , Just size <- B.elemIndex 0x3E (B.drop (a + 1) doc) ->
        let b = a + 1 + size + 1
            range = pad_byte_range (concat ["[0 ", show a, " ", show b, " ", show (total - b), "]"])
        in if B.length range == byte_range_width
             then B.concat [B.take range_offset doc, range, B.drop (range_offset + byte_range_width) doc]
             else doc
      _ -> doc
  where
    total = B.length doc
    -- the key is in the first bytes of the object: see 'signatureValue'
    key_offset = maybe (-1) (object_offset +) (find_from byte_range_key (B.take 200 (B.drop object_offset doc)))
    at_key = B.drop key_offset doc
    range_offset = key_offset + B.length byte_range_key
    -- offset of the "<" of /Contents
    a = range_offset + byte_range_width + B.length contents_key - 1

-- | Offset of the first occurrence of a pattern
find_from :: B.ByteString -> B.ByteString -> Maybe Int64
find_from pat = go 0
  where
    go :: Int64 -> B.ByteString -> Maybe Int64
    go !i s
      | B.null s = Nothing
      | pat `B.isPrefixOf` s = Just i
      | otherwise = case B.elemIndex (B.head pat) (B.drop 1 s) of
          Nothing -> Nothing
          Just k -> go (i + 1 + k) (B.drop (1 + k) s)

-- | Find the signature of a rendered document that is not signed: the
-- offsets of the "<" and after the ">" of its /Contents.
--
-- A byte range is used only if it agrees with the document: its end is the
-- end of the document and it excludes exactly one string in hexadecimal.
-- 'Left' gives the reason no signature can be written.
findSignature :: B.ByteString -> Either String (Int64, Int64)
findSignature doc = go 0 doc
  where
    total = B.length doc
    key = byte_range_key `B.append` C.pack "[0 "
    go offset s = case find_from key s of
      Nothing -> Left "the document has no signature field: use addSignature"
      Just k ->
        let here = offset + k
            rest = B.drop (k + B.length key) s
        in case numbers (C.unpack (C.takeWhile (/= ']') (B.take byte_range_width rest))) of
             Just [a, b, c] | a > 0, b > a + 2, b + c == total, c > 0
                            , B.index doc a == 0x3C, B.index doc (b - 1) == 0x3E ->
               let hex = B.take (b - a - 2) (B.drop (a + 1) doc)
               in if B.all (== 0x30) hex then Right (a, b)
                  else if B.all is_hex hex then Left "the document is already signed"
                  else go (here + 1) (B.drop (k + 1) s)
             _ -> go (here + 1) (B.drop (k + 1) s)
    numbers t = case words t of
      ws | all (\w -> not (null w) && all isDigit w && length w <= 10) ws -> Just (map read ws)
      _ -> Nothing
    is_hex w = (w >= 0x30 && w <= 0x39) || (w >= 0x41 && w <= 0x46) || (w >= 0x61 && w <= 0x66)

-- | Write a signature in the /Contents that 'findSignature' gave.
writeSignature :: (Int64, Int64) -> S.ByteString -> B.ByteString -> Either String B.ByteString
writeSignature (a, b) sig doc
    | S.null sig = Left "the signature is empty"
    | used > room = Left ("the signature has " ++ show (S.length sig) ++ " bytes, but the document reserves "
                          ++ show (room `div` 2) ++ " bytes: use a larger sigSize")
    | otherwise = Right (B.concat [B.take (a + 1) doc, hex, B.drop (a + 1 + used) doc])
  where
    room = b - a - 2
    used = 2 * fromIntegral (S.length sig)
    hex = B.pack (concatMap (\w -> [digit (w `div` 16), digit (w `mod` 16)]) (S.unpack sig))
    digit d | d < 10 = 0x30 + d
            | otherwise = 0x41 + d - 10

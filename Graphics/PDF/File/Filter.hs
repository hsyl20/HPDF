---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The decoding of the data of a stream
---------------------------------------------------------
module Graphics.PDF.File.Filter
  ( decodeStream
  ) where

import Control.Monad (foldM, unless)
import Data.Char (digitToInt, isHexDigit)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import Data.Word (Word64, Word8)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Graphics.PDF.File.Object
import Graphics.PDF.LowLevel.Inflate (checkLineFilters, inflateUpTo, unfilterLines)

-- Note [Filters of the reader]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The reader decodes the streams that it must read itself: the
-- cross-reference streams and the object streams. Their writers use
-- FlateDecode, with or without a PNG predictor (PDF 32000-1, 7.4.4):
--
--   /Filter /FlateDecode /DecodeParms << /Predictor 12 /Columns 5 >>
--
-- Thus 'decodeStream' knows FlateDecode with the predictor 1 (none) or
-- 10 to 15 (PNG). The removal of a layer also reads the content streams
-- (Graphics.PDF.File.Layers), and old files encode them as text too:
-- 'decodeStream' knows ASCIIHexDecode and ASCII85Decode, and a list of
-- these filters:
--
--   /Filter [/ASCII85Decode /FlateDecode] /DecodeParms [null << ... >>]
--
-- It gives 'Left' for the other filters and for the TIFF predictor 2.
-- The other streams of a file are copied as they are, so that the reader
-- does not need to know their filters.
--
-- The PNG predictors are the line filters of a PNG image: each line has a
-- byte that gives its filter ('Graphics.PDF.LowLevel.Inflate').

-- | Decode the data of a stream. The first argument is the largest number
-- of bytes that the decoded data can have.
--
-- See Note [Filters of the reader]
decodeStream :: Int -> Dict -> S.ByteString -> Either String S.ByteString
decodeStream limit dict raw = do
  filters <- case dictLookup (C.pack "Filter") dict of
    Nothing -> Right []
    Just (Name f) -> Right [f]
    Just (Array fs) -> mapM filter_name fs
    Just _ -> Left "the /Filter of a stream is not a name"
  parms <- case dictLookup (C.pack "DecodeParms") dict of
    _ | null filters -> Right []
    Nothing -> Right (map (const Nothing) filters)
    Just (Dict d) | [_] <- filters -> Right [Just d]
    Just (Array ps) | length ps == length filters -> mapM parm ps
    Just (Array []) -> Right (map (const Nothing) filters)
    Just _ -> Left "the /DecodeParms of a stream are not supported"
  unless (all known filters) $
    Left ("the filters " ++ show (map C.unpack filters) ++ " are not supported")
  result <- foldM decode raw (zip filters parms)
  if S.length result <= limit then Right result else Left "a stream is larger than the limit"
  where
    filter_name (Name f) = Right f
    filter_name _ = Left "the /Filter of a stream is not a name"
    parm (Dict d) = Right (Just d)
    parm Null = Right Nothing
    parm _ = Left "the /DecodeParms of a stream are not supported"
    known f = f `elem` map C.pack ["FlateDecode", "ASCIIHexDecode", "ASCII85Decode"]
    decode bytes (f, p)
      | f == C.pack "FlateDecode" = do
          (inflated, more) <- inflateUpTo "FlateDecode" limit (L.fromStrict bytes)
          if more then Left "a stream is larger than the limit" else predictor p inflated
      | f == C.pack "ASCIIHexDecode" = ascii_hex limit bytes
      | otherwise = ascii85 limit bytes

-- | The bytes of ASCIIHexDecode data (PDF 32000-1, 7.4.2): pairs of hex
-- digits, white space, and ">" at the end. A last digit alone is
-- followed by 0. The first argument is the largest number of bytes.
ascii_hex :: Int -> S.ByteString -> Either String S.ByteString
ascii_hex limit input
  | not (S.all is_hex_digit digits) = Left "the data of ASCIIHexDecode has a character that is not a hex digit"
  | size > limit = Left "a stream is larger than the limit"
  | otherwise = Right (fst (S.unfoldrN size (\i -> Just (fromIntegral (digit i * 16 + digit (i + 1)), i + 2)) 0))
  where
    digits = S.filter (not . is_space) (S.takeWhile (/= 0x3E) input)
    size = (S.length digits + 1) `div` 2
    digit i
      | i >= S.length digits = 0
      | otherwise = digitToInt (C.index digits i)
    is_hex_digit w = isHexDigit (toEnum (fromIntegral w))

-- | The bytes of ASCII85Decode data (PDF 32000-1, 7.4.3): groups of 5
-- characters from "!" to "u" for 4 bytes, "z" for 4 bytes 0, white space,
-- and "~>" at the end. A last group of n characters (2 to 4) gives n - 1
-- bytes. The first argument is the largest number of bytes.
ascii85 :: Int -> S.ByteString -> Either String S.ByteString
ascii85 limit input = go 0 (0 :: Int) (0 :: Word64) mempty 0
  where
    n = S.length input
    go i count acc out size
      | size > limit = Left "a stream is larger than the limit"
      | i >= n = finish count acc out
      | otherwise = case S.index input i of
          w | w == 0x7E -> finish count acc out
            | is_space w -> go (i + 1) count acc out size
            | w == 0x7A && count == 0 -> go (i + 1) 0 0 (out <> B.word32BE 0) (size + 4)
            | w >= 0x21 && w <= 0x75 ->
                let acc' = acc * 85 + fromIntegral (w - 33)
                in if count + 1 < 5 then go (i + 1) (count + 1) acc' out size
                   else if acc' > 0xFFFFFFFF then Left "a group of ASCII85Decode is too large"
                   else go (i + 1) 0 0 (out <> B.word32BE (fromIntegral acc')) (size + 4)
            | otherwise -> Left "the data of ASCII85Decode has a character that is not correct"
    finish count acc out
      | count == 0 = Right (L.toStrict (B.toLazyByteString out))
      | count == 1 = Left "the data of ASCII85Decode ends with one character"
      | otherwise =
          let full = iterate (\a -> a * 85 + 84) acc !! (5 - count)
              bytes = [ fromIntegral ((full `div` (256 ^ k)) `mod` 256) | k <- [3, 2, 1, 0 :: Int] ]
          in if full > 0xFFFFFFFF then Left "a group of ASCII85Decode is too large"
             else Right (L.toStrict (B.toLazyByteString (out <> B.byteString (S.pack (take (count - 1) bytes)))))

is_space :: Word8 -> Bool
is_space w = w `elem` [0x20, 0x09, 0x0D, 0x0A, 0x0C, 0x00]

-- | Remove the predictor of the decoded data
predictor :: Maybe Dict -> S.ByteString -> Either String S.ByteString
predictor Nothing bytes = Right bytes
predictor (Just parms) bytes = do
  p <- integer_parm "Predictor" 1
  if p == 1 then Right bytes
  else if p < 10 || p > 15 then Left ("the predictor " ++ show p ++ " is not supported")
  else do
    colors <- integer_parm "Colors" 1
    bits <- integer_parm "BitsPerComponent" 8
    columns <- integer_parm "Columns" 1
    if colors < 1 || colors > 32 || bits `notElem` [1, 2, 4, 8, 16] || columns < 1 || columns > 1000000
      then Left "the parameters of a predictor are not correct"
      else do
        let unit = max 1 (colors * bits `div` 8)
            rb = (colors * bits * columns + 7) `div` 8
            lines_ = S.length bytes `div` (rb + 1)
        if checkLineFilters rb lines_ bytes
          then Right (unfilterLines unit rb lines_ bytes)
          else Left "a line of a stream has an unknown predictor"
  where
    integer_parm key def = case dictLookup (C.pack key) parms of
      Nothing -> Right def
      Just (Integer n) | n >= 0 && n <= 100000000 -> Right (fromInteger n :: Int)
      Just _ -> Left ("the /" ++ key ++ " of a predictor is not correct")

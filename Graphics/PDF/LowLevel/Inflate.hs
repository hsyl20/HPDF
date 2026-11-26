---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Bounded zlib expansion, and the line filters of PNG. A PNG image and a
-- PDF stream with a PNG predictor (/Predictor 10 to 15) use the same
-- filters.
---------------------------------------------------------
module Graphics.PDF.LowLevel.Inflate
  ( inflateUpTo
  , inflateExactly
  , checkLineFilters
  , unfilterLines
  ) where

import qualified Codec.Compression.Zlib.Internal as ZI
import Control.Monad (forM_)
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as SU
import Data.Word (Word8)
import Foreign.Storable (peekByteOff, pokeByteOff)

-- Note [Bounded expansion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
-- A small zlib stream can expand to a very large size. Thus the expansion
-- stops when it has the number of bytes that the caller can accept, and
-- it does not keep the bytes after this number: a file cannot make the
-- reader use more memory than the limit.

-- | Expand a zlib stream, to this number of bytes at most. The result has
-- the bytes, and 'True' if the stream gives more bytes than the limit.
-- The first argument is the start of the error messages, for example
-- @"PNG"@.
--
-- See Note [Bounded expansion]
inflateUpTo :: String -> Int -> L.ByteString -> Either String (S.ByteString, Bool)
inflateUpTo context limit input = do
    (chunks, more) <- ZI.foldDecompressStreamWithInput
        (\chunk next size ->
           let size' = size + S.length chunk
           in if size' > limit
                then Right ([S.take (limit - size) chunk], True)
                else (\(cs, m) -> (chunk : cs, m)) <$> next size')
        (\_ _ -> Right ([], False))
        (\e _ -> Left (context ++ ": the data cannot be expanded: " ++ show e))
        (ZI.decompressST ZI.zlibFormat ZI.defaultDecompressParams)
        input
        0
    Right (S.concat chunks, more)

-- | Expand a zlib stream that must give exactly this number of bytes.
-- See Note [Bounded expansion]
inflateExactly :: String -> Int -> L.ByteString -> Either String S.ByteString
inflateExactly context expected input = do
    (bytes, more) <- inflateUpTo context expected input
    if more then Left (context ++ ": the data is longer than expected")
    else if S.length bytes < expected then Left (context ++ ": the data is shorter than expected")
    else Right bytes

-- | The filter byte of each line is 0 to 4. The input has, for each line,
-- the byte of the filter and then the bytes of the line; its length is at
-- least @lines * (bytes + 1)@.
checkLineFilters :: Int -- ^ Bytes of a line
                 -> Int -- ^ Number of lines
                 -> S.ByteString
                 -> Bool
checkLineFilters rb lines_ raw =
    S.length raw >= lines_ * (rb + 1)
      && all (\y -> SU.unsafeIndex raw (y * (rb + 1)) <= 4) [0 .. lines_ - 1]

-- | Remove the filter of each line. The input has, for each line, the
-- byte of the filter and then the bytes of the line. Its length and its
-- filter bytes are checked before ('checkLineFilters').
unfilterLines :: Int -- ^ Bytes of a pixel for the filters
              -> Int -- ^ Bytes of a line
              -> Int -- ^ Number of lines
              -> S.ByteString
              -> S.ByteString
unfilterLines unit rb lines_ raw = SI.unsafeCreate (rb * lines_) $ \out ->
    forM_ [0 .. lines_ - 1] $ \y -> do
      let src = y * (rb + 1)
          dst = y * rb
          ftype = SU.unsafeIndex raw src
          get :: Int -> IO Int
          get i = (fromIntegral :: Word8 -> Int) <$> peekByteOff out i
          left x = if x >= unit then get (dst + x - unit) else return 0
          up x = if y > 0 then get (dst - rb + x) else return 0
          upleft x = if y > 0 && x >= unit then get (dst - rb + x - unit) else return 0
      forM_ [0 .. rb - 1] $ \x -> do
        let v = fromIntegral (SU.unsafeIndex raw (src + 1 + x)) :: Int
        r <- case ftype of
          0 -> return v
          1 -> (v +) <$> left x
          2 -> (v +) <$> up x
          3 -> do { a <- left x; b <- up x; return (v + (a + b) `div` 2) }
          _ -> do { a <- left x; b <- up x; c <- upleft x; return (v + paeth a b c) }
        pokeByteOff out (dst + x) (fromIntegral r :: Word8)
  where
    paeth :: Int -> Int -> Int -> Int
    paeth a b c =
      let p = a + b - c
          pa = abs (p - a)
          pb = abs (p - b)
          pc = abs (p - c)
      in if pa <= pb && pa <= pc then a else if pb <= pc then b else c

{-# LANGUAGE BangPatterns #-}
---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- PNG images: from the bytes of a file to the streams of a PDF image
---------------------------------------------------------
module Graphics.PDF.LowLevel.PNG
  ( PngImage(..)
  , PngColour(..)
  , parsePngImage
  ) where

import qualified Codec.Compression.Zlib as Z
import Control.Monad (forM_)
import Data.Bits (shiftL, shiftR, (.&.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Internal as SI
import qualified Data.ByteString.Lazy as L
import Graphics.PDF.LowLevel.Inflate (checkLineFilters, inflateExactly, unfilterLines)
import qualified Data.ByteString.Unsafe as SU
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)

-- Note [PNG images]
-- ~~~~~~~~~~~~~~~~~
-- A PNG file has a header (IHDR), an optional palette (PLTE), an optional
-- transparency (tRNS) and the pixels (IDAT): a zlib stream of the lines of
-- the image, each line with one byte that gives its filter.
--
-- A PDF image with the FlateDecode filter and the PNG predictor (/Predictor
-- 15) has exactly this form. Thus the pixels of most PNG files go to the
-- document as they are:
--
--   PNG                          PDF image
--   gray, 1 to 16 bits           /DeviceGray, the IDAT bytes
--   RGB, 8 or 16 bits            /DeviceRGB, the IDAT bytes
--   palette, 1 to 8 bits         [/Indexed /DeviceRGB n <palette>], the IDAT bytes
--   one transparent colour       /Mask [v v] (a colour key)
--
-- The other files are decoded: the zlib stream is expanded, the filters
-- are removed ('unfilter'), and the pixels are written to new streams.
--
--   an alpha channel             the colours, and the alpha as an /SMask image
--   a palette with alpha values  the indices, and the alpha of each pixel
--                                as an /SMask image
--   an interlaced file           the pixels in the order of the lines
--                                ('deinterlace')
--
-- Limits:
--
-- * An interlaced file with less than 8 bits for each sample is not read.
-- * The colour chunks (gAMA, cHRM, iCCP, sRGB) are not used: the samples
--   are device colours.
-- * The check values (CRC) of the chunks are not verified. The zlib stream
--   has its own check value, and its length must be the length that the
--   header gives.
--
-- The parser is total: a file that is not a correct PNG file gives 'Left'.
-- The size of the expanded pixels is known from the header, so the
-- expansion stops if a file gives more bytes ('inflate').

-- | Colour space of the image stream
data PngColour
  = PngGray
  | PngRgb
  | PngIndexed !S.ByteString -- ^ The palette: three bytes for each colour
  deriving (Eq, Show)

-- | A PNG image, ready for a PDF document
data PngImage = PngImage
  { pngWidth :: !Int
  , pngHeight :: !Int
  , pngBits :: !Int -- ^ Bits for each component of the image stream
  , pngColour :: !PngColour
  , pngData :: L.ByteString -- ^ The image stream: a zlib stream
  , pngPredictor :: !Bool -- ^ The image stream has the filter byte of each line
  , pngColourKey :: Maybe [Int] -- ^ The transparent colour: the value of each component
  , pngAlpha :: Maybe L.ByteString -- ^ The alpha of each pixel, with 'pngAlphaBits' bits: a zlib stream
  , pngAlphaBits :: !Int
  }

data Header = Header
  { hWidth :: !Int
  , hHeight :: !Int
  , hDepth :: !Int
  , hType :: !Int
  , hInterlaced :: !Bool
  }

-- | The largest number of bytes of pixels that is expanded
max_pixel_bytes :: Int
max_pixel_bytes = 1024 * 1024 * 1024

check :: Bool -> String -> Either String ()
check ok msg = if ok then Right () else Left ("PNG: " ++ msg)

be32 :: S.ByteString -> Int
be32 = S.foldl' (\a w -> a * 256 + fromIntegral w) 0 . S.take 4

be16 :: S.ByteString -> Int
be16 = S.foldl' (\a w -> a * 256 + fromIntegral w) 0 . S.take 2

-- | The chunks of a file: their type and their data, up to IEND
png_chunks :: S.ByteString -> Either String [(S.ByteString, S.ByteString)]
png_chunks file = do
    check (S.take 8 file == S.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]) "this is not a PNG file"
    go (S.drop 8 file)
  where
    go s
      | S.null s = Right []
      | S.length s < 12 = Left "PNG: the file is cut in a chunk"
      | otherwise = do
          let len = be32 s
              name = S.take 4 (S.drop 4 s)
          check (len <= S.length s - 12) "the file is cut in a chunk"
          let body = S.take len (S.drop 8 s)
          if name == C.pack "IEND"
            then Right []
            else ((name, body) :) <$> go (S.drop (len + 12) s)

parse_header :: S.ByteString -> Either String Header
parse_header d = do
    check (S.length d == 13) "the header has a wrong length"
    let w = be32 d
        h = be32 (S.drop 4 d)
        depth = fromIntegral (S.index d 8)
        ctype = fromIntegral (S.index d 9) :: Int
    check (w > 0 && h > 0) "the image has no pixel"
    check (w < 0x40000000 && h < 0x40000000) "the image is too large"
    check (S.index d 10 == 0 && S.index d 11 == 0) "unknown compression or filter method"
    check (S.index d 12 <= 1) "unknown interlace method"
    check (depth `elem` depths ctype) "this number of bits is not possible for this colour type"
    Right (Header w h depth ctype (S.index d 12 == 1))
  where
    depths :: Int -> [Int]
    depths 0 = [1, 2, 4, 8, 16]
    depths 2 = [8, 16]
    depths 3 = [1, 2, 4, 8]
    depths 4 = [8, 16]
    depths 6 = [8, 16]
    depths _ = []

-- | Number of samples of a pixel
channels :: Header -> Int
channels h = case hType h of
  0 -> 1
  2 -> 3
  3 -> 1
  4 -> 2
  _ -> 4

-- | Number of bytes of a line of pixels
row_bytes :: Header -> Int -> Int
row_bytes h w = (w * channels h * hDepth h + 7) `div` 8

-- | Number of bytes of a pixel for the filters: 1 at least
filter_unit :: Header -> Int
filter_unit h = max 1 (channels h * hDepth h `div` 8)

-- | Expand the image data, which must give exactly this number of bytes
inflate :: Int -> L.ByteString -> Either String S.ByteString
inflate = inflateExactly "PNG"

-- | The filter byte of each line is 0 to 4
check_filters :: Int -> Int -> S.ByteString -> Either String ()
check_filters rb lines_ raw = check (checkLineFilters rb lines_ raw) "unknown filter of a line"

-- | The seven passes of an interlaced image: first column, first line,
-- step of the columns, step of the lines
adam7 :: [(Int, Int, Int, Int)]
adam7 = [(0, 0, 8, 8), (4, 0, 8, 8), (0, 4, 4, 8), (2, 0, 4, 4), (0, 2, 2, 4), (1, 0, 2, 2), (0, 1, 1, 2)]

-- | The size of each pass that has pixels
pass_sizes :: Header -> [((Int, Int, Int, Int), Int, Int)]
pass_sizes h =
    [ (p, pw, ph)
    | p@(x0, y0, dx, dy) <- adam7
    , let pw = (hWidth h - x0 + dx - 1) `div` dx
    , let ph = (hHeight h - y0 + dy - 1) `div` dy
    , pw > 0, ph > 0 ]

-- | The pixels of an interlaced image in the order of the lines. Each
-- sample has 8 or 16 bits. The length and the filter bytes of the input
-- are checked before.
deinterlace :: Header -> S.ByteString -> S.ByteString
deinterlace h raw = SI.unsafeCreate (hWidth h * hHeight h * pixel) $ \out ->
    go out 0 (pass_sizes h)
  where
    pixel = channels h * hDepth h `div` 8
    go :: Ptr Word8 -> Int -> [((Int, Int, Int, Int), Int, Int)] -> IO ()
    go _ _ [] = return ()
    go out offset (((x0, y0, dx, dy), pw, ph) : rest) = do
      let rb = row_bytes h pw
          size = ph * (rb + 1)
          lines_ = unfilterLines (filter_unit h) rb ph (S.take size (S.drop offset raw))
      forM_ [0 .. ph - 1] $ \j ->
        forM_ [0 .. pw - 1] $ \i -> do
          let src = (j * pw + i) * pixel
              dst = ((y0 + j * dy) * hWidth h + x0 + i * dx) * pixel
          forM_ [0 .. pixel - 1] $ \k ->
            pokeByteOff out (dst + k) (SU.unsafeIndex lines_ (src + k))
      go out (offset + size) rest

-- | Take some bytes of each pixel: the bytes from an offset
take_bytes :: Int -- ^ Bytes of a pixel
           -> Int -- ^ Offset of the first byte to take
           -> Int -- ^ Number of bytes to take
           -> S.ByteString
           -> S.ByteString
take_bytes pixel offset n pixels =
    let count = S.length pixels `div` pixel
    in fst (S.unfoldrN (count * n) (\i -> Just (SU.unsafeIndex pixels ((i `div` n) * pixel + offset + i `mod` n), i + 1)) 0)

-- | The alpha of each pixel of an image with a palette: one byte for each
-- pixel. The lines of indices are packed, with 1 to 8 bits for each index.
palette_alpha :: Header -> S.ByteString -> S.ByteString -> S.ByteString
palette_alpha h alphas pixels =
    fst (S.unfoldrN (hWidth h * hHeight h) (\i -> Just (alpha_of (index_at i), i + 1)) 0)
  where
    depth = hDepth h
    rb = row_bytes h (hWidth h)
    index_at i =
      let (y, x) = i `divMod` hWidth h
          byte = fromIntegral (SU.unsafeIndex pixels (y * rb + (x * depth) `div` 8)) :: Int
          shift = 8 - depth - (x * depth) `mod` 8
      in (byte `shiftR` shift) .&. ((1 `shiftL` depth) - 1)
    alpha_of k = if k < S.length alphas then SU.unsafeIndex alphas k else 255

-- | Parse a PNG file.
-- See Note [PNG images]
parsePngImage :: S.ByteString -> Either String PngImage
parsePngImage file = do
    chunks <- png_chunks file
    h <- case chunks of
      (name, d) : _ | name == C.pack "IHDR" -> parse_header d
      _ -> Left "PNG: the file does not start with a header"
    let find name = [ d | (n, d) <- chunks, n == C.pack name ]
        idat = L.fromChunks (find "IDAT")
        trns = case find "tRNS" of
          t : _ -> Just t
          [] -> Nothing
        w = hWidth h
        rb = row_bytes h w
    check (not (L.null idat)) "the file has no image data"
    colour <- case hType h of
      3 -> case find "PLTE" of
        p : _ -> do
          check (not (S.null p) && S.length p `mod` 3 == 0 && S.length p <= 768) "the palette has a wrong length"
          Right (PngIndexed p)
        [] -> Left "PNG: an image with a palette has no palette"
      t | t == 0 || t == 4 -> Right PngGray
      _ -> Right PngRgb
    -- the transparent colour of an image that has no palette and no alpha
    key <- case (hType h, trns) of
      (0, Just t) -> do
        check (S.length t == 2) "the transparency has a wrong length"
        Right (Just [be16 t .&. sample_mask h])
      (2, Just t) -> do
        check (S.length t == 6) "the transparency has a wrong length"
        Right (Just [ be16 (S.drop k t) .&. sample_mask h | k <- [0, 2, 4] ])
      _ -> Right Nothing
    let palette_alphas = case (hType h, trns) of
          (3, Just t) | S.any (/= 255) t -> Just t
          _ -> Nothing
        has_alpha = hType h == 4 || hType h == 6
        image = PngImage
          { pngWidth = w, pngHeight = hHeight h, pngBits = hDepth h, pngColour = colour
          , pngData = idat, pngPredictor = True, pngColourKey = key
          , pngAlpha = Nothing, pngAlphaBits = 8 }
    check (hHeight h <= max_pixel_bytes `div` (rb + 1)) "the image is too large"
    if hInterlaced h
      then do
        check (hDepth h >= 8) "an interlaced image with less than 8 bits for each sample is not supported"
        let sizes = pass_sizes h
            expected = sum [ ph * (row_bytes h pw + 1) | (_, pw, ph) <- sizes ]
        raw <- inflate expected idat
        -- the filter bytes of each pass
        let offsets = scanl (+) 0 [ ph * (row_bytes h pw + 1) | (_, pw, ph) <- sizes ]
        forM_ (zip offsets sizes) $ \(o, (_, pw, ph)) ->
          check_filters (row_bytes h pw) ph (S.drop o raw)
        Right (split h image palette_alphas has_alpha (deinterlace h raw))
      else do
        raw <- inflate (hHeight h * (rb + 1)) idat
        check_filters rb (hHeight h) raw
        if has_alpha || palette_alphas /= Nothing
          then Right (split h image palette_alphas has_alpha (unfilterLines (filter_unit h) rb (hHeight h) raw))
          else Right image
  where
    sample_mask h = (1 `shiftL` hDepth h) - 1

-- | Make the image stream and the alpha stream from the pixels without
-- their filters.
split :: Header -> PngImage -> Maybe S.ByteString -> Bool -> S.ByteString -> PngImage
split h image palette_alphas has_alpha pixels
    | has_alpha =
        let sample = hDepth h `div` 8
            pixel = channels h * sample
            colours = (channels h - 1) * sample
        in image { pngData = deflate (take_bytes pixel 0 colours pixels)
                 , pngPredictor = False
                 , pngAlpha = Just (deflate (take_bytes pixel colours sample pixels))
                 , pngAlphaBits = hDepth h }
    | otherwise =
        image { pngData = deflate pixels
              , pngPredictor = False
              , pngAlpha = fmap (\t -> deflate (palette_alpha h t pixels)) palette_alphas
              , pngAlphaBits = 8 }
  where
    deflate = Z.compress . L.fromStrict

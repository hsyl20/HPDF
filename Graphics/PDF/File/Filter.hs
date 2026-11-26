---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The decoding of the data of a stream
---------------------------------------------------------
module Graphics.PDF.File.Filter
  ( decodeStream
  ) where

import qualified Data.ByteString as S
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
-- Thus 'decodeStream' knows no filter, or FlateDecode with the predictor
-- 1 (none) or 10 to 15 (PNG). It gives 'Left' for the other filters and
-- for the TIFF predictor 2. The other streams of a file are copied as they
-- are, so that the reader does not need to know their filters.
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
    Nothing -> Right Nothing
    Just (Dict d) -> Right (Just d)
    Just (Array [Dict d]) -> Right (Just d)
    Just (Array [Null]) -> Right Nothing
    Just (Array []) -> Right Nothing
    Just _ -> Left "the /DecodeParms of a stream are not supported"
  case filters of
    [] -> if S.length raw <= limit then Right raw else Left "a stream is larger than the limit"
    [f] | f == C.pack "FlateDecode" -> do
      (bytes, more) <- inflateUpTo "FlateDecode" limit (L.fromStrict raw)
      if more then Left "a stream is larger than the limit" else predictor parms bytes
    _ -> Left ("the filters " ++ show (map C.unpack filters) ++ " are not supported")
  where
    filter_name (Name f) = Right f
    filter_name _ = Left "the /Filter of a stream is not a name"

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

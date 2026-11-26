---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The objects of an object stream (PDF 32000-1, 7.5.7)
---------------------------------------------------------
module Graphics.PDF.File.ObjectStream
  ( objectStreamObjects
  , objectStreamNumbers
  ) where

import Control.Monad (forM, unless, when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM
import Graphics.PDF.File.Filter (decodeStream)
import Graphics.PDF.File.Lexer
import Graphics.PDF.File.Object

-- Note [Objects of an object stream]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- An object stream has, after its decoding, N pairs "number offset", then
-- the values of the objects. An offset counts from /First:
--
--   << /Type /ObjStm /N 2 /First 9 >>
--   stream
--   2 0 3 45            <- /First is the offset after this line (9 bytes)
--   << /Type /Pages ... >>
--   << /Type /Page ... >>
--   endstream
--
-- A cross-reference entry gives the object stream and the index of the
-- pair. The reader checks that the pair at this index has the number of
-- the entry, and that the value is not a stream: an object stream cannot
-- have a stream (PDF 32000-1, 7.5.7).
--
-- /N is checked against the size of the data before the pairs are read:
-- a pair has 4 bytes at least ("1 0 ").
--
-- The offsets must increase, and an object is read only from its offset
-- to the offset of the next object. Thus the objects do not overlap, and
-- the reading of all the objects is linear in the size of the data:
-- pairs that all give the offset 0 of a large array would make each
-- object read and keep the array again.

-- | The objects of an object stream that the cross-reference entries give.
-- Gives the objects, and the size of the decoded data.
--
-- See Note [Objects of an object stream]
objectStreamObjects :: Int -- ^ Largest size of the decoded data
                    -> Int -- ^ Depth of the nesting of the values
                    -> Int -- ^ Number of the object stream
                    -> Object -- ^ The object stream
                    -> [(Int, Int)] -- ^ The objects: number, and index in the stream
                    -> Either String (IM.IntMap (Int, Object), Int)
objectStreamObjects limit depth container object wanted = do
  (decoded, table) <- stream_table limit container object
  objects <- forM wanted $ \(number, index) -> do
    (number', start, end) <- maybe (Left ("the object stream " ++ show container ++ " has no index " ++ show index)) Right
                                   (IM.lookup index table)
    when (number' /= fromIntegral number) $
      Left ("the object stream " ++ show container ++ " does not have the object " ++ show number ++ " at its index")
    let slice = S.take end decoded
    (v, after) <- runParser (valueWith depth) slice start
    (is_stream, _) <- runParser (optionalKeyword "stream") slice after
    when is_stream $ Left ("the object stream " ++ show container ++ " has a stream")
    Right (number, (0, Plain v))
  Right (IM.fromList objects, S.length decoded)

-- | The numbers of the objects of an object stream, in the order of their
-- indexes, and the size of the decoded data. A number that is not an
-- object number is not given.
-- See Note [Objects of an object stream]
objectStreamNumbers :: Int -- ^ Largest size of the decoded data
                    -> Int -- ^ Number of the object stream
                    -> Object -- ^ The object stream
                    -> Either String ([(Int, Int)], Int) -- ^ The number and the index of each object
objectStreamNumbers limit container object = do
  (decoded, table) <- stream_table limit container object
  Right ( [ (fromInteger n, i) | (i, (n, _, _)) <- IM.toList table, n >= 1, n < 2 ^ (31 :: Int) ]
        , S.length decoded )

-- | The decoded data of an object stream, and for each index: the number
-- of the object, and its start and its end in the data.
-- See Note [Objects of an object stream]
stream_table :: Int -> Int -> Object -> Either String (S.ByteString, IM.IntMap (Integer, Int, Int))
stream_table limit container object = do
  (dict, raw) <- case object of
    Stream d r | dictLookup (C.pack "Type") d == Just (Name (C.pack "ObjStm")) -> Right (d, r)
    _ -> Left ("the object " ++ show container ++ " is not an object stream")
  decoded <- decodeStream limit dict raw
  n <- case dictLookup (C.pack "N") dict of
    Just (Integer k) | k >= 0 && k <= fromIntegral (S.length decoded `div` 4 + 1) -> Right (fromInteger k :: Int)
    _ -> Left ("the /N of the object stream " ++ show container ++ " is not correct")
  first <- case dictLookup (C.pack "First") dict of
    Just (Integer k) | k >= 0 && k <= fromIntegral (S.length decoded) -> Right (fromInteger k :: Int)
    _ -> Left ("the /First of the object stream " ++ show container ++ " is not correct")
  (pairs, _) <- runParser (forM [1 .. n] (const ((,) <$> integerToken <*> integerToken))) decoded 0
  let size = S.length decoded - first
      offsets = map snd pairs
  unless (all (\o -> o >= 0 && o <= fromIntegral size) offsets && and (zipWith (<) offsets (drop 1 offsets))) $
    Left ("the offsets of the object stream " ++ show container ++ " do not increase")
  -- each object: its number, its start and its end in the decoded data
  let ends = map (\o -> first + fromInteger o) (drop 1 offsets) ++ [S.length decoded]
  Right (decoded, IM.fromList (zip [0 ..] (zip3 (map fst pairs) (map (\o -> first + fromInteger o) offsets) ends)))

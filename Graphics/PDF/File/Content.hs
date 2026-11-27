---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- The operations of a content stream, with their places in the stream,
-- and the removal of the content that a layer hides.
---------------------------------------------------------
module Graphics.PDF.File.Content
  ( Operation(..)
  , contentOperations
  , Hidden(..)
  , removeHiddenContent
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Graphics.PDF.File.Lexer
import Graphics.PDF.File.Object
import Graphics.PDF.File.Write (renderValue)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L

-- Note [Content streams]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- A content stream is a list of operations: operands, then an operator
-- (PDF 32000-1, 7.8.2):
--
--   0 0 m 10 10 l S /OC /L1 BDC BT /F1 12 Tf (Hi) Tj ET EMC
--
-- 'contentOperations' reads the operations of a decoded stream with the
-- lexer of the values. Each operation keeps its place: from its first
-- operand to the end of its operator. The bytes between the operations
-- (white space, comments) are not in an operation. Thus a change cuts the
-- bytes of some operations and keeps all the other bytes.
--
-- An inline image is one operation "BI": from BI to EI. Its data is
-- binary. Its end is given by /L or /Length (PDF 2.0) when the image
-- dictionary has it, or else it is the first EI that has white space
-- before it, and white space, a delimiter or the end of the stream after
-- it (as most readers do).
--
-- A stream that the lexer cannot read (for example a "}" or a value that
-- is nested too deep) gives Left.

-- | An operation of a content stream. See Note [Content streams]
data Operation = Operation
  { operationStart :: !Int -- ^ The offset of the first byte: the first operand, or the operator
  , operationEnd :: !Int -- ^ The offset after the last byte of the operator
  , operationOperator :: !S.ByteString -- ^ The operator. An inline image is one operation, "BI"
  , operationOperands :: [Value] -- ^ The operands, or the entries of an inline image
  } deriving (Eq, Show)

-- | The operations of a decoded content stream. The argument is the depth
-- that the values can have. See Note [Content streams]
contentOperations :: Int -> S.ByteString -> Either String [Operation]
contentOperations depth bytes = fst <$> runParser (go []) bytes 0
  where
    go acc = do
      skipBlanks
      done <- atEnd
      if done then return (reverse acc) else do
        op <- operation
        go (op : acc)
    -- the operands, then the operator
    operation = do
      start <- getPos
      operands start []
    operands start acc = do
      skipBlanks
      mb <- peekByte
      case mb of
        Nothing -> failAt "operands with no operator at the end of the content"
        Just w | isRegular w -> do
          t <- regularToken
          case simple_value t of
            Just v -> operands start (v : acc)
            Nothing
              | t == C.pack "BI" -> inline_image start
              | otherwise -> do
                  end <- getPos
                  return (Operation start end t (reverse acc))
        Just _ -> do
          v <- valueWith depth
          operands start (v : acc)
    -- BI, the entries of the image, ID, the data, EI
    inline_image start = do
      entries <- image_entries []
      data_start <- (+ 1) <$> getPos
      let size = case [ n | (Name k, Integer n) <- pairs entries, k == C.pack "L" || k == C.pack "Length" ] of
            n : _ | n >= 0, n <= toInteger (S.length bytes - data_start) -> Just (fromInteger n)
            _ -> Nothing
      end <- case size of
        Just n -> do
          setPos (data_start + n)
          keyword "EI"
          getPos
        Nothing -> case image_end bytes data_start of
          Just e -> return e
          Nothing -> failAt "an inline image has no EI"
      setPos end
      return (Operation start end (C.pack "BI") entries)
    image_entries acc = do
      skipBlanks
      mb <- peekByte
      case mb of
        Nothing -> failAt "an inline image has no ID"
        Just w | isRegular w -> do
          t <- regularToken
          case simple_value t of
            Just v -> image_entries (v : acc)
            Nothing
              | t == C.pack "ID" -> return (reverse acc)
              | otherwise -> failAt ("unexpected keyword in an inline image: " ++ show (C.unpack (S.take 40 t)))
        Just _ -> do
          v <- valueWith depth
          image_entries (v : acc)
    pairs (k : v : rest) = (k, v) : pairs rest
    pairs _ = []

-- | A number, true, false or null
simple_value :: S.ByteString -> Maybe Value
simple_value t
  | t == C.pack "true" = Just (Bool True)
  | t == C.pack "false" = Just (Bool False)
  | t == C.pack "null" = Just Null
  | otherwise = case numberToken t of
      Just (Left n) -> Just (Integer n)
      Just (Right (m, e)) -> Just (Real m e)
      Nothing -> Nothing

-- | The offset after the EI of an inline image whose data starts at this
-- offset. See Note [Content streams]
image_end :: S.ByteString -> Int -> Maybe Int
image_end bytes = search
  where
    search from = case S.breakSubstring (C.pack "EI") (S.drop from bytes) of
      (before, rest)
        | S.null rest -> Nothing
        | otherwise ->
            let at = from + S.length before
                ok_before = at > 0 && isBlank (S.index bytes (at - 1))
                ok_after = at + 2 >= S.length bytes || not (isRegular (S.index bytes (at + 2)))
            in if ok_before && ok_after then Just (at + 2) else search (at + 1)

-- | What a removal hides: the marked-content sequences (from the operands
-- of BDC) and the XObjects (from the name of Do).
data Hidden = Hidden
  { hiddenSequence :: [Value] -> Bool
  , hiddenXObject :: S.ByteString -> Bool
  }


-- Note [Removal of hidden content]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'removeHiddenContent' removes the marked-content sequences that a layer
-- hides, and the Do of the XObjects that it hides. Hidden content still
-- changes the graphics state (PDF 32000-1, 8.11.3.2), thus a sequence is
-- not removed as a whole. In a sequence:
--
-- * these operations are removed: the paths that are painted, the text
--   objects (BT ... ET), Do, sh, the inline images, MP, DP, the marked
--   content in the sequence, and the q ... Q blocks that end in the
--   sequence (they change nothing after their Q);
-- * these operations stay: the operations of the graphics state and of
--   the colours, the text state of the removed text objects (Tc Tw Tz TL
--   Tf Tr Ts, and the Tw and Tc of "), a clipping path (its painting
--   operator becomes n), a q whose Q is after the sequence, and a Q of a
--   q that is before the sequence.
--
--   q 1 0 0 rg /OC /L1 BDC 0 1 0 rg 0 0 10 10 re f q 2 w Q EMC 5 5 m 9 9 l S Q
--   q 1 0 0 rg
--   0 1 0 rg 5 5 m 9 9 l S Q
--
-- A q that is open at the end of the sequence keeps its operations of the
-- graphics state, which apply until its Q. Illustrator writes such
-- sequences:
--
--   /OC /MC0 BDC q 0 0 10 10 re W n /GS0 gs q ... f Q EMC Q
--   q
--   0 0 10 10 re
--   W
--   n
--   /GS0 gs Q
--
-- A path that starts before the sequence and is painted in it ends with n:
--
--   0 0 m /OC /L1 BDC 10 10 l S EMC      0 0 m n
--
-- These sequences give Left:
--
-- * a sequence inside a text object that shows text: its text moves the
--   text that comes after it, and the font metrics are necessary to keep
--   this;
-- * a text object that clips (a text rendering mode from 4 to 7, Tr):
--   its clipping path stays after ET, and only its glyphs give it;
-- * a sequence with no EMC, a sequence that has an ET, or a BT with no
--   end;
-- * a path that starts in the sequence and is not painted in it.

-- | The operators that paint a path
painting :: [S.ByteString]
painting = map C.pack ["S", "s", "f", "F", "f*", "B", "B*", "b", "b*", "n"]

path_construction :: [S.ByteString]
path_construction = map C.pack ["m", "l", "c", "v", "y", "h", "re"]

text_showing :: [S.ByteString]
text_showing = map C.pack ["Tj", "TJ", "'", "\""]

text_positioning :: [S.ByteString]
text_positioning = map C.pack ["Td", "TD", "Tm", "T*"]

-- | The operations that are removed in a sequence, outside of a path and
-- a text object. See Note [Removal of hidden content]
removed_in_sequence :: [S.ByteString]
removed_in_sequence = map C.pack ["Do", "sh", "BI", "MP", "DP"]

-- | The state of the content around the hidden sequences
data Outside = Outside
  { out_cursor :: !Int -- ^ The offset after the last byte that is in the output or removed
  , out_chunks :: [S.ByteString] -- ^ The output, last chunk first
  , out_text :: !Bool -- ^ A text object is open
  , out_path :: !Bool -- ^ A path is open: its painting operator is not read yet
  , out_modes :: [Integer] -- ^ The text rendering mode (Tr), with one value for each q that is open
  , out_changed :: !Bool
  }

-- | A hidden sequence that is being removed
data Sequence = Sequence
  { seq_depth :: !Int -- ^ The marked-content sequences that are open in it, with itself
  , seq_blocks :: [[S.ByteString]] -- ^ The q ... Q blocks that are open in it, the last one first, with the bytes that stay in each (last chunk first)
  , seq_text :: !Bool -- ^ A text object (BT) is open in it
  , seq_in_text :: !Bool -- ^ It starts inside a text object
  , seq_path :: [Operation] -- ^ The path that is being built, last operation first
  , seq_clip :: !Bool -- ^ The path is a clipping path (W or W*)
  , seq_outer_path :: !Bool -- ^ A path that starts before the sequence is open
  }

-- | The content without the hidden content, or Nothing when nothing is
-- hidden. See Note [Removal of hidden content]
removeHiddenContent :: Hidden -> S.ByteString -> [Operation] -> Either String (Maybe S.ByteString)
removeHiddenContent hidden bytes ops = go ops (Outside 0 [] False False [0] False) Nothing
  where
    slice a b = S.take (b - a) (S.drop a bytes)
    op_bytes o = slice (operationStart o) (operationEnd o)
    keep o = [C.pack "\n", op_bytes o]

    go [] st Nothing
      | out_changed st = Right (Just (S.concat (reverse (S.drop (out_cursor st) bytes : out_chunks st))))
      | otherwise = Right Nothing
    go [] _ (Just _) = Left "a hidden marked-content sequence has no EMC"
    go (o : rest) st Nothing
      | op == C.pack "BDC" && hiddenSequence hidden (operationOperands o) =
          go rest (cut o st) (Just (Sequence 1 [] False (out_text st) [] False (out_path st)))
      | op == C.pack "Do", [Name n] <- operationOperands o, hiddenXObject hidden n = go rest (cut o st) Nothing
      | otherwise = go rest (track o st) Nothing
      where op = operationOperator o
    go (o : rest) st (Just s) = do
      (kept, s') <- in_sequence o (track o st) s
      let st' = (track o st) { out_cursor = operationEnd o, out_chunks = reverse kept ++ out_chunks st }
          -- after the sequence: the path that is still open
          st'' = case s' of
            Nothing -> st' {out_path = seq_outer_path s}
            Just _ -> st'
      go rest st'' s'

    -- remove an operation: the bytes before it go to the output
    cut o st = st { out_cursor = operationEnd o
                  , out_chunks = slice (out_cursor st) (operationStart o) : out_chunks st
                  , out_changed = True }

    -- the state that the operations change, in and out of a sequence
    track o st
      | op == C.pack "BT" = st {out_text = True}
      | op == C.pack "ET" = st {out_text = False}
      | op `elem` path_construction || op == C.pack "W" || op == C.pack "W*" = st {out_path = True}
      | op `elem` painting = st {out_path = False}
      | op == C.pack "q" = st {out_modes = take 1 (out_modes st) ++ out_modes st}
      | op == C.pack "Q" = st {out_modes = case out_modes st of
                                 _ : m : ms -> m : ms
                                 ms -> ms}
      | op == C.pack "Tr", [Integer m] <- operationOperands o = st {out_modes = m : drop 1 (out_modes st)}
      | otherwise = st
      where op = operationOperator o

    -- the bytes that stay: in the q ... Q block that is open, or else in
    -- the output
    stay chunks s = case seq_blocks s of
      block : others -> Right ([], Just s {seq_blocks = (reverse chunks ++ block) : others})
      [] -> Right (chunks, Just s)

    -- an operation of a hidden sequence: the bytes that stay in the
    -- output, and the new state of the sequence (Nothing: the sequence
    -- ends). The state of the content is the one after the operation.
    in_sequence o st s
      | marked_start = Right ([], Just s {seq_depth = seq_depth s + 1})
      | op == C.pack "EMC" =
          if seq_depth s > 1 then Right ([], Just s {seq_depth = seq_depth s - 1})
          else if seq_text s then Left "a BT of a hidden marked-content sequence has no ET in it"
          else if not (null (seq_path s)) then Left "a path of a hidden marked-content sequence is not painted"
          -- the q ... Q blocks that are still open stay: their Q is after
          -- the sequence
          else Right (concatMap reverse (reverse (seq_blocks s)), Nothing)
      | op `elem` path_construction = Right ([], Just s {seq_path = o : seq_path s})
      | op == C.pack "W" || op == C.pack "W*" = Right ([], Just s {seq_path = o : seq_path s, seq_clip = True})
      | op `elem` painting =
          let s' = s {seq_path = [], seq_clip = False, seq_outer_path = False}
          in if seq_clip s
               then stay (concatMap keep (reverse (seq_path s)) ++ [C.pack "\nn"]) s'
               -- a path that starts before the sequence ends here
               else if seq_outer_path s then stay [C.pack "\nn"] s'
               else Right ([], Just s')
      | op == C.pack "q" = Right ([], Just s {seq_blocks = reverse (keep o) : seq_blocks s})
      | op == C.pack "Q" = case seq_blocks s of
          -- a block that ends in the sequence: it changes nothing
          _ : others -> Right ([], Just s {seq_blocks = others})
          -- the Q of a q that is before the sequence
          [] -> Right (keep o, Just s)
      | op == C.pack "BT" =
          if seq_in_text s then Left "a hidden marked-content sequence has a BT in a text object"
          else Right ([], Just s {seq_text = True})
      | op == C.pack "ET" =
          if seq_text s then Right ([], Just s {seq_text = False})
          else Left "a hidden marked-content sequence has the end of its text object (ET) in it"
      | op `elem` text_showing =
          if seq_in_text s
            then Left "a hidden marked-content sequence in a text object shows text: its removal would move the text after it"
            else if take 1 (out_modes st) >= [4]
              then Left "a hidden text object clips (a text rendering mode from 4 to 7): its glyphs give the clipping path"
              -- " sets the word spacing and the character spacing
              else case (op == C.pack "\"", operationOperands o) of
                (True, [aw, ac, _]) -> stay [C.pack "\n", value aw, C.pack " Tw ", value ac, C.pack " Tc"] s
                _ -> Right ([], Just s)
      | op `elem` text_positioning = if seq_in_text s then stay (keep o) s else Right ([], Just s)
      | op `elem` removed_in_sequence = Right ([], Just s)
      -- the graphics state, the colours, the text state, and the other
      -- operations: they stay
      | otherwise = stay (keep o) s
      where
        op = operationOperator o
        marked_start = op == C.pack "BDC" || op == C.pack "BMC"
        value = L.toStrict . B.toLazyByteString . renderValue

---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The tokens and the values of a PDF file
---------------------------------------------------------
module Graphics.PDF.File.Lexer
  ( Parser
  , runParser
  , failAt
  , getPos
  , setPos
  , peekByte
  , atEnd
  , skipBlanks
  , regularToken
  , keyword
  , optionalKeyword
  , integerToken
  , valueWith
  , parseValue
  , parseValueWith
  , isBlank
  , isRegular
  , numberToken
  ) where

import Control.Monad (when)
import Data.Bits ((.&.))
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as SU
import Data.Char (digitToInt, isDigit)
import qualified Data.Map.Strict as M
import Data.Word (Word8)
import Graphics.PDF.File.Object

-- Note [Lexer of a PDF file]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The parser works on the bytes of the whole file and an offset. It is
-- total: a malformed file gives 'Left' with the offset, and never an
-- exception. Each read of a byte checks the offset first.
--
-- The nesting of arrays and dictionaries has a limit (the depth), so that
-- a file with many "[" cannot use all the stack. A number has 255
-- characters at most.
--
-- The rules of PDF 32000-1, 7.2 and 7.3:
--
--   blanks       NUL, TAB, LF, FF, CR, SPACE, and comments ("%" to the
--                end of the line)
--   delimiters   ( ) < > [ ] { } / %
--   regular      all the other bytes; a keyword or a number is a run of
--                regular bytes
--
-- See Note [Names and strings of the reader] for the decoding of names
-- and strings.

-- | A parser of the bytes of a file, from an offset
newtype Parser a = Parser { runParser :: S.ByteString -> Int -> Either String (a, Int) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s i -> case p s i of
    Left e -> Left e
    Right (a, j) -> Right (f a, j)

instance Applicative Parser where
  pure a = Parser $ \_ i -> Right (a, i)
  Parser pf <*> Parser pa = Parser $ \s i -> case pf s i of
    Left e -> Left e
    Right (f, j) -> case pa s j of
      Left e -> Left e
      Right (a, k) -> Right (f a, k)

instance Monad Parser where
  Parser p >>= k = Parser $ \s i -> case p s i of
    Left e -> Left e
    Right (a, j) -> runParser (k a) s j

-- | Stop with an error, at the offset of the parser
failAt :: String -> Parser a
failAt msg = Parser $ \_ i -> Left (msg ++ " (offset " ++ show i ++ ")")

getPos :: Parser Int
getPos = Parser $ \_ i -> Right (i, i)

setPos :: Int -> Parser ()
setPos j = Parser $ \_ _ -> Right ((), j)

-- | The byte at the offset, if there is one
peekByte :: Parser (Maybe Word8)
peekByte = Parser $ \s i -> Right (byte_at s i, i)

byte_at :: S.ByteString -> Int -> Maybe Word8
byte_at s i = if i >= 0 && i < S.length s then Just (SU.unsafeIndex s i) else Nothing

atEnd :: Parser Bool
atEnd = Parser $ \s i -> Right (i >= S.length s, i)

advance :: Int -> Parser ()
advance n = Parser $ \_ i -> Right ((), i + n)

isBlank :: Word8 -> Bool
isBlank w = w == 0x20 || w == 0x0A || w == 0x0D || w == 0x09 || w == 0x0C || w == 0x00

is_delimiter :: Word8 -> Bool
is_delimiter w = w `S.elem` delimiters
  where delimiters = C.pack "()<>[]{}/%"

isRegular :: Word8 -> Bool
isRegular w = not (isBlank w || is_delimiter w)

is_eol :: Word8 -> Bool
is_eol w = w == 0x0A || w == 0x0D

-- | Skip the blanks and the comments
skipBlanks :: Parser ()
skipBlanks = Parser $ \s i -> Right ((), go s i)
  where
    go s i = case byte_at s i of
      Just w | isBlank w -> go s (i + 1)
             | w == 0x25 -> go s (comment_end s (i + 1))
      _ -> i
    comment_end s i = case byte_at s i of
      Just w | not (is_eol w) -> comment_end s (i + 1)
      _ -> i

-- | The run of regular bytes at the offset (it can be empty)
regularToken :: Parser S.ByteString
regularToken = Parser $ \s i ->
  let t = S.takeWhile isRegular (S.drop i s) in Right (t, i + S.length t)

-- | Skip the blanks, then read this keyword
keyword :: String -> Parser ()
keyword k = do
  found <- optionalKeyword k
  if found then return () else failAt ("expected " ++ show k)

-- | Skip the blanks, then read this keyword if it is there
optionalKeyword :: String -> Parser Bool
optionalKeyword k = do
  skipBlanks
  start <- getPos
  t <- regularToken
  if t == C.pack k then return True else setPos start >> return False

-- | Skip the blanks, then read an integer
integerToken :: Parser Integer
integerToken = do
  skipBlanks
  t <- regularToken
  case numberToken t of
    Just (Left n) -> return n
    _ -> failAt "expected an integer"

-- | A number: an integer, or a real (mantissa, number of decimals).
-- See Note [Real numbers of a PDF file] in Graphics.PDF.File.Object
numberToken :: S.ByteString -> Maybe (Either Integer (Integer, Int))
numberToken t
  | S.null t || S.length t > 255 = Nothing
  | otherwise =
      let (negative, body) = case C.uncons t of
            Just ('-', r) -> (True, r)
            Just ('+', r) -> (False, r)
            _ -> (False, t)
          (whole, rest) = C.span isDigit body
          signed n = if negative then negate n else n
      in case C.uncons rest of
           Nothing | not (S.null whole) -> Just (Left (signed (digits whole)))
           Just ('.', fraction)
             | C.all isDigit fraction && not (S.null whole && S.null fraction) ->
                 let fraction' = if S.null fraction then C.pack "0" else fraction
                 in Just (Right (signed (digits (whole <> fraction')), S.length fraction'))
           _ -> Nothing
  where
    digits = C.foldl' (\a c -> a * 10 + fromIntegral (digitToInt c)) 0

-- | Parse one value. The argument is the depth that the nesting can
-- have. See Note [Lexer of a PDF file]
valueWith :: Int -> Parser Value
valueWith depth = do
  when (depth <= 0) (failAt "the value is nested too deep")
  skipBlanks
  mb <- peekByte
  case mb of
    Nothing -> failAt "a value is missing at the end of the data"
    Just 0x2F -> advance 1 >> (Name <$> name_body)
    Just 0x28 -> advance 1 >> (String <$> literal_string)
    Just 0x3C -> do
      start <- getPos
      second <- Parser $ \s i -> Right (byte_at s (i + 1), i)
      if second == Just 0x3C
        then setPos (start + 2) >> (Dict <$> dictionary (depth - 1))
        else advance 1 >> (String <$> hex_string)
    Just 0x5B -> advance 1 >> (Array <$> array (depth - 1))
    Just w | isRegular w -> number_or_keyword
    Just _ -> failAt "unexpected character"

number_or_keyword :: Parser Value
number_or_keyword = do
  t <- regularToken
  if t == C.pack "true" then return (Bool True)
  else if t == C.pack "false" then return (Bool False)
  else if t == C.pack "null" then return Null
  else case numberToken t of
    Just (Right (m, e)) -> return (Real m e)
    Just (Left n) -> reference_or t n
    Nothing -> failAt ("unexpected keyword " ++ show (C.unpack (S.take 40 t)))

-- | "n g R" is a reference; an integer alone is an integer. The two
-- numbers of a reference have digits only, with no sign.
reference_or :: S.ByteString -> Integer -> Parser Value
reference_or first n = do
  after_first <- getPos
  skipBlanks
  second <- regularToken
  case numberToken second of
    Just (Left g) | C.all isDigit second && C.all isDigit first -> do
      found <- optionalKeyword "R"
      if not found
        then setPos after_first >> return (Integer n)
        else if n < 2 ^ (31 :: Int) && g <= 65535
          then return (Ref (fromInteger n) (fromInteger g))
          else failAt "a reference is out of range"
    _ -> setPos after_first >> return (Integer n)

dictionary :: Int -> Parser Dict
dictionary depth = go M.empty
  where
    go d = do
      skipBlanks
      mb <- peekByte
      case mb of
        Just 0x3E -> do
          start <- getPos
          second <- Parser $ \s i -> Right (byte_at s (i + 1), i)
          if second == Just 0x3E then setPos (start + 2) >> return d
            else failAt "a dictionary is not closed with >>"
        Just 0x2F -> do
          advance 1
          key <- name_body
          v <- valueWith depth
          -- the first value of a key is kept
          go (M.insertWith (\_ old -> old) key v d)
        Nothing -> failAt "a dictionary is not closed"
        Just _ -> failAt "a key of a dictionary is not a name"

array :: Int -> Parser [Value]
array depth = go []
  where
    go acc = do
      skipBlanks
      mb <- peekByte
      case mb of
        Just 0x5D -> advance 1 >> return (reverse acc)
        Nothing -> failAt "an array is not closed"
        Just _ -> valueWith depth >>= \v -> go (v : acc)

-- Note [Names and strings of the reader]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A name or a string keeps its bytes, with the escapes decoded:
--
--   /A#20B        name "A B"
--   /A#2          name "A#2": a "#" without two hexadecimal digits stays
--                 (PDF 1.1 had no escapes in names)
--   (a\(b\)\101)  string "a(b)A"
--   (a\           the backslash at the end of a line continues the
--   b)            string: "ab"
--   (a<CR><LF>b)  an end of line in a string is LF: "a\nb"
--   <41 4 >       string "A@": the blanks are ignored, and a missing last
--                 digit is 0
--
-- A literal string and a hexadecimal string are the same value: the
-- writer chooses the form again (Graphics.PDF.File.Write).

-- Note [Linear decoding of names and strings]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A name or a string of a file can be large (a string of 50 MB). Thus it
-- is decoded in two passes, with no list of bytes:
--
-- 1. find the end of the name or the string, and check its bytes;
-- 2. write the decoded bytes in one buffer ('S.unfoldrN'). The decoded
--    value is never longer than the bytes of the file, thus the size of
--    this buffer is the length of these bytes.
--
-- A name with no "#" and a string with no escape are slices of the file,
-- with no copy.

-- | The bytes of a name, after the "/".
-- See Note [Linear decoding of names and strings]
name_body :: Parser S.ByteString
name_body = do
  t <- regularToken
  return (decode_name t)
  where
    decode_name t
      | not (C.elem '#' t) = t
      | otherwise = fst (S.unfoldrN (S.length t) (step t) 0)
    step t k = case byte_at t k of
      Nothing -> Nothing
      Just 0x23 | Just a <- hex_value =<< byte_at t (k + 1)
                , Just b <- hex_value =<< byte_at t (k + 2) -> Just (fromIntegral (a * 16 + b), k + 3)
      Just w -> Just (w, k + 1)

hex_value :: Word8 -> Maybe Int
hex_value w
  | w >= 0x30 && w <= 0x39 = Just (fromIntegral w - 0x30)
  | w >= 0x41 && w <= 0x46 = Just (fromIntegral w - 0x37)
  | w >= 0x61 && w <= 0x66 = Just (fromIntegral w - 0x57)
  | otherwise = Nothing

-- | The bytes of a literal string, after the "(".
-- See Note [Linear decoding of names and strings]
literal_string :: Parser S.ByteString
literal_string = Parser $ \s i -> case string_end s i of
  Nothing -> Left ("a string is not closed (offset " ++ show i ++ ")")
  Just e ->
    let body = S.take (e - i) (S.drop i s)
        decoded = if S.any (\w -> w == 0x5C || w == 0x0D) body
                    then fst (S.unfoldrN (S.length body) (decode_step body) 0)
                    else body
    in Right (decoded, e + 1)

-- | The offset of the ")" that closes a literal string, from the offset
-- after its "("
string_end :: S.ByteString -> Int -> Maybe Int
string_end s = go (0 :: Int)
  where
    go open i = case byte_at s i of
      Nothing -> Nothing
      Just 0x29 | open == 0 -> Just i
                | otherwise -> go (open - 1) (i + 1)
      Just 0x28 -> go (open + 1) (i + 1)
      -- the escaped byte cannot open or close
      Just 0x5C -> go open (i + 2)
      Just _ -> go open (i + 1)

-- | One decoded byte of the body of a literal string, from an offset
decode_step :: S.ByteString -> Int -> Maybe (Word8, Int)
decode_step body k = case byte_at body k of
  Nothing -> Nothing
  -- an end of line is LF
  Just 0x0D -> Just (0x0A, if byte_at body (k + 1) == Just 0x0A then k + 2 else k + 1)
  Just 0x5C -> case byte_at body (k + 1) of
    Nothing -> Nothing
    Just e
      | e == 0x6E -> Just (0x0A, k + 2)
      | e == 0x72 -> Just (0x0D, k + 2)
      | e == 0x74 -> Just (0x09, k + 2)
      | e == 0x62 -> Just (0x08, k + 2)
      | e == 0x66 -> Just (0x0C, k + 2)
      -- a backslash at the end of a line: the line continues
      | e == 0x0D -> decode_step body (if byte_at body (k + 2) == Just 0x0A then k + 3 else k + 2)
      | e == 0x0A -> decode_step body (k + 2)
      | e >= 0x30 && e <= 0x37 ->
          let octal = S.take 3 (S.takeWhile (\w -> w >= 0x30 && w <= 0x37) (S.drop (k + 1) body))
              v = S.foldl' (\a w -> a * 8 + fromIntegral w - 0x30) (0 :: Int) octal
          in Just (fromIntegral (v .&. 0xFF), k + 1 + S.length octal)
      -- another escaped byte is the byte itself: \( \) \\ and the others
      | otherwise -> Just (e, k + 2)
  Just w -> Just (w, k + 1)

-- | The bytes of a hexadecimal string, after the "<".
-- See Note [Linear decoding of names and strings]
hex_string :: Parser S.ByteString
hex_string = Parser $ \s i ->
  let body = S.takeWhile (/= 0x3E) (S.drop i s)
      e = i + S.length body
      digits = S.filter (not . isBlank) body
  in if byte_at s e /= Just 0x3E
       then Left ("a hexadecimal string is not closed (offset " ++ show i ++ ")")
     else if not (S.all (\w -> hex_value w /= Nothing) digits)
       then Left ("a hexadecimal string has a byte that is not a digit (offset " ++ show i ++ ")")
     else
       -- a missing last digit is 0
       let digit k = maybe 0 id (hex_value =<< byte_at digits k)
           pair k = if k < S.length digits then Just (fromIntegral (digit k * 16 + digit (k + 1)), k + 2) else Nothing
       in Right (fst (S.unfoldrN ((S.length digits + 1) `div` 2) pair 0), e + 1)

-- | Parse one value, with blanks and comments around it, and nothing else.
-- The depth of the nesting is 256 at most.
parseValue :: S.ByteString -> Either String Value
parseValue = parseValueWith 256

-- | Like 'parseValue', with a limit of the depth of the nesting
parseValueWith :: Int -> S.ByteString -> Either String Value
parseValueWith depth bytes = do
  (v, end) <- runParser (valueWith depth <* skipBlanks) bytes 0
  if end == S.length bytes then Right v
    else Left ("data after the value (offset " ++ show end ++ ")")

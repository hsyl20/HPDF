---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Text strings of a PDF file (PDF 32000-1, 7.9.2.2): the names of the
-- layers, the passwords of the standard security handler.
---------------------------------------------------------
module Graphics.PDF.File.Text
  ( decodeTextString
  , encodeTextString
  , pdfDocEncoding
  ) where

import qualified Data.ByteString as S
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import Data.Word (Word8)

-- Note [Text strings]
-- ~~~~~~~~~~~~~~~~~~~
-- A text string is UTF-16BE with the byte order mark FE FF, UTF-8 with
-- the mark EF BB BF (PDF 2.0), or else PDFDocEncoding: ISO Latin 1 with
-- other characters for the codes 18 to 1F and 80 to A0 (PDF 32000-1,
-- Annex D).
--
-- 'decodeTextString' replaces the bytes that are not correct with U+FFFD.
-- 'encodeTextString' writes printable ASCII as it is, and other texts as
-- UTF-16BE, as the builder does ('Graphics.PDF.Archive.pdfTextString').

-- | The text of a text string. See Note [Text strings]
decodeTextString :: S.ByteString -> T.Text
decodeTextString s
  | S.pack [0xFE, 0xFF] `S.isPrefixOf` s = TE.decodeUtf16BEWith TE.lenientDecode (even_length (S.drop 2 s))
  | S.pack [0xEF, 0xBB, 0xBF] `S.isPrefixOf` s = TE.decodeUtf8With TE.lenientDecode (S.drop 3 s)
  | otherwise = T.pack (map pdf_doc_char (S.unpack s))
  where
    even_length b = S.take (S.length b - S.length b `mod` 2) b

-- | The bytes of a text string. See Note [Text strings]
encodeTextString :: T.Text -> S.ByteString
encodeTextString t
  | T.all (\c -> c >= ' ' && c <= '~') t = TE.encodeUtf8 t
  | otherwise = S.pack [0xFE, 0xFF] <> TE.encodeUtf16BE t

-- | The bytes of a text in PDFDocEncoding (PDF 32000-1, Annex D), or
-- Nothing for a character that PDFDocEncoding does not have.
pdfDocEncoding :: T.Text -> Maybe S.ByteString
pdfDocEncoding = fmap S.pack . traverse code . T.unpack
  where
    code c
      | Just w <- lookup c pdf_doc_specials = Just w
      | c < '\x18' || (c >= ' ' && c <= '~') || (c >= '\xA1' && c <= '\xFF' && c /= '\xAD') = Just (fromIntegral (fromEnum c))
      | otherwise = Nothing

-- | The character of a code of PDFDocEncoding. A code that has no
-- character gives U+FFFD.
pdf_doc_char :: Word8 -> Char
pdf_doc_char w = case M.lookup w specials of
  Just c -> c
  Nothing
    | w < 0x18 || (w >= 0x20 && w <= 0x7E) || (w >= 0xA1 && w /= 0xAD) -> toEnum (fromIntegral w)
    | otherwise -> '\xFFFD'
  where
    specials = M.fromList [ (code, c) | (c, code) <- pdf_doc_specials ]

-- | The codes of PDFDocEncoding that are not the ones of ISO Latin 1
pdf_doc_specials :: [(Char, Word8)]
pdf_doc_specials =
     zip "\x02D8\x02C7\x02C6\x02D9\x02DD\x02DB\x02DA\x02DC" [0x18 .. 0x1F]
  ++ zip ("\x2022\x2020\x2021\x2026\x2014\x2013\x0192\x2044\x2039\x203A\x2212\x2030\x201E\x201C\x201D\x2018"
          ++ "\x2019\x201A\x2122\xFB01\xFB02\x0141\x0152\x0160\x0178\x017D\x0131\x0142\x0153\x0161\x017E")
         [0x80 .. 0x9E]
  ++ [('\x20AC', 0xA0)]

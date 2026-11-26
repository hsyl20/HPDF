{-# LANGUAGE OverloadedStrings #-}
-- | Encrypted PDF files: the reader decrypts them.
-- See Note [Standard security handler] in Graphics.PDF.File.Crypt
module Tests.Encryption (tests) where

import Test.Tasty (TestTree, localOption, mkTimeout, testGroup)
import Tests.Common
import qualified Graphics.PDF.File as F
import Control.Exception (evaluate)
import Control.Monad (forM_, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.List (isInfixOf, sort)

tests :: TestTree
tests = testGroup "Encryption"
  [ test "qpdfFiles" test_qpdfFiles
  , test "emptyUserPassword" test_emptyUserPassword
  , test "clearMetadata" test_clearMetadata
  , test "metadataBeforeV4" test_metadataBeforeV4
  , test "pdfDocEncodingPasswords" test_pdfDocEncodingPasswords
  , test "permissionsOfTheFile" test_permissionsOfTheFile
  , test "encryptionIsNotTheCatalog" test_encryptionIsNotTheCatalog
  , test "keyLengthOfTheCryptFilter" test_keyLengthOfTheCryptFilter
  , test "emptyAesString" test_emptyAesString
  , test "badEncryption" test_badEncryption
  , localOption (mkTimeout 120000000) (test "malformedEncryptedFiles" test_malformedEncryptedFiles)
  ]

-- | The files of Test/encrypted, with the user password "user" and the
-- owner password "owner". See Test/encrypted/README
qpdf_files :: [FilePath]
qpdf_files = ["r2-rc4-40", "r3-rc4-128", "r4-rc4-128", "r4-aes-128", "r5-aes-256", "r6-aes-256"]

fixture :: FilePath -> IO BS.ByteString
fixture name = BS.readFile ("Test/encrypted/" ++ name ++ ".pdf")

with_password :: String -> BS.ByteString -> Either String F.PdfFile
with_password p = F.parsePdfFileWithPassword F.defaultLimits (T.pack p)

-- | The bytes of an ASCII text in hexadecimal, upper case
hex :: String -> String
hex = concatMap (\c -> [digits !! (fromEnum c `div` 16), digits !! (fromEnum c `mod` 16)])
  where digits = "0123456789ABCDEF"

-- | The decoded data of all the streams of a file, sorted
decoded_streams :: F.PdfFile -> Either String [BS.ByteString]
decoded_streams f = sort <$> sequence [ F.decodeStream maxBound d raw | (_, F.Stream d raw) <- IM.elems (F.pdfObjects f) ]

-- | The file with one change: the text must be in the file one time
replace_in :: BS.ByteString -> BS.ByteString -> BS.ByteString -> IO BS.ByteString
replace_in old new bytes = case occurrences old bytes of
  [i] -> return (BS.concat [BS.take i bytes, new, BS.drop (i + BS.length old) bytes])
  is -> assertFailure ("the text is in the file " ++ show (length is) ++ " times: " ++ show old)

-- | The file with an update (PDF 32000-1, 7.5.6): new objects, and keys
-- of the trailer. The other keys come from the trailer of the file.
with_update :: BS.ByteString -> [(Int, BS.ByteString)] -> BS.ByteString -> BS.ByteString
with_update bytes objects keys =
  let start = C.takeWhile (/= '\n') (C.dropWhile (== '\n') (BS.drop 9 (snd (BS.breakSubstring "startxref" (BS.drop (BS.length bytes - 64) bytes)))))
      bodies = [ C.pack (show n ++ " 0 obj\n") <> o <> "\nendobj\n" | (n, o) <- objects ]
      offsets = scanl (+) (BS.length bytes + 1) (map BS.length bodies)
      xref = BS.concat [ C.pack (show n ++ " 1\n" ++ pad10 off ++ " 00000 n \n") | ((n, _), off) <- zip objects offsets ]
      pad10 x = replicate (10 - length (show x)) '0' ++ show x
  in BS.concat ([bytes, "\n"] ++ bodies ++ ["xref\n", xref, "trailer\n<< /Size 100 /Prev ", start, " ", keys, " >>\nstartxref\n"
                                           , C.pack (show (last offsets)), "\n%%EOF\n"])

-- | The dictionary that starts at the first occurrence of a text: its
-- bytes up to its ">>", with the dictionaries in it
dict_at :: BS.ByteString -> BS.ByteString -> BS.ByteString
dict_at start bytes = go 0 0 (snd (BS.breakSubstring start bytes))
  where
    go :: Int -> Int -> BS.ByteString -> BS.ByteString
    go depth i s
      | i >= BS.length s = s
      | "<<" `BS.isPrefixOf` BS.drop i s = go (depth + 1) (i + 2) s
      | ">>" `BS.isPrefixOf` BS.drop i s = if depth == 1 then BS.take (i + 2) s else go (depth - 1) (i + 2) s
      | otherwise = go depth (i + 1) s

-- | The keys of a dictionary, without the "<<" and the ">>"
inside :: BS.ByteString -> BS.ByteString
inside d = BS.take (BS.length d - 4) (BS.drop 2 d)

left_with :: String -> Either String a -> IO ()
left_with words_of_error r = case r of
  Left e -> assertBoolVerbose e (words_of_error `isInfixOf` e)
  Right _ -> assertFailure ("no error, but " ++ show words_of_error ++ " was expected")

-- qpdf (an independent implementation) encrypted the same document with
-- each revision: each file gives the streams of the document with the
-- user password and with the owner password, and only with them.
test_qpdfFiles :: IO ()
test_qpdfFiles = do
  Right source <- F.parsePdfFile <$> fixture "source"
  let Right expected = decoded_streams source
  -- HPDF writes the text in hexadecimal
  assertBool (any (C.pack ("<" ++ hex "Encrypted text" ++ "> Tj") `BS.isInfixOf`) expected)
  forM_ qpdf_files $ \name -> do
    bytes <- fixture name
    forM_ ["user", "owner"] $ \password -> case with_password password bytes of
      Left e -> assertFailure (name ++ ", " ++ password ++ ": " ++ e)
      Right f -> do
        assertEqual (Right expected) (decoded_streams f)
        -- the /Length of a stream is the length of its decrypted data
        assertBool (and [ F.dictLookup "Length" d == Just (F.Integer (fromIntegral (BS.length raw)))
                        | (_, F.Stream d raw) <- IM.elems (F.pdfObjects f) ])
        assertEqual Nothing (M.lookup "Encrypt" (F.pdfTrailer f))
        -- the trailer is not encrypted
        assertEqual (M.lookup "Info" (F.pdfTrailer source)) (M.lookup "Info" (F.pdfTrailer f))
        -- the file is written again without encryption
        assertEqual (Right expected) (F.parsePdfFile (BL.toStrict (F.renderPdfFile f)) >>= decoded_streams)
    left_with "password is not correct" (with_password "users" bytes)
    left_with "(no password was given)" (F.parsePdfFile bytes)

-- Most encrypted files have an empty user password: 'F.parsePdfFile'
-- reads them.
test_emptyUserPassword :: IO ()
test_emptyUserPassword = do
  Right source <- F.parsePdfFile <$> fixture "source"
  bytes <- fixture "r3-rc4-128-empty-user"
  assertEqual (decoded_streams source) (either Left decoded_streams (F.parsePdfFile bytes))
  assertEqual (decoded_streams source) (either Left decoded_streams (with_password "owner" bytes))
  left_with "password is not correct" (with_password "user" bytes)

-- /EncryptMetadata false: the metadata stream is not encrypted, and for
-- /R 4 the key of the file has 4 more bytes in its MD5.
-- See Note [Objects that are not encrypted] in Graphics.PDF.File.Crypt
test_clearMetadata :: IO ()
test_clearMetadata = do
  Right source <- F.parsePdfFile <$> fixture "source-metadata"
  let Right expected = decoded_streams source
  assertBool (any ("Metadata in clear text" `BS.isInfixOf`) expected)
  forM_ ["r4-aes-128-clear-metadata", "r6-aes-256-clear-metadata"] $ \name -> do
    bytes <- fixture name
    -- the metadata is in the bytes of the file, and the text is not
    assertBool ("Metadata in clear text" `BS.isInfixOf` bytes)
    forM_ ["user", "owner"] $ \password ->
      assertEqual (Right expected) (with_password password bytes >>= decoded_streams)
    -- with /EncryptMetadata true, the key of /R 4 is not the same
    when (name == "r4-aes-128-clear-metadata") $
      replace_in "/EncryptMetadata false" "/EncryptMetadata true " bytes >>= left_with "password is not correct" . with_password "user"

-- /EncryptMetadata is a key of /V 4 and 5 only: with /V 2, the metadata
-- stream is encrypted, also with /EncryptMetadata false.
-- See Note [Objects that are not encrypted] in Graphics.PDF.File.Crypt
test_metadataBeforeV4 :: IO ()
test_metadataBeforeV4 = do
  Right source <- F.parsePdfFile <$> fixture "source-metadata"
  bytes <- fixture "r3-rc4-128-metadata"
  let encrypt = dict_at "<< /Filter /Standard" bytes
      Right f0 = with_password "user" bytes
      Just root = M.lookup "Root" (F.pdfTrailer f0)
      updated = with_update bytes [(90, "<< " <> inside encrypt <> " /EncryptMetadata false >>")]
                  ("/Encrypt 90 0 R /Root " <> BL.toStrict (B.toLazyByteString (F.renderValue root)))
  assertBool ("/V 2" `BS.isInfixOf` encrypt)
  assertEqual (decoded_streams source) (with_password "user" updated >>= decoded_streams)

-- Passwords of /R 2 to 4 in PDFDocEncoding: "’" is 0x90 and "€" is 0xA0.
-- See Note [Passwords of an encrypted file] in Graphics.PDF.File.Crypt
test_pdfDocEncodingPasswords :: IO ()
test_pdfDocEncodingPasswords = do
  Right source <- F.parsePdfFile <$> fixture "source"
  bytes <- fixture "r3-rc4-128-pdfdoc"
  forM_ ["l\x2019\233t\233", "5\x20AC"] $ \password ->
    assertEqual (decoded_streams source) (with_password password bytes >>= decoded_streams)
  -- the same text in ISO Latin 1 is another password
  left_with "password is not correct" (with_password "l\x90\233t\233" bytes)

-- The reader records the permissions of a file that the user password
-- opened, and tries the owner password first.
-- See Note [Permissions of a decrypted file] in Graphics.PDF.File.Crypt
test_permissionsOfTheFile :: IO ()
test_permissionsOfTheFile = do
  let permissions name password = fmap F.pdfPermissions . with_password password <$> fixture name
      restricted = F.noPermissions { F.permExtractForAccessibility = True }
  forM_ ["r3-rc4-128-restricted", "r6-aes-256-restricted"] $ \name -> do
    permissions name "user" >>= assertEqual (Right (Just restricted))
    permissions name "owner" >>= assertEqual (Right Nothing)
  permissions "r6-aes-256" "user" >>= assertEqual (Right (Just F.allPermissions))
  -- an empty user password
  r <- fmap F.pdfPermissions . F.parsePdfFile <$> fixture "r6-aes-256-empty-user-restricted"
  assertEqual (Right (Just restricted { F.permPrint = True })) r
  -- a password that is the user password and the owner password gives
  -- the rights of the owner
  permissions "r6-aes-256-same-passwords" "same" >>= assertEqual (Right Nothing)
  -- the /P of the files
  assertEqual restricted (F.permissionsFromValue (-3392))
  assertEqual F.allPermissions (F.permissionsFromValue 4294967292)
  forM_ [F.allPermissions, F.noPermissions, restricted] $ \p ->
    assertEqual p (F.permissionsFromValue (F.permissionsValue p))

-- The reader removes the /Encrypt object: it cannot be the catalog
test_encryptionIsNotTheCatalog :: IO ()
test_encryptionIsNotTheCatalog = do
  bytes <- fixture "r3-rc4-128"
  let encrypt = inside (dict_at "<< /Filter /Standard" bytes)
      Right f0 = with_password "user" bytes
      Just (F.Ref root _) = M.lookup "Root" (F.pdfTrailer f0)
      Just (F.Plain (F.Dict catalog)) = F.getObject f0 root
      pages = BL.toStrict (B.toLazyByteString (F.renderValue (catalog M.! "Pages")))
      update object = with_update bytes [(90, object)] "/Encrypt 90 0 R /Root 90 0 R"
  left_with "not an encryption dictionary" (with_password "user" (update ("<< " <> encrypt <> " /Type /Catalog /Pages " <> pages <> " >>")))
  left_with "is the catalog" (with_password "user" (update ("<< " <> encrypt <> " /Pages " <> pages <> " >>")))

-- The /Length of /V 4 can be in the crypt filter only, in bytes
test_keyLengthOfTheCryptFilter :: IO ()
test_keyLengthOfTheCryptFilter = do
  bytes <- fixture "r4-rc4-128"
  let encrypt = dict_at "<< /CF" bytes
      without_length = BS.concat (let (a, b) = BS.breakSubstring "/Length 128 " encrypt in [a, BS.drop 12 b])
      Right f0 = with_password "user" bytes
      Just root = M.lookup "Root" (F.pdfTrailer f0)
      update e = with_update bytes [(90, e)] ("/Encrypt 90 0 R /Root " <> BL.toStrict (B.toLazyByteString (F.renderValue root)))
      replace old new s = let (a, b) = BS.breakSubstring old s in BS.concat [a, new, BS.drop (BS.length old) b]
  assertBool ("/Length 16" `BS.isInfixOf` without_length && not ("/Length 128" `BS.isInfixOf` without_length))
  -- 16 bytes: the key of the file
  assertBool (either (const False) (const True) (with_password "user" (update without_length)))
  -- 5 bytes: another key
  left_with "password is not correct" (with_password "user" (update (replace "/Length 16" "/Length 5" without_length)))

-- An empty string of a file with AES is an empty string: some writers
-- write "()", with no IV
test_emptyAesString :: IO ()
test_emptyAesString = forM_ ["r4-aes-128", "r6-aes-256"] $ \name -> do
  bytes <- fixture name
  let Right f0 = with_password "user" bytes
      Just root = M.lookup "Root" (F.pdfTrailer f0)
      updated = with_update bytes [(90, "<< /X () >>")] ("/Root " <> BL.toStrict (B.toLazyByteString (F.renderValue root)))
  case with_password "user" updated of
    Left e -> assertFailure e
    Right f -> assertEqual (Just (F.Plain (F.Dict (M.singleton "X" (F.String ""))))) (F.getObject f 90)

-- An /Encrypt dictionary that is not correct gives Left
test_badEncryption :: IO ()
test_badEncryption = do
  r6 <- fixture "r6-aes-256"
  r4 <- fixture "r4-aes-128"
  r2 <- fixture "r2-rc4-40"
  -- /P is in the key of /R 2 to 4, and in /Perms for /R 6
  replace_in "/P -4" "/P -8" r6 >>= left_with "/Perms" . with_password "user"
  replace_in "/P -4" "/P -8" r4 >>= left_with "password is not correct" . with_password "user"
  replace_in "/R 6" "/R 7" r6 >>= left_with "/R 7" . with_password "user"
  replace_in "/V 5" "/V 3" r6 >>= left_with "/V 3" . with_password "user"
  replace_in "/Filter /Standard" "/Filter /Standarx" r6 >>= left_with "not /Standard" . with_password "user"
  replace_in "/CFM /AESV2" "/CFM /AESV9" r4 >>= left_with "crypt filter" . with_password "user"
  replace_in "/StmF /StdCF" "/StmF /Other" r4 >>= left_with "not in /CF" . with_password "user"
  replace_in "/Length 128" "/Length 999" r4 >>= left_with "/Length" . with_password "user"
  replace_in "/Length 40" "/Length 48" r2 >>= \f ->
    -- /R 2 has a key of 40 bits whatever its /Length
    assertBool (either (const False) (const True) (with_password "user" f))
  replace_in "/U <48" "/U <49" r2 >>= left_with "password is not correct" . with_password "user"
  replace_in "/O <94" "/O <95" r2 >>= \f -> do
    -- /O gives the key of the file: no password opens it
    left_with "password is not correct" (with_password "owner" f)
    left_with "password is not correct" (with_password "user" f)
  -- /OE has 32 bytes: here 31 bytes, and two spaces (the offsets of the
  -- file do not change)
  replace_in "/OE <b7" "/OE <  " r6 >>= left_with "/OE" . with_password "user"

-- A file that is not correct gives Left, and never an exception
test_malformedEncryptedFiles :: IO ()
test_malformedEncryptedFiles = forM_ ["r4-aes-128", "r6-aes-256", "r3-rc4-128"] $ \name -> do
  bytes <- fixture name
  let outcome b = case with_password "user" b of
        Left e -> length e
        Right f -> fromIntegral (BL.length (F.renderPdfFile f))
  forM_ [0, 7 .. BS.length bytes] $ \n -> evaluate (outcome (BS.take n bytes)) >>= assertBool . (>= 0)
  forM_ (mutationsOf 1000 bytes) $ \m -> evaluate (outcome m) >>= assertBool . (>= 0)

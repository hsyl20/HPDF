{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- A signed document: a signature field, and a detached CMS signature made
-- with a key and its certificate.
--
-- > cabal run HPDF-Sign-Demo -- key.pem certificates.pem
--
-- Writes signed.pdf, and encrypted-signed.pdf: the same document,
-- encrypted (AES-256, the user password "user" and the owner password
-- "owner", printing only), then signed. Then signed-twice.pdf and
-- encrypted-signed-twice.pdf: each file with a second signature, in an
-- incremental update.
--
-- key.pem and certificates.pem are in PEM: a private key that is not
-- encrypted (RSA, or EC on a named curve), and its certificate followed
-- by the other certificates of its chain. For a test:
--
-- > openssl req -x509 -newkey rsa:2048 -nodes -keyout key.pem \\
-- >   -out certificates.pem -days 30 -subj "/CN=Test"
---------------------------------------------------------
module Main where

import Graphics.PDF
import Graphics.PDF.Encryption
import Graphics.PDF.Signature
import qualified Graphics.PDF.File as F
import qualified Data.ByteString.Lazy as BL
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  (keyPath, certsPath) <- case args of
    [k, c] -> return (k, c)
    _ -> die "usage: HPDF-Sign-Demo KEY.pem CERTIFICATES.pem"
  signer <- readSigner keyPath certsPath >>= either die return
  Right helvetica <- mkStdFont Helvetica
  let unsigned = renderPdf standardDocInfo { author = "HPDF", compressed = True } (PDFRect 0 0 420 200) $ do
        page <- addPage Nothing
        drawWithPage page $ do
          drawText $ text (PDFFont helvetica 16) 24 166 "A signed document"
          drawText $ text (PDFFont helvetica 10) 24 140 "The signature field is not visible: see the signature panel of the reader."
        -- The field, and the space for the signature
        addSignature page signature { sigName = "HPDF", sigReason = "Demonstration" }
  -- The signature of all the bytes of the document
  signPdfWith signer unsigned >>= either die (BL.writeFile "signed.pdf")
  putStrLn "signed.pdf written"
  -- The encryption, then the signature of the encrypted bytes
  let enc = (encryption "user" "owner") { encPermissions = noPermissions { permPrint = True, permPrintHighQuality = True } }
  encrypted <- encryptPdf enc unsigned >>= either die return
  signPdfWith signer encrypted >>= either die (BL.writeFile "encrypted-signed.pdf")
  putStrLn "encrypted-signed.pdf written"
  -- A second signature of each file, in an incremental update: the first
  -- signature stays valid. The owner password opens the encrypted file,
  -- and the update is encrypted with the key of the file.
  let second = signature { sigFieldName = "Signature2", sigName = "HPDF", sigReason = "Second signature" }
      twice password from to = do
        source <- F.readPdfSource password from >>= either die return
        file <- either die return (addSignatureField second 1 (F.sourceFile source))
        update <- F.updatePdfFile source file >>= either die return
        signPdfWith signer update >>= either die (BL.writeFile to)
        putStrLn (to ++ " written")
  twice "" "signed.pdf" "signed-twice.pdf"
  twice "owner" "encrypted-signed.pdf" "encrypted-signed-twice.pdf"

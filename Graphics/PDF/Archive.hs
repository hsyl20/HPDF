{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2026, HPDF contributors
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- PDF/A (ISO 19005) output, embedded associated files (PDF/A-3) and the
-- Factur-X / ZUGFeRD invoice metadata.
--
-- > writePdf "invoice.pdf" info rect $ do
-- >   setPDFA (pdfa3b iccBytes) { pdfaTitle = "Invoice 42"
-- >                             , pdfaExtensions = [facturXExtension "BASIC"] }
-- >   _ <- embedAssociatedFile (associatedFile "factur-x.xml" "text/xml" xml)
-- >          { afRelationship = Alternative }
-- >   ... draw the pages with embedded (TrueType) fonts only ...
--
-- In PDF/A mode the document gets, when it is saved: an XMP metadata
-- stream (@pdfaid@, Dublin Core title\/creator\/description, the producer,
-- and every 'XMPExtension' with its PDF/A extension schema description), an
-- output intent with the given ICC profile, an indirect Info dictionary
-- that agrees with the metadata, a PDF 1.7 header with a binary comment,
-- and a trailer @/ID@ (a hash of the document's objects, so the output
-- stays deterministic). A document that does not call 'setPDFA' is
-- written as before.
--
-- What is checked: the part is 2 or 3 and the conformance level is B; each
-- font is embedded (the standard 14 fonts are not); the output intent is an
-- RGB, CMYK or gray ICC profile, and the document uses only the device
-- colours that this profile permits; each embedded file has a MIME
-- type, and a PDF/A-2 document has no embedded file; there is no text
-- annotation (it would need an appearance stream); the prefix and the
-- property names of the XMP extensions are XML names. A document that
-- fails a check is not written: 'Graphics.PDF.renderPdfE'
-- returns the reasons, 'Graphics.PDF.writePdf' raises an exception.
-- See Note [Device colours in a PDF/A document] in "Graphics.PDF".
--
-- What is not checked: transparency, encryption, JavaScript, LZW and
-- actions are never written by HPDF unless asked for; a document that asks
-- for them is not PDF/A, and nothing here stops it.
---------------------------------------------------------
module Graphics.PDF.Archive(
  -- * PDF/A
    PDFA(..)
  , pdfa3b
  , setPDFA
  , XMPExtension(..)
  , XMPProperty(..)
  -- * Associated files
  , AssociatedFile(..)
  , AFRelationship(..)
  , associatedFile
  , embedAssociatedFile
  -- * Factur-X
  , facturXNamespace
  , facturXExtension
  , facturXExtensionFor
  -- * Internals used when the document is saved
  , xmpPacket
  , pdfTextString
  , escapeName
  , iccColorSpace
  , pdfaProblems
  , xmlText
  , textStringBytes
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (ord, isAscii, isAlpha, isAlphaNum)
import Text.Printf (printf)
import Control.Monad.Writer (tell)
import Data.Binary.Builder (fromLazyByteString)
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw
import Graphics.PDF.Pages (createContent, addObject)

-- | PDF/A-3B with this ICC profile as the output intent, no title, no
-- extension schema. Set 'pdfaOutputCondition' too for a profile that is
-- not sRGB.
--
-- The profile is an RGB, CMYK or gray profile. It decides the colours that
-- the document can use: 'Graphics.PDF.Rgb' colours and RGB images with an
-- RGB profile, 'Graphics.PDF.Cmyk' colours and CMYK images with a CMYK
-- profile, and 'Graphics.PDF.Gray' colours with any profile.
pdfa3b :: B.ByteString -> PDFA
pdfa3b icc = PDFA
  { pdfaPart = 3
  , pdfaConformance = "B"
  , pdfaTitle = T.empty
  , pdfaOutputProfile = icc
  , pdfaOutputCondition = "sRGB IEC61966-2.1"
  , pdfaExtensions = []
  }

-- | Write the document as PDF/A. Call it once, anywhere in the 'PDF'
-- action; the PDF/A objects are written when the document is saved.
setPDFA :: PDFA -> PDF ()
setPDFA a = modifyStrict $ \s -> s {pdfaSettings = Just a}

-- | How an associated file relates to the document (PDF 2.0, 14.13;
-- PDF/A-3).
data AFRelationship = Source | Data | Alternative | Supplement | Unspecified
  deriving (Eq, Show)

-- | A file embedded in the document and associated with it (catalog
-- @/AF@), listed in the @EmbeddedFiles@ name tree.
data AssociatedFile = AssociatedFile
  { afName :: T.Text -- ^ file name (@/F@ and @/UF@)
  , afMimeType :: String -- ^ MIME type, the stream's @/Subtype@ (e.g. @text/xml@); a PDF/A document must have it
  , afRelationship :: AFRelationship
  , afDescription :: T.Text -- ^ @/Desc@; empty for none
  , afCreationDate :: Maybe String -- ^ @/Params /CreationDate@, a PDF date such as @D:20261106120000+01'00'@
  , afModDate :: Maybe String -- ^ @/Params /ModDate@, a PDF date
  , afContents :: B.ByteString
  }

-- | A file with a name, MIME type and contents, related as 'Data', with
-- no description and no date.
associatedFile :: T.Text -> String -> B.ByteString -> AssociatedFile
associatedFile n mime bytes = AssociatedFile n mime Data T.empty Nothing Nothing bytes

-- | Embeds the file (an @EmbeddedFile@ stream and its file
-- specification) and associates it with the document. Two files can have
-- the same name.
embedAssociatedFile :: AssociatedFile -> PDF (PDFReference PDFDictionary)
embedAssociatedFile af = do
  stream <- createContent body Nothing
  let fileRef = AnyPdfObject stream
  spec <- addObject . dictFromList $
    [ entry "Type" (PDFName "Filespec")
    , entry "F" (pdfDocFileName (afName af))
    , entry "UF" (pdfTextString (afName af))
    , entry "EF" (dictFromList [entry "F" fileRef, entry "UF" fileRef])
    , entry "AFRelationship" (PDFName (show (afRelationship af)))
    ] ++ [entry "Desc" (pdfTextString (afDescription af)) | not (T.null (afDescription af))]
  modifyStrict $ \s -> s {associatedFiles = (afName af, afMimeType af, spec) : associatedFiles s}
  return spec
 where
  body :: Draw ()
  body = do
    modifyStrict $ \s -> s {otherRsrcs = dictFromList $
      [ entry "Type" (PDFName "EmbeddedFile") ] ++
      -- a name cannot be empty
      [ entry "Subtype" (PDFName (escapeName (afMimeType af))) | not (null (afMimeType af)) ] ++
      [ entry "Params" (dictFromList $
          [ entry "Size" (PDFInteger (fromIntegral (B.length (afContents af)))) ]
          ++ [ entry "CreationDate" (toAsciiString d) | Just d <- [afCreationDate af] ]
          ++ [ entry "ModDate" (toAsciiString d) | Just d <- [afModDate af] ])
      ]}
    tell (fromLazyByteString (afContents af))

-- | A PDF name with the characters outside the regular ones written as
-- @#xx@ (@text/xml@ becomes @text#2Fxml@).
escapeName :: String -> String
escapeName = concatMap esc
 where
  esc c | isAscii c && (isAlphaNum c || c `elem` ("-_.+" :: String)) = [c]
        | otherwise = concatMap (printf "#%02X") (S.unpack (TE.encodeUtf8 (T.singleton c)))

-- | A text string: a literal ASCII string when the text is printable
-- ASCII, else UTF-16BE with a byte order mark.
pdfTextString :: T.Text -> AnyPdfObject
pdfTextString t
  | is_literal t = AnyPdfObject (toAsciiString (T.unpack t))
  | otherwise = AnyPdfObject (toPDFString t)

-- | Whether a text is printable ASCII only.
is_literal :: T.Text -> Bool
is_literal = T.all (\c -> ord c >= 32 && ord c < 127)

-- | The colour space signature in the header of an ICC profile (ICC.1,
-- 7.2.6), for example @"RGB "@, @"GRAY"@ or @"CMYK"@. 'Left' for a profile
-- too short to have a header.
iccColorSpace :: B.ByteString -> Either String String
iccColorSpace profile
  | B.length profile < 128 = Left "it is shorter than an ICC header"
  | otherwise = Right (map (toEnum . fromIntegral) (B.unpack (B.take 4 (B.drop 16 profile))))

-- | The bytes of the string that 'pdfTextString' writes. A name tree is
-- sorted by these bytes, not by the characters of the text.
textStringBytes :: T.Text -> S.ByteString
textStringBytes t
  | is_literal t = TE.encodeUtf8 t
  | otherwise = S.pack [0xFE, 0xFF] <> TE.encodeUtf16BE t

-- | A file name for @/F@, which is a byte string in PDFDocEncoding and not
-- a text string (PDF 32000-1, 7.11.2). A character that is not the same in
-- Latin-1 and in PDFDocEncoding becomes @_@. @/UF@ has the full name.
pdfDocFileName :: T.Text -> AnyPdfObject
pdfDocFileName = AnyPdfObject . toAsciiString . map enc . T.unpack
  where enc c | c >= ' ' && c <= '~' = c
              | c >= '\xA1' && c <= '\xFF' && c /= '\xAD' = c
              | otherwise = '_'

-- | The Factur-X XMP namespace.
facturXNamespace :: T.Text
facturXNamespace = "urn:factur-x:pdfa:CrossIndustryDocument:invoice:1p0#"

-- | The Factur-X metadata of an invoice whose XML is attached as
-- @factur-x.xml@, at this conformance level (@MINIMUM@, @BASIC WL@,
-- @BASIC@, @EN 16931@, @EXTENDED@).
facturXExtension :: T.Text -> XMPExtension
facturXExtension = facturXExtensionFor "INVOICE" "factur-x.xml" "1.0"

-- | The Factur-X metadata: document type, attached file name, version and
-- conformance level.
facturXExtensionFor :: T.Text -> T.Text -> T.Text -> T.Text -> XMPExtension
facturXExtensionFor docType fileName version level = XMPExtension
  { xmpSchemaName = "Factur-X PDFA Extension Schema"
  , xmpNamespace = facturXNamespace
  , xmpPrefix = "fx"
  , xmpProperties =
      [ prop "DocumentFileName" "The name of the embedded XML document" fileName
      , prop "DocumentType" "The type of the hybrid document in capital letters, e.g. INVOICE or ORDER" docType
      , prop "Version" "The actual version of the standard applying to the embedded XML document" version
      , prop "ConformanceLevel" "The conformance level of the embedded XML document" level
      ]
  }
 where
  prop n d v = XMPProperty n "Text" "external" d v

-- | The XMP metadata packet of a PDF/A document.
xmpPacket :: PDFA -> PDFDocumentInfo -> T.Text -> B.ByteString
xmpPacket a info producer = B.fromStrict . TE.encodeUtf8 . T.unlines $
  [ "<?xpacket begin=\"\xFEFF\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>"
  , "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">"
  , " <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"
  , "  <rdf:Description rdf:about=\"\" xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">"
  , "   <pdfaid:part>" <> T.pack (show (pdfaPart a)) <> "</pdfaid:part>"
  , "   <pdfaid:conformance>" <> esc (T.pack (pdfaConformance a)) <> "</pdfaid:conformance>"
  , "  </rdf:Description>"
  ]
  ++ dublinCore
  ++
  [ "  <rdf:Description rdf:about=\"\" xmlns:pdf=\"http://ns.adobe.com/pdf/1.3/\">"
  , "   <pdf:Producer>" <> esc producer <> "</pdf:Producer>"
  , "  </rdf:Description>"
  ]
  ++ (if null exts then [] else extensionSchemas ++ concatMap values exts)
  ++
  [ " </rdf:RDF>"
  , "</x:xmpmeta>"
  ]
  -- padding, so the packet can be edited in place
  ++ replicate 20 (T.replicate 99 " ")
  ++ [ "<?xpacket end=\"w\"?>" ]
 where
  exts = pdfaExtensions a
  title = pdfaTitle a
  dublinCore
    | T.null title && T.null (author info) && T.null (subject info) = []
    | otherwise =
        [ "  <rdf:Description rdf:about=\"\" xmlns:dc=\"http://purl.org/dc/elements/1.1/\">" ]
        ++ [ "   <dc:title><rdf:Alt><rdf:li xml:lang=\"x-default\">" <> esc title <> "</rdf:li></rdf:Alt></dc:title>" | not (T.null title) ]
        ++ [ "   <dc:creator><rdf:Seq><rdf:li>" <> esc (author info) <> "</rdf:li></rdf:Seq></dc:creator>" | not (T.null (author info)) ]
        ++ [ "   <dc:description><rdf:Alt><rdf:li xml:lang=\"x-default\">" <> esc (subject info) <> "</rdf:li></rdf:Alt></dc:description>" | not (T.null (subject info)) ]
        ++ [ "  </rdf:Description>" ]
  extensionSchemas =
    [ "  <rdf:Description rdf:about=\"\""
    , "    xmlns:pdfaExtension=\"http://www.aiim.org/pdfa/ns/extension/\""
    , "    xmlns:pdfaSchema=\"http://www.aiim.org/pdfa/ns/schema#\""
    , "    xmlns:pdfaProperty=\"http://www.aiim.org/pdfa/ns/property#\">"
    , "   <pdfaExtension:schemas>"
    , "    <rdf:Bag>"
    ]
    ++ concatMap schema exts ++
    [ "    </rdf:Bag>"
    , "   </pdfaExtension:schemas>"
    , "  </rdf:Description>"
    ]
  schema e =
    [ "     <rdf:li rdf:parseType=\"Resource\">"
    , "      <pdfaSchema:schema>" <> esc (xmpSchemaName e) <> "</pdfaSchema:schema>"
    , "      <pdfaSchema:namespaceURI>" <> esc (xmpNamespace e) <> "</pdfaSchema:namespaceURI>"
    , "      <pdfaSchema:prefix>" <> esc (xmpPrefix e) <> "</pdfaSchema:prefix>"
    , "      <pdfaSchema:property>"
    , "       <rdf:Seq>"
    ]
    ++ concatMap property (xmpProperties e) ++
    [ "       </rdf:Seq>"
    , "      </pdfaSchema:property>"
    , "     </rdf:li>"
    ]
  property p =
    [ "        <rdf:li rdf:parseType=\"Resource\">"
    , "         <pdfaProperty:name>" <> esc (xmpPropName p) <> "</pdfaProperty:name>"
    , "         <pdfaProperty:valueType>" <> esc (xmpPropValueType p) <> "</pdfaProperty:valueType>"
    , "         <pdfaProperty:category>" <> esc (xmpPropCategory p) <> "</pdfaProperty:category>"
    , "         <pdfaProperty:description>" <> esc (xmpPropDescription p) <> "</pdfaProperty:description>"
    , "        </rdf:li>"
    ]
  values e =
    [ "  <rdf:Description rdf:about=\"\" xmlns:" <> xmpPrefix e <> "=\"" <> esc (xmpNamespace e) <> "\">" ]
    ++ [ "   <" <> q <> ">" <> esc (xmpPropValue p) <> "</" <> q <> ">"
       | p <- xmpProperties e, let q = xmpPrefix e <> ":" <> xmpPropName p ]
    ++ [ "  </rdf:Description>" ]
  esc = T.concatMap (\c -> case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    _ -> T.singleton c) . xmlText

-- | The text without the characters that an XML 1.0 document cannot have
-- (the control characters other than tab, line feed and carriage return,
-- U+FFFE and U+FFFF). The Info dictionary of a PDF/A document gets the same
-- text as the metadata, because the two must agree.
xmlText :: T.Text -> T.Text
xmlText = T.filter (\c -> (c >= ' ' || c `elem` ("\t\n\r" :: String)) && c /= '\xFFFE' && c /= '\xFFFF')

-- | The reasons these settings cannot give a PDF/A document, without a look
-- at the document: a part or a conformance level that HPDF does not write,
-- or an extension whose prefix or property names are not XML names.
pdfaProblems :: PDFA -> [String]
pdfaProblems a =
     [ "PDF/A: part " ++ show (pdfaPart a) ++ " is not supported (only 2 and 3)" | pdfaPart a `notElem` [2, 3] ]
  ++ [ "PDF/A: conformance level " ++ show (pdfaConformance a) ++ " is not supported (only \"B\")" | pdfaConformance a /= "B" ]
  ++ [ "PDF/A: " ++ what ++ " " ++ show n ++ " of the XMP extension " ++ show (xmpSchemaName e) ++ " is not an XML name"
     | e <- pdfaExtensions a
     , (what, n) <- ("the prefix", xmpPrefix e) : [ ("the property name", xmpPropName p) | p <- xmpProperties e ]
     , not (is_xml_name n) ]
 where
  is_xml_name n = case T.uncons n of
    Just (c, rest) -> (isAlpha c || c == '_') && T.all (\x -> isAlphaNum x || x `elem` ("_-." :: String)) rest
    Nothing -> False

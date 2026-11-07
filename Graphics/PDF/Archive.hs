{-# LANGUAGE OverloadedStrings #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2026, HPDF contributors
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- PDF/A (ISO 19005) output.
--
-- > pdfByteStringE info rect $ do
-- >   setPDFA (pdfa3b iccBytes) { pdfaTitle = "Report" }
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
-- What is not checked: the document must not use the standard 14 fonts
-- (they are not embedded); 'pdfByteStringE' reports it. Transparency,
-- encryption, JavaScript, LZW and actions are never written by HPDF unless
-- asked for; a document that asks for them is not PDF/A, and nothing here
-- stops it.
---------------------------------------------------------
module Graphics.PDF.Archive(
  -- * PDF/A
    PDFA(..)
  , pdfa3b
  , setPDFA
  , XMPExtension(..)
  , XMPProperty(..)
  -- * Internals used when the document is saved
  , xmpPacket
  , pdfTextString
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (ord)
import Graphics.PDF.LowLevel.Types
import Graphics.PDF.Draw

-- | PDF/A-3B with this ICC profile (sRGB) as the output intent, no title,
-- no extension schema.
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

-- | A text string: a literal ASCII string when the text is printable
-- ASCII, else UTF-16BE with a byte order mark.
pdfTextString :: T.Text -> AnyPdfObject
pdfTextString t
  | T.all (\c -> ord c >= 32 && ord c < 127) t = AnyPdfObject (toAsciiString (T.unpack t))
  | otherwise = AnyPdfObject (toPDFString t)

-- | The XMP metadata packet of a PDF/A document.
xmpPacket :: PDFA -> PDFDocumentInfo -> T.Text -> B.ByteString
xmpPacket a info producer = B.fromStrict . TE.encodeUtf8 . T.unlines $
  [ "<?xpacket begin=\"\xFEFF\" id=\"W5M0MpCehiHzreSzNTczkc9d\"?>"
  , "<x:xmpmeta xmlns:x=\"adobe:ns:meta/\">"
  , " <rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\">"
  , "  <rdf:Description rdf:about=\"\" xmlns:pdfaid=\"http://www.aiim.org/pdfa/ns/id/\">"
  , "   <pdfaid:part>" <> T.pack (show (pdfaPart a)) <> "</pdfaid:part>"
  , "   <pdfaid:conformance>" <> T.pack (pdfaConformance a) <> "</pdfaid:conformance>"
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
  esc = T.concatMap $ \c -> case c of
    '<' -> "&lt;"
    '>' -> "&gt;"
    '&' -> "&amp;"
    '"' -> "&quot;"
    _ -> T.singleton c

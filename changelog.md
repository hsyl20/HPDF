# Changelog for [`HPDF` package](http://hackage.haskell.org/package/HPDF)

## 1.9

   * Add the `PDFT` monad transformer. `PDF` is now `PDFT Identity` and the
     library functions work in `PDFT m` for any monad `m`, so that other
     effects (e.g. IO) can be interleaved with the construction of the
     document (#21)
   * Add `writePdfT`, `renderPdfT` and `liftPDF`
   * Rename `runPdf` to `writePdf` and `pdfByteString` to `renderPdf`. The old
     names are kept as deprecated aliases
   * Add a step by step interface: `emptyPdfBuilder`, `stepPDF`, `stepPDFT`
     and `buildPdf`
   * The font dictionary of a standard or Type 1 font is written one time
     as an indirect object, and no longer in the resources of each page
     (#5). Documents that use these fonts have different bytes (and are
     smaller); they are displayed as before. `pdffonts` no longer reports
     an internal error for them.

   * Export the `PdfBuilder` type (abstractly)
   * `newSection` and `newSectionWithPage` return the result of their
     contents: their type is now `... -> PDFT m a -> PDFT m a` (#12)
   * Fix custom Type 1 fonts that PDF readers embedded but did not use:
     write a valid font descriptor, an encoding with the glyph names, and the
     font file without the PFB segment headers (#24)
   * AFM files (#9): the parser now reads the files that font tools write
     (glyph names with `_`, `.` or `-`, real numbers, CR LF line ends,
     ligature and other fields, missing `EndFontMetrics`). A file that is
     not an AFM file gives `Left`, never an exception. A glyph named
     `uniXXXX` or `uXXXX[XX]` gives its Unicode character when the encoding
     does not name it. A `space` or `hyphen` glyph without a character code
     is not used.
   * Embedded TrueType fonts with Unicode text, `Graphics.PDF.Fonts.TrueType`:
     `readTrueTypeFont` and `mkTrueTypeFont` write a Type0 font over a
     CIDFontType2 with the Identity-H encoding, the font program as
     FontFile2, /W widths and a ToUnicode CMap, so text extracts and copies
     out as Unicode.
   * `GlyphCode` is now 16 bits; `IsFont` gains `glyphCodeBytes` (default 1)
     and `Graphics.PDF.Fonts.Font` exports `encodeGlyphs`/`decodeGlyphs`;
     `fontGlyphs` builds a glyph run for any font. Standard 14 and Type1
     fonts write the same bytes as before.
   * TrueType fonts are subset by default: when the document is saved, the
     font program keeps only the glyphs shown with it (plus glyph 0 and the
     components of composite glyphs; glyph ids are unchanged, so
     CIDToGIDMap stays /Identity), /W and ToUnicode cover those glyphs, and
     the BaseFont gets an `ABCDEF+` subset tag. `mkTrueTypeFontWith
     EmbedWhole` embeds the whole font. Glyphs reach the subset through
     `recordGlyphs`, which `displayGlyphs`, `displayText`, `text` and the
     typesetting code call.
   * The name of an embedded TrueType font in the content streams is its
     PostScript name and an object number (`DejaVuSans_7`), so two fonts
     with the same PostScript name are two fonts. A control character and a
     soft hyphen (U+00AD) show no glyph with a TrueType font. A font whose
     loca table is not in increasing order is embedded whole.
   * Typesetting: a soft hyphen (U+00AD) between two characters is a place
     where the line can break, and it is not shown in other positions.
   * `wordSpace` has no effect with a two-byte (TrueType) font: this is
     how the Tw operator works.
   * PDF/A-3b output, `Graphics.PDF.Archive`: `setPDFA (pdfa3b icc)` writes,
     when the document is saved, an uncompressed XMP metadata stream
     (pdfaid, Dublin Core title/creator/description, pdf:Producer, and any
     `XMPExtension` with its PDF/A extension schema description), an
     OutputIntent with the given ICC profile, an indirect Info dictionary, a
     trailer /ID (a hash of the objects, so output stays deterministic), a
     PDF 1.7 header with a binary comment line, and no image interpolation.
     Documents that do not call `setPDFA` are written exactly as before.
   * A document that has problems is not written. A document without a
     page is a problem. The problems of a PDF/A document are: a part other
     than 2 and 3 or a conformance level other than B, a font that is not
     embedded (a standard 14 font), an output intent that is not an RGB ICC
     profile, a DeviceCMYK image, a text annotation (it has no appearance
     stream), an embedded file without a MIME type or in a PDF/A-2
     document, and an XMP extension prefix or property name that is not an
     XML name. The link annotations of a PDF/A document get the Print flag
     (/F 4). `renderPdfE`, `renderPdfTE` and `buildPdfE` return the
     problems as a `Left`; `renderPdf`, `renderPdfT` and `buildPdf` call
     `error`; `writePdf` and `writePdfT` raise an `IOError` and write no
     file. `IsFont` gains `fontEmbedded` (default `True`).
   * Embedded and associated files (PDF/A-3): `embedAssociatedFile` writes
     an EmbeddedFile stream (MIME /Subtype, /Params with /Size and optional
     /CreationDate and /ModDate) and a Filespec (/F in PDFDocEncoding, /UF,
     /AFRelationship, /Desc), listed in the catalog's /AF array and in the
     /Names /EmbeddedFiles name tree (sorted by the bytes of the keys).
     Two files can have the same name.
     `facturXExtension` is the Factur-X / ZUGFeRD XMP schema.
   * Digital signatures, `Graphics.PDF.Signature` (#8): `addSignature` adds
     a signature field that is not visible and reserves the space of the
     signature. `signPdfWith` signs the rendered document with a key and its
     certificates (`readSigner`, `parseSigner`): a detached CMS signature
     with SHA-256, for RSA keys and EC keys on a named curve. `signPdf`
     gives the bytes to sign to a function of the caller instead, for a key
     that the program cannot read. A document has one signature.
     New dependencies: `crypton`, `crypton-x509`, `crypton-x509-store` and
     `ram`. New example: `HPDF-Sign-Demo` (Test/sign.hs).
   * New examples: `HPDF-Unicode-Demo` (Test/unicode.hs) and
     `HPDF-PDFA-Demo` (Test/pdfa.hs).
   * Text in more than one rectangle (#6): `displayFormattedTextIn` and
     `displayBoxes` fill a list of rectangles in order and return the boxes
     that did not fit.
   * Less memory to build a large document: the content of a page is
     evaluated to bytes when it is drawn, and no longer kept as a
     computation until the document is rendered. A document of 300 pages
     of text needs 58 MiB and no longer 396 MiB. The output is the same.

## 1.8

   * Added Github CI
   * Allow specifying encodings and merging them (#17)

## 1.7

   * Add transparency support and more function types (#16)

## 1.6.1

   * Fix for building with mtl-2.3

## 1.6.0

   * Allow parsing afm-data from bytestring

## 1.5.3

   * Fixed distribution issue (font files not correctly embedded in the sdist
     archive)

## 1.5.2

   * Embed fonts and encodings

## 1.5.1

   * Fix JPEG support

## 1.5.0

   * Fix build with recent compilers
   * Use hyphenation package

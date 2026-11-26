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
     embedded (a standard 14 font), an output intent that is not an RGB,
     CMYK or gray ICC profile, a device colour that this profile does not
     permit, a text annotation (it has no appearance
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
   * Add `drawWithPage'`, a strict `drawWithPage`: it evaluates the drawing
     to bytes immediately, and the document does not keep the drawing with
     the data that it refers to. Use it for a document with many pages: a
     document of 300 pages of text needs 58 MiB with it, and 320 MiB with
     `drawWithPage`. `drawWithPage` stays lazy, so that a drawing can use
     a value that is known only after it (for example the total number of
     pages).
   * Faster rendering: real numbers and hexadecimal strings are no longer
     written with `printf`, and the text monad is one function of its
     state. A document of text and shapes is rendered in less than half
     the time.
   * Real numbers are written in another form. The values are the same,
     but the bytes of almost all documents change:
       - a number has five decimals at most, and no zeros at the end:
         `12.5` and `3`, where it was `12.50000` and `3.00000`. A document
         is smaller (`demo.pdf`: 5% smaller);
       - the number that is written is the nearest to the exact value,
         with the even last digit on a tie, as the `printf` of C does.
         Before, `printf` of Haskell rounded the shortest decimal digits
         of the number: for example 0.000005 was written `0.00000` and is
         now `0.00001`. This occurs only for a number whose shortest
         digits end with a 5 at the sixth decimal.
   * A number that is not finite (NaN, Infinity) in a drawing is an error:
     the rendering raises an exception, and `writePdf` leaves no file.
     Before, "NaN" or "Infinity" was written in the document, which is
     not a PDF number. The library now depends on `directory`.
   * Gray and CMYK colours: `Color` has two new constructors, `Gray` and
     `Cmyk`. `fillColor` and `strokeColor` write them with the operators of
     DeviceGray and DeviceCMYK. `setUncoloredFillPattern` and
     `setUncoloredStrokePattern` paint a pattern in the device colour
     space of the colour. Code that matches on all the constructors of
     `Color` must add the two cases.
   * PDF/A: the output intent can be a CMYK or a gray ICC profile, and no
     longer only an RGB profile. The profile decides the device colours
     that the document can use: RGB colours and images with an RGB
     profile, CMYK colours and images with a CMYK profile, gray with each
     profile. A colour, an image, a shading or a pattern in another device
     colour space is a problem of the document.
   * PNG images, in `Graphics.PDF.Image`: `readPngFile`, `parsePng`,
     `pngBounds` and `createPDFPng`. All the colour types and numbers of
     bits of PNG are read, with their transparency (an alpha channel, a
     palette with alpha values, or one transparent colour). An interlaced
     image is read if its samples have 8 or 16 bits. The pixels of a file
     without an alpha channel go to the document as they are, with no
     decoding. A file that is not a correct PNG file gives `Left`.
   * Read existing PDF files, `Graphics.PDF.File`: `readPdfFile` and
     `parsePdfFile` give the objects of a file as values (`PdfFile`,
     `Value`, `Object`), and `renderPdfFile` and `writePdfFile` write them
     again with one cross-reference table. The reader knows the
     cross-reference tables with their updates (/Prev), the
     cross-reference streams and object streams of PDF 1.5, and the
     hybrid files. It does not repair a file whose offsets are wrong. It
     is total, with limits of memory (`Limits`). A signature field that is
     not signed is written so that `signPdf` can still sign the file. New
     example: `HPDF-Rewrite-Demo` (Test/rewrite.hs).
   * Read encrypted PDF files: `parsePdfFileWithPassword` and
     `readPdfFileWithPassword` decrypt a file with its user password or its
     owner password. The reader knows all the revisions of the standard
     security handler: RC4 40 to 128 bits (/R 2 to 4), AES-128 (/R 4) and
     AES-256 (/R 5 and 6). `parsePdfFile` reads an encrypted file whose
     user password is empty. `pdfPermissions` has the permissions of a file
     that the user password opened (`Permissions`); the reader does not
     apply them, and a file written again is not encrypted.
   * Encryption, `Graphics.PDF.Encryption`: `encryptPdf` encrypts a
     rendered document with a user password and an owner password
     (AES-256, /R 6 of PDF 2.0), and the permissions of the document
     (`Permissions`). `Graphics.PDF.File.encryptPdfFile` encrypts a
     `PdfFile`. A document can be encrypted, then signed. A PDF/A document
     cannot be encrypted. A file whose permissions are restricted
     (`pdfPermissions`) is not encrypted again, unless
     `encIgnoreSourcePermissions` is True. `HPDF-Sign-Demo` also writes an
     encrypted and signed document.

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

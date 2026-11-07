# Changelog for [`HPDF` package](http://hackage.haskell.org/package/HPDF)

## Unreleased

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
   * PDF/A-3b output, `Graphics.PDF.Archive`: `setPDFA (pdfa3b icc)` writes,
     when the document is saved, an uncompressed XMP metadata stream
     (pdfaid, Dublin Core title/creator/description, pdf:Producer, and any
     `XMPExtension` with its PDF/A extension schema description), an
     OutputIntent with the given ICC profile, an indirect Info dictionary, a
     trailer /ID (a hash of the objects, so output stays deterministic), a
     PDF 1.7 header with a binary comment line, and no image interpolation.
     `pdfByteStringE` returns the document or the reasons it is not PDF/A
     (a standard 14 font, which is not embedded). Documents that do not
     call `setPDFA` are written exactly as before.
   * Embedded and associated files (PDF/A-3): `embedAssociatedFile` writes
     an EmbeddedFile stream (MIME /Subtype, /Params with /Size and optional
     /CreationDate and /ModDate) and a Filespec (/F, /UF, /AFRelationship,
     /Desc), listed in the catalog's /AF array and /Names /EmbeddedFiles.
     `facturXExtension` is the Factur-X / ZUGFeRD XMP schema.

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

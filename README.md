[![Hackage](https://img.shields.io/hackage/v/HPDF.svg)](https://hackage.haskell.org/package/HPDF)
[![Master CI badge](https://github.com/hsyl20/HPDF/actions/workflows/ci.yml/badge.svg?branch=master)](https://github.com/hsyl20/HPDF/actions/workflows/ci.yml?query=branch%3Amaster)

# HPDF

A PDF library with support for several pages, page transitions, outlines,
annotations, compression, colors, shapes, patterns, jpegs, fonts, typesetting...

Have a look at the "Graphics.PDF.Documentation" module to see how to use it. 

Or, download the package and look at the test.hs file in the Test folder. That
file is giving an example of each feature.

## Unicode text with embedded TrueType fonts

The standard 14 fonts only cover Latin-1. To write other scripts (Greek,
Cyrillic, Latin Extended...), embed a TrueType font:

```haskell
Right ttf <- readTrueTypeFont "DejaVuSans.ttf"
runPdf "unicode.pdf" standardDocInfo (PDFRect 0 0 300 100) $ do
  font <- mkTrueTypeFont ttf          -- a subset is embedded when the document is saved
  page <- addPage Nothing
  drawWithPage page $
    drawText $ text (PDFFont font 12) 10 50 "Ça déçoit · Καλημέρα · Привіт"
```

The font is a Type0/CIDFontType2 font with a ToUnicode CMap, so text can be
copied out of the PDF. There is no shaping (one glyph per character) and no
kerning, so scripts that need shaping (Arabic, Indic) do not render
correctly. `cabal run HPDF-Unicode-Demo -- font.ttf` writes `unicode.pdf`.

## PDF/A-3b and attached files

`Graphics.PDF.Archive` writes PDF/A-3b documents (XMP metadata, an ICC
output intent) and embeds files associated with the document:

```haskell
import Graphics.PDF.Archive

pdfByteStringE standardDocInfo rect $ do
  setPDFA (pdfa3b srgbIccBytes) { pdfaTitle = "Report" }
  _ <- embedAssociatedFile (associatedFile "data.csv" "text/csv" csvBytes)
  font <- mkTrueTypeFont ttf          -- PDF/A needs embedded fonts
  ...
```

`pdfByteStringE` returns `Left` with the reasons a PDF/A document is not
conforming (a standard 14 font, which is not embedded).
`cabal run HPDF-PDFA-Demo -- font.ttf sRGB.icc` writes `pdfa.pdf`.

## Documentation

The library documentation can be accessed on [Hackage](https://hackage.haskell.org/package/HPDF)


## Testing

```
$ cabal test all
$ cabal run HPDF-demo
$ open demo.pdf
$ cabal run HPDF-Unicode-Demo  # writes unicode.pdf
$ cabal run HPDF-PDFA-Demo     # writes pdfa.pdf
```

The TrueType tests and the two demos look for DejaVu Sans or Noto Sans in
the usual system locations (or `$HPDF_TEST_TTF` for the tests); the tests
are reported pending when there is none. The PDF/A demo also needs an sRGB
ICC profile (e.g. colord's `/usr/share/color/icc/colord/sRGB.icc`).

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
   * Export the `PdfBuilder` type (abstractly)

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

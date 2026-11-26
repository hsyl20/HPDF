#!/bin/sh
# Run in a debian container, in the directory of source.pdf:
#   docker run --rm -v "$PWD":/w -w /w debian:sid sh make-fixtures.sh
set -e
apt-get update -qq >/dev/null
apt-get install -y -qq qpdf >/dev/null
qpdf --version | head -1
W=--allow-weak-crypto
qpdf $W --encrypt user owner 40 -- source.pdf r2-rc4-40.pdf
qpdf $W --encrypt user owner 128 --use-aes=n -- source.pdf r3-rc4-128.pdf
qpdf $W --encrypt "" owner 128 --use-aes=n -- source.pdf r3-rc4-128-empty-user.pdf
qpdf $W --encrypt user owner 128 --use-aes=n --force-V4 -- source.pdf r4-rc4-128.pdf
qpdf --object-streams=generate --encrypt user owner 128 --use-aes=y -- source.pdf r4-aes-128.pdf
qpdf --encrypt user owner 256 --force-R5 -- source.pdf r5-aes-256.pdf
qpdf --object-streams=generate --encrypt user owner 256 -- source.pdf r6-aes-256.pdf
qpdf --encrypt user owner 128 --use-aes=y --cleartext-metadata -- source-metadata.pdf r4-aes-128-clear-metadata.pdf
qpdf --encrypt user owner 256 --cleartext-metadata -- source-metadata.pdf r6-aes-256-clear-metadata.pdf
qpdf $W --encrypt user owner 128 --use-aes=n -- source-metadata.pdf r3-rc4-128-metadata.pdf
# qpdf gives the bytes of these passwords in PDFDocEncoding
qpdf $W --encrypt 'l’été' '5€' 128 --use-aes=n -- source.pdf r3-rc4-128-pdfdoc.pdf
qpdf $W --encrypt user owner 128 --use-aes=n --print=none --modify=none --extract=n -- source.pdf r3-rc4-128-restricted.pdf
qpdf --encrypt user owner 256 --print=none --modify=none --extract=n -- source.pdf r6-aes-256-restricted.pdf
qpdf --encrypt "" owner 256 --print=low --modify=none --extract=n -- source.pdf r6-aes-256-empty-user-restricted.pdf
qpdf --encrypt same same 256 --print=none -- source.pdf r6-aes-256-same-passwords.pdf
for f in r*.pdf; do
  echo "== $f"
  qpdf --password=user --show-encryption "$f" | head -3
done

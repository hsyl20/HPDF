---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The permissions of an encrypted document (the /P of the standard
-- security handler)
---------------------------------------------------------
module Graphics.PDF.File.Permissions
  ( Permissions(..)
  , allPermissions
  , noPermissions
  , permissionsValue
  , permissionsFromValue
  ) where

import Data.Bits (testBit)

-- | What a user who opens a document with the user password can do. The
-- owner password gives all of them. A reader applies them: they are not
-- a protection of the content, which the user password decrypts
-- (ISO 32000-2, 7.6.4.2).
data Permissions = Permissions
  { permPrint :: !Bool -- ^ Print the document
  , permModify :: !Bool -- ^ Change the document
  , permCopy :: !Bool -- ^ Copy or extract the text and the graphics
  , permAnnotate :: !Bool -- ^ Add or change the annotations, and fill the form fields
  , permFillForms :: !Bool -- ^ Fill the form fields, also when 'permAnnotate' is False
  , permExtractForAccessibility :: !Bool -- ^ Extract the text and the graphics for accessibility. PDF 2.0 readers ignore this bit
  , permAssemble :: !Bool -- ^ Insert, rotate or delete pages, and add bookmarks
  , permPrintHighQuality :: !Bool -- ^ Print with all the quality, when 'permPrint' is True
  } deriving (Eq, Show)

-- | All the permissions
allPermissions :: Permissions
allPermissions = Permissions True True True True True True True True

-- | No permission
noPermissions :: Permissions
noPermissions = Permissions False False False False False False False False

-- | The bit of each permission in /P (1 is the lowest bit)
permission_bits :: [(Permissions -> Bool, Int)]
permission_bits =
  [ (permPrint, 3), (permModify, 4), (permCopy, 5), (permAnnotate, 6)
  , (permFillForms, 9), (permExtractForAccessibility, 10), (permAssemble, 11)
  , (permPrintHighQuality, 12) ]

-- | The /P of the permissions: a signed 32-bit integer. The bits 3 to 6
-- and 9 to 12 are the permissions; the bits 1 and 2 are 0, and the other
-- bits are 1 (ISO 32000-2, Table 22).
permissionsValue :: Permissions -> Integer
permissionsValue p =
  let reserved = 0xFFFFFFFF - 3 - sum [ 2 ^ (b - 1) | (_, b) <- permission_bits ] :: Integer
      v = reserved + sum [ 2 ^ (b - 1) | (f, b) <- permission_bits, f p ]
  in if v >= 2 ^ (31 :: Int) then v - 2 ^ (32 :: Int) else v

-- | The permissions of a /P (signed or not)
permissionsFromValue :: Integer -> Permissions
permissionsFromValue v =
  let bit b = testBit (v `mod` 2 ^ (32 :: Int)) (b - 1)
  in Permissions (bit 3) (bit 4) (bit 5) (bit 6) (bit 9) (bit 10) (bit 11) (bit 12)

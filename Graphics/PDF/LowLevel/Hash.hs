---------------------------------------------------------
-- |
-- Copyright   : (c) 2026, HPDF contributors
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- The FNV-1a 64 hash, for the names and identifiers that must be the same
-- for the same input (subset tags, document identifiers).
---------------------------------------------------------
module Graphics.PDF.LowLevel.Hash
  ( fnv1a64OffsetBasis
  , fnv1a64Step
  ) where

import Data.Bits (xor)
import Data.Word (Word64)

-- | The standard start value of the hash.
fnv1a64OffsetBasis :: Word64
fnv1a64OffsetBasis = 14695981039346656037

-- | One step of the hash: fold it over the values to hash, starting from
-- 'fnv1a64OffsetBasis' (or another start value, for an independent hash).
fnv1a64Step :: Word64 -> Word64 -> Word64
fnv1a64Step h x = (h `xor` x) * 1099511628211

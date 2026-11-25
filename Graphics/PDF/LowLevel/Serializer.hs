{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Stability   : experimental
-- Portability : portable
--
-- Serializer
---------------------------------------------------------
-- #hide
module Graphics.PDF.LowLevel.Serializer(
  SerializeValue(..)
 , realNumber
 ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BPI
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke, pokeByteOff)



class (Monoid s) => SerializeValue s a where
    serialize :: a -> s
    cons :: a -> s -> s
    cons a b = (serialize a) `mappend` b
    
instance SerializeValue B.ByteString Word8 where
    serialize = B.singleton
    cons = B.cons
    
instance SerializeValue B.ByteString Char where
    serialize = C.singleton
    cons = C.cons  
    
instance SerializeValue B.ByteString [Char] where
    serialize = C.pack
    
instance SerializeValue B.ByteString B.ByteString where
    serialize = id


instance SerializeValue B.ByteString Int where
    serialize = C.pack . show
 
instance SerializeValue B.ByteString Double where
    serialize = BU.toLazyByteString . realNumber

    
instance SerializeValue BU.Builder Word8 where
    serialize = BU.singleton

instance SerializeValue BU.Builder Char where
    serialize = BU.singleton . c2w
    
instance SerializeValue BU.Builder [Char] where
    serialize = BB.string8

instance SerializeValue BU.Builder B.ByteString where
    serialize = BU.fromLazyByteString

instance SerializeValue BU.Builder Int where
    serialize = BB.intDec

instance SerializeValue BU.Builder Double where
    serialize = realNumber

-- Note [Numbers with five decimals]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A real number of a PDF document is a decimal number without an
-- exponent, and a reader keeps about five decimals. Thus 'realNumber'
-- writes five decimals at most, and it does not write the zeros at the
-- end:
--
--   12.3456789  "12.34568"        12.5  "12.5"        3  "3"
--   -0.25       "-0.25"           -0.000001  "0"      (no negative zero)
--
-- The number that is written is the nearest to the exact value of the
-- Double. If two numbers are as near (a tie), it is the one with an even
-- last digit. This is the rounding of the printf of C.
--
--   12.3456789   is   round (12.3456789 * 100000) = 1234568
--                and  1234568 `quotRem` 100000    = (12, 34568)
--
-- 'realNumber' has two paths:
--
-- * The fast path multiplies in floating point. The product is not exact:
--   for a number below 10000 it is below 1e9, and its error is 6e-8 at
--   most (half of the distance between two Doubles near 1e9). This path
--   is used only when the fraction of the product is more than 1e-5 away
--   from 0.5 (a tie), thus the error cannot change the last digit.
-- * The exact path ('exact_real_number') does the same with the mantissa
--   and the exponent of the number, in integer arithmetic.
--
-- HPDF 1.8 and before used @printf "%.5f"@ of Haskell, which was 3/4 of
-- the time and of the allocation of a document of text and shapes. That
-- function wrote the zeros at the end ("3.00000"), and it did not round
-- the exact value: it rounded the shortest decimal digits that give the
-- number back. The two rounding rules differ only for a number whose
-- shortest digits are a tie:
--
--   0.000005      exact value 0.00000500000000000000040901...
--                 printf of Haskell: 0.00000      'realNumber': 0.00001
--
-- A number that is not finite cannot be in a PDF document. 'realNumber'
-- writes it as 'show' does, as before.

-- | A real number with five decimals at most.
-- See Note [Numbers with five decimals]
realNumber :: Double -> BU.Builder
realNumber x
    | a < 10000 && abs (frac - 0.5) > 1.0e-5 =
        BP.primBounded real_number_prim (x < 0, if frac > 0.5 then whole + 1 else whole)
    | isNaN x || isInfinite x = BB.string7 (show x)
    | otherwise = exact_real_number (x < 0) a
  where
    a = abs x
    scaled = a * 100000
    whole = truncate scaled :: Int
    frac = scaled - fromIntegral whole

-- | The exact path of 'realNumber', for a finite number that is not
-- negative.
-- See Note [Numbers with five decimals]
exact_real_number :: Bool -> Double -> BU.Builder
exact_real_number negative a
    | n == 0 = BB.char7 '0'
    | otherwise = (if negative then BB.char7 '-' else mempty) <> BB.integerDec q <> BB.string7 decimals
  where
    -- a = mantissa * 2 ^ e
    (mantissa, e) = decodeFloat a
    numerator = mantissa * 100000
    n :: Integer
    n | e >= 0 = numerator * 2 ^ e
      | otherwise =
          let denominator = 2 ^ negate e
              (q0, r0) = numerator `quotRem` denominator
          in case compare (2 * r0) denominator of
               GT -> q0 + 1
               -- a tie: the even number
               EQ -> if odd q0 then q0 + 1 else q0
               LT -> q0
    (q, r) = n `quotRem` 100000
    decimals
      | r == 0 = ""
      | otherwise = let t = show r
                    in '.' : reverse (dropWhile (== '0') (reverse (replicate (5 - length t) '0' ++ t)))

-- | Writes a number of 100000ths that is not negative and not more than
-- 10000 * 100000 (a number below 10000 can round to 10000), with its
-- sign.
--
-- It writes 11 bytes at most: the sign, four digits, the point and five
-- digits. The number 10000 has five digits, but then no decimals. The
-- bound of the buffer is 12 bytes: the sign, five digits, the point and
-- five digits, which is more than any number needs.
--
-- The sign is given apart, and it is written only if the number of
-- 100000ths is not zero.
real_number_prim :: BP.BoundedPrim (Bool, Int)
real_number_prim = BPI.boundedPrim 12 write
  where
    write (_, 0) p0 = poke p0 0x30 >> return (p0 `plusPtr` 1)
    write (negative, n) p0 = do
      p1 <- if negative then poke p0 0x2D >> return (p0 `plusPtr` 1) else return p0
      let (q, r) = n `quotRem` 100000
      p2 <- integer q p1
      if r == 0
        then return p2
        else do
          poke p2 0x2E
          digit (r `quot` 10000) p2 1
          digit (r `quot` 1000) p2 2
          digit (r `quot` 100) p2 3
          digit (r `quot` 10) p2 4
          digit r p2 5
          -- the bytes after the last digit that is not zero are not kept
          return (p2 `plusPtr` (1 + decimals r))
    -- number of decimals without the zeros at the end, for 0 < r < 100000
    decimals :: Int -> Int
    decimals r
      | r `rem` 10 /= 0 = 5
      | r `rem` 100 /= 0 = 4
      | r `rem` 1000 /= 0 = 3
      | r `rem` 10000 /= 0 = 2
      | otherwise = 1
    digit :: Int -> Ptr Word8 -> Int -> IO ()
    digit d p offset = pokeByteOff p offset (fromIntegral (0x30 + d `rem` 10) :: Word8)
    -- one to five digits
    integer :: Int -> Ptr Word8 -> IO (Ptr Word8)
    integer q p
      | q < 10 = digit q p 0 >> return (p `plusPtr` 1)
      | q < 100 = digit (q `quot` 10) p 0 >> digit q p 1 >> return (p `plusPtr` 2)
      | q < 1000 = digit (q `quot` 100) p 0 >> digit (q `quot` 10) p 1 >> digit q p 2 >> return (p `plusPtr` 3)
      | q < 10000 = digit (q `quot` 1000) p 0 >> digit (q `quot` 100) p 1 >> digit (q `quot` 10) p 2
                      >> digit q p 3 >> return (p `plusPtr` 4)
      | otherwise = digit (q `quot` 10000) p 0 >> digit (q `quot` 1000) p 1 >> digit (q `quot` 100) p 2
                      >> digit (q `quot` 10) p 3 >> digit q p 4 >> return (p `plusPtr` 5)

{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- Serializer
---------------------------------------------------------
-- #hide
module Graphics.PDF.LowLevel.Serializer(
  SerializeValue(..)
 ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Builder as BU
import qualified Data.ByteString.Lazy.Char8 as C
import Data.ByteString.Internal (c2w)
import Data.Word (Word8)
import Text.Printf (printf)



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
    serialize = C.pack . printf "%.5f"

    
instance SerializeValue BU.Builder Word8 where
    serialize = BU.singleton

instance SerializeValue BU.Builder Char where
    serialize = BU.singleton . c2w
    
instance SerializeValue BU.Builder [Char] where
    serialize = BU.fromLazyByteString . serialize

instance SerializeValue BU.Builder B.ByteString where
    serialize = BU.fromLazyByteString

instance SerializeValue BU.Builder Int where
    serialize = BU.fromLazyByteString . serialize

instance SerializeValue BU.Builder Double where
    serialize = BU.fromLazyByteString . serialize

    

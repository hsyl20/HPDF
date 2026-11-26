---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The objects of a PDF file, as values
---------------------------------------------------------
module Graphics.PDF.File.Object
  ( Dict
  , Value(..)
  , Object(..)
  , PdfFile(..)
  , dictLookup
  , realToRational
  , getObject
  , resolveValue
  , pdfPages
  , isSignatureDict
  ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Graphics.PDF.File.Permissions (Permissions)

-- | A dictionary. The keys are the bytes of the names, without the @/@
-- and with their @#xx@ escapes decoded.
type Dict = M.Map S.ByteString Value

-- Note [Real numbers of a PDF file]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A real number keeps the decimal digits that the file has: a mantissa
-- and the number of digits after the point. Thus a file is written back
-- with the same numbers:
--
--   "0.50"   Real 50 2      written "0.50"
--   "-.002"  Real (-2) 3    written "-0.002"
--   "4."     Real 40 1      written "4.0"
--
-- The number of digits after the point is 1 at least, so that a real is
-- never written as an integer. A 'Rational' could not keep "0.50", and
-- could not be written in decimal in all cases (1/3). 'realToRational'
-- gives the value. Two reals are equal only if they have the same digits:
-- Real 5 1 is not equal to Real 50 2.

-- | A value of a PDF file
data Value
  = Null
  | Bool !Bool
  | Integer !Integer
  | Real !Integer !Int -- ^ The mantissa, and the number of digits after the point (1 at least). See Note [Real numbers of a PDF file]
  | Name !S.ByteString -- ^ The bytes of the name, without the @/@ and with the @#xx@ escapes decoded
  | String !S.ByteString -- ^ The bytes of the string, with the escapes decoded. A literal string and a hexadecimal string are the same value
  | Array [Value]
  | Dict !Dict
  | Ref !Int !Int -- ^ Object number and generation number
  deriving (Eq, Show)

-- | An object of a PDF file
data Object
  = Plain !Value
  | Stream !Dict !S.ByteString -- ^ The dictionary and the data of the stream, still encoded. The @/Length@ of the dictionary is the length of the data
  deriving (Eq, Show)

-- | The objects of a PDF file
data PdfFile = PdfFile
  { pdfVersion :: !(Int, Int) -- ^ The version of the header, for example (1, 7)
  , pdfTrailer :: !Dict -- ^ The trailer: @/Root@, @/Info@, @/ID@. It has no @/Size@ and no @/Prev@
  , pdfObjects :: !(IM.IntMap (Int, Object)) -- ^ Each object, with its generation number
  , pdfPermissions :: !(Maybe Permissions)
    -- ^ The permissions of an encrypted file that the reader opened with
    -- the user password. 'Nothing': all the rights (the file is not
    -- encrypted, or the owner password opened it). The reader does not
    -- apply them: the caller does. See Note [Permissions of a decrypted
    -- file] in Graphics.PDF.File.Crypt
  } deriving (Eq, Show)

-- | The value of a key. A @null@ value is the same as no value.
dictLookup :: S.ByteString -> Dict -> Maybe Value
dictLookup key d = case M.lookup key d of
  Just Null -> Nothing
  other -> other

-- | The value of a real number.
-- See Note [Real numbers of a PDF file]
realToRational :: Integer -> Int -> Rational
realToRational mantissa decimals = fromInteger mantissa / 10 ^ max 0 decimals

-- | Is a dictionary a signature dictionary (PDF 32000-1, 12.8.1)? It has
-- a /ByteRange array, a /Filter name, and no /Type or the /Type /Sig or
-- /DocTimeStamp. Another dictionary (an annotation) can have these keys
-- too, but not this /Type.
isSignatureDict :: Dict -> Bool
isSignatureDict d = case (dictLookup (C.pack "ByteRange") d, dictLookup (C.pack "Filter") d, dictLookup (C.pack "Type") d) of
  (Just (Array _), Just (Name _), t) -> t `elem` [Nothing, Just (Name (C.pack "Sig")), Just (Name (C.pack "DocTimeStamp"))]
  _ -> False

-- | An object of the file
getObject :: PdfFile -> Int -> Maybe Object
getObject file n = snd <$> IM.lookup n (pdfObjects file)

-- | The pages of a file, in their order: the number and the generation of
-- each page object. The pages are the leaves of the page tree (the
-- /Pages of the catalog, then the /Kids of each node), with /Type /Page
-- or with no /Type and no /Kids.
-- An object of the tree is visited one time: a loop, or a node in two
-- places, does not give more pages.
pdfPages :: PdfFile -> [(Int, Int)]
pdfPages file = reverse (snd (go (IS.empty, []) pages_root))
  where
    pages_root = case resolveValue file <$> dictLookup (C.pack "Root") (pdfTrailer file) of
      Just (Dict catalog) -> dictLookup (C.pack "Pages") catalog
      _ -> Nothing
    go (visited, acc) v = case v of
      Just (Ref n g) | not (IS.member n visited), Dict d <- resolveValue file (Ref n g) ->
        let visited' = IS.insert n visited
            kids = resolveValue file <$> dictLookup (C.pack "Kids") d
        in case (dictLookup (C.pack "Type") d, kids) of
             (Just (Name t), _) | t == C.pack "Page" -> (visited', (n, g) : acc)
             (_, Just (Array ks)) -> foldl (\s k -> go s (Just k)) (visited', acc) ks
             -- a leaf with no /Type is a page, as for poppler
             (Nothing, _) -> (visited', (n, g) : acc)
             _ -> (visited', acc)
      _ -> (visited, acc)

-- | The value that a reference gives. Another value is given as it is.
-- A reference to an object that does not exist, or that has another
-- generation, gives 'Null' (PDF 32000-1, 7.3.10). A reference to a stream
-- also gives 'Null': use 'getObject' for a stream.
resolveValue :: PdfFile -> Value -> Value
resolveValue file (Ref n g) = case IM.lookup n (pdfObjects file) of
  Just (g', Plain v) | g' == g -> v
  _ -> Null
resolveValue _ v = v

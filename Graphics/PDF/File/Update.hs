---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- Incremental updates of a PDF file (PDF 32000-1, 7.5.6)
---------------------------------------------------------
module Graphics.PDF.File.Update
  ( updatePdfFile
  , updatePdfFileWith
  , UpdateOptions(..)
  , defaultUpdateOptions
  ) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State.Strict (State, evalState)
import Control.Monad.Trans (lift)
import Crypto.Random (ChaChaDRG, MonadRandom)
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IM
import Data.Int (Int64)
import qualified Data.Map.Strict as M
import Graphics.PDF.File.Cipher (newGenerator, randomBytes)
import Graphics.PDF.File.Crypt (encryptObject)
import Graphics.PDF.File.Object
import Graphics.PDF.File.Parse (PdfSource(..))
import Graphics.PDF.File.Permissions (Permissions(..))
import Graphics.PDF.File.Write

-- Note [Incremental update]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- An incremental update (PDF 32000-1, 7.5.6) adds bytes after the end of
-- a file, and does not change the bytes of the file:
--
--   %PDF-1.7 ... %%EOF             the bytes of the file
--   12 0 obj ... endobj            a new object, or a changed object
--   xref
--   0 1
--   0000000005 65535 f             the first free entry
--   5 1
--   0000000000 00001 f             a deleted object: the next generation
--   12 1
--   0000004242 00000 n
--   trailer
--   << /Prev 1234 /Root 1 0 R /Size 13 ... >>
--   startxref
--   4321
--   %%EOF
--
-- Thus the signatures of the file stay valid: they sign the old bytes. A
-- new signature dictionary of the update signs all the file (Note
-- [Signatures of the writer] in Graphics.PDF.File.Write).
--
-- The update has:
--
-- * each object of the new file that the old file does not have, or that
--   is not equal to the object of the old file;
-- * a free entry for each object of the old file that the new file does
--   not have;
-- * the trailer of the new file, with /Prev (the previous section), /Size
--   and the /Encrypt of the old file.
--
-- The section is a cross-reference stream if the newest section of the
-- file is a stream: some readers do not accept a table after a stream.
-- Else it is a table. With a stream, the objects of the update are in
-- object streams, as in Note [Object streams of the writer] in
-- Graphics.PDF.File.Write ('updateObjectStreams'). The object streams
-- and the cross-reference stream get numbers after all the numbers of
-- the file, also those of the references ('largestNumber'): a reference
-- to no object stays a reference to no object. In an encrypted file, the
-- object streams are encrypted, and the objects in them are not.
--
-- A new object gets a number after the numbers of the file
-- ('sourceSize') and of the references of the new file ('largestNumber'),
-- and its references change, when its number is one of the file:
--
-- * an object that the reader removed (Note [Objects that the reader
--   removes] in Graphics.PDF.File.Parse): an object stream is still in
--   the file, and its objects would be lost;
-- * a free entry: its next generation is not 0. In an encrypted file
--   with RC4, an object with the number and the generation of an old
--   object would also have the key of the old object.
--
-- In an encrypted file, the objects of the update are encrypted with the
-- key of the file (Note [Objects that are not encrypted] in
-- Graphics.PDF.File.Crypt). The first string of the /ID is a part of the
-- key: it cannot change. A trailer /Info that is a dictionary becomes an
-- object (the standard requires a reference): the trailer is not
-- encrypted.
--
-- When the new file has the /ID of the old file, the second string of the
-- /ID is a new random string (PDF 32000-1, 14.4). A file with no /ID that
-- is not encrypted gets a random one (PDF 2.0 requires it); in an
-- encrypted file, the /ID is a part of the key. The keys of a
-- cross-reference section in the trailer of the new file are removed: the
-- update writes its own (an old /XRefStm would give the deleted objects
-- again). A higher version of the
-- new file becomes the /Version of the catalog: the header does not
-- change.

-- Note [Restrictions of an update]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Two things of a file can forbid changes:
--
-- * the permissions of an encrypted file that the user password opened
--   ('pdfPermissions', Note [Permissions of a decrypted file] in
--   Graphics.PDF.File.Crypt). An update needs the permission to modify
--   the document (bit 4 of /P). This is more strict than the standard: a
--   file with the permission to fill forms (bit 9) only permits to sign
--   it, but the update does not know which changes are form fields;
-- * a DocMDP signature (PDF 32000-1, 12.8.2.2) that certifies the file
--   with no changes (/P 1): a change makes the certification not valid.
--   /P 2 and 3 permit some changes (forms, signatures, annotations): the
--   update does not check them.
--
-- The caller can ignore each restriction ('UpdateOptions'): the reader
-- does not apply them (the bytes are not protected), and the owner of a
-- document can choose to make a change.

-- | What an update does with the restrictions of a file.
-- See Note [Restrictions of an update]
data UpdateOptions = UpdateOptions
  { updateIgnorePermissions :: !Bool
    -- ^ Update a file whose permissions do not permit to modify it
    -- ('pdfPermissions': the user password opened it). False: 'Left'
  , updateIgnoreCertification :: !Bool
    -- ^ Update a file that a DocMDP signature certifies with no changes.
    -- The certification is then not valid. False: 'Left'
  , updateObjectStreams :: !Bool
    -- ^ Put the objects of the update in object streams, when the newest
    -- section of the file is a cross-reference stream. See Note
    -- [Incremental update]
  } deriving (Eq, Show)

-- | The restrictions of the file are respected, and the objects of the
-- update are in object streams when the file has a cross-reference stream
defaultUpdateOptions :: UpdateOptions
defaultUpdateOptions = UpdateOptions False False True

-- | The bytes of a file with an incremental update: the bytes of the
-- source, then the objects that the new file changes. The new file is
-- usually the 'sourceFile' of the source, with changes. A new file that
-- is equal to the source gives the bytes of the source.
--
-- 'Left' for:
--
-- * a source that the reader repaired ('sourceRepaired');
-- * a new file with no /Root, or with an /Encrypt (an update cannot
--   encrypt a file: see 'Graphics.PDF.File.encryptPdfFile');
-- * a lower version, or a higher version with no catalog;
-- * a change of the first string of the /ID of an encrypted file;
-- * a file whose permissions do not permit to modify it, and a file that
--   a DocMDP signature certifies with no changes. See
--   'updatePdfFileWith'.
--
-- The random bytes give the /ID, and the IVs of AES in an encrypted file.
-- See Note [Incremental update]
updatePdfFile :: MonadRandom m => PdfSource -> PdfFile -> m (Either String L.ByteString)
updatePdfFile = updatePdfFileWith defaultUpdateOptions

-- | 'updatePdfFile', with options for the restrictions of the file.
-- See Note [Restrictions of an update]
updatePdfFileWith :: MonadRandom m => UpdateOptions -> PdfSource -> PdfFile -> m (Either String L.ByteString)
updatePdfFileWith opts src new = evalState (runExceptT (update opts src new)) <$> newGenerator

update :: UpdateOptions -> PdfSource -> PdfFile -> ExceptT String (State ChaChaDRG) L.ByteString
update opts src new0 = do
  let old = sourceFile src
      encrypt_value = snd <$> sourceEncryption src
  -- See Note [Repair of a file] in Graphics.PDF.File.Parse
  when (sourceRepaired src) $
    throwError "the cross-reference of the file is not correct (the reader repaired it): write the file again (renderPdfFile), then update it"
  when (M.member (C.pack "Encrypt") (pdfTrailer new0)) $
    throwError "an update cannot encrypt a file: the trailer has an /Encrypt"
  case dictLookup (C.pack "Root") (pdfTrailer new0) of
    Just (Ref _ _) -> return ()
    _ -> throwError "the trailer has no /Root"
  new <- either throwError (return . renumber src) (with_version (pdfVersion old) new0)
  let changes objs = [ (n, x) | (n, x) <- IM.toList objs, IM.lookup n (pdfObjects old) /= Just x ]
      deleted = [ (n, min 65535 (g + 1)) | (n, (g, _)) <- IM.toList (pdfObjects old), IM.notMember n (pdfObjects new) ]
  if null (changes (snd (IM.split 0 (pdfObjects new)))) && null deleted && pdfTrailer new == pdfTrailer old
    then return (L.fromStrict (sourceBytes src)) else do
    -- a trailer /Info that is a dictionary becomes an object. See Note
    -- [Incremental update]
    let info_n = maximum [sourceSize src, largestNumber new + 1]
        (objects, new_trailer) = case M.lookup (C.pack "Info") (pdfTrailer new) of
          Just (Dict info) -> ( IM.insert info_n (0, Plain (Dict info)) (snd (IM.split 0 (pdfObjects new)))
                              , M.insert (C.pack "Info") (Ref info_n 0) (pdfTrailer new) )
          _ -> (snd (IM.split 0 (pdfObjects new)), pdfTrailer new)
        changed = changes objects
    -- See Note [Restrictions of an update]
    case pdfPermissions old of
      Just p | not (permModify p), not (updateIgnorePermissions opts) ->
        throwError "the permissions of the file do not permit to modify it (the user password opened it): use the owner password, or updateIgnorePermissions"
      _ -> return ()
    when (certified_with_no_changes old && not (updateIgnoreCertification opts)) $
      throwError "a DocMDP signature certifies the file with no changes (/P 1): use updateIgnoreCertification"
    file_id <- new_id (dictLookup (C.pack "ID") (pdfTrailer old)) (dictLookup (C.pack "ID") new_trailer)
                      (encrypt_value == Nothing)
    when (encrypt_value /= Nothing && first_id file_id /= first_id (dictLookup (C.pack "ID") (pdfTrailer old))) $
      throwError "an update cannot change the first string of the /ID of an encrypted file: it is a part of the key"
    let size = maximum [sourceSize src, maybe 1 ((+ 1) . fst) (IM.lookupMax objects)]
        -- the new objects get numbers after all the numbers of the file,
        -- also those of the references. See Note [Incremental update]
        first = max size (largestNumber new { pdfObjects = objects, pdfTrailer = new_trailer } + 1)
        (kept, containers, in_streams) = packInObjectStreams first (IM.fromList changed)
        packed = updateObjectStreams opts && sourceXrefStream src && not (IM.null in_streams)
                 && first + IM.size containers <= 8388607
        -- the objects of the update, and the number of a cross-reference
        -- stream
        (unencrypted, places, number)
          | packed = (IM.toList (IM.union kept containers), in_streams, first + IM.size containers)
          | sourceXrefStream src = (changed, IM.empty, first)
          | otherwise = (changed, IM.empty, size)
    -- the objects in an object stream are not encrypted: the object
    -- stream is. See Note [Objects that are not encrypted] in
    -- Graphics.PDF.File.Crypt
    written <- case sourceEncryption src of
      Nothing -> return unencrypted
      Just (dec, _) -> mapM (\(n, (g, o)) -> do
                               r <- lift (encryptObject (randomBytes 16) dec n g o)
                               either throwError (\o' -> return (n, (g, o'))) r) unencrypted
    let trailer = M.unions
          [ M.fromList ([ (C.pack "Prev", Integer (sourceStartXref src)) ]
                        ++ [ (C.pack "Encrypt", e) | Just e <- [encrypt_value] ]
                        ++ [ (C.pack "ID", i) | Just i <- [file_id] ])
          , foldr M.delete new_trailer (C.pack "ID" : sectionKeys) ]
    return (render src number trailer written places deleted)
  where
    -- the keys of a cross-reference section: the update writes its own
    first_id v = case v of
      Just (Array (x : _)) -> Just x
      _ -> Nothing
    -- See Note [Incremental update]
    new_id old_id this_id can_add = case this_id of
      Just (Array [first, _]) | this_id == old_id -> do
        second <- lift (randomBytes 16)
        return (Just (Array [first, String second]))
      Nothing | can_add -> do
        first <- lift (randomBytes 16)
        return (Just (Array [String first, String first]))
      _ -> return this_id

-- | Does a signature forbid all changes of the file? A DocMDP signature
-- with /P 1: the /DocMDP of the /Perms of the catalog is a signature
-- dictionary, and its /Reference has the transform. Or a signed field
-- of the form whose /Lock has /P 1 (ISO 32000-2, 12.7.5.5).
-- See Note [Restrictions of an update]
certified_with_no_changes :: PdfFile -> Bool
certified_with_no_changes file = docmdp || locked
  where
    key k d = resolveValue file <$> dictLookup (C.pack k) d
    dict_of v = case resolveValue file v of
      Dict d -> Just d
      _ -> Nothing
    catalog = dictLookup (C.pack "Root") (pdfTrailer file) >>= dict_of
    no_changes d = key "P" d == Just (Integer 1)
    docmdp = or
      [ maybe False no_changes (dict_of t >>= dictLookup (C.pack "TransformParams") >>= dict_of)
      | Just perms <- [catalog >>= dictLookup (C.pack "Perms") >>= dict_of]
      , Just mdp <- [dictLookup (C.pack "DocMDP") perms >>= dict_of]
      , Just (Array refs) <- [key "Reference" mdp]
      , t <- refs
      , (dict_of t >>= key "TransformMethod") == Just (Name (C.pack "DocMDP")) ]
    locked = or
      [ True
      | Just form <- [catalog >>= dictLookup (C.pack "AcroForm") >>= dict_of]
      , Just (Array fields) <- [key "Fields" form]
      , Just field <- map dict_of fields
      , Just lock <- [dictLookup (C.pack "Lock") field >>= dict_of]
      , no_changes lock
      , Just _ <- [dictLookup (C.pack "V") field >>= dict_of] ]

-- | The bytes of the source and of the update.
-- See Note [Incremental update]
render :: PdfSource -> Int -> Dict -> [(Int, (Int, Object))] -> IM.IntMap (Int, Int) -> [(Int, Int)] -> L.ByteString
render src size trailer written in_streams deleted = L.fromStrict bytes <> separator <> renderObjects start written section
  where
    bytes = sourceBytes src
    -- "%%EOF" can be the last bytes of the file
    separator = if S.null bytes || C.last bytes `elem` "\r\n" then L.empty else L.singleton 0x0A
    start = fromIntegral (S.length bytes) + L.length separator
    base = fromIntegral (sourceBase src) :: Int64
    -- the free entries make a list from the entry 0 (PDF 32000-1, 7.5.4)
    -- each free entry gives the next one, and the last one gives 0
    free = zipWith (\(n, g) next -> (n, FreeEntry next g)) ((0, 65535) : deleted) (map fst deleted ++ [0])
    entries offsets = IM.fromList (free ++ [ (n, InUse (off - base) g) | ((n, (g, _)), off) <- zip written offsets ]
                                        ++ [ (n, InStream c i) | (n, (c, i)) <- IM.toList in_streams ])
    size_entry s = M.insert (C.pack "Size") (Integer (fromIntegral s))
    section offsets at
      | sourceXrefStream src =
          -- the stream is the last object: see 'xrefStream'
          let with_self = IM.insert size (InUse (at - base) 0) (entries offsets)
          in B.toLazyByteString (xrefStream size trailer with_self <> end_of at)
      | otherwise =
          B.toLazyByteString (xrefTable (entries offsets) <> B.string7 "trailer\n" <> renderDict (size_entry size trailer)
                              <> B.string7 "\n" <> end_of at)
    end_of at = B.string7 ("startxref\n" ++ show (at - base) ++ "\n%%EOF\n")

-- | A higher version of the new file in the /Version of the catalog.
-- See Note [Incremental update]
with_version :: (Int, Int) -> PdfFile -> Either String PdfFile
with_version old file
  | pdfVersion file == old = Right file
  | pdfVersion file < old = Left "an update cannot lower the version of a file"
  | otherwise = case dictLookup (C.pack "Root") (pdfTrailer file) of
      Just (Ref n g) | Just (g', Plain (Dict catalog)) <- IM.lookup n (pdfObjects file), g == g' ->
        let (major, minor) = pdfVersion file
            version = Name (C.pack (show major ++ "." ++ show minor))
            catalog' = if maybe True (< pdfVersion file) (dictLookup (C.pack "Version") catalog >>= name_version)
                         then M.insert (C.pack "Version") version catalog else catalog
        in Right file { pdfVersion = old, pdfObjects = IM.insert n (g, Plain (Dict catalog')) (pdfObjects file) }
      _ -> Left "the catalog of the file is not a dictionary: it cannot have a /Version"
  where
    name_version v = case v of
      Name s | (major, rest) <- C.span isDigit s, Just ('.', minor) <- C.uncons rest
             , not (S.null major), not (S.null minor), S.length major <= 2, S.length minor <= 2, C.all isDigit minor ->
        Just (read (C.unpack major), read (C.unpack minor))
      _ -> Nothing

-- | New numbers for the new objects whose numbers the file uses: for an
-- object that the reader removed. See Note [Incremental update]
renumber :: PdfSource -> PdfFile -> PdfFile
renumber src file
  | IM.null table = file
  | otherwise = file
      { pdfObjects = IM.fromList [ (rename n, (g, if IM.lookup n old == Just (g, o) then o else on_object o))
                                 | (n, (g, o)) <- IM.toList (pdfObjects file) ]
      , pdfTrailer = fmap on_value (pdfTrailer file) }
  where
    old = pdfObjects (sourceFile src)
    -- a number of the file: in use (an object that the reader removed) or
    -- free. See Note [Incremental update]
    clashes = [ n | n <- IM.keys (pdfObjects file), n > 0, IM.notMember n old, n < sourceSize src ]
    first_free = maximum [sourceSize src, largestNumber file + 1]
    table = IM.fromList (zip clashes [first_free ..])
    rename n = IM.findWithDefault n n table
    on_object o = case o of
      Plain v -> Plain (on_value v)
      Stream d bytes -> Stream (fmap on_value d) bytes
    on_value v = case v of
      Ref n g -> Ref (rename n) g
      Array vs -> Array (map on_value vs)
      Dict d -> Dict (fmap on_value d)
      _ -> v

---------------------------------------------------------
-- |
-- License     : BSD-style
--
-- The reader of a PDF file: the cross-reference sections, the objects
-- and the object streams
---------------------------------------------------------
module Graphics.PDF.File.Parse
  ( Limits(..)
  , defaultLimits
  , parsePdfFile
  , parsePdfFileWith
  , parsePdfFileWithPassword
  , PdfSource(..)
  , parsePdfSource
  ) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Data.List (isInfixOf)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as SU
import Data.Char (isDigit)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Graphics.PDF.File.Crypt (Decryptor, decryptObject, decryptorPermissions, openEncryption)
import Graphics.PDF.File.Filter (decodeStream)
import Graphics.PDF.File.Lexer
import Graphics.PDF.File.Object
import Graphics.PDF.File.ObjectStream (objectStreamNumbers, objectStreamObjects)

-- Note [Limits of the reader]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The reader is total: a file that is not correct gives 'Left', never an
-- exception. A small file also cannot make the reader use much memory or
-- time:
--
-- * a decoded stream (a cross-reference stream, an object stream) has a
--   size limit, and all the decoded streams of a file together have a
--   second limit ('limitTotal'). A cross-reference stream is decoded one
--   time, also when several sections give it (/XRefStm);
-- * all the cross-reference sections together have a limit of entries
--   ('limitObjects'). This limit is checked before the entries of a
--   section are made: an entry uses about 150 bytes of memory, and a
--   cross-reference stream of a few KB can give millions of entries;
-- * the nesting of arrays and dictionaries has a depth limit;
-- * each count of a file (entries of a cross-reference table, objects of
--   an object stream) is checked against the size of the data before it
--   is used, and each number of a file is checked before it becomes an
--   'Int';
-- * the objects of an object stream do not overlap
--   (Note [Objects of an object stream] in Graphics.PDF.File.ObjectStream),
--   and names and strings are decoded in linear time
--   (Note [Linear decoding of names and strings] in Graphics.PDF.File.Lexer).

-- | The limits of the reader. See Note [Limits of the reader]
data Limits = Limits
  { limitStream :: !Int -- ^ Bytes of one decoded stream
  , limitTotal :: !Int -- ^ Bytes of all the decoded streams of a file
  , limitObjects :: !Int -- ^ Entries of all the cross-reference sections of a file
  , limitDepth :: !Int -- ^ Depth of the nesting of arrays and dictionaries
  , limitRepair :: !Bool
    -- ^ Repair a file whose cross-reference sections are not correct,
    -- with a scan of the file. False: such a file gives 'Left'. See
    -- Note [Repair of a file]
  } deriving (Eq, Show)

-- | 256 MiB for a stream, 512 MiB for all the decoded streams, 2 million
-- cross-reference entries (about 300 MB of memory), a depth of 256, and
-- the repair of a file
defaultLimits :: Limits
defaultLimits = Limits (256 * 1024 * 1024) (512 * 1024 * 1024) 2000000 256 True

-- | What a file can still use. See Note [Limits of the reader]
data Budget = Budget
  { budgetBytes :: !Int -- ^ Decoded bytes
  , budgetEntries :: !Int -- ^ Cross-reference entries
  }

-- | Use some entries of the budget
use_entries :: Int -> Budget -> Either String Budget
use_entries n b
  | n <= budgetEntries b = Right b { budgetEntries = budgetEntries b - n }
  | otherwise = Left too_many_entries

-- | The limit of the next decoded stream
stream_limit :: Limits -> Budget -> Int
stream_limit limits b = min (limitStream limits) (budgetBytes b)

-- | Use some decoded bytes of the budget
use_bytes :: Int -> Budget -> Budget
use_bytes n b = b { budgetBytes = budgetBytes b - n }

-- | Where an object is: the cross-reference entry of an object number
data Entry
  = Free
  | InFile !Int !Int -- ^ Offset in the file and generation
  | InStream !Int !Int -- ^ Number of the object stream, and index in it
  deriving (Eq, Show)

-- | One cross-reference section: its entries, its trailer, the offset of
-- the previous section, and the offset of a cross-reference stream
-- (/XRefStm of a hybrid file)
data Section = Section !(M.Map Int Entry) !Dict !(Maybe Integer) !(Maybe Integer)

-- | Read a PDF file with the default limits. An encrypted file is read if
-- its user password is empty.
parsePdfFile :: S.ByteString -> Either String PdfFile
parsePdfFile = parsePdfFileWith defaultLimits

-- | Read a PDF file. An encrypted file is read if its user password is
-- empty.
-- See Note [Order of the cross-reference sections] and Note [Objects that
-- the reader removes]
parsePdfFileWith :: Limits -> S.ByteString -> Either String PdfFile
parsePdfFileWith limits = parsePdfFileWithPassword limits T.empty

-- | Read a PDF file, and decrypt it with a password if it is encrypted:
-- the owner password or the user password. The 'PdfFile' is not
-- encrypted: it has no /Encrypt. 'pdfPermissions' has the permissions of
-- the file when the user password opened it; the reader does not apply
-- them, and 'renderPdfFile' writes a file that is not encrypted.
-- See Note [Standard security handler] and Note [Permissions of a
-- decrypted file] in Graphics.PDF.File.Crypt
parsePdfFileWithPassword :: Limits -> T.Text -> S.ByteString -> Either String PdfFile
parsePdfFileWithPassword limits password bytes = sourceFile <$> parsePdfSource limits password bytes

-- | A file that the reader read, with what an incremental update of the
-- file needs. See Note [Incremental update] in Graphics.PDF.File.Update
data PdfSource = PdfSource
  { sourceFile :: !PdfFile -- ^ The objects of the file
  , sourceBytes :: !S.ByteString -- ^ The bytes of the file
  , sourceStartXref :: !Integer -- ^ The offset of the newest cross-reference section, as the file gives it
  , sourceBase :: !Int -- ^ The base of the offsets. See Note [Garbage before the header]
  , sourceXrefStream :: !Bool -- ^ The newest cross-reference section is a stream
  , sourceSize :: !Int -- ^ The number after the numbers of the cross-reference sections (in use or free)
  , sourceEncryption :: !(Maybe (Decryptor, Value)) -- ^ The key of an encrypted file, and the /Encrypt of its trailer
  , sourceRepaired :: !Bool -- ^ The reader repaired the file: see Note [Repair of a file]
  }

-- | Read a PDF file for an incremental update, and decrypt it with a
-- password if it is encrypted. See 'parsePdfFileWithPassword'
parsePdfSource :: Limits -> T.Text -> S.ByteString -> Either String PdfSource
parsePdfSource limits password bytes = prefix_error $ do
  (version, header) <- find_header bytes
  let budget = Budget (limitTotal limits) (limitObjects limits)
      -- the structure of the file: see Note [Repair of a file]
      structure = do
        start <- find_startxref bytes
        let read_with base = do
              r@(entries, trailers, _) <- read_sections limits bytes base budget start
              check_structure bytes entries trailers
              Right r
        -- See Note [Garbage before the header]
        ((entries, trailers, budget'), base) <- case read_with 0 of
          Left e | header > 0 -> either (const (Left e)) (\r -> Right (r, header)) (read_with header)
          r -> (\x -> (x, 0)) <$> r
        -- the section that "startxref" gives was read: its offset is in the file
        (is_table, _) <- runParser (optionalKeyword "xref") bytes (fromInteger start + base)
        Right (Sections start base (not is_table), entries, trailers, budget')
      -- See Note [Repair of a file]
      repaired = do
        scan <- scan_file limits bytes budget
        source_of limits password bytes version (Repaired (scan_containers scan)) (scan_entries scan)
                  (scan_trailers scan) (scan_budget scan)
  case structure of
    Right (origin, entries, trailers, budget') -> source_of limits password bytes version origin entries trailers budget'
    Left e | limitRepair limits, not (any (`isInfixOf` e) limit_errors) ->
      either (\r -> Left (e ++ " (and the repair: " ++ r ++ ")")) Right repaired
    Left e -> Left e
  where
    prefix_error = either (Left . ("PDF: " ++)) Right

-- | The errors of the limits of the reader: such a file is not repaired.
-- See Note [Repair of a file]
limit_errors :: [String]
limit_errors =
  [ too_many_entries, "a cross-reference stream is too large"
  , "a stream is larger than the limit" -- Graphics.PDF.File.Filter
  , "the value is nested too deep" ] -- Graphics.PDF.File.Lexer

too_many_entries :: String
too_many_entries = "the file has too many cross-reference entries"

-- | The structure that the cross-reference sections give is correct: the
-- trailer has a /Root, each offset is the one of its object ("n g obj"),
-- and each object of an object stream has the entry of an object stream
-- of the file. See Note [Repair of a file]
check_structure :: S.ByteString -> M.Map Int Entry -> [Dict] -> Either String ()
check_structure bytes entries trailers = do
  unless (any (\t -> case dictLookup (C.pack "Root") t of
                      Just (Ref _ _) -> True
                      _ -> False) trailers) $
    Left "the trailer has no /Root"
  forM_ (M.toList entries) $ \(n, e) -> case e of
    InFile off g -> case runParser header bytes off of
      Right ((n', g'), _) | n' == fromIntegral n && g' == fromIntegral g -> Right ()
      _ -> Left ("the object at the offset " ++ show off ++ " is not the object " ++ show n ++ " " ++ show g)
    InStream c _ -> case M.lookup c entries of
      Just (InFile _ _) -> Right ()
      _ -> Left ("the object " ++ show c ++ " is not an object stream of the file")
    Free -> Right ()
  where
    header = do
      n <- integerToken
      g <- integerToken
      keyword "obj"
      return (n, g)

-- | Where the entries of a file come from: the cross-reference sections
-- (the offset of the newest one, as the file gives it, the base of the
-- offsets, and a section that is a stream), or a scan of the file (the
-- object streams that it found). See Note [Repair of a file]
data Origin = Sections !Integer !Int !Bool | Repaired ![(Int, Int)]

-- | The file of the entries and of the trailers (the newest first)
source_of :: Limits -> T.Text -> S.ByteString -> (Int, Int) -> Origin -> M.Map Int Entry -> [Dict] -> Budget
          -> Either String PdfSource
source_of limits password bytes version origin entries0 trailers budget = do
  let trailer = foldr (M.unionWith (\newer _ -> newer)) M.empty trailers
      has_root t = case dictLookup (C.pack "Root") t of
        Just (Ref _ _) -> True
        _ -> False
      repaired = case origin of
        Repaired _ -> True
        Sections {} -> False
  -- a repaired file can have its catalog in an object stream: see Note
  -- [Repair of a file]
  unless (has_root trailer || repaired) $ Left "the trailer has no /Root"
  decryption <- case dictLookup (C.pack "Encrypt") trailer of
    Nothing -> Right Nothing
    Just e -> do
      (number, d) <- encryption_dict e
      case dictLookup (C.pack "Root") trailer of
        Just (Ref r _) | Just r == number -> Left "the /Encrypt of the trailer is the catalog"
        _ -> Right ()
      let file_id = case dictLookup (C.pack "ID") trailer of
            Just (Array (String s : _)) -> s
            _ -> S.empty
          no_password = if T.null password then (++ " (no password was given)") else id
      dec <- either (Left . no_password) Right (openEncryption d file_id password)
      Right (Just (dec, number, e))
  (entries, budget') <- case origin of
    Repaired containers -> with_object_streams limits bytes budget ((\(dec, _, _) -> dec) <$> decryption) containers entries0
    Sections {} -> Right (entries0, budget)
  objects <- load_objects limits bytes budget' entries ((\(dec, n, _) -> (dec, n)) <$> decryption)
  root <- case dictLookup (C.pack "Root") trailer of
    Just r@(Ref _ _) -> Right r
    _ -> case [ Ref n g | (n, (g, Plain (Dict d))) <- IM.toDescList objects
                        , dictLookup (C.pack "Type") d == Just (Name (C.pack "Catalog")) ] of
      r : _ -> Right r
      [] -> Left "the trailer has no /Root, and the file has no catalog"
  let encryption_object n = maybe False (\(_, number, _) -> number == Just n) decryption
      file = PdfFile
        { pdfVersion = version
        , pdfTrailer = M.insert (C.pack "Root") root (foldr M.delete trailer (map C.pack ("Encrypt" : xref_keys)))
        , pdfObjects = IM.filterWithKey (\n (_, o) -> not (removed o || encryption_object n)) objects
        , pdfPermissions = decryption >>= \(dec, _, _) -> decryptorPermissions dec
        }
      (start, base, stream) = case origin of
        Sections st b x -> (st, b, x)
        Repaired _ -> (0, 0, False)
  Right PdfSource
    { sourceFile = file
    , sourceBytes = bytes
    , sourceStartXref = start
    , sourceBase = base
    , sourceXrefStream = stream
    -- the /Size of the trailer can be larger than the numbers of the
    -- file: a new object would get a number that readers refuse
    , sourceSize = maybe 1 ((+ 1) . fst) (M.lookupMax entries)
    , sourceEncryption = (\(dec, _, e) -> (dec, e)) <$> decryption
    , sourceRepaired = repaired
    }
  where
    xref_keys = ["Size", "Prev", "XRefStm", "Type", "W", "Index", "Length", "Filter", "DecodeParms", "DL"]
    -- the /Encrypt dictionary: direct, or an object of the file that is not
    -- encrypted. See Note [Objects that are not encrypted] in Graphics.PDF.File.Crypt
    --
    -- The reader removes the /Encrypt object: it must not be an object of
    -- the document (the catalog, a page). It has no /Type, or /Type /Encrypt
    encryption_dict e = case e of
      Dict d -> Right (Nothing, d)
      Ref n g | Just (InFile off g') <- M.lookup n entries0, g == g' -> do
        (n', g'', object) <- read_object limits bytes (S.length bytes) (const Nothing) off
        case object of
          Plain (Dict d) | n' == n && g'' == g
                         , dictLookup (C.pack "Type") d `elem` [Nothing, Just (Name (C.pack "Encrypt"))] -> Right (Just n, d)
          _ -> Left "the /Encrypt of the trailer is not an encryption dictionary"
      _ -> Left "the /Encrypt of the trailer is not correct"

-- Note [Repair of a file]
-- ~~~~~~~~~~~~~~~~~~~~~~~
-- When the structure of a file is not correct, the reader finds the
-- objects with a scan of the file, as poppler and qpdf do ('limitRepair'):
-- no "startxref", cross-reference sections that cannot be read, no /Root,
-- an offset that is not the one of its object ("n g obj"), or an object
-- of an object stream that is not in the file ('check_structure'). An
-- error of the limits, of a value of an object (the newest version of an
-- object is not correct: the repair would give an older one), of a
-- stream or of the decryption is not repaired.
--
--
-- * each "n g obj" is an object. The scan reads it, and goes on after it:
--   the data of a stream ("5 0 obj" in a content stream) gives no object.
--   A /Length that is a reference is read in the objects "n g obj
--   <integer> endobj" of the file. The data of a stream with no correct
--   /Length ends at the next
--   "endstream", and the value of an object or of a trailer ends at the
--   next "n g obj" (or the next "trailer"): the scan reads each byte one
--   time, also in a file made to be read many times. A value with "n g
--   obj" in a string is thus lost. For a number that comes again, the object that is the last in the
--   file wins: an update comes after the file;
-- * the trailers are the "trailer" dictionaries outside of the objects,
--   and the cross-reference streams. The trailer that is the last in the
--   file wins. With no /Root, the /Type /Catalog of the largest number
--   is the root, also in an object stream;
-- * the objects of the object streams are objects of the file, when the
--   object stream comes after the object with the same number. Their
--   numbers are read after the key of an encrypted file is known: an
--   object stream is encrypted. The decoded data and the entries count in
--   the limits of the reader;
-- * the error of a file that the scan cannot read is the error of its
--   cross-reference sections, then the error of the scan.
--
-- The repair does not change the objects: a reference to an object that
-- the file lost (a truncated file) is null, also in the /Kids of the
-- pages (qpdf removes such pages). An object that an update deleted
-- comes back. 'sourceRepaired' tells it:
-- an incremental update of a repaired file is not possible, as its
-- /Prev would give a section that is not correct.

-- | What the scan of a file finds. See Note [Repair of a file]
data Scan = Scan
  { scan_entries :: !(M.Map Int Entry)
  , scan_trailers :: ![Dict] -- ^ The newest first
  , scan_containers :: ![(Int, Int)] -- ^ The object streams: their numbers and their offsets, in the order of the file
  , scan_budget :: !Budget
  }

-- | The objects and the trailers of a file, from a scan of the file.
-- See Note [Repair of a file]
scan_file :: Limits -> S.ByteString -> Budget -> Either String Scan
scan_file limits bytes budget0 = do
  (entries, spans, xref_dicts, containers, budget) <- go 0 M.empty IM.empty [] [] budget0
  let -- a "trailer" in the data of an object is not a trailer
      outside p = case IM.lookupLE p spans of
        Just (_, e) -> p >= e
        Nothing -> True
      trailer_starts = occurrences (C.pack "trailer") 0
      -- a trailer ends before the next trailer and the next object
      next_object p = maybe (S.length bytes) fst (IS.minView (snd (IS.split p candidates)))
      trailers = [ (p, d) | (p, next) <- zip trailer_starts (drop 1 trailer_starts ++ [S.length bytes]), outside p
                          , Right (Dict d, _) <- [runParser (keyword "trailer" >> valueWith (limitDepth limits))
                                                            (S.take (min next (next_object p)) bytes) p] ]
      newest_first = map snd (reverse (merge_by_offset trailers (reverse xref_dicts)))
      -- the object streams whose entries are the scanned ones (the last
      -- object with their numbers)
      latest = [ (c, off) | (c, off) <- containers, Just (InFile o _) <- [M.lookup c entries], o == off ]
  when (M.null entries) $ Left "the file has no object"
  Right (Scan entries newest_first latest budget)
  where
    -- the search of "endstream" of a stream with no correct /Length stops
    -- at the next "endstream" of the file: the scan stays linear, also
    -- with many streams that have no "endstream"
    endstreams = IS.fromList (occurrences (C.pack "endstream") 0)
    stream_bound s = maybe s (+ 9) (IS.lookupGE s endstreams)
    -- the starts of the "n g obj" of the file: the value of an object
    -- ends at the next one, thus each byte is read one time at most, also
    -- for objects that cannot be read
    candidates = IS.fromList [ s | p <- occurrences (C.pack "obj") 0, Just s <- [object_start p] ]
    value_end s = maybe (S.length bytes) id (IS.lookupGT s candidates)
    -- the objects that are an integer: the /Length of a stream can be a
    -- reference to one, also after the stream. The last one wins
    lengths = IM.fromList [ (fromInteger n, v) | s <- IS.toAscList candidates
                                               , Right ((n, v), _) <- [runParser integer_only (S.take (value_end s) bytes) s]
                                               , n >= 1, n < 2 ^ (31 :: Int), v >= 0 ]
    integer_only = do
      n <- integerToken
      _ <- integerToken
      keyword "obj"
      v <- integerToken
      keyword "endobj"
      return (n, v)
    -- the offsets of a pattern, from an offset
    occurrences pat from = case S.breakSubstring pat (S.drop from bytes) of
      (before, rest) | S.null rest -> []
                     | otherwise -> let p = from + S.length before in p : occurrences pat (p + 1)
    merge_by_offset xs [] = xs
    merge_by_offset [] ys = ys
    merge_by_offset (x : xs) (y : ys)
      | fst x <= fst y = x : merge_by_offset xs (y : ys)
      | otherwise = y : merge_by_offset (x : xs) ys
    -- the objects, from an offset; the spans of the objects; the
    -- dictionaries of the cross-reference streams (the last first); the
    -- object streams
    go pos entries spans xrefs containers budget =
      case S.breakSubstring (C.pack "obj") (S.drop pos bytes) of
        (before, rest) | S.null rest -> Right (entries, spans, xrefs, reverse containers, budget)
                       | otherwise ->
          let p = pos + S.length before
          in case object_start p of
               Nothing -> go (p + 3) entries spans xrefs containers budget
               Just s -> case read_object_end limits bytes (value_end s) (stream_bound s) (`IM.lookup` lengths) s of
                 Right (n, g, o, end) | end > p -> do
                   budget' <- use_entries 1 budget
                   let is_type t d = dictLookup (C.pack "Type") d == Just (Name (C.pack t))
                       xrefs' = case o of
                         Stream d _ | is_type "XRef" d -> (s, d) : xrefs
                         _ -> xrefs
                       containers' = case o of
                         Stream d _ | is_type "ObjStm" d -> (n, s) : containers
                         _ -> containers
                   go end (M.insert n (InFile s g) entries) (IM.insert s end spans) xrefs' containers' budget'
                 _ -> go (p + 3) entries spans xrefs containers budget
    -- "n g obj": the offset of n, from the offset of "obj". The reading
    -- of the object checks the keyword "obj"
    object_start p = do
      a <- spaces_before (p - 1)
      (g_start, _) <- digits_before a 5
      b <- spaces_before (g_start - 1)
      (n_start, _) <- digits_before b 10
      if n_start == 0 || not (regular (n_start - 1)) then Just n_start else Nothing
    -- the last offset of spaces that end at an offset, and one at least
    spaces_before i
      | i >= 0 && is_space (SU.unsafeIndex bytes i) =
          let j = last (takeWhile (\k -> k >= 0 && is_space (SU.unsafeIndex bytes k)) [i, i - 1 .. max 0 (i - 64)])
          in Just (j - 1)
      | otherwise = Nothing
    -- the first offset of the digits that end at an offset, with a number of digits
    digits_before i most =
      let ds = takeWhile (\k -> k >= 0 && is_digit (SU.unsafeIndex bytes k)) [i, i - 1 .. max 0 (i - most)]
      in if null ds || length ds > most then Nothing else Just (last ds, length ds)
    is_space w = w == 0x20 || w == 0x0A || w == 0x0D || w == 0x09 || w == 0x0C || w == 0x00
    is_digit w = w >= 0x30 && w <= 0x39
    -- a byte of a name or a number, before "n"
    regular i = i >= 0 && i < S.length bytes &&
      let w = SU.unsafeIndex bytes i in not (is_space w) && w `S.notElem` C.pack "()<>[]{}/%"

-- | The entries of the objects of the object streams that a scan found:
-- the object stream must come after the object with the same number.
-- An object stream that cannot be read gives no object, but a limit of
-- the reader gives 'Left': the file would have older objects.
-- See Note [Repair of a file]
with_object_streams :: Limits -> S.ByteString -> Budget -> Maybe Decryptor -> [(Int, Int)] -> M.Map Int Entry
                    -> Either String (M.Map Int Entry, Budget)
with_object_streams limits bytes budget0 decryption containers entries0 = foldM add (entries0, budget0) containers
  where
    -- the decoded data and the entries count: see Note [Limits of the reader]
    add (entries, budget) (c, off) = case read_object limits bytes (S.length bytes) (const Nothing) off of
      -- the /N of the object stream, before its table: see Note [Limits of the reader]
      Right (_, _, Stream d _) | Just (Integer n) <- dictLookup (C.pack "N") d
                               , n > fromIntegral (budgetEntries budget) -> Left too_many_entries
      Right (_, g, o) | Right o' <- maybe (Right o) (\dec -> decryptObject dec c g o) decryption ->
        case objectStreamNumbers (stream_limit limits budget) c o' of
          Right (numbers, size) -> do
            let new = [ (n, i) | (n, i) <- numbers, older n off entries ]
            budget' <- use_entries (length new) (use_bytes size budget)
            Right (foldl (\es (n, i) -> M.insert n (InStream c i) es) entries new, budget')
          -- an object stream that a limit stops would give older objects
          Left e | any (`isInfixOf` e) limit_errors -> Left e
          Left _ -> Right (entries, budget)
      _ -> Right (entries, budget)
    -- the entry of a number is older than an offset
    older n off es = case M.lookup n es of
      Nothing -> True
      Just (InFile o _) -> o < off
      Just (InStream c _) -> case M.lookup c es of
        Just (InFile o _) -> o < off
        _ -> True
      Just Free -> True

-- Note [Objects that the reader removes]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- 'PdfFile' is the document, and not the structure of the file. Thus the
-- reader removes the objects that only give the structure:
--
-- * the cross-reference streams (/Type /XRef) and the object streams
--   (/Type /ObjStm): the objects of an object stream are objects of the
--   document;
-- * the linearization dictionary (/Linearized): its offsets are wrong in
--   a file that is written again;
-- * the free entries;
-- * the /Encrypt dictionary of an encrypted file: the objects are
--   decrypted.
--
-- The writer then gives a file with a cross-reference table
-- (Graphics.PDF.File.Write).

removed :: Object -> Bool
removed (Stream d _) = dictLookup (C.pack "Type") d `elem` [Just (Name (C.pack "XRef")), Just (Name (C.pack "ObjStm"))]
removed (Plain (Dict d)) = M.member (C.pack "Linearized") d
removed _ = False

-- Note [Garbage before the header]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Some files have bytes before "%PDF-" (for example a mail header). The
-- offsets of such a file can count from the start of the file, or from
-- the header. Thus the reader reads the cross-reference sections with the
-- offsets as they are, then, if this fails, with the position of the
-- header added. The base that reads the sections is then the base of all
-- the offsets of the file: of the objects, and of their bounds
-- ('load_objects').

-- | The version of the header, and its position
find_header :: S.ByteString -> Either String ((Int, Int), Int)
find_header bytes =
  let (before, rest) = S.breakSubstring (C.pack "%PDF-") (S.take 1024 bytes)
      (major, after) = C.span isDigit (S.drop 5 rest)
      (minor, _) = C.span isDigit (S.drop 1 after)
  in if S.null rest then Left "the file has no %PDF- header"
     else if S.null major || C.take 1 after /= C.pack "." || S.null minor || S.length major > 2 || S.length minor > 2
       then Left "the version of the header is not correct"
       else Right ((read (C.unpack major), read (C.unpack minor)), S.length before)

-- | The offset after the last "startxref" of the file, as the file gives it
find_startxref :: S.ByteString -> Either String Integer
find_startxref bytes = do
  let from = max 0 (S.length bytes - 2048)
      end = S.drop from bytes
      key = C.pack "startxref"
      positions = go 0 end
      go k s = case S.breakSubstring key s of
        (b, r) | S.null r -> []
               | otherwise -> (k + S.length b) : go (k + S.length b + 1) (S.drop (S.length b + 1) s)
  case positions of
    [] -> Left "the file has no startxref"
    _ -> fst <$> runParser integerToken bytes (from + last positions + S.length key)

-- | An offset of the file, with the base of the offsets.
-- See Note [Garbage before the header]
offset_in_file :: S.ByteString -> Int -> Integer -> Either String Int
offset_in_file bytes base n
  | n >= 0 && n + fromIntegral base < fromIntegral (S.length bytes) = Right (fromInteger n + base)
  | otherwise = Left ("an offset is out of the file: " ++ show n)

-- Note [Order of the cross-reference sections]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- A file that was updated has several cross-reference sections: the
-- "startxref" gives the newest one, and each one gives the previous one
-- with /Prev. The entry of an object number in the newest section is the
-- correct one, also when it is a free entry: an update can delete an
-- object. The trailers are merged the same way.
--
-- An offset that comes again is a loop of /Prev, and the file is not
-- correct.

-- | Read the cross-reference sections from the newest one. The offsets of
-- the entries are offsets in the file.
read_sections :: Limits -> S.ByteString -> Int -> Budget -> Integer
              -> Either String (M.Map Int Entry, [Dict], Budget)
read_sections limits bytes base budget0 start = go IS.empty IM.empty (Just start) M.empty [] budget0
  where
    go _ _ Nothing entries trailers budget = Right (entries, reverse trailers, budget)
    go visited streams (Just n) entries trailers budget = do
      offset <- offset_in_file bytes base n
      when (IS.member offset visited) $ Left "the cross-reference sections make a loop"
      (is_table, _) <- runParser (optionalKeyword "xref") bytes offset
      (Section section trailer prev _, streams', budget') <-
        if is_table then do
          (table, b) <- read_table limits bytes base budget offset
          hybrid table streams b
        else do
          (stream_section, b) <- read_xref_stream limits bytes base budget offset
          Right (stream_section, streams, b)
      -- the newer entries are already in the map
      go (IS.insert offset visited) streams' prev (M.union entries section) (trailer : trailers) budget'
    -- See Note [Hybrid files]
    hybrid table@(Section entries trailer prev xrefstm) streams budget = case xrefstm of
      Nothing -> Right (table, streams, budget)
      Just n -> do
        at <- offset_in_file bytes base n
        (stream_entries, streams', budget') <- case IM.lookup at streams of
          Just known -> Right (known, streams, budget)
          Nothing -> do
            (Section e _ _ _, b) <- read_xref_stream limits bytes base budget at
            Right (e, IM.insert at e streams, b)
        let (used, free) = M.partition (/= Free) entries
            merged = M.unions [used, stream_entries, free]
        -- the merged entries are entries of this section
        budget'' <- use_entries (M.size stream_entries) budget'
        Right (Section merged trailer prev Nothing, streams', budget'')

-- Note [Hybrid files]
-- ~~~~~~~~~~~~~~~~~~~
-- A hybrid file has a cross-reference table for the readers of PDF 1.4,
-- and a cross-reference stream (/XRefStm in the trailer of the table) with
-- the objects that are in object streams. The table gives them as free.
-- Thus the entries of the stream are merged with the entries of the
-- table: an entry in use of the table wins, then an entry of the stream,
-- then a free entry of the table.
--
-- A cross-reference stream is decoded one time, also when several tables
-- give it (Note [Limits of the reader]).

read_table :: Limits -> S.ByteString -> Int -> Budget -> Int -> Either String (Section, Budget)
read_table limits bytes base budget0 offset = do
  ((entries, trailer, budget), _) <- runParser table bytes offset
  Right (Section entries trailer (integer_key "Prev" trailer) (integer_key "XRefStm" trailer), budget)
  where
    table = do
      keyword "xref"
      subsections M.empty budget0
    subsections acc budget = do
      done <- optionalKeyword "trailer"
      if done
        then do
          v <- valueWith (limitDepth limits)
          case v of
            Dict d -> return (acc, d, budget)
            _ -> failAt "the trailer is not a dictionary"
        else do
          first <- integerToken
          count <- integerToken
          remaining <- (S.length bytes -) <$> getPos
          -- an entry has 5 bytes at least ("0 0 n")
          unless (first >= 0 && count >= 0 && first + count <= 2 ^ (31 :: Int) && count * 5 <= fromIntegral remaining) $
            failAt "a subsection of the cross-reference table is not correct"
          budget' <- either failAt return (use_entries (fromInteger count) budget)
          entries <- forM [0 .. count - 1] $ \i -> do
            off <- integerToken
            gen <- integerToken
            skipBlanks
            kind <- regularToken
            unless (off >= 0 && off < fromIntegral (S.length bytes) && gen >= 0 && gen <= 65535) $
              failAt "an entry of the cross-reference table is not correct"
            entry <- if kind == C.pack "n" then return (in_file base (fromInteger off) (fromInteger gen))
                     else if kind == C.pack "f" then return Free
                     else failAt "an entry of the cross-reference table is not n or f"
            return (fromInteger (first + i), entry)
          -- the first entry of an object number in a section is kept
          subsections (M.union acc (M.fromListWith (\_ old -> old) entries)) budget'

-- Note [Objects at the offset 0]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- The offset 0 of a file is the header, and no object can be there. Some
-- writers give such an entry for an object that they did not write: for
-- example pdfunite, for the fields of a signed file. The reader takes
-- these entries as free, as poppler does: a reference to the object then
-- gives null.

-- | An entry in use, at an offset that the file gives.
-- See Note [Objects at the offset 0] and Note [Garbage before the header]
in_file :: Int -> Int -> Int -> Entry
in_file _ 0 _ = Free
in_file base off g = InFile (off + base) g

integer_key :: String -> Dict -> Maybe Integer
integer_key key d = case dictLookup (C.pack key) d of
  Just (Integer n) -> Just n
  _ -> Nothing

-- | A cross-reference stream (PDF 32000-1, 7.5.8)
read_xref_stream :: Limits -> S.ByteString -> Int -> Budget -> Int -> Either String (Section, Budget)
read_xref_stream limits bytes base budget offset = do
  (_, _, object) <- read_object limits bytes (S.length bytes) (const Nothing) offset
  (dict, raw) <- case object of
    Stream d r | dictLookup (C.pack "Type") d == Just (Name (C.pack "XRef")) -> Right (d, r)
    _ -> Left ("there is no cross-reference section at the offset " ++ show offset)
  widths <- case dictLookup (C.pack "W") dict of
    Just (Array [Integer a, Integer b, Integer c]) | all (\w -> w >= 0 && w <= 8) [a, b, c] ->
      Right (fromInteger a, fromInteger b, fromInteger c)
    _ -> Left "the /W of a cross-reference stream is not correct"
  size <- case integer_key "Size" dict of
    Just n | n >= 0 && n <= 2 ^ (31 :: Int) -> Right n
    _ -> Left "the /Size of a cross-reference stream is not correct"
  ranges <- case dictLookup (C.pack "Index") dict of
    Nothing -> Right [(0, size)]
    Just (Array xs) -> pairs xs
    Just _ -> Left "the /Index of a cross-reference stream is not correct"
  let row = (\(a, b, c) -> a + b + c) widths
      rows = sum (map snd ranges)
  when (row == 0 || rows * fromIntegral row > fromIntegral (stream_limit limits budget)) $
    Left "a cross-reference stream is too large"
  -- each row is an entry: see Note [Limits of the reader]
  budget' <- use_entries (fromInteger rows) budget
  decoded <- decodeStream (stream_limit limits budget') dict raw
  when (S.length decoded < fromInteger rows * row) $
    Left "a cross-reference stream has less data than its /Index gives"
  let numbers = concat [ [first .. first + count - 1] | (first, count) <- ranges ]
      (w1, w2, w3) = widths
      -- the three fields of a line, big-endian
      fields i = let line = S.drop (i * row) decoded
                 in ( big_endian (S.take w1 line)
                    , big_endian (S.take w2 (S.drop w1 line))
                    , big_endian (S.take w3 (S.drop (w1 + w2) line)) )
      -- a field of width 0 has its default value: type 1, generation 0
      entry (t, b, c) = case (if w1 == 0 then 1 else t) of
        0 -> Right (Just Free)
        1 | b < fromIntegral (S.length bytes) && c <= 65535 -> Right (Just (in_file base (fromInteger b) (if w3 == 0 then 0 else fromInteger c)))
          | otherwise -> Left "an entry of a cross-reference stream is out of the file"
        2 | b < 2 ^ (31 :: Int) && c < 2 ^ (31 :: Int) -> Right (Just (InStream (fromInteger b) (fromInteger c)))
          | otherwise -> Left "an entry of a cross-reference stream is not correct"
        -- an unknown type is ignored (PDF 32000-1, 7.5.8.3)
        _ -> Right Nothing
  -- one map, with no list of the entries: see Note [Limits of the reader]
  entries <- foldM (\m (i, n) -> do
                      e <- entry (fields i)
                      -- the first entry of an object number is kept
                      Right (maybe m (\x -> M.insertWith (\_ old -> old) (fromInteger n) x m) e))
                   M.empty (zip [0 ..] numbers)
  Right (Section entries dict (integer_key "Prev" dict) Nothing, use_bytes (S.length decoded) budget')
  where
    pairs (Integer a : Integer b : rest)
      | a >= 0 && b >= 0 && a + b <= 2 ^ (31 :: Int) = ((a, b) :) <$> pairs rest
    pairs [] = Right []
    pairs _ = Left "the /Index of a cross-reference stream is not correct"
    -- a field has 8 bytes at most: an Integer, with no overflow
    big_endian :: S.ByteString -> Integer
    big_endian = S.foldl' (\a w -> a * 256 + fromIntegral w) 0

-- Note [Length of a stream]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~
-- The /Length of a stream can be a reference to an integer object, which
-- can come after the stream (HPDF writes such files), or which can be in
-- an object stream (pdfTeX writes such files). Thus the reader loads the
-- objects in two steps:
--
-- 1. the object streams, whose /Length must be an integer or a reference
--    to "n g obj <integer>" in the file;
-- 2. the other objects, whose /Length can also be a reference to an
--    integer of an object stream.
--
-- A reference is followed one time, with no other reference: a /Length
-- cannot make a loop.
--
-- The keyword "stream" is followed by an end of line, then the data. The
-- reader also accepts spaces before this end of line, as poppler does.
--
-- A length is used only if "endstream" follows the data. Otherwise (no
-- length, a reference that cannot be read, a wrong length), the reader
-- looks for "endstream" between the start of the data and the next
-- object of the file ('read_object'), and removes the end of line before
-- it.

-- | Read the object at an offset: its number, its generation and its
-- value. The second argument is the end of the object (the next offset
-- of the file); the third gives the integer value of an object number,
-- for a /Length that is a reference.
--
-- See Note [Length of a stream]
read_object :: Limits -> S.ByteString -> Int -> (Int -> Maybe Integer) -> Int
            -> Either String (Int, Int, Object)
read_object limits bytes bound length_of offset =
  (\(n, g, o, _) -> (n, g, o)) <$> read_object_end limits bytes (S.length bytes) bound length_of offset

-- | 'read_object', and the offset after the value or after "endstream".
-- The first offset is the end of the value: the scan of a file reads a
-- value only to the next object. See Note [Repair of a file]
read_object_end :: Limits -> S.ByteString -> Int -> Int -> (Int -> Maybe Integer) -> Int
                -> Either String (Int, Int, Object, Int)
read_object_end limits bytes value_end bound length_of offset = do
  let header = do
        n <- integerToken
        g <- integerToken
        keyword "obj"
        v <- valueWith (limitDepth limits)
        after <- getPos
        return (n, g, v, after)
  ((number, generation, v, after), _) <- runParser header (S.take value_end bytes) offset
  unless (number >= 1 && number < 2 ^ (31 :: Int) && generation >= 0 && generation <= 65535) $
    Left ("the object at the offset " ++ show offset ++ " has a wrong number")
  (object, end) <- case v of
    Dict d -> do
      (is_stream, start) <- runParser (optionalKeyword "stream") bytes after
      if not is_stream then Right (Plain v, after) else do
        let data_start = skip_eol (skip_spaces start)
            limit = max data_start (min bound (S.length bytes))
            from_length = case dictLookup (C.pack "Length") d of
              Just (Integer n) -> Just n
              Just (Ref r _) -> length_of r
              _ -> Nothing
            checked = case from_length of
              Just n | n >= 0 && n <= fromIntegral (S.length bytes - data_start)
                     , ends_with_endstream (data_start + fromInteger n) -> Just (fromInteger n)
              _ -> Nothing
        (content, data_end) <- case checked of
          Just n -> Right (S.take n (S.drop data_start bytes), data_start + n)
          Nothing ->
            let window = S.take (limit - data_start) (S.drop data_start bytes)
                (before, found) = S.breakSubstring (C.pack "endstream") window
            in if S.null found then Left ("the stream at the offset " ++ show offset ++ " has no endstream")
               else Right (strip_eol before, data_start + S.length before)
        Right ( Stream (M.insert (C.pack "Length") (Integer (fromIntegral (S.length content))) d) content
              , either (const data_end) snd (runParser (optionalKeyword "endstream") bytes data_end) )
    _ -> Right (Plain v, after)
  Right (fromInteger number, fromInteger generation, object, end)
  where
    -- "stream" can have spaces before its end of line
    skip_spaces i = if byte i == Just 0x20 || byte i == Just 0x09 then skip_spaces (i + 1) else i
    skip_eol i
      | byte i == Just 0x0D && byte (i + 1) == Just 0x0A = i + 2
      | byte i == Just 0x0A || byte i == Just 0x0D = i + 1
      | otherwise = i
    byte i = if i >= 0 && i < S.length bytes then Just (SU.unsafeIndex bytes i) else Nothing
    ends_with_endstream i = case runParser (optionalKeyword "endstream") bytes i of
      Right (True, _) -> True
      _ -> False
    strip_eol s
      | C.pack "\r\n" `S.isSuffixOf` s = S.take (S.length s - 2) s
      | C.pack "\n" `S.isSuffixOf` s || C.pack "\r" `S.isSuffixOf` s = S.take (S.length s - 1) s
      | otherwise = s

-- | The integer of "n g obj <integer>" at an offset, and nothing else.
-- See Note [Length of a stream]
integer_object :: S.ByteString -> Int -> Maybe Integer
integer_object bytes at = case runParser (integerToken >> integerToken >> keyword "obj" >> integerToken) bytes at of
  Right (n, _) | n >= 0 && n <= fromIntegral (S.length bytes) -> Just n
  _ -> Nothing

-- | Load the objects of the entries, and decrypt them: the key, and the
-- number of the /Encrypt object, which is not encrypted. The offsets of
-- the entries are offsets in the file.
-- See Note [Length of a stream] for the order, and Note [Objects that are
-- not encrypted] in Graphics.PDF.File.Crypt
load_objects :: Limits -> S.ByteString -> Budget -> M.Map Int Entry -> Maybe (Decryptor, Maybe Int)
             -> Either String (IM.IntMap (Int, Object))
load_objects limits bytes budget entries decryption = do
  let in_streams = M.fromListWith (++) [ (c, [(n, i)]) | (n, InStream c i) <- M.toList entries, n > 0 ]
  -- 1. the object streams: a /Length in the file only. An object stream is
  -- decrypted, and its objects are not
  containers <- fmap IM.fromList . forM (M.keys in_streams) $ \c -> case M.lookup c entries of
    Just (InFile off g) -> (\o -> (c, (g, o))) <$> (load c off g in_file_length >>= decrypt c g)
    _ -> Left ("the object " ++ show c ++ " is not an object stream of the file")
  (compressed, _) <- foldM (load_container containers) (IM.empty, budget) (M.toList in_streams)
  -- 2. the other objects: a /Length in the file or in an object stream
  let any_length r = case M.lookup r entries of
        Just (InStream _ _) -> case IM.lookup r compressed of
          Just (_, Plain (Integer n)) | n >= 0 -> Just n
          _ -> Nothing
        _ -> in_file_length r
  loaded <- fmap IM.fromList . forM [ (n, off, g) | (n, InFile off g) <- M.toList entries, n > 0 ] $ \(n, off, g) ->
    case IM.lookup n containers of
      Just known -> Right (n, known)
      Nothing -> (\o -> (n, (g, o))) <$> (load n off g any_length >>= decrypt n g)
  Right (IM.union loaded compressed)
  where
    decrypt n g o = case decryption of
      Just (dec, encryption) | encryption /= Just n -> decryptObject dec n g o
      _ -> Right o
    load n off g length_of = do
      (n', g', obj) <- read_object limits bytes (bound off) length_of off
      if n' == n && g' == g then Right obj
        else Left ("the object at the offset " ++ show off ++ " is not the object " ++ show n ++ " " ++ show g)
    -- the end of an object: the next offset of the file
    offsets = IS.fromList (S.length bytes : [ o | InFile o _ <- M.elems entries ])
    bound o = maybe (S.length bytes) id (IS.lookupGT o offsets)
    in_file_length r = case M.lookup r entries of
      Just (InFile o _) -> integer_object bytes o
      _ -> Nothing
    -- See Note [Objects of an object stream] in Graphics.PDF.File.ObjectStream
    load_container containers (acc, b) (container, wanted) = do
      object <- maybe (Left ("the object " ++ show container ++ " is not an object stream")) (Right . snd)
                      (IM.lookup container containers)
      (objects, size) <- objectStreamObjects (stream_limit limits b) (limitDepth limits) container object wanted
      Right (IM.union acc objects, use_bytes size b)

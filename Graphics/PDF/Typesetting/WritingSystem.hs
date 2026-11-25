---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Font
---------------------------------------------------------
module Graphics.PDF.Typesetting.WritingSystem(
      WritingSystem(..)
    , mapToSpecialGlyphs
) where 

import qualified Data.Text as T
import Graphics.PDF.LowLevel.Types
import qualified Text.Hyphenation as H
import Data.List(intersperse)
import Data.Char 
import Data.List(unfoldr)

data WritingSystem = Latin H.Hyphenator
                   | UnknownWritingSystem


myWords' :: T.Text -> Maybe (T.Text, T.Text)
myWords' l  | T.null l = Nothing
            | otherwise = if T.null h then Just (h', t') else Just (T.singleton ' ', t)
    where 
        (h, t) = T.span isSpace l
        (h', t') = T.span (not . isSpace) l
 
 
-- | Split a sentence into words keeping the space but shortening them to 1 space
myWords :: T.Text -> [T.Text]     
myWords l = concatMap onlyWord . unfoldr myWords' $ l 
 where
  onlyWord s = 
     let (w,p) = T.span isAlpha s in
     case (T.null w,T.null p) of
         (True,True) -> []
         (False,True) -> [w]
         (True,False) -> [p]
         (False,False) -> [w,p]

addHyphens :: H.Hyphenator -> T.Text -> T.Text
addHyphens hn f =
    T.concat . map (T.concat . intersperse (T.pack "/-") . hyphenate) . myWords $ f
  where
    hyphenate = fmap T.pack . H.hyphenate hn . T.unpack

-- Note [Soft hyphen]
-- ~~~~~~~~~~~~~~~~~~
-- The soft hyphen U+00AD is not a character to show: it is a place where a
-- line can break. Between two characters of a text in a Latin writing
-- system it becomes a 'BreakingHyphen', as the breaks that the hyphenation
-- finds. In all other positions it is removed.
--
-- Example: "extra\173ordinary" is set as "extraordinary", or as "extra-"
-- and "ordinary" on two lines.

-- | See Note [Soft hyphen]
mapToSpecialGlyphs :: WritingSystem -> T.Text -> [SpecialChar]
mapToSpecialGlyphs UnknownWritingSystem theText = 
  let getBreakingGlyphs ('\xAD':l) = getBreakingGlyphs l
      getBreakingGlyphs (' ':l) = NormalSpace:getBreakingGlyphs l 
      getBreakingGlyphs (a:l) = NormalChar a:getBreakingGlyphs l 
      getBreakingGlyphs [] = []
  in getBreakingGlyphs (T.unpack theText)
mapToSpecialGlyphs (Latin hn) theText =
  let getBreakingGlyphs [] = []
      getBreakingGlyphs (a:'/':'-':d:l) = (NormalChar a):BreakingHyphen:getBreakingGlyphs (d:l)
      getBreakingGlyphs (a:'\xAD':d:l) | a /= ' ' && a /= '\xAD' && d /= '\xAD' = (NormalChar a):BreakingHyphen:getBreakingGlyphs (d:l)
      getBreakingGlyphs ('\xAD':l) = getBreakingGlyphs l
      getBreakingGlyphs (',':' ':l) = NormalChar ',':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs (';':' ':l) = NormalChar ';':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs ('.':' ':l) = NormalChar '.':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs (':':' ':l) = NormalChar ':':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs ('!':' ':l) = NormalChar '!':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs ('?':' ':l) = NormalChar '?':BiggerSpace:getBreakingGlyphs l
      getBreakingGlyphs (' ':l) = NormalSpace:getBreakingGlyphs l
      getBreakingGlyphs (a:l) = NormalChar a:getBreakingGlyphs l
  in getBreakingGlyphs (T.unpack . addHyphens hn $ theText)

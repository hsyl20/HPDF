---------------------------------------------------------
-- |
-- Copyright   : (c) 2006-2016, alpheccar.org
-- License     : BSD-style
--
-- Maintainer  : misc@NOSPAMalpheccar.org
-- Stability   : experimental
-- Portability : portable
--
-- PDF Actions
---------------------------------------------------------

module Graphics.PDF.Action(
   -- * Actions
   -- ** Types
     Action
   , GoToURL(..)
   -- ** Functions
 ) where
     
import Graphics.PDF.LowLevel.Types
import Network.URI 


--  Media action
--data MediaAction = Play
--                 | Stop
--                 | Pause
--                 | Resume
--                 deriving(Enum)

class PdfObject a => Action a

-- | Action of going to an URL
newtype GoToURL = GoToURL URI

--data Rendition = Rendition
--instance PdfObject Rendition where
--  toPDF a = toPDF . dictFromList $
--                    [ entry "Type" (PDFName $ "Rendition")
--                    , entry "S" (PDFName $ "MR")
--                    , entry "C" movie
--                    ]
--    where
--        movie = dictFromList $
--               [ entry "Type" (PDFName $ "MediaClip")
--               , entry "S" (PDFName $ "MCD")
--               , entry "CT" (toPDFString $ "video/3gpp")
--               , entry "D" (toPDFString "17.3gp")
--               ]

--  Action to control a media
--data ControlMedia = ControlMedia MediaAction Int (PDFReference Rendition)
    
urlToPdfString :: URI -> AsciiString 
urlToPdfString uri = 
    let s = uriToString id uri "" 
    in
    toAsciiString s


instance PdfObject GoToURL where
    toPDF (GoToURL s) = toPDF . dictFromList $
                         [ entry "Type" (PDFName $ "Action")
                         , entry "S" (PDFName "URI")
                         , entry "URI" (urlToPdfString s)
                         ]
instance Action GoToURL

instance PdfLengthInfo GoToURL where


--instance PdfObject ControlMedia where
--    toPDF (ControlMedia operation relatedScreenAnnotation rendition) = toPDF . dictFromList $
--                         [ entry "Type" (PDFName $ "Action")
--                         , entry "S" (PDFName "Rendition")
--                         , entry "R" rendition
--                         , entry "OP" (PDFInteger $ fromEnum operation)
--                         , entry "AN" ((PDFReference relatedScreenAnnotation :: PDFReference AnyPdfObject))
--                         ]
--                         
--instance Action ControlMedia

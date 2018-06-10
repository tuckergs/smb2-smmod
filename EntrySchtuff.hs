

module EntrySchtuff where

import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Word
import System.IO



data Entry = Entry { getStgID :: Word16 , getDifficulty :: Word16 }

type EntryList = [Entry]


------ IMPORTANT CONSTANTS ------

startOfStoryArea :: Integer
startOfStoryArea = 0x20B448

endOfStoryArea :: Integer
endOfStoryArea = 0x20B5D8

sizeOfRel :: Integer
sizeOfRel = 3000268


------ ENTRY SCHTUFF ------

writeEntry :: Handle -> Entry -> IO ()
writeEntry handle (Entry stgID diff) = 
  BB.hPutBuilder handle $ BB.word16BE stgID <> BB.word16BE diff
  

writeAllEntries :: Handle -> EntryList -> IO ()
writeAllEntries handle = mapM_ $ writeEntry handle

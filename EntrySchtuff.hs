

module EntrySchtuff where

import Data.Bits
import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Word
import System.IO

import Types

------ IMPORTANT CONSTANTS ------

startOfStoryArea :: Integer
startOfStoryArea = 0x20B448

endOfStoryArea :: Integer
endOfStoryArea = 0x20B5D8

sizeOfRel :: Integer
sizeOfRel = 3000268

------ ENTRY SCHTUFF ------

writeEntry :: Handle -> EntryType -> Entry -> IO ()
writeEntry handle Vanilla (Entry stgID diff _) = 
  BB.hPutBuilder handle $ BB.word16BE stgID <> BB.word16BE diff
writeEntry handle New (Entry stgID diff time) =
  let diffStgIDWord = (diff `shiftL` 12) .|. stgID
  in BB.hPutBuilder handle $ BB.word16BE diffStgIDWord <> BB.word16BE time
  
writeAllEntries :: Handle -> EntryType -> EntryList -> IO ()
writeAllEntries = curry $ mapM_ . uncurry writeEntry 

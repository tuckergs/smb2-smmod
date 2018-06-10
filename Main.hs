
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import System.IO

import HexStuff
import ParseMonad

import CodeSchtuff
import ConfigSchtuff
import EntrySchtuff

main = do
  args <- getArgs
  when (length args /= 3) $
    die "Usage: ./Main [input REL] [level order config] [output REL]"
  let (inFileName:configFileName:outFileName:_) = args

  -- Read config
  (allEntries,timeSharingSlots) <- readConfig configFileName

  -- Prepare to write REL
  inFile <- openFile inFileName ReadMode
  outFile <- openFile outFileName WriteMode
  hSetBinaryMode inFile True
  hSetBinaryMode outFile True
  hSetBuffering outFile (BlockBuffering Nothing)
  let cpByte = hGetChar inFile >>= hPutChar outFile
      cpBytes ls = (mapM_ (const cpByte) ls) >> hFlush outFile
  
  -- Copy bytes
  hPutStrLn stderr "Copying up to the code that specifies the slots that share max times..."
  cpBytes [1..startOfTimeSharingSlotsCodeArea]

  -- Write code for time sharing slots
  hPutStrLn stderr "Writing code for the time sharing slots..."
  writeTimeSharingSlotsCode inFile outFile timeSharingSlots

  -- Copy more bytes
  hPutStrLn stderr "Copying up to story entry area..."
  cpBytes [endOfTimeSharingSlotsCodeArea..startOfStoryArea-1]

  -- Write story entries
  hPutStrLn stderr "Writing story mode entries..."
  writeAllEntries outFile allEntries
  hSeek inFile AbsoluteSeek endOfStoryArea

  -- Copy the rest of the bytes
  hPutStrLn stderr "Copying the rest of the bytes..."
  cpBytes [endOfStoryArea..sizeOfRel-1]

  -- Done!
  hPutStrLn stderr "Done!"
  hClose inFile
  hClose outFile


module CodeSchtuff where

import Data.Bits
import qualified Data.ByteString.Builder as BB
import Data.Word
import System.IO

import Types

------ IMPORTANT CONSTANTS ------

startOfTimeSharingSlotsCodeArea = 0xa52dc
endOfTimeSharingSlotsCodeArea = 0xa52f4


------ CODE SCHTUFF ------

modCmp :: Word32 -> Word16 -> Word32
modCmp inst imm = (inst .&. 0xFFFF0000) .|. (fromIntegral $ imm)

modBc :: Word32 -> Op -> Word32
modBc inst op = (inst .&. 0xFC00FFFF) .|. (opToBits op)

modLi :: Word32 -> Word16 -> Word32
modLi = modCmp

writeTimeSharingSlotsCode :: Handle -> Handle -> (Op,Word16,Word16,Word16) -> IO ()
writeTimeSharingSlotsCode inFile outFile (op,cmpNum,time2,normalTime) = do
  let cpBytes = mapM_ $ const $ hGetChar inFile >>= hPutChar outFile
  BB.hPutBuilder outFile $ BB.word32BE $ modCmp 0x2C000000 cmpNum
  BB.hPutBuilder outFile $ BB.word32BE $ modBc 0x40000008 op
  hSeek inFile RelativeSeek 8
  cpBytes [1..4]
  BB.hPutBuilder outFile $ BB.word32BE $ modLi 0x38600000 time2
  hSeek inFile RelativeSeek 4
  cpBytes [1..4]
  BB.hPutBuilder outFile $ BB.word32BE $ modLi 0x38600000 normalTime
  hSeek inFile RelativeSeek 4


module Types where

import Data.Word


data Entry = Entry { getStgID :: Word16 , getDifficulty :: Word16 , getTime :: Word16 }

type EntryList = [Entry]

data EntryType = Vanilla | New
  deriving Eq

data Op = LessThan | GreaterThan | LessThanEqual | GreaterThanEqual | Equal | NotEqual | AL

opToBits :: Op -> Word32
opToBits GreaterThanEqual = 0x00800000
opToBits LessThanEqual = 0x00810000
opToBits NotEqual  = 0x00820000
opToBits LessThan  = 0x01800000
opToBits GreaterThan  = 0x01810000
opToBits Equal  = 0x01820000
opToBits AL  = 0x02800000


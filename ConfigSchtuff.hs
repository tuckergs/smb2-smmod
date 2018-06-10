
{-# LANGUAGE LambdaCase #-}

module ConfigSchtuff (readConfig) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.Monoid
import Data.Char
import Data.List
import Data.Word
import System.Exit
import System.IO

import ParseMonad

import CodeSchtuff
import EntrySchtuff


comment = (ws <|> (ws >> token '%' >> list item)) >> return Nothing

parseNum :: Read a => Parse String a
parseNum = fmap read $ (list1 $ spot $ isDigit) <|> (tokens "0x" >> fmap ("0x"++) (list1 $ spot $ isHexDigit))

parseName :: Parse String String
parseName = do
  c <- spot isAlpha
  cs <- list $ spot isAlphaNum
  return (c:cs)


data NormalLine = TimeSharingSlots (Op,Word16,Word16,Word16) | BeginWorld Int

parseNormalLine :: Parse String (Maybe NormalLine)
parseNormalLine = comment <|> timeSharingSlotsLine <|> beginWorldLine
  where
    timeSharingSlotsLine = do
      ws
      tokens "#timeSharingSlots"
      ws1
      tokens "if"
      ws1
      token '('
      ws
      op <- 
        (tokens "==" >> return Equal)
        <|> (tokens "!=" >> return NotEqual)
        <|> (tokens ">=" >> return GreaterThanEqual)
        <|> (tokens "<=" >> return LessThanEqual)
        <|> (token '>' >> return GreaterThan)
        <|> (token '<' >> return LessThan)
      ws
      cmpNum <- parseNum 
      ws
      token ')'
      ws1
      tokens "then"
      ws1
      time2 <- parseNum
      normalTime <- ((comment >> return 3600) <|>) $ do  -- The normal timer defaults to 60 seconds
        ws1
        tokens "else"
        ws1
        parseNum >>= ((comment >>) . return)
      return $ Just $ TimeSharingSlots (op,cmpNum,time2,normalTime)
    beginWorldLine = do
      ws
      tokens "#beginWorld"
      ws1
      wrldNum <- parseNum
      comment
      return $ Just $ BeginWorld wrldNum

parseEntryLine :: Parse String (Maybe (Maybe Entry))
parseEntryLine = comment <|> entryLine <|> endWorldLine
  where 
    endWorldLine = do
      ws
      tokens "#endWorld"
      comment
      return $ Just Nothing
    entryLine = do
      ws
      stgID <- parseNum
      ws1
      diff <- parseNum
      comment
      return $ Just $ Just $ Entry stgID diff
  

readConfig :: String -> IO (EntryList,(Op,Word16,Word16,Word16))
readConfig cfgFileName = do
  cfgFile <- openFile cfgFileName ReadMode
  allLns <- fmap lines $ hGetContents cfgFile
  rt <- parseConfig allLns 
  hClose cfgFile
  return rt

data ConfigLoopRecord = CLR { getLineNum :: Int ,
                              getLines :: [String] ,
                              getEntryLists :: [(Int,EntryList)] ,
                              getTmpEntryList :: EntryList ,
                              getWorldNumMaybe :: Maybe Int ,
                              getTimeSharingSlotsMaybe :: Maybe (Op,Word16,Word16,Word16)
                            }

parseConfig :: [String] -> IO (EntryList,(Op,Word16,Word16,Word16))
parseConfig allLns = 
  let
    initRecord = CLR { getLineNum = 1 ,
                        getLines = allLns ,
                        getEntryLists = [] ,
                        getTmpEntryList = [] ,
                        getWorldNumMaybe = Nothing ,
                        getTimeSharingSlotsMaybe = Nothing
                     }
  in 
    flip fix initRecord $ \loop curRecord -> do
      let curLineNum = getLineNum curRecord
          curLns = getLines curRecord
          curEntryLists = getEntryLists curRecord
          curTmpEntryList = getTmpEntryList curRecord
          curWorldNumMaybe = getWorldNumMaybe curRecord
          curTimeSharingSlotsMaybe = getTimeSharingSlotsMaybe curRecord
      case curLns of
        [] -> do
          when (length curEntryLists /= 10) $ 
            die $ "You didn\'t specify all ten worlds in your config"
          let cmpPairs (n1,_) (n2,_) = compare n1 n2
              sortedEntryLists = sortBy cmpPairs curEntryLists -- Sort by world number
              allEntries = concat $ map snd sortedEntryLists   -- Put entries in one list
              timeSharingSlots = case curTimeSharingSlotsMaybe of
                Nothing -> (Equal,30,1800,3600)                -- By default, Melting Pot has 30 seconds while every other has 60 seconds
                Just tss -> tss
          return (allEntries,timeSharingSlots)
        (ln:lns) -> do
          let nextRecord = curRecord { getLineNum = curLineNum + 1, getLines = lns }
              err = die $ "Error on line " ++ (show curLineNum)
              err2 msg = die $ "Line " ++ (show curLineNum) ++ ": " ++ msg
          case curWorldNumMaybe of
            Nothing -> do -- We are parsing normal lines
              case parse parseNormalLine ln of
                Left _ -> err                                             -- Error
                Right Nothing -> loop nextRecord                          -- Comment
                Right (Just (TimeSharingSlots tss)) ->                       -- Saw #timeSharingSlots
                  loop $ nextRecord { getTimeSharingSlotsMaybe = Just tss }
                Right (Just (BeginWorld wrldNum)) -> do                   -- Saw #beginWorld
                  let curWorldNums = map fst curEntryLists
                  when (not $ (1 <= wrldNum) && (wrldNum <= 10)) $
                    err2 $ "Each world number must be between 1 and 10"
                  when (elem wrldNum curWorldNums) $ do
                    err2 $ "World " ++ (show wrldNum) ++ " is defined at least twice"
                  loop $ nextRecord { getWorldNumMaybe = Just wrldNum ,
                                      getTmpEntryList = []
                                    }
            Just wrldNum -> do -- We are parsing an entry list
              case parse parseEntryLine ln of
                Left _ -> err                                                               -- Error
                Right Nothing -> loop nextRecord                                            -- Comment
                Right (Just (Just entry)) -> do                                             -- Saw an entry
                  let diff = getDifficulty entry
                  when (not $ (0 <= diff) && (diff <= 10)) $
                    err2 "Difficulty must be between 1 and 10"
                  loop $ nextRecord { getTmpEntryList = entry:curTmpEntryList }
                Right (Just Nothing) -> do                                                  -- Saw #endWorld
                  when (length curTmpEntryList /= 10) $ 
                    err2 $ "Each world must have 10 entries"
                  let fixedEntries = reverse curTmpEntryList
                  loop $ nextRecord { getTmpEntryList = [] ,
                                      getWorldNumMaybe = Nothing ,
                                      getEntryLists = (wrldNum,fixedEntries):curEntryLists
                                    }
                  
              

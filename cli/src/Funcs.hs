module Funcs
    ( funcsList
    , cat
    , echo
    , wc
    , pwd
    , exit
    ) where

import           Control.Monad    (forM, forM_, when)
import           Data.Foldable    (foldl')
import           Lib              (getContentIfExist)
import           System.Directory (getCurrentDirectory)
import           System.Exit      (exitSuccess)
import           System.IO        (readFile)
import           Types


funcsList :: [(FuncName, FuncType)]
funcsList = [ ("cat" , cat)
            , ("echo", echo)
            , ("wc"  , wc)
            , ("pwd" , pwd)
            , ("exit", exit) ]


-- cat ----------------------------------------------------------------------------------------------
cat :: FuncType
cat args mode =
  case mode of
-- for every arguments try to get content
    Normal -> do
      allContent <- forM args getContentIfExist
      return . init . unlines $ allContent
-- return input
    Pipe   -> do
      when (null args) $ putStrLn "null args cat"
      return . head $ args


-- echo ---------------------------------------------------------------------------------------------
echo :: FuncType
echo args _ = return . unwords $ args


-- wc -----------------------------------------------------------------------------------------------
wc :: FuncType
wc args mode =
  case mode of
-- for every arguments try to count statistics
    Normal    -> do
      statistics <- forM args $ \fileName -> do
        content <- getContentIfExist fileName
        if null content
          then return ([0, 0, 0], "")
          else let stat = countStat content
                in return (stat, unwords . (++ [fileName]) $ prettyPrint <$> stat)
      let (stat, resList) = unzip statistics
      return . init . unlines $ if length stat < 2
                           then resList
                           else let totalStat = foldl' (zipWith (+)) [0, 0, 0] stat
                                    total     = unwords . (++ ["total"]) $ prettyPrint <$> totalStat
                                 in resList ++ [total]
-- return statistics for input
    Pipe      -> if null args
                   then error "wc args"
                   else let stat@(x : xs) = countStat . head $ args
                         in return . unwords . fmap prettyPrint $ if x == 0 then 1 : xs else stat

-- get statistics by string
countStat :: String -> [Int]
countStat content =
  let line = length . filter (== '\n') $ content
      word = length . words $ content
      byte = length content
   in [line, word, byte]

-- formatting print
prettyPrint :: Int -> String
prettyPrint num =
  let numStr = show num
   in replicate (6 - length numStr) ' ' ++ numStr


-- pwd ----------------------------------------------------------------------------------------------
pwd :: FuncType
pwd _ _ = getCurrentDirectory


-- exit ---------------------------------------------------------------------------------------------
exit :: FuncType
exit _ mode =
  case mode of
-- exit from cli
    Normal -> do
      exitSuccess
      return ""
-- nothing
    Pipe   -> return ""


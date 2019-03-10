module Funcs
    ( funcsList
    , cat
    , echo
    , wc
    , pwd
    , exit
    , ls
    , cd
    ) where

import           Control.Monad     (forM, forM_, when)
import           Data.Foldable     (foldl')
import           Lib               (getContentIfExist)
import           System.Directory  (getCurrentDirectory,listDirectory,setCurrentDirectory)
import           System.Exit       (exitSuccess)
import           System.IO         (readFile)
import           Types
import           Data.List         (intercalate)

import           Control.Exception (SomeException, try)

funcsList :: [(FuncName, FuncType)]
funcsList = [ ("cat" , cat)
            , ("echo", echo)
            , ("wc"  , wc)
            , ("pwd" , pwd)
            , ("exit", exit)
            , ("cd"  , cd)
            , ("ls"  , ls)]


-- cat ----------------------------------------------------------------------------------------------
cat :: FuncType
cat []   _    = return "cat: no arguments"
cat args mode =
  case mode of
-- for every arguments try to get content
    Normal -> do
      allContent <- forM args getContentIfExist
      return . init . unlines $ allContent
-- return input
    Pipe   -> return . head $ args


-- echo ---------------------------------------------------------------------------------------------
echo :: FuncType
echo args _ = return . (++ "\n") . unwords $ args

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
pwd _ _ = (++ "\n") <$> getCurrentDirectory


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

-- cd -----------------------------------------------------------------------------------------------
cd :: FuncType
cd [] _ = return ""
cd (arg:_) _ = do
    res <- try (setCurrentDirectory arg) :: IO (Either SomeException ())
    case res of
        Left ex -> return $ "Unable to set new current directory: " ++ arg ++ ".\n"
        Right _ -> return ""

-- ls -----------------------------------------------------------------------------------------------
ls :: FuncType
ls [] mode = do
  curE <- try getCurrentDirectory :: IO (Either SomeException String)
  case curE of
    Left ex -> return $ "Current directory doesn't exists.\n"
    Right cur -> ls [cur] mode

ls (arg:_) _ = do
  list <- try (listDirectory arg) :: IO (Either SomeException [String])
  pure $ lsDir list

lsDir (Left _) = "Unable to get the list of files.\n"
lsDir (Right list) = intercalate "\n" list ++ "\n"

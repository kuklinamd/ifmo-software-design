module Lib
    ( getContentIfExist
    ) where


import           System.Directory (doesFileExist)


-- check existance of file and return empty string or its content
getContentIfExist :: FilePath -> IO String
getContentIfExist fileName = do
  fileEx <- doesFileExist fileName
  if fileEx
    then readFile fileName
    else putStrLn ("Error file path: " ++ fileName) >> return ""

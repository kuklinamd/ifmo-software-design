module GrepArguments
  ( parseOpts
  , Flag (..)
  ) where

import           Control.Monad         (when, unless)
import           System.Console.GetOpt (OptDescr(..), getOpt, ArgDescr(..), ArgOrder(..), usageInfo)
import           System.Directory      (doesFileExist)

import           Exit


data Flag = NonCase             -- -i
          | HoleWord            -- -w
          | PrintLines String   -- -A n
  deriving (Show, Eq, Ord)


options :: [OptDescr Flag]
options =
  [ Option ['i'] [] (NoArg NonCase)        ""
  , Option ['w'] [] (NoArg HoleWord)       ""
  , Option ['A'] [] (ReqArg PrintLines "") ""              
  ]

parseOpts :: [String] -> IO ([Flag], String, [String])
parseOpts argv =
  case getOpt Permute options argv of
    (flags, (pattern : files), []) -> return (flags, pattern, files)
    (_, _, errs)                   -> exitFailFlag $ "exitFailFlag"


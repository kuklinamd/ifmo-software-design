module Exit
  ( exitSucc
  , exitFailArgsNum
  , exitFailFlag
  , exitFailPath
  , exitFailReadBook
  , exitFailMarker
  , exitFailMarkerVal
  ) where

import System.Exit (exitWith, ExitCode(..), die)


exitSucc :: IO a
exitSucc          = exitWith ExitSuccess

exitFailArgsNum :: IO a
exitFailArgsNum   = die "Error number of files!"

exitFailFlag :: String -> IO a     
exitFailFlag      = die

exitFailPath :: String -> IO a 
exitFailPath file = die $ "File " ++ file ++ " is not exist!"

exitFailReadBook :: String -> IO a 
exitFailReadBook  = die

exitFailMarker :: IO a 
exitFailMarker    = die "You enter wrong chapter number!"

exitFailMarkerVal :: IO a 
exitFailMarkerVal = die "You enter not existence chapter!"


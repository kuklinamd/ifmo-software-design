module Main where

import           Control.Monad.State (runState)
import           Interpreter         (interpret)
import           Parser              (handlingInput)
import           System.IO           (BufferMode (..), hSetBuffering, stdout)
import           Types


-- call main loop with empty environment
main :: IO ()
main = mainLoop []


mainLoop :: Env -> IO ()
mainLoop env = do
  hSetBuffering stdout NoBuffering  
  putStr "cli> "
  -- get user input
  input <- getLine
  -- handling input end return func to execution or new environment
  let (maybeFunc, env') = runState (handlingInput input) env
  -- execute function (if it function)
  traverse interpret maybeFunc
  mainLoop env'


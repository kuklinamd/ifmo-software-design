module Interpreter
    ( interpret
    , interpretTest
    ) where

import           Funcs               (funcsList)
import           System.Process      (readProcess)
import           Types


-- check funcs list and if current funcion in it, call custom funcion;
-- otherwise - call Process
call :: FuncInfo -> Mode -> IO String
call (name, args) mode =
  case lookup name funcsList of
    Just func -> func args mode
    Nothing   -> readProcess name args ""


-- interpret funcion or pipe
interpret :: [FuncInfo] -> IO ()
interpret []                = putStrLn "Empty pipe"
-- first function in pipe execute in `normal` mode
-- others - with mode `pipe`
interpret (funcInfo : rest) = call funcInfo Normal >>= go rest
  where
    go :: [FuncInfo] -> String -> IO ()
    go []                       output = putStrLn output
    go (fi@(name, args) : rest) output
-- funcion from pipe ignore output if has custom arguments
      | null args = call (name, [output]) Pipe >>= go rest
      | otherwise = call fi Pipe >>= go rest


-- helpful function for testing in test/Spec.hs
interpretTest :: [FuncInfo] -> IO String
interpretTest []                = return "Empty pipe"
interpretTest (funcInfo : rest) = call funcInfo Normal >>= go rest
  where
    go :: [FuncInfo] -> String -> IO String
    go []                       output = return output
    go (fi@(name, args) : rest) output
      | null args = call (name, [output]) Pipe >>= go rest
      | otherwise = call fi Pipe >>= go rest


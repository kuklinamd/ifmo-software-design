{-# LANGUAGE ScopedTypeVariables #-}

module Interpreter
    ( interpret
    , interpretTest
    ) where

import           Control.Exception
import           Funcs             (funcsList)
import           System.Process    (readProcess)
import           Types


-- check funcs list and if current funcion in it, call custom funcion;
-- otherwise - call Process
call :: FuncInfo -> Mode -> IO String
call (name, args) mode =
  case lookup name funcsList of
    Just func -> func args mode
    Nothing   -> catch
                   (readProcess name args "")
                   (\(e :: SomeException) -> return $ name ++ ": command not found\n")


-- interpret funcion or pipe
interpret :: [FuncInfo] -> IO ()
interpret []                = putStrLn "Empty pipe"
interpret [fi@("exit", _)]  = call fi Normal >>= putStr
interpret (funcInfo : rest) = call funcInfo Normal >>= go rest
  where
    go :: [FuncInfo] -> String -> IO ()
    go []                       output = putStr output
    go (fi@(name, args) : rest) output
   -- funcion from the pipe ignore output if has custom arguments
      | "exit" <- name = call fi Pipe >>= go rest
      | null args, "echo" <- name = call (name, []) Pipe >>= go rest
      | null args     = call (name, [output]) Pipe >>= go rest
      | "cat" <- name = call fi Normal >>= go rest
      | "wc" <- name  = call fi Normal >>= go rest
      | otherwise     = call fi Pipe >>= go rest


-- helpful function for testing in test/Spec.hs
interpretTest :: [FuncInfo] -> IO String
interpretTest []                = return "Empty pipe"
interpretTest [fi@("exit", _)]  = call fi Normal
interpretTest (funcInfo : rest) = call funcInfo Normal >>= go rest
  where
    go :: [FuncInfo] -> String -> IO String
    go []                       output = return output
    go (fi@(name, args) : rest) output
      | "exit" <- name = call fi Pipe >>= go rest
      | null args, "echo" <- name = call (name, []) Pipe >>= go rest
      | null args = call (name, [output]) Pipe >>= go rest
      | "cat" <- name = call fi Normal >>= go rest
      | "wc" <- name = call fi Normal >>= go rest
      | otherwise = call fi Pipe >>= go rest


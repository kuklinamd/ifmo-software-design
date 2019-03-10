{-# LANGUAGE TupleSections #-}

module Parser
    ( handlingInput
    , isMaybeDef
    , checkVarName
    , updateEnv
    , parseDefinition
    , parseSpecSymbols
    ) where

import           Control.Monad       (when)
import           Control.Monad.State (State (..), get, modify, runState)
import           Data.Maybe          (fromJust, isJust)
import           Funcs               (funcsList)
import           Safe                (headMay)
import           Types


-- "main loop" in handling input --------------------------------------------------------------------
handlingInput :: String -> State Env (Maybe [FuncInfo])
handlingInput []          = return Nothing
handlingInput spInputSp =
  -- remove leading and ending spaces
  let inputSp = snd . span (== ' ') $ spInputSp
      input   = reverse . snd . span (== ' ') . reverse $ inputSp
   in case isMaybeDef input of
    Just uncheckDef -> do
      def <- parseDefinition uncheckDef
      -- if is a `true` definition, modify environment
      when (isJust def) $ modify (updateEnv . fromJust $ def)
      return Nothing
    -- parse function if is not a defition
    Nothing         -> parseFunc input


-- work with definition -----------------------------------------------------------------------------
isMaybeDef :: String -> Maybe VarInfo
isMaybeDef ""    = Nothing
isMaybeDef input =
  -- сheck: there are symbols around =
  let (maybeDef, rest) = break (== ' ') input
      (var, eqPartValue) = break (== '=') maybeDef
   in case eqPartValue of
        "="               -> Nothing
        ('=' : partValue) -> Just (var, partValue ++ rest)
        _                 -> Nothing

checkVarName :: String -> Bool
checkVarName (x : xs) = and $ (x `elem` headSymb) : ((`elem` bodySymb) <$> xs)
  where
    headSymb = '_' : ['a'..'z'] ++ ['A'..'Z']
    bodySymb = headSymb ++ ['0'..'9']

-- parse the correctness of the definition
parseDefinition :: VarInfo -> State Env (Maybe VarInfo)
parseDefinition (var, value) =
  -- check the correctness of the variable name
  if checkVarName var
    then do
           -- substitude variables - in case of some args choose first
           maybeSubstedValue <- parseSpecSymbols value
           return $ (var,) <$> (maybeSubstedValue >>= headMay)
    else return Nothing

-- update environment
updateEnv :: VarInfo -> Env -> Env
updateEnv def@(var, _) env
  | null env  = [def]
  | otherwise =
      -- if such variable exists update its value, otherwise add new
      let (part, rest) = break (\(varEnv, _) -> varEnv == var) env
       in if null rest
            then def : part
            else part ++ (def : tail rest)


-- work with funcion --------------------------------------------------------------------------------
parseFunc :: String -> State Env (Maybe [FuncInfo])
parseFunc input =
  -- this function has argumens?
  let (nameStr, argsStr) = break (== ' ') input
   in do
        -- check variables in the function name
        name <- checkDollar nameStr
        if (null argsStr)
          -- return res for func without args
          then return . Just $ [(name, [])]
          -- parse special symbols in the argument string
          else do
            maybeArgs <- parseSpecSymbols argsStr
            return $ do
              args <- maybeArgs
              -- parse a pipe (in case of nothing we haven't the pipe)
              (firArgs, pipe) <- parsePipe args
              return $ if null pipe then [(name, args)] else (name, firArgs) : pipe


-- parse special symbols in the argument string -----------------------------------------------------
parseSpecSymbols :: String -> State Env (Maybe Args)
-- function hasn't arguments
parseSpecSymbols ""    = return . Just $ []
parseSpecSymbols input =
  -- check " '
  let (freePart, restSymb) = break (`elem` ['"', '\'']) input
   in case restSymb of
        -- " or ' were not found
        ""                 -> do
                                -- check variables in the arguments
                                dollarPart <- checkDollar freePart
                                return . Just . words $ dollarPart
        -- " was found
        ('"' : restQuote)  -> let (partQuote, rest) = break (== '"') restQuote
                               in if null rest
                                   -- we didn't find second " and return Nothing
                                   then return Nothing
                                   -- otherwise check variables in `free` and quote-arguments
                                   else do
                                          maybeDollarPartF <- parseSpecSymbols freePart
                                          dollarPartQ      <- checkDollar partQuote
                                          maybeRestPart    <- parseSpecSymbols . tail $ rest
                                          return $ do
                                            dollarPartF <- maybeDollarPartF
                                            restPart    <- maybeRestPart
                                            return $ dollarPartF ++ (dollarPartQ : restPart)
        -- ' was found
        ('\'' : restApost) -> let (partApost, rest) = break (== '\'') restApost
                               in if null rest
                                    -- we didn't find second ' and return Nothing
                                    then return Nothing
                                    -- otherwise check variables in `free` arguments
                                    else do
                                           maybeDollarPart <- parseSpecSymbols freePart
                                           maybeRestPart   <- parseSpecSymbols . tail $ rest
                                           return $ do
                                             dollarPart <- maybeDollarPart
                                             restPart <- maybeRestPart
                                             return $ dollarPart ++ (partApost : restPart)


checkDollar :: String -> State Env String
checkDollar ""    = return ""
checkDollar input =
  let (freePart, restDollar) = break (== '$') input
   in if null restDollar
        -- if we haven't $
        then return freePart
        -- get variable name to substitude
        else let (partDollar, rest) = break (`elem` [' ', '\'']) . drop 1 $ restDollar
              in do
                   env <- get
                   restPart <- checkDollar rest
                   -- if variable wasn't found in the environment return empty string
                   return $ freePart ++ (maybe "" id $ lookup partDollar env) ++ restPart


-- work with pipe -----------------------------------------------------------------------------------
-- if pipe wasn't found return Nothing
parsePipe :: [String] -> Maybe ([String], [FuncInfo])
parsePipe input =
  let (args, rest) = break (== "|") input
   in if null rest
        then Just (args, [])
        else do
          funcInfoList <- go . tail $ rest
          return (args, funcInfoList)
  where
    go :: [String] -> Maybe [FuncInfo]
    go []            = Just []
    go ["|"]         = Nothing
    go (func : rest) = do
      (args, funcInfo) <- parsePipe rest
      return $ (func, args) : funcInfo


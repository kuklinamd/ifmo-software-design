module Types where


-- functions types ----------------------------------------------------------------------------------
type Args = [String]

-- mode of func execution
data Mode = Normal -- usual execution
          | Pipe   -- with arguments from the pipe
  deriving (Show, Eq, Ord)

type FuncName = String

type FuncInfo = (FuncName, Args)

type FuncType = Args -> Mode -> IO String


-- variables types ----------------------------------------------------------------------------------
type VarName = String

type VarValue = String

type VarInfo = (VarName, VarValue)

-- type of variables list
type Env = [VarInfo]


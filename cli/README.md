# cli

## How to get the executable file from Haskell code
 - Install stack
   - https://docs.haskellstack.org/en/stable/README/#how-to-install
 - Go to
   - /ifmo-software-design/cli
 - Open cmd and put
   - ```
     stack build
     stack ghc -- -O app/Main.hs src/Base.hs src/CustomFuncs.hs
     ```
 - Get the executable file and put it somewhere
   - app/.Main or app/Main.exe

## About architecture
 - all custom types in stc/Types
 - `mainLoop` is in app/Main
 - It calls `handlingInput` from src/Parser
   - `handlingInput` parses definitions, functions (and pipes) and replaces variables via other functions in src/Parser
 - After that it calls `interpreter` for functions and pipes from src/Interpreter
   - `interpreter` either calls custom functions or do extern call
   - custom functions called from stc/Funcs


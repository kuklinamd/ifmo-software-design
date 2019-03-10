import Test.Hspec

import Funcs
import Interpreter
import Parser
import Types
import Control.Monad.State (runState)


main :: IO ()
main = hspec $ do
-----------------------------------------------------------------------------------------------------
  describe "definition ->" $ do
    it "isMaybeDef: correct def" $ do
      isMaybeDef "qw=12" `shouldBe` Just ("qw", "12")
    it "isMaybeDef: correct def with \"" $ do
      isMaybeDef "qw=\"qwe asd\"" `shouldBe` Just ("qw", "\"qwe asd\"")
    it "parseDefinition: correct def with \"" $ do
      (fst $ runState (parseDefinition ("qw", "\"qwe asd\"")) []) `shouldBe` Just ("qw", "qwe asd")
    it "isMaybeDef: correct def with more then one args" $ do
      isMaybeDef "qw=qwe asd" `shouldBe` Just ("qw", "qwe asd")
    it "parseDefinition: correct def with \"" $ do
      (fst $ runState (parseDefinition ("qw", "$as qw")) [("as", "12")]) `shouldBe` Just ("qw", "12")
    it "isMaybeDef: uncorrect def" $ do
      isMaybeDef "%qw=12" `shouldBe` Just ("%qw", "12")
    it "checkVarName: uncorrect" $ do
      checkVarName "%qw" `shouldBe` False
    it "checkVarName: correct" $ do
      checkVarName "asds23" `shouldBe` True
    it "checkVarName: correct with _" $ do
      checkVarName "_23" `shouldBe` True
    it "updateEnv: empty env" $ do
      updateEnv ("_23", "12") [] `shouldBe` [("_23", "12")]
    it "updateEnv: hit env" $ do
      updateEnv ("_23", "wef") [("_23", "12")] `shouldBe` [("_23", "wef")]
    it "updateEnv: add env" $ do
      updateEnv ("qw", "12") [("_23", "wef")] `shouldBe` [("qw", "12"), ("_23", "wef")]
-----------------------------------------------------------------------------------------------------
  describe "funcs: normal ->" $ do
    it "cat: 1 file" $ do
      out <- cat ["test/ex.txt"] Normal
      out `shouldBe` "Some example text\n"
    it "cat: 2 files" $ do
      out <- cat ["test/ex.txt", "test/ex.txt"] Normal
      out `shouldBe` "Some example text\n\nSome example text\n"
    it "cat: 2 files + 1 error file" $ do
      out <- wc ["", "LICENSE", "test/ex.txt"] Normal
      out `shouldBe` "\n     2      6     37 LICENSE\n     1      3     18 test/ex.txt\n     3      9     55 total"
    it "wc: 1 file" $ do
      out <- wc ["test/ex.txt"] Normal
      out `shouldBe` "     1      3     18 test/ex.txt"
    it "wc: 2 files" $ do
      out <- wc ["test/ex.txt", "test/ex.txt"] Normal
      out `shouldBe` "     1      3     18 test/ex.txt\n     1      3     18 test/ex.txt\n     2      6     36 total"
    it "wc: 3 files" $ do
      out <- wc ["test/ex.txt", "LICENSE", "test/ex.txt"] Normal
      out `shouldBe` "     1      3     18 test/ex.txt\n     2      6     37 LICENSE\n     1      3     18 test/ex.txt\n     4     12     73 total"
    it "wc: 2 files + 1 error file" $ do
      out <- wc ["", "LICENSE", "test/ex.txt"] Normal
      out `shouldBe` "\n     2      6     37 LICENSE\n     1      3     18 test/ex.txt\n     3      9     55 total"
-----------------------------------------------------------------------------------------------------
  describe "funcs: pipe ->" $ do
    it "1 wc" $ do
      out <- interpretTest [("wc", ["test/ex.txt"])]
      out `shouldBe` "     1      3     18 test/ex.txt"
    it "cat and wc" $ do
      out <- interpretTest [("cat", ["test/ex.txt"]), ("wc", [])]
      out `shouldBe` "     1      3     18"
    it "cat and echo" $ do
      out <- interpretTest [("cat", ["test/ex.txt"]), ("echo", [""])]
      out `shouldBe` "\n"
    it "2 wc" $ do
      out <- interpretTest [("wc", ["test/ex.txt"]), ("wc", [])]
      out `shouldBe` "     1      4     32"
    it "3 wc" $ do
      out <- interpretTest [("wc", ["test/ex.txt"]), ("wc", []), ("wc", [])]
      out `shouldBe` "     1      3     20"
    it "echo and wc" $ do
      out <- interpretTest [("echo", ["123"]), ("wc", [])]
      out `shouldBe` "     1      1      4"
    it "echo and empty cat" $ do
      out <- interpretTest [("echo", ["hello"]), ("cat", [])]
      out `shouldBe` "hello\n"
    it "echo and cat" $ do
      out <- interpretTest [("echo", ["hello"]), ("cat", ["test/ex.txt"])]
      out `shouldBe` "Some example text\n" 
    it "echo, cat, empty cat" $ do
      out <- interpretTest [("echo", ["hello"]), ("cat", ["test/ex.txt"]), ("cat", [])]
      out `shouldBe` "Some example text\n"
    it "echo, cat, empty wc" $ do
      out <- interpretTest [("echo", ["hello"]), ("cat", ["test/ex.txt"]), ("wc", [])]
      out `shouldBe` "     1      3     18"
    it "echo, cat, wc" $ do
      out <- interpretTest [("echo", ["hello"]), ("cat", ["test/ex.txt"]), ("wc", ["test/ex.txt"])]
      out `shouldBe` "     1      3     18 test/ex.txt"
    it "echo and echo" $ do
      out <- interpretTest [("echo", ["123"]), ("echo", ["456"])]
      out `shouldBe` "456\n"

-----------------------------------------------------------------------------------------------------
  describe "[\", \', $] ->" $ do
    it "parseSpecSymbols: ' and $" $ do
      (fst $ runState (parseSpecSymbols "$qw   as zx '$qw    $qw jkj' $qw") [("qw", "12")]) `shouldBe` Just ["12", "as", "zx", "$qw    $qw jkj", "12"]
    it "parseSpecSymbols: \" and $" $ do
      (fst $ runState (parseSpecSymbols "\"$qw   as zx '$qw    $qw jkj' $qw\"") [("qw", "12")]) `shouldBe` Just ["12   as zx '12    12 jkj' 12"]
    it "parseSpecSymbols: \" and ' and $" $ do
      (fst $ runState (parseSpecSymbols "'$qw   $as' \"zx  '$qw \"   $qw $as") [("qw", "12"), ("as", "trdn rt")]) `shouldBe` Just ["$qw   $as","zx  '12 ", "12", "trdn", "rt"]
-----------------------------------------------------------------------------------------------------
  describe "funcs: general ->" $ do
    it "pwd" $ do
      out <- pwd [] Normal
      out `shouldBe` "/home/irina/ifmo-software-design/cli\n"
    it "echo" $ do
      out <- echo ["12", " $qw  ", "afewf"] Normal
      out `shouldBe` "12  $qw   afewf\n"


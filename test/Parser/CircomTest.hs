module Parser.CircomTest where
import           BenchUtils
import           AST.Circom
import           Test.Tasty.HUnit
import           Parser.Circom.Lexer            ( tokenize )
import           Parser.Circom                  ( parseFile
                                                , loadFilesRecursively
                                                , loadMain
                                                )

circomParserTests :: BenchTest
circomParserTests = benchTestGroup
  "Circom"
  [ testLex "test/Code/Circom/inout.circom"
  , testParse "test/Code/Circom/binsum.circom"
  , testParse "test/Code/Circom/eddsamimcsponge.circom"
  , testParse "test/Code/Circom/poseidon.circom"
  , testParse "test/Code/Circom/gates.circom"
  , testParse "test/Code/Circom/mux4.circom"
  , testParse "test/Code/Circom/pedersen.circom"
  , testParse "test/Code/Circom/compconstant.circom"
  , testParse "test/Code/Circom/bitify.circom"
  , testParse "test/Code/Circom/aliascheck.circom"
  , testParse "test/Code/Circom/comparators.circom"
  , testLoad "test/Code/Circom/bitify.circom"
  , testLoadMain "test/Code/Circom/bitify-main.circom"
  , testLoadMain "test/Code/Circom/bitify4.circom"
  , testLoadMain "test/Code/Circom/bitify4-wrapper.circom"
  ]

testLex :: String -> BenchTest
testLex path = benchTestCase ("lex: " ++ path) $ do
  string <- readFile path
  let tokens = tokenize string
  not (null tokens) @? error "0 tokens!"

testParse :: String -> BenchTest
testParse path = benchTestCase ("parse: " ++ path) $ do
  ast <- parseFile path
  length ast > 0 @? "0 items!"

testLoad :: String -> BenchTest
testLoad path = benchTestCase ("load: " ++ path) $ do
  pgm <- loadFilesRecursively path
  length pgm > 0 @? "0 files!"

testLoadMain :: String -> BenchTest
testLoadMain path = benchTestCase ("load main: " ++ path) $ do
  pgm <- loadMain path
  length (show (main pgm)) > 0 @? "empty main!"

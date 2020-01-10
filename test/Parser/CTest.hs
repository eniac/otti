module Parser.CTest where
import           BenchUtils
import           Control.Monad (unless)
import           Data.Either   (fromLeft, isRight)
import           Parser.C
import           Utils

cParserTests :: BenchTest
cParserTests = benchTestGroup "C parser" [testAdd]

testParse :: String -> BenchTest
testParse name = benchTestCase name $ do
  wasm <- parseC name
  unless (isRight wasm) $ error "Parse failure"

testAdd :: BenchTest
testAdd = testParse "test/Code/C/add.c"


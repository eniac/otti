module Parser.WASMTest where
import           BenchUtils
import           Control.Monad (unless)
import           Data.Either   (fromLeft, isRight)
import           Parser.WASM
import           Utils

wasmParserTests :: BenchTest
wasmParserTests = benchTestGroup "WASM parser" [testAdd]

testParse :: String -> BenchTest
testParse name = benchTestCase name $ do
  wasm <- parseWasm name
  unless (isRight wasm) $ error "Parse failure"

testAdd :: BenchTest
testAdd = testParse "test/Code/WASM/add.wat"

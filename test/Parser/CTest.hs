module Parser.CTest where
import           BenchUtils
import           Control.Monad                  ( unless )
import           Data.Either                    ( isRight )
import           Parser.C

cParserTests :: BenchTest
cParserTests = benchTestGroup "C parser" [testAdd, testMemcpy]

testParse :: String -> BenchTest
testParse name = benchTestCase name $ do
  c <- parseCE name
  unless (isRight c) $ error $ show c

testAdd :: BenchTest
testAdd = testParse "test/Code/C/add.c"

testMemcpy :: BenchTest
testMemcpy = testParse "test/Code/C/memcpy_pp.c"


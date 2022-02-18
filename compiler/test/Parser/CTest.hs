{-# LANGUAGE OverloadedStrings #-}
module Parser.CTest where
import           BenchUtils
import           Control.Monad                  ( unless )
import           Data.Either                    ( isRight )
import           Parser.C
import           Util.Cfg                       ( evalCfgDefault )
import           Language.C.Pretty
import           Data.Text

cParserTests :: BenchTest
cParserTests = benchTestGroup "C parser" [testAdd, testMemcpy, testInclude]

testParse :: String -> BenchTest
testParse name = testParseAndFind name Nothing

testParseAndFind :: String -> Maybe Text -> BenchTest
testParseAndFind file (Just pattrn) = benchTestCase file $ do
  (Right tu) <- evalCfgDefault $ parseCE file
  let stringy = show . pretty $ tu
  unless (isInfixOf pattrn . pack $ stringy)
    $  error
    $  "Could not find "
    ++ unpack pattrn
    ++ " in\n"
    ++ stringy
testParseAndFind file Nothing = benchTestCase file $ do
  c <- evalCfgDefault $ parseCE file
  unless (isRight c) $ error $ show c

testAdd :: BenchTest
testAdd = testParse "test/Code/C/add.c"

testMemcpy :: BenchTest
testMemcpy = testParse "test/Code/C/memcpy_pp.c"

testInclude :: BenchTest
testInclude = testParseAndFind "test/Code/C/include.c" (Just "42")

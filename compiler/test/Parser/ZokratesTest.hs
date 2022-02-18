
{-# LANGUAGE LambdaCase #-}
module Parser.ZokratesTest where
import           BenchUtils
import           Test.Tasty.HUnit
import qualified Data.Map                      as Map
import           Parser.Zokrates.Lexer
import           Parser.Zokrates.Parser
import           Parser.Zokrates

zokratesParserTests :: BenchTest
zokratesParserTests = benchTestGroup
  "Zokrates"
  [ testParseExpr "a"
  , testParseExpr "h[5]"
  , testParseExpr "a + b"
  , testParseExpr "a + h[5]"
  , testParseExpr "[]"
  , testParseExpr "[a]"
  , testParseExpr "[\na\n]"
  , testParseExpr "[a\n]"
  , testParseExpr "[\na]"
  , testParseExpr "[...a]"
  , testParseExpr "[a,1,2]"
  , testParseExpr "[\na,\n1,\n2]"
  , testParseExpr "[a,\n1,\n2\n]"
  , testParseExpr "A { a: 1, b: 2}"
  , testParseStatement "a = h[5]\n"
  , testParseStatement "h[5] = a+7\n"
  , testParseStatement "field[5] d = a+7\n"
  , testParseBlock "return h[5]"
  , testParseBlock "h[5] = a+7\na=5\nreturn 5"
  , testParseBlock "h[5] = a+7\na=5\nreturn h"
  , testParseItem "def a() -> field:\n return 4"
  , testParseItems "def a() -> field:\n return 4"
  , testParseItems "struct A{\nfield x\n}\ndef a() -> u8:\nreturn 0x00"
  , testParseItems "def a() -> u8:\nreturn 0x00\nstruct A{\nfield x\n}\n"
  , testParseItems "def a() -> field:\n return 4\ndef b() -> u8:\nreturn 0x00"
  , testParseItems "def a() -> field:\n return 4\ndef b() -> u8:\nreturn 0x00"
  , testParse "test/Code/Zokrates/stdlib/ecc/babyjubjubParams.zok"
  , testParse "test/Code/Zokrates/stdlib/ecc/edwardsCompress.zok"
  , testParse "test/Code/Zokrates/stdlib/ecc/edwardsOrderCheck.zok"
  , testParse "test/Code/Zokrates/stdlib/ecc/proofOfOwnership.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/mimc7/constants.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/mimcSponge/IVconstants.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/mimcSponge/mimcFeistel.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/mimcSponge/mimcSponge.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/pedersen/512bit.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/sha256/1024bitPadded.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/sha256/512bitPadded.zok"
  , testParse "test/Code/Zokrates/stdlib/hashes/sha256/shaRound.zok"
  , testParse
    "test/Code/Zokrates/stdlib/hashes/utils/256bitsDirectionHelper.zok"
  , testParse "test/Code/Zokrates/stdlib/signatures/verifyEddsa.zok"
  , testLoad "test/Code/Zokrates/stdlib/signatures/verifyEddsa.zok"
  ]

testParse :: String -> BenchTest
testParse path = benchTestCase ("parse: " ++ path) $ do
  ast <- parseFile path
  length ast > 0 @? "0 items!"

testLoad :: String -> BenchTest
testLoad path = benchTestCase ("parse: " ++ path) $ do
  ast <- loadFilesRecursively path
  Map.size ast > 0 @? "0 files!"

tokenize' =
  filter
      (\case
        Posnd _ Newline -> False
        _               -> True
      )
    . either error id
    . tokenize

testParseExpr :: String -> BenchTest
testParseExpr s = benchTestCase ("expr: " ++ show s) $ do
  let ts  = either error id $ tokenize s
  let ast = parseZokratesExpr ts
  ast `seq` return ()

testParseStatement :: String -> BenchTest
testParseStatement s = benchTestCase ("stmt: " ++ show s) $ do
  let ts  = either error id $ tokenize s
  let ast = parseZokratesStatement ts
  ast `seq` return ()
testParseBlock :: String -> BenchTest
testParseBlock s = benchTestCase ("block: " ++ show s) $ do
  let ts  = either error id $ tokenize s
  let ast = parseZokratesBlock ts
  ast `seq` return ()

testParseItem :: String -> BenchTest
testParseItem s = benchTestCase ("item: " ++ show s) $ do
  let ts  = either error id $ tokenize s
  let ast = parseZokratesItem ts
  ast `seq` return ()

testParseItems :: String -> BenchTest
testParseItems s = benchTestCase ("items: " ++ show s) $ do
  let ts  = either error id $ tokenize s
  let ast = parseZokratesFile ts
  ast `seq` return ()

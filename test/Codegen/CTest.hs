module Codegen.CTest where
import           AST.Simple
import           BenchUtils
import           Codegen.C
import           Codegen.CompilerMonad
import qualified Data.Map              as M
import           IR.SMT                (initMem, smtPop, smtPush)
import           Parser.C
import           Utils

cTests :: BenchTest
cTests = benchTestGroup "C codegen test" [ basicTest ]

basicTest :: BenchTest
basicTest = benchTestCase "basic" $ do
  result <- parseC "test/Code/C/add.c"
  print result


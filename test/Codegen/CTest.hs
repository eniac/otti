module Codegen.CTest where
import           AST.Simple
import           BenchUtils
import           Codegen.C
import           Codegen.CompilerMonad
import qualified Data.Map              as M
import           IR.SMT                (initMem, smtPop, smtPush)
import           Parser.C
import           Test.Tasty.HUnit
import           Utils

cTests :: BenchTest
cTests = benchTestGroup "C codegen test" [ basicTest ]

basicTest :: BenchTest
basicTest = benchTestCase "basic" $ do
  result <- parseC "test/Code/C/add.c"
  case result of
    Left error -> assertFailure $ unwords ["Should not see", show error]
    Right tu -> do

      evalCodegen Nothing $ do
        codegenC tu



module Codegen.CTest where
import           AST.Simple
import           BenchUtils
import           Codegen.C
import           Codegen.CompilerMonad
import qualified Data.Map              as M
import           IR.SMT                (initMem, smtPop, smtPush)
import           Targets.SMT.Assert (execAssert, AssertState(..))
import           Parser.C
import           Test.Tasty.HUnit
import           Utils

cTests :: BenchTest
cTests = benchTestGroup "C codegen test" [basicTest ]

basicTest :: BenchTest
basicTest = benchTestCase "basic" $ do
  result <- parseC "test/Code/C/add.c"
  case result of
    Left error -> assertFailure $ unwords ["Should not see", show error]
    Right tu -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenC tu
      print ""
      print $ asserted assertions
--
--featuresTest :: BenchTest
--featuresTest = benchTestCase "features" $ do
--  result <- parseC "test/Code/C/test.c"
--  case result of
--    Left error -> assertFailure $ unwords ["Should not see", show error]
--    Right tu -> do
--
--      r1 <- evalCodegen Nothing $ do
--        liftIR initMem
--        codegenC tu
--        runSolverOnSMT
--
--      vtest r1 $ M.fromList [ ("w_0", 7) ]
--
--memcpyTest :: BenchTest
--memcpyTest = benchTestCase "memcpy" $ do
--  result <- parseC "test/Code/C/memcpy_pp.c"
--  case result of
--    Left error -> assertFailure $ unwords ["Should not see", show error]
--    Right tu -> do
--
--      r1 <- evalCodegen Nothing $ do
--        liftIR initMem
--        codegenC tu
--        runSolverOnSMT
--
--      vtest r1 $ M.fromList [ ("w_0", 7) ]
--

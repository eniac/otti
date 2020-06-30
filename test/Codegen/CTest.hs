module Codegen.CTest where
import           AST.Simple
import           BenchUtils
import           Codegen.C
import           Codegen.CompilerMonad
import qualified Data.Map                      as M
import           IR.SMT                         ( initMem
                                                , smtPop
                                                , smtPush
                                                )
import           Targets.SMT.Assert             ( execAssert
                                                , AssertState(..)
                                                )
import           Parser.C
import           Test.Tasty.HUnit
import           Utils

cTests :: BenchTest
cTests = benchTestGroup "C codegen test" [basicTest, loopTest, ifTest, initRetTest, fnCallTest]

basicTest :: BenchTest
basicTest = benchTestCase "basic" $ do
  result <- parseC "test/Code/C/add.c"
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenC tu
      3 @=? length (asserted assertions)

loopTest :: BenchTest
loopTest = benchTestCase "loop" $ do
  result <- parseC "test/Code/C/loop.c"
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenC tu
      (2 + 4 * 4) @=? length (asserted assertions)

ifTest :: BenchTest
ifTest = benchTestCase "if" $ do
  result <- parseC "test/Code/C/if.c"
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenC tu
      2 @=? length (asserted assertions)

initRetTest :: BenchTest
initRetTest = benchTestCase "initialize and return" $ do
  result <- parseC "test/Code/C/init.c"
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenC tu
      2 @=? length (asserted assertions)

fnCallTest :: BenchTest
fnCallTest = benchTestCase "function call" $ do
  result <- parseC "test/Code/C/fn_call.c"
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenC tu
      -- TODO check
      5 @=? length (asserted assertions)

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

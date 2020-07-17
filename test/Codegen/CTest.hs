module Codegen.CTest
  ( cTests
  )
where
import           AST.Simple
import           BenchUtils
import           Codegen.C
import           Codegen.CompilerMonad
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust )
import           Targets.SMT.Assert             ( execAssert
                                                , AssertState(..)
                                                )
import qualified IR.TySmt                      as Ty
import           Parser.C
import           Test.Tasty.HUnit
import           Utils

cTests :: BenchTest
cTests = benchTestGroup "C codegen test" [toSmtTests, ubTests]

toSmtTests = benchTestGroup
  "SMT conversion"
  [basicTest, loopTest, ifTest, initRetTest, fnCallTest]

constraintCountTest :: String -> FilePath -> Int -> BenchTest
constraintCountTest name path constraints = benchTestCase name $ do
  result <- parseC path
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenAll tu
      constraints @=? length (asserted assertions)

basicTest :: BenchTest
basicTest = constraintCountTest "basic" "test/Code/C/add.c" 8

loopTest :: BenchTest
loopTest = constraintCountTest "loop" "test/Code/C/loop.c" 26

ifTest :: BenchTest
ifTest = constraintCountTest "if" "test/Code/C/if.c" 6

initRetTest :: BenchTest
initRetTest = constraintCountTest "return" "test/Code/C/init.c" 4

fnCallTest :: BenchTest
fnCallTest = constraintCountTest "function call" "test/Code/C/fn_call.c" 13

ubCheckTest :: String -> String -> FilePath -> Bool -> BenchTest
ubCheckTest name fnName path undef = benchTestCase name $ do
  result <- parseC path
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen Nothing $ codegenAll tu
      r          <- Ty.evalZ3 $ Ty.BoolNaryExpr Ty.And (asserted assertions)
      undef @=? isJust r

ubTests = benchTestGroup
  "UB Checks"
  [ ubCheckTest "signed overflow" "inner" "test/Code/C/fn_call.c" True
  , ubCheckTest "signed overflow in fn" "outer" "test/Code/C/fn_call.c" True
  , ubCheckTest "unsigned overflow" "add" "test/Code/C/add_unsigned.c" False
  , ubCheckTest "constant in if" "add" "test/Code/C/if.c" False
  , ubCheckTest "constant loop" "add" "test/Code/C/loop.c" False
  ]

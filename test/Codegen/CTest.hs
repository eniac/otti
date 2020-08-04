module Codegen.CTest
  ( cTests
  )
where
import           AST.Simple
import           BenchUtils
import           Control.Monad
import           Codegen.C
import           Codegen.C.CompilerMonad
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import           IR.SMT.Assert                  ( execAssert
                                                , AssertState(..)
                                                , runAssert
                                                , asserted
                                                )
import qualified IR.SMT.TySmt                  as Ty
import           Parser.C
import           Test.Tasty.HUnit
import           Utils

cTests :: BenchTest
cTests = benchTestGroup "C codegen test" [toSmtTests, ubTests, satCircuitTests]


constraintCountTest :: String -> FilePath -> Int -> BenchTest
constraintCountTest name path constraints = benchTestCase name $ do
  result <- parseC path
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen True $ codegenAll tu
      constraints @=? length (asserted assertions)

toSmtTests = benchTestGroup
  "SMT conversion"
  [ constraintCountTest "basic"         "test/Code/C/add.c"     8
  , constraintCountTest "loop"          "test/Code/C/loop.c"    26
  , constraintCountTest "if"            "test/Code/C/if.c"      6
  , constraintCountTest "return"        "test/Code/C/init.c"    4
  , constraintCountTest "function call" "test/Code/C/fn_call.c" 13
  ]

ubCheckTest :: String -> String -> FilePath -> Bool -> BenchTest
ubCheckTest name fnName path undef = benchTestCase name $ do
  result <- parseC path
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      assertions <- execAssert $ evalCodegen True $ codegenFn tu fnName
      r          <- Ty.evalZ3 $ Ty.BoolNaryExpr Ty.And (asserted assertions)
      undef @=? isJust r

ubTests = benchTestGroup
  "UB Checks"
  [ ubCheckTest "signed overflow" "inner" "test/Code/C/fn_call.c" True
  , ubCheckTest "signed overflow in fn" "outer" "test/Code/C/fn_call.c" True
  , ubCheckTest "unsigned overflow" "add" "test/Code/C/add_unsigned.c" False
  , ubCheckTest "constant in if" "sum" "test/Code/C/if.c" False
  , ubCheckTest "constant loop" "sum" "test/Code/C/loop.c" False
  ]

satCircuitTest :: String -> String -> FilePath -> BenchTest
satCircuitTest name fnName path = benchTestCase name $ do
  result <- parseC path
  case result of
    Left  error -> assertFailure $ unwords ["Should not see", show error]
    Right tu    -> do
      (compState, assertState) <-
        runAssert $ execCodegen False $ initValues >> codegenFn tu fnName
      let assertions = asserted assertState
      let env        = fromJust $ values compState
      forM_ assertions $ \a -> Ty.ValBool True @=? Ty.eval env a

satCircuitTests = benchTestGroup
  "SAT Circuit checks"
  [ satCircuitTest "constant in if" "sum" "test/Code/C/if.c"
  , satCircuitTest "constant in if" "sum" "test/Code/C/loop.c"
  , satCircuitTest "constant in if" "add" "test/Code/C/add_unsigned.c"
  ]

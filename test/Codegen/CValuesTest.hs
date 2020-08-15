module Codegen.CValuesTest where
import           BenchUtils
import           Codegen.C
import           Codegen.C.CompilerMonad
import           Control.Monad
import           Control.Monad           (forM_)
import qualified Data.Map                as M
import           IR.SMT.Assert           (AssertState (..), asserted,
                                          execAssert, runAssert)
import qualified IR.SMT.TySmt            as Ty
import           Parser.C
import           Test.Tasty.HUnit

cValueTests :: BenchTest
cValueTests = benchTestGroup
  "C value test"
  [ constraintValueTest "assign" "test/Code/C/assign.c" [("f0_add_lex0__x_v0 ", 4)]
  , constraintValueTest "simple if" "test/Code/C/simple_if.c" [("f0_foo_lex0__y_v2 ", 2)]
  , constraintValueTest "assign if" "test/Code/C/assign_if.c" [("f0_foo_lex0__y_v1 ", 2)]
  , constraintValueTest "return" "test/Code/C/return.c" [("f0_foo__return_v0 ", 3)]
  , constraintValueTest "return if" "test/Code/C/return_if.c" [("f0_foo__return_v0 ", 3)]
  , constraintValueTest "return complex 0" "test/Code/C/return_complex.c" [("f0_foo__return_v0 ", 5)]
  , constraintValueTest "return complex 1" "test/Code/C/return_complex.c" [("f1_foo_bar__return_v0 ", 40)]
  ]

constraintValueTest :: String -> FilePath -> [(String, Int)] -> BenchTest
constraintValueTest name path expected = benchTestCase name $ do
  tu         <- parseC path
  -- Don't check for undefined behavior
  assertions <- execAssert $ evalCodegen False $ codegenAll tu
  r          <- Ty.evalZ3Model $ Ty.BoolNaryExpr Ty.And (asserted assertions)
  forM_ expected $ \(evar, eval) -> do
    case M.lookup evar r of
      Just aval -> eval @=? aval
      Nothing   -> error $ unwords ["No variable", show evar, "in model", show r]



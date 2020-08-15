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
  [ constraintValueTest "ops" "test/Code/C/ops.c" [ ("f0_foo_lex1__a_v0 ", 17)
                                                  , ("f0_foo_lex1__b_v0 ", 4294967289) -- -7
                                                  , ("f0_foo_lex1__c_v0 ", 60)
                                                  , ("f0_foo_lex1__d_v0 ", 0)
                                                  , ("f0_foo_lex1__e_v0 ", 5)
                                                  , ("f0_foo_lex1__f_v0 ", 0)
                                                  , ("f0_foo_lex1__g_v0 ", 1)
                                                  , ("f0_foo_lex1__h_v0 ", 1)
                                                  , ("f0_foo_lex1__i_v0 ", 4294967295)
                                                  , ("f0_foo_lex1__j_v0 ", 8)
                                                  , ("f0_foo_lex1__k_v0 ", 25)
                                                  , ("f0_foo_lex1__l_v0 ", 1)
                                                  , ("f0_foo_lex1__a_v1 ", 18)
                                                  , ("f0_foo_lex1__a_v2 ", 19)
                                                  , ("f0_foo_lex1__a_v3 ", 18)
                                                  , ("f0_foo_lex1__a_v4 ", 17)
                                                  ]
  , constraintValueTest "assign" "test/Code/C/assign.c" [ ("f0_foo_lex1__x_v0 ", 4) ]
  , constraintValueTest "assign if" "test/Code/C/assign_if.c" [ ("f0_foo_lex1__y_v2 ", 2) ]
  , constraintValueTest "return" "test/Code/C/return.c" [ ("f0_foo__return_v0 ", 3) ]
  , constraintValueTest "return if" "test/Code/C/return_if.c" [ ("f0_foo__return_v0 ", 3) ]
  , constraintValueTest "return complex 0" "test/Code/C/return_complex.c" [ ("f0_foo__return_v0 ", 5) ]
  , constraintValueTest "return complex 1" "test/Code/C/return_complex.c" [ ("f1_foo_bar__return_v0 ", 40) ]
  , constraintValueTest "return complex 2" "test/Code/C/return_complex.c" [ ("f2_foo_bar_baz__return_v0 ", 10) ]
  , constraintValueTest "return complex 3" "test/Code/C/return_complex.c" [ ("f3_foo_bar_baz_qux__return_v0 ", 606) ]
  , constraintValueTest "scope" "test/Code/C/scope.c" [ ("f0_foo_lex1__x_v0 ", 4)
                                                      , ("f0_foo__return_v0 ", 4)
                                                      ]
  , constraintValueTest "scope if" "test/Code/C/scope_if.c" [ ("f0_foo_lex1__x_v0 ", 0)
                                                            , ("f0_foo_lex2__x_v0 ", 500)
                                                            , ("f0_foo_lex3__x_v0 ", 2)
                                                            , ("f0_foo__return_v0 ", 0)
                                                            ]
  , constraintValueTest "scope for" "test/Code/C/scope_for.c" [ ("f0_foo_lex1__x_v0 ", 4)
                                                              , ("f0_foo_lex2__x_v0 ", 50)
                                                              , ("f0_foo_lex2__x_v1 ", 51)
                                                              , ("f0_foo__return_v0 ", 4)
                                                              ]
  , constraintValueTest "for bound" "test/Code/C/for_bound.c" [ ("f0_foo__return_v0 ", 2) ]
  , constraintValueTest "for unbound" "test/Code/C/for_unbound.c" [ ("f0_foo__return_v0 ", 5) ] -- I'm actually not sure what we want to do here...
  , constraintValueTest "memory 0" "test/Code/C/memory_0.c" [ ("f0_foo__return_v0 ", 100) ]
  , constraintValueTest "memory 1" "test/Code/C/memory_0.c" [ ("f0_foo__return_v0 ", 100) ]
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



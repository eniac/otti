module Codegen.CValuesTest where
import           BenchUtils
import           Codegen.C
import           Codegen.C.CompilerMonad
import           Control.Monad           (forM_)
import qualified Data.Map                as M
import           IR.SMT.Assert           (AssertState (..), asserted,
                                          execAssert, runAssert)
import           IR.SMT.TySmt            (Val, nan, nz)
import qualified IR.SMT.TySmt            as Ty
import           Parser.C
import           Test.Tasty.HUnit

i = Ty.i_
b = Ty.b_
d = Ty.d_

cValueTests :: BenchTest
cValueTests = benchTestGroup
  "C value test"
  [ constraintValueTest "ops" "foo" "test/Code/C/ops.c" [ ("f0_foo_lex1__a_v0", i 17)
                                                        , ("f0_foo_lex1__b_v0", i 4294967289) -- -7
                                                        , ("f0_foo_lex1__c_v0", i 60)
                                                        , ("f0_foo_lex1__d_v0", i 0)
                                                        , ("f0_foo_lex1__e_v0", i 5)
                                                        , ("f0_foo_lex1__f_v0", i 0)
                                                        , ("f0_foo_lex1__g_v0", i 1)
                                                        , ("f0_foo_lex1__h_v0", i 1)
                                                        , ("f0_foo_lex1__i_v0", i 4294967295)
                                                        , ("f0_foo_lex1__j_v0", i 8)
                                                        , ("f0_foo_lex1__k_v0", i 25)
                                                        , ("f0_foo_lex1__l_v0", b True)
                                                        , ("f0_foo_lex1__m_v0", b False)
                                                        , ("f0_foo_lex1__n_v0", b True)
                                                        , ("f0_foo_lex1__o_v0", b True)
                                                        , ("f0_foo_lex1__a_v1", i 18)
                                                        , ("f0_foo_lex1__a_v2", i 19)
                                                        , ("f0_foo_lex1__a_v3", i 18)
                                                        , ("f0_foo_lex1__a_v4", i 17)
                                                        , ("f0_foo_lex1__p_v0", i 1)
                                                        , ("f0_foo_lex1__q_v0", i 18)
                                                        , ("f0_foo_lex1__r_v0", i 0)
                                                        , ("f0_foo_lex1__s_v0", i 4294967295)
                                                        ]
  , constraintValueTest "cast" "foo" "test/Code/C/cast.c" [ ("f0_foo_lex1__a_v0", i 1)
                                                          , ("f0_foo_lex1__b_v0", i 1)
                                                          , ("f0_foo_lex1__c_v0", i 0)
                                                          , ("f0_foo_lex1__e_v0", i 0)
                                                          , ("f0_foo_lex1__g_v0", i 0)
                                                          , ("f0_foo_lex1__i_v0", i 0)
                                                          , ("f0_foo_lex1__k_v0", i 0)
                                                          ]
  , constraintValueTest "assign" "foo" "test/Code/C/assign.c" [ ("f0_foo_lex1__x_v0", i 4) ]
  , constraintValueTest "assign if" "foo" "test/Code/C/assign_if.c" [ ("f0_foo_lex1__y_v2", i 2) ]
  , constraintValueTest "global" "foo" "test/Code/C/global.c" [ ("f0_foo_lex1__x_v0", i 4) ]
  , constraintValueTest "return" "foo" "test/Code/C/return.c" [ ("f0_foo__return_v0", i 3) ]
  , constraintValueTest "return if" "foo" "test/Code/C/return_if.c" [ ("f0_foo__return_v0", i 3) ]
  , constraintValueTest "short circuit if" "foo" "test/Code/C/short_if.c" [ ("f0_foo__return_v0", i 12) ]
  , constraintValueTest "return complex 0" "foo" "test/Code/C/return_complex.c" [ ("f0_foo__return_v0", i 5) ]
  , constraintValueTest "return complex 1" "bar" "test/Code/C/return_complex.c" [ ("f0_bar__return_v0", i 40) ]
  , constraintValueTest "return complex 2" "baz" "test/Code/C/return_complex.c" [ ("f0_baz__return_v0", i 10) ]
  , constraintValueTest "return complex 3" "qux" "test/Code/C/return_complex.c" [ ("f0_qux__return_v0", i 606) ]
  , constraintValueTest "function 0" "main" "test/Code/C/function.c" [ ("f0_main__return_v0", i 7) ]
  , constraintValueTest "function 1" "main" "test/Code/C/function_1.c" [ ("f0_main__return_v0", i 6) ]
  , constraintValueTest "function 2" "main" "test/Code/C/function_2.c" [ ("f0_main__return_v0", i 12) ]
  , constraintValueTest "function 3" "main" "test/Code/C/function_3.c" [ ("f0_main__return_v0", i 9) ]
  , constraintValueTest "function 4" "main" "test/Code/C/function_4.c" [ ("f0_main__return_v0", i 5) ]
  , constraintValueTest "function 5" "main" "test/Code/C/function_5.c" [ ("f0_main__return_v0", i 6) ]
  -- Recursion, broken:
  -- , constraintValueTest "function 6" "main" "test/Code/C/function_6.c" [ ("f0_main__return_v0", 0) ]
  , constraintValueTest "scope" "foo" "test/Code/C/scope.c" [ ("f0_foo_lex1__x_v0", i 4)
                                                            , ("f0_foo__return_v0", i 4)
                                                            ]
  , constraintValueTest "scope if" "foo" "test/Code/C/scope_if.c" [ ("f0_foo_lex1__x_v0", i 0)
                                                                  , ("f0_foo_lex2__x_v0", i 500)
                                                                  , ("f0_foo_lex3__x_v0", i 2)
                                                                  , ("f0_foo__return_v0", i 0)
                                                                  ]
  , constraintValueTest "scope for" "foo" "test/Code/C/scope_for.c" [ ("f0_foo_lex1__x_v0", i 4)
                                                                    , ("f0_foo_lex2__x_v0", i 50)
                                                                    , ("f0_foo_lex2__x_v1", i 51)
                                                                    , ("f0_foo__return_v0", i 4)
                                                                    ]
  , constraintValueTest "for bound" "foo" "test/Code/C/for_bound.c" [ ("f0_foo__return_v0", i 2) ]
  , constraintValueTest "for unbound" "foo" "test/Code/C/for_unbound.c" [ ("f0_foo__return_v0", i 5) ] -- I'm actually not sure what we want to do here...
  , constraintValueTest "do while bound" "foo" "test/Code/C/do_while.c" [ ("f0_foo__return_v0", i 7) ]
  , constraintValueTest "do while once" "bar" "test/Code/C/do_while.c" [ ("f0_bar__return_v0", i 5) ]
  , constraintValueTest "while bound" "foo" "test/Code/C/while.c" [ ("f0_foo__return_v0", i 7) ]
  , constraintValueTest "while zero" "bar" "test/Code/C/while.c" [ ("f0_bar__return_v0", i 4) ]
  , constraintValueTest "memory 0" "foo" "test/Code/C/memory_0.c" [ ("f0_foo__return_v0", i 100) ]
-- Segfaults everytime
--  , constraintValueTest "memory 1" "test/Code/C/memory_1.c" [ ("f0_foo__return_v0", 100) ] -- Again, not sure what we want to do here. UC symex vs not UC symex?
  , constraintValueTest "short &&" "short_and" "test/Code/C/short-circuit.c" [ ("f0_short_and__return_v0", i 0) ]
  , constraintValueTest "short ||" "short_or" "test/Code/C/short-circuit.c" [ ("f0_short_or__return_v0", i 0) ]
  , constraintValueTest "sizeof(expr)" "sizeof_expr" "test/Code/C/sizeof.c" [ ("f0_sizeof_expr__return_v0", i 4) ]
  , constraintValueTest "sizeof(expr) no eval" "sizeof_expr_no_eval" "test/Code/C/sizeof.c" [ ("f0_sizeof_expr_no_eval__return_v0", i 0) ]
  , constraintValueTest "sizeof(type)" "sizeof_type" "test/Code/C/sizeof.c" [ ("f0_sizeof_type__return_v0", i 4) ]
  , constraintValueTest "char -> int" "char_to_int" "test/Code/C/casts.c" [ ("f0_char_to_int__return_v0", i 0) ]
  , constraintValueTest "uint -> uchar" "unsigned_to_uchar" "test/Code/C/casts.c" [ ("f0_unsigned_to_uchar__return_v0", i 255) ]
  ]

cRealTests :: BenchTest
cRealTests = benchTestGroup
  "C real program test"
  [ constraintValueTest "set bits" "main" "test/Code/C/setbits.c" [ ("f0_main__return_v0", i 1028) ]
  , constraintValueTest "invert" "main" "test/Code/C/invert.c" [ ("f0_main__return_v0", i 123811) ]
  , constraintValueTest "rot" "main" "test/Code/C/rot.c" [ ("f0_main__return_v0", i 1073741824) ]
  , constraintValueTest "rot" "other_main" "test/Code/C/rot.c" [ ("f0_other_main__return_v0", i 1073741824) ]
  , constraintValueTest "bitcount" "main" "test/Code/C/bitcount.c" [ ("f0_main__return_v0", i 1) ]
  , constraintValueTest "binsearch" "main" "test/Code/C/binsearch.c" [ ("f0_main__return_v0", i 3) ]
  , constraintValueTest "lower" "main" "test/Code/C/lower.c" [ ("f0_main__return_v0", i 108) ]
  , constraintValueTest "power" "main" "test/Code/C/power.c" [ ("f0_main__return_v0", i 8) ]
  , constraintValueTest "farenheit" "main" "test/Code/C/farenheit.c" [ ("f0_main__return_v0", i 60) ]
  ]

-- These tests pull from the Pepper project example applications:
-- https://github.com/pepper-project/pepper/tree/master/pepper/apps_sfdl
cPequinTests :: BenchTest
cPequinTests = benchTestGroup
  "C pequin compiler tests"
  [ constraintValueTest "base 2 log" "main" "test/Code/C/base_2_log.c" [ ("f0_main_return_v0", i 8) ]
  , constraintValueTest "sha1" "main" "test/Code/C/sha1.c" [ ]
  , constraintValueTest "sqrt" "main" "test/Code/C/sqrt.c" [ ("f0_main_return_v0", i 3) ]
  ]

constraintValueTest :: String -> String -> FilePath -> [(String, Val)] -> BenchTest
constraintValueTest name fnName path expected = benchTestCase name $ do
  tu <- parseC path
  r  <- evalFn tu fnName
  forM_ expected $ \(evar, eval) -> do
    case M.lookup evar r of
      Just aval -> eval @=? aval
      Nothing   -> error $ unwords ["No variable", show evar, "in model", show r]



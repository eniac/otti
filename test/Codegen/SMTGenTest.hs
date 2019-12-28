module Codegen.SMTGenTest where
import           AST.Simple
import           BenchUtils
import           Codegen.CompilerMonad
import           Codegen.SMTGen
import qualified Data.Map              as M
import           Utils

codegenTests :: BenchTest
codegenTests = benchTestGroup "Codegen tests" [ binOpTest
                                              , callTest
                                              ]

-- Fix so that declared but not defined variables have undef bit set
binOpTest :: BenchTest
binOpTest = benchTestCase "bin op" $ do

  r <- evalCodegen Nothing $ do
    let result = Var U8 "result"
        one = NumExpr $ INum U8 1
        three = NumExpr $ INum U8 3
        body = [ Decl result
               , Assign result $ Add one three
               , Assign result $ Add three three
               ]
        fun = Function "fun" U8 [] body
    genFunctionSMT fun
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_fun_1", 4)
                       , ("result_fun_1_undef", 0)
                       , ("result_fun_2", 6)
                       , ("result_fun_2_undef", 0)
                       ]

-- Disambiguate the return values
callTest :: BenchTest
callTest = benchTestCase "call" $ do

  r <- evalCodegen Nothing $ do
    let input = VarExpr $ Var U8 "input"
        one = NumExpr $ INum U8 1
        body = [ Return $ Add input one ]
        addOne = Function "addOne" U8 [("input", U8)] body

        two = NumExpr $ INum U8 2
        result = Var U8 "result"
        body2 = [ Decl result
                , Assign result $ Call "addOne" [two]
                , Return $ VarExpr result
                ]
        funThree = Function "three" U8 [] body2
    registerFunction addOne
    genFunctionSMT funThree
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_three_1", 3)
                       , ("input_addOne_three_0", 2)
                       , ("addOne_retVal", 3)
                       , ("three_retVal", 3)
                       ]


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

  vtest r $ M.fromList [ ("result_1", 4)
                       , ("result_1_undef", 0)
                       , ("result_2", 6)
                       , ("result_2_undef", 0)
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
        three = NumExpr $ INum U8 3
        result = Var U8 "result"
        distractor = Var U8 "distractor"
        body2 = [ Decl result
                , Decl distractor
                , Assign result $ Call "addOne" [two]
                , Assign distractor $ Call "addOne" [three]
                , Assign distractor $ Call "addOne" [VarExpr distractor]
                , Return $ VarExpr result
                ]
        funThree = Function "three" U8 [] body2
    registerFunction addOne
    genFunctionSMT funThree
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_1", 3)
                       , ("distractor_1", 4)
                       , ("distractor_2", 5)
                       , ("input_1", 2)
                       , ("input_2", 3)
                       , ("addOne_retVal_1", 3)
                       , ("addOne_retVal_2", 4)
                       , ("three_retVal", 3)
                       ]


module IR.SMTTest where
import           AST.Simple           (Type (..))
import           BenchUtils
import qualified Data.Map             as M
import           IR.SMT
import           Targets.SMT.SMTMonad
import           Utils

{-| Unit tests for the SMT IR layer. There are also automatically-generated quickcheck tests -}

irTests :: BenchTest
irTests = benchTestGroup "IR tests" [ negTest
                                    , bitwiseNegTest
                                    , addTest
                                    ]

negTest :: BenchTest
negTest = benchTestCase "neg" $ do

  r <- evalSMT Nothing $ do

    one <- newInt S32 1
    onePointTwo <- newDouble Double 1.2


    result <- newSMTVar S32 "result"
    cppNeg one >>= smtAssign result

    resultDouble <- newSMTVar Double "result_double"
    cppNeg onePointTwo >>= smtAssign resultDouble

    runSolver

  vtest r $ M.fromList [ ("result", 4294967295)
                       , ("result_double", -1.2)
                       ]
  satTest r

bitwiseNegTest :: BenchTest
bitwiseNegTest = benchTestCase "bitwise neg" $ do

  r <- evalSMT Nothing $ do

    one <- newInt Bool 1
    umax <- newInt U32 4294967295
    zero <- newInt U32 0

    result0 <- newSMTVar Bool "result0"
    cppBitwiseNeg one >>= smtAssign result0

    result1 <- newSMTVar U32 "result1"
    cppBitwiseNeg umax >>= smtAssign result1

    result2 <- newSMTVar U32 "result2"
    cppBitwiseNeg zero >>= smtAssign result2

    runSolver

  vtest r $ M.fromList [ ("result0", 0)
                       , ("result1", 0)
                       , ("result2", 4294967295)
                       ]
  satTest r


addTest :: BenchTest
addTest = benchTestCase "add" $ do

  r <- evalSMT Nothing $ do

    one <- newInt U32 1
    two <- newInt U32 2
    umax <- newInt U32 4294967295

    result <- newSMTVar U32 "result"
    cppAdd one two >>= smtAssign result

    overflow <- newSMTVar U32 "overflow"
    cppAdd one umax >>= smtAssign overflow

    runSolver

  vtest r $ M.fromList [ ("result", 3)
                       , ("result_undef", 0)
                       , ("overflow", 0)
                       , ("overflow_undef", 1)
                       ]
  satTest r


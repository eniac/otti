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
                                    , compareTest
                                    , bitwiseOpTest
                                    , subTest
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

compareTest :: BenchTest
compareTest = benchTestCase "comparisons" $ do

  r <- evalSMT Nothing $ do

    sOne <- newInt S32 1
    uOne <- newInt U32 1
    sMax <- newInt S32 (-1)
    uMax <- newInt U32 4294967295
    d1 <- newDouble Double 75635.12
    d2 <- newDouble Double 23.11

    result0 <- newSMTVar Bool "result0"
    cppEq sOne uOne >>= smtAssign result0

    result1 <- newSMTVar Bool "result1"
    cppGt uMax uOne >>= smtAssign result1

    result2 <- newSMTVar Bool "result2"
    cppGt sMax sOne >>= smtAssign result2

    result3 <- newSMTVar Bool "result3"
    cppGt uMax sOne >>= smtAssign result3

    result4 <- newSMTVar Bool "result4"
    cppLt uMax sOne >>= smtAssign result4

    result5 <- newSMTVar Bool "result5"
    cppGte sMax uOne >>= smtAssign result5

    result6 <- newSMTVar Bool "result6"
    cppLte uOne sMax >>= smtAssign result6

    result7 <- newSMTVar Bool "result7"
    cppLte d1 d2 >>= smtAssign result7

    runSolver

  vtest r $ M.fromList [ ("result0", 1)
                       , ("result1", 1)
                       , ("result2", 0)
                       , ("result3", 1)
                       , ("result4", 0)
                       , ("result5", 1)
                       , ("result6", 1)
                       , ("result7", 0)
                       ]
  satTest r

bitwiseOpTest :: BenchTest
bitwiseOpTest = benchTestCase "bitwise op" $ do

  r <- evalSMT Nothing $ do

    one <- newInt U32 1
    two <- newInt U32 2
    umax <- newInt U32 4294967295

    -- Or, xor, and

    result0 <- newSMTVar U32 "result0"
    cppAnd two umax >>= smtAssign result0

    result1 <- newSMTVar U32 "result1"
    cppAnd two one >>= smtAssign result1

    result2 <- newSMTVar U32 "result2"
    cppOr umax one >>= smtAssign result2

    result3 <- newSMTVar U32 "result3"
    cppXor umax one >>= smtAssign result3

    runSolver

  vtest r $ M.fromList [ ("result0", 2)
                       , ("result1", 0)
                       , ("result2", 4294967295)
                       , ("result3", 4294967294)
                       ]
  satTest r

subTest :: BenchTest
subTest = benchTestCase "sub" $ do

  r <- evalSMT Nothing $ do

    uOne <- newInt U32 1
    uTwo <- newInt U32 2
    uMax <- newInt U32 4294967295
    sOne <- newInt U32 1
    sTwo <- newInt U32 2
    sMax <- newInt U32 4294967295

    result0 <- newSMTVar U32 "result0"
    cppSub uTwo uOne >>= smtAssign result0

    result1 <- newSMTVar U32 "result1"
    cppSub uOne uTwo >>= smtAssign result1

    runSolver

  vtest r $ M.fromList [ ("result0", 1)
                       , ("result0_undef", 0)
                       , ("result1", 4294967295)
                       , ("result1_undef", 0)
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


module IR.SMTTest where
import           AST.Simple (Type (..))
import           BenchUtils
import qualified Data.Map   as M
import           IR.SMT
import           Utils

{-| Unit tests for the SMT IR layer. There are also automatically-generated quickcheck tests -}

irTests :: BenchTest
irTests = benchTestGroup "IR tests" [ solverTest
                                    , negTest
                                    , bitwiseNegTest
                                    , compareTest
                                    , bitwiseOpTest
                                    , subTest
                                    , addTest
                                    , structTest
                                    , arrayTest
                                    , memTest
                                    ]

solverTest :: BenchTest
solverTest = benchTestCase "solver" $ do

  (r1, r2) <- evalIR Nothing $ do

    one <- newInt S32 1
    result1 <- newVar S32 "result1"
    smtAssign result1 one
    r1 <- smtResult

    two <- newInt S32 2
    result2 <- newVar S32 "result2"
    smtAssign result2 two
    r2 <- smtResult
    return (r1, r2)

  vtest r1 $ M.fromList [ ("result1", 1) ]
  vtest r2 $ M.fromList [ ("result2", 2) ]

negTest :: BenchTest
negTest = benchTestCase "neg" $ do

  r <- evalIR Nothing $ do

    one <- newInt S32 1
    onePointTwo <- newDouble Double 1.2


    result <- newVar S32 "result"
    cppNeg one >>= smtAssign result

    resultDouble <- newVar Double "result_double"
    cppNeg onePointTwo >>= smtAssign resultDouble

    smtResult

  vtest r $ M.fromList [ ("result", 4294967295)
                       , ("result_double", -1.2)
                       ]

bitwiseNegTest :: BenchTest
bitwiseNegTest = benchTestCase "bitwise neg" $ do

  r <- evalIR Nothing $ do

    one <- newInt Bool 1
    umax <- newInt U32 4294967295
    zero <- newInt U32 0

    result0 <- newVar Bool "result0"
    cppBitwiseNeg one >>= smtAssign result0

    result1 <- newVar U32 "result1"
    cppBitwiseNeg umax >>= smtAssign result1

    result2 <- newVar U32 "result2"
    cppBitwiseNeg zero >>= smtAssign result2

    smtResult

  vtest r $ M.fromList [ ("result0", 0)
                       , ("result1", 0)
                       , ("result2", 4294967295)
                       ]

compareTest :: BenchTest
compareTest = benchTestCase "comparisons" $ do

  r <- evalIR Nothing $ do

    sOne <- newInt S32 1
    uOne <- newInt U32 1
    sMax <- newInt S32 (-1)
    uMax <- newInt U32 4294967295
    d1 <- newDouble Double 75635.12
    d2 <- newDouble Double 23.11

    result0 <- newVar Bool "result0"
    cppEq sOne uOne >>= smtAssign result0

    result1 <- newVar Bool "result1"
    cppGt uMax uOne >>= smtAssign result1

    result2 <- newVar Bool "result2"
    cppGt sMax sOne >>= smtAssign result2

    result3 <- newVar Bool "result3"
    cppGt uMax sOne >>= smtAssign result3

    result4 <- newVar Bool "result4"
    cppLt uMax sOne >>= smtAssign result4

    result5 <- newVar Bool "result5"
    cppGte sMax uOne >>= smtAssign result5

    result6 <- newVar Bool "result6"
    cppLte uOne sMax >>= smtAssign result6

    result7 <- newVar Bool "result7"
    cppLte d1 d2 >>= smtAssign result7

    smtResult

  vtest r $ M.fromList [ ("result0", 1)
                       , ("result1", 1)
                       , ("result2", 0)
                       , ("result3", 1)
                       , ("result4", 0)
                       , ("result5", 1)
                       , ("result6", 1)
                       , ("result7", 0)
                       ]

bitwiseOpTest :: BenchTest
bitwiseOpTest = benchTestCase "bitwise op" $ do

  r <- evalIR Nothing $ do

    one <- newInt U32 1
    two <- newInt U32 2
    umax <- newInt U32 4294967295

    -- Or, xor, and

    result0 <- newVar U32 "result0"
    cppAnd two umax >>= smtAssign result0

    result1 <- newVar U32 "result1"
    cppAnd two one >>= smtAssign result1

    result2 <- newVar U32 "result2"
    cppOr umax one >>= smtAssign result2

    result3 <- newVar U32 "result3"
    cppXor umax one >>= smtAssign result3

    smtResult

  vtest r $ M.fromList [ ("result0", 2)
                       , ("result1", 0)
                       , ("result2", 4294967295)
                       , ("result3", 4294967294)
                       ]

subTest :: BenchTest
subTest = benchTestCase "sub" $ do

  r <- evalIR Nothing $ do

    uOne <- newInt U32 1
    uTwo <- newInt U32 2
    uMax <- newInt U32 4294967295
    sOne <- newInt U32 1
    sTwo <- newInt U32 2
    sMax <- newInt U32 4294967295

    result0 <- newVar U32 "result0"
    cppSub uTwo uOne >>= smtAssign result0

    result1 <- newVar U32 "result1"
    cppSub uOne uTwo >>= smtAssign result1

    smtResult

  vtest r $ M.fromList [ ("result0", 1)
                       , ("result0_undef", 0)
                       , ("result1", 4294967295)
                       , ("result1_undef", 0)
                       ]

addTest :: BenchTest
addTest = benchTestCase "add" $ do

  r <- evalIR Nothing $ do

    one <- newInt U32 1
    two <- newInt U32 2
    umax <- newInt U32 4294967295

    result <- newVar U32 "result"
    cppAdd one two >>= smtAssign result

    overflow <- newVar U32 "overflow"
    cppAdd one umax >>= smtAssign overflow

    smtResult

  vtest r $ M.fromList [ ("result", 3)
                       , ("result_undef", 0)
                       , ("overflow", 0)
                       , ("overflow_undef", 1)
                       ]

structTest :: BenchTest
structTest = benchTestCase "structs" $ do

  r <- evalIR Nothing $ do
    let structTy = Struct [U8, S8]
    -- 00000011 | 00100100
    struct <- newIntStruct structTy [3, 36]
    newVar structTy "resultVar" >>= smtAssign struct

    elemOne <- getField struct 0
    elemTwo <- getField struct 1
    newVar U8 "elemOne" >>= smtAssign elemOne
    newVar S8 "elemTwo" >>= smtAssign elemTwo

    newOne <- newInt U8 8
    newTwo <- newInt S8 40
    structOne <- setField struct 0 newOne
    structTwo <- setField structOne 1 newTwo
    -- 00001000 | 00101000
    newVar structTy "newStructVar" >>= smtAssign structTwo

    smtResult

  vtest r $ M.fromList [ ("resultVar", 804)
                       , ("elemOne", 3)
                       , ("elemTwo", 36)
                       , ("newStructVar", 2088)
                       ]

arrayTest :: BenchTest
arrayTest = benchTestCase "arrays" $ do

  r <- evalIR Nothing $ do
    let arrayTy = Array 4 U8
    -- 00000011 | 00000100 | 00000101 | 00000110
    array <- newIntArray arrayTy [3, 4, 5, 6]
    newVar arrayTy "resultVar" >>= smtAssign array

    idx <- newInt U32 2
    elemThree <- getIdx array idx
    newVar U8 "elemThree" >>= smtAssign elemThree

    newElemThree <- newInt U8 15
    newArray <- setIdx array idx newElemThree
    finalElemThree <- getIdx newArray idx
    newVar U8 "finalElemThree" >>= smtAssign finalElemThree

    smtResult

  vtest r $ M.fromList [ ("resultVar", 50595078)
                       , ("elemThree", 5)
                       , ("finalElemThree", 15)
                       ]

memTest :: BenchTest
memTest = benchTestCase "memory" $ do

  r <- evalIR Nothing $ do
    initMem
    addr <- newPtr (Ptr32 S32) 0
    val <- newInt S32 123
    smtStore addr val
    result <- smtLoad addr
    newVar S32 "resultVar" >>= smtAssign result

    let arrayTy = Array 5 U8
    addr <- newPtr (Ptr32 arrayTy) 64
    array <- newIntArray arrayTy [3, 4, 5, 6, 7]
    smtStore addr array
    arrayResult <- smtLoad addr
    idx <- newInt U32 2
    elemThree <- getIdx arrayResult idx
    newVar U8 "elemThree" >>= smtAssign elemThree


    smtResult

  vtest r $ M.fromList [ ("resultVar", 123)
                       , ("elemThree", 5)
                       ]


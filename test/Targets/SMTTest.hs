module Targets.SMTTest where
import           BenchUtils
import qualified Data.Map    as M
import           Targets.SMT
import           Utils

smtTests :: BenchTest
smtTests = benchTestGroup "SMT tests" [ getBitsTest
                                      , setBitsTest
                                      ]

getBitsTest :: BenchTest
getBitsTest = benchTestCase "getBitsFrom" $ do

  r <- evalSMT Nothing $ do

    bv8 <- bvSort 8
    bv16 <- bvSort 16
    bv32 <- bvSort 32

    zero32 <- bvNum 32 0
    result0 <- newVar "result0" bv16
    getBitsFromLE zero32 16 zero32 >>= assign result0

    -- 1010101010101010
    oneZero16 <- bvNum 16 43690
    result1 <- newVar "result1" bv8
    index <- bvNum 8 12
    -- 01010101
    getBitsFromLE oneZero16 8 index >>= assign result1

    -- 1011010001010101
    rando16 <- bvNum 16 46165
    result2 <- newVar "result2" bv16
    index <- bvNum 16 15
    getBitsFromLE rando16 16 index >>= assign result2

    rando16 <- bvNum 16 46165
    result3 <- newVar "result3" bv16
    index <- bvNum 16 0
    getBitsFromBE rando16 16 index >>= assign result3

    -- 1010101010101010
    oneZero16 <- bvNum 16 43690
    result4 <- newVar "result4" bv8
    index <- bvNum 8 3
    -- 01010101
    getBitsFromBE oneZero16 8 index >>= assign result4

    zero32 <- bvNum 32 0
    result5 <- newVar "result5" bv16
    getBitsFromBE zero32 16 zero32 >>= assign result5

    runSolver

  vtest r $ M.fromList [ ("result0", 0)
                       , ("result1", 85)
                       , ("result2", 46165)
                       , ("result3", 46165)
                       , ("result4", 85)
                       , ("result5", 0)
                       ]
  satTest r

setBitsTest :: BenchTest
setBitsTest = benchTestCase "setBitsTo" $ do

  r <- evalSMT Nothing $ do

    bv8 <- bvSort 8
    bv16 <- bvSort 16
    bv32 <- bvSort 32

    result0 <- newVar "result0" bv8
    lowSet <- bvNum 8 15
    highBits <- bvNum 4 15
    index <- bvNum 16 0
    setBitsTo highBits lowSet index >>= assign result0


    runSolver

  vtest r $ M.fromList [ ("result0", 255)
                       ]


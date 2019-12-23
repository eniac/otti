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
    getBitsFrom zero32 16 zero32 >>= assign result0

    -- 1010101010101010
    oneZero16 <- bvNum 16 43690
    result1 <- newVar "result1" bv8
    index <- bvNum 8 3
    -- 01010101
    getBitsFrom oneZero16 8 index >>= assign result1

    -- 1011010001010101
    rando16 <- bvNum 16 46165
    result2 <- newVar "result2" bv16
    index <- bvNum 16 0
    getBitsFrom rando16 16 index >>= assign result2

    runSolver

  vtest r $ M.fromList [ ("result0", 0)
                       , ("result1", 85)
                       , ("result2", 46165)
                       ]
  satTest r

setBitsTest :: BenchTest
setBitsTest = benchTestCase "setBitsTo" $ do

  r <- evalSMT Nothing $ do

    bv8 <- bvSort 8
    bv16 <- bvSort 16
    bv32 <- bvSort 32

    ones16 <- ones 16
    result0 <- newVar "result0" bv16
    assign ones16 result0

    -- 1111111111111110
    result1 <- newVar "result1" bv16
    bits16 <- bvNum 16 65534
    one1 <- bvNum 1 1
    index <- bvNum 4 0
    -- 1111111111111111
    setBitsTo one1 bits16 index >>= assign result1

    -- result0 <- newVar "result0" bv8
    -- highSet <- bvNum 8 240
    -- lowSet <- bvNum 4 15
    -- index <- bvNum 16 0
    -- setBitsTo lowSet highSet index >>= assign result0



    runSolver

  vtest r $ M.fromList [-- ("result0", 255)
--                        ("result1", 65535)
                       ("result0", 65535)
                       ]


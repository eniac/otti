module Targets.SMTTest where
import           BenchUtils
import qualified Data.Map    as M
import           Targets.SMT
import           Utils

smtTests :: BenchTest
smtTests = benchTestGroup "SMT tests" [ getBitsTest
                                     ]

getBitsTest :: BenchTest
getBitsTest = benchTestCase "getBitsFrom" $ do

  r <- evalSMT Nothing $ do

    zero32 <- bvNum 32 0
    bv16 <- bvSort 16

    result0 <- newVar "result0" bv16
    getBitsFrom zero32 16 zero32

    runSolver

  vtest r $ M.fromList [ ("result0", 0)
                       ]
  satTest r


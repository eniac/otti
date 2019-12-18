module IR.SMTTest where
import           AST.Simple           (Type (..))
import           BenchUtils
import qualified Data.Map             as M
import           IR.SMT
import           Targets.SMT.SMTMonad
import           Utils

{-| Unit tests for the SMT IR layer. There are also automatically-generated quickcheck tests -}

irTests :: BenchTest
irTests = benchTestGroup "IR tests" [ addTest ]

addTest :: BenchTest
addTest = benchTestCase "add" $ do

  r <- evalSMT Nothing $ do
    -- Numbers for the test
    one <- newInt U32 1
    two <- newInt U32 2

    -- Make sure it gets additions of constants right
    result <- newSMTVar U32 "result"
    cppAdd one two >>= smtAssign result

    runSolver

  vtest r $ M.fromList [ ("result", 3)
                       ]
  satTest r


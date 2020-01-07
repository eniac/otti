import           BenchUtils
import           Codegen.SMTGenTest
import           IR.SMTTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

allTests :: [BenchTest]
allTests = [ smtTests
           -- , irTests
            -- codegenTests
           ]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

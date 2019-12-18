import           BenchUtils
import           Codegen.SMTGenTest
import           IR.SMTTest
import           Targets.SMT
import           Test.Tasty

allTests :: [BenchTest]
allTests = [irTests]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

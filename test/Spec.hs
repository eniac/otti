import           BenchUtils
import           Codegen.CTest
import           Codegen.CircomTest
import           Codegen.C.MemoryTest
import           Codegen.C.CUtilsTest
import           IR.SMT.OptTest
import           IR.R1cs.OptTest
import           IR.SMT.ToPfTest
import           IR.SMT.TySmtTest
import           Parser.CTest
import           Parser.CircomTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [cParserTests, circomParserTests]

generatorTests :: BenchTest
generatorTests = benchTestGroup "Generator tests" [memoryTest, circomGenTests, cTests, cutilsTest]

irTests :: BenchTest
irTests = benchTestGroup "IR tests" [tySmtTests, cutilsTest, toPfTests, optTests, r1csOptTests]


allTests :: [BenchTest]
allTests = [parserTests, irTests, generatorTests]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

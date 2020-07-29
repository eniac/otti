import           BenchUtils
import           Codegen.CTest
import           Codegen.CircomTest
import           Codegen.C.MemoryTest
import           Codegen.C.CUtilsTest
import           IR.SMT.OptTest
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
generatorTests = benchTestGroup "Generator tests" [circomGenTests, cTests, toPfTests, optTests]

irTests :: BenchTest
irTests = benchTestGroup "IR tests" [tySmtTests, memoryTest, cutilsTest]


allTests :: [BenchTest]
allTests = [parserTests, irTests, generatorTests]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

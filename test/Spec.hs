import           BenchUtils
import           Codegen.C.TermTest
import           Codegen.C.EvalTest
import           Codegen.C.Test
import           Codegen.Circify.MemoryTest
import           Codegen.CircomTest
import           IR.R1cs.OptTest
import           IR.SMT.OptTest
import           IR.SMT.ToPfTest
import           IR.SMT.TySmtTest
import           Parser.CircomTest
import           Parser.CTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [cParserTests, circomParserTests]

generatorTests :: BenchTest
generatorTests = benchTestGroup
  "Generator tests"
  [ memoryTest
  , circomGenTests
  , cTests
  , cutilsTest
  , cValueTests
  , cRealTests
  , cPequinTests
  ]

irTests :: BenchTest
irTests =
  benchTestGroup "IR tests" [tySmtTests, toPfTests, optTests, r1csOptTests]


allTests :: [BenchTest]
allTests = [parserTests, irTests, generatorTests]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

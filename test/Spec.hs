import           BenchUtils
import           Codegen.C.TermTest
import           Codegen.C.EvalTest
import           Codegen.C.Test
import           Codegen.Circify.MemoryTest
import           Codegen.CircomTest
import           IR.R1cs.OptTest
import           IR.SMT.MemRouteTest
import           IR.SMT.OptTest
import           IR.SMT.ToPfTest
import           IR.SMT.TySmtTest
import           Parser.CircomTest
import           Parser.CTest
import           Test.Tasty
import           Targets.ToZ3

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

benesTests :: BenchTest
benesTests = benchTestGroup "Waksman routing" [rt, r3]
 where
  rt = benchTestProperty "Arbitrary-size" test_benesRoute
  r3 = benchTestProperty "size-3" test_benesRoute3

irTests :: BenchTest
irTests = benchTestGroup
  "IR tests"
  [tySmtTests, toPfTests, optTests, r1csOptTests, benesTests]


allTests :: [BenchTest]
allTests = [parserTests, irTests, generatorTests, tySmtToZ3Tests]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

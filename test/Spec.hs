import           BenchUtils
import           Codegen.CTest
import           Codegen.SMTGenTest
import           IR.SMTTest
import           IR.TySmtTest
import           IR.MemoryTest
import           Parser.CTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [ cParserTests ]

allTests :: [BenchTest]
allTests = [ -- parserTests
           -- , smtTests
           -- , irTests
            cTests
           -- , codegenTests
           , tySmtTests
           , memoryTest
           ]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

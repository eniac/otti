import           BenchUtils
import           Codegen.CTest
import           Codegen.CircomTest
import           Codegen.SMTGenTest
import           IR.SMTTest
import           IR.TySmtTest
import           IR.MemoryTest
import           IR.CUtilsTest
import           Parser.CTest
import           Parser.CircomTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [
                                            cParserTests,
                                            circomParserTests
                                            ]

generatorTests :: BenchTest
generatorTests = benchTestGroup "Generator tests" [ circomGenTests
                                                  ]

allTests :: [BenchTest]
allTests = [ parserTests
           -- , smtTests
           -- , codegenTests
           , irTests
           , tySmtTests
           , memoryTest
           , cutilsTest
           , generatorTests
           ]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

import           BenchUtils
import           Codegen.CircomTest
import           Codegen.SMTGenTest
import           IR.SMTTest
import           IR.TySmtTest
import           Parser.CTest
import           Parser.WASMTest
import           Parser.CircomTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [ 
                                            circomParserTests
                                            ]

generatorTests :: BenchTest
generatorTests = benchTestGroup "Generator tests" [ circomGenTests
                                                  ]

allTests :: [BenchTest]
allTests = [ parserTests
           , irTests
           , tySmtTests
           ]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

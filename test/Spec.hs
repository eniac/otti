import           BenchUtils
import           Codegen.CircomTest
import           Codegen.SMTGenTest
import           IR.SMTTest
import           Parser.CTest
import           Parser.WASMTest
import           Parser.CircomTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [ cParserTests
                                            , wasmParserTests
                                            , circomParserTests
                                            ]

generatorTests :: BenchTest
generatorTests = benchTestGroup "Generator tests" [ circomGenTests
                                                  ]

allTests :: [BenchTest]
allTests = [ parserTests
           , smtTests
           , irTests
           , codegenTests
           , generatorTests
           ]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

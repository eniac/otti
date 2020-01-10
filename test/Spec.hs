import           BenchUtils
import           Codegen.SMTGenTest
import           IR.SMTTest
import           Parser.WASMTest
import           Parser.WASMTest
import           Targets.SMT
import           Targets.SMTTest
import           Test.Tasty

parserTests :: BenchTest
parserTests = benchTestGroup "Parser tests" [ wasmParserTests ]

allTests :: [BenchTest]
allTests = [ parserTests
           , smtTests
           , irTests
           , codegenTests
           ]

main :: IO ()
main = defaultMain $ testGroup "All tests" $ map getTest allTests

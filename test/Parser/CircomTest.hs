module Parser.CircomTest where
import           BenchUtils
import           Control.Monad (unless)
import           Data.Either   (fromLeft, isRight)
import           AST.Circom
import           Parser.Circom.Lexer (tokenize)
import           Parser.Circom.Parser (parseCircomExpr)
import           Utils

circomParserTests :: BenchTest
circomParserTests = benchTestGroup "Circom tests"
    [ testLex "test/Code/Circom/inout.circom"
    , testExprParse "5" Nothing
    , testExprParse "x" $ Just $ LValue $ Ident "x"
    , testExprParse "x + 5" $ Nothing
    , testExprParse "in[j][k] * 2**k" Nothing --  https://github.com/iden3/circomlib/blob/master/circuits/binsum.circom#L77
    , testExprParse "2**k * in[j][k]" $ Just $ BinExpr Mul (BinExpr Pow (NumLit 2) (LValue (Ident "k"))) (LValue (Index (Index (Ident "in") (LValue (Ident "j"))) (LValue (Ident "k"))))
    , testExprParse "2**(k * in[j][k])" $ Nothing
    , testExprParse "out[k] * (out[k] - 1)" Nothing --  https://github.com/iden3/circomlib/blob/master/circuits/binsum.circom#L85
    , testExprParse "[x, y, z + 5]" $ Just $ ArrayLit [LValue $ Ident "x", LValue $ Ident "y", BinExpr Add (LValue $ Ident "z") (NumLit 5)]
    , testExprParse "x + -x" $ Just $ BinExpr Add (LValue $ Ident "x") (UnExpr UnNeg (LValue $ Ident "x"))
    ]

testLex :: String -> BenchTest
testLex path = benchTestCase ("lex " ++ path) $ do
  string <- readFile path
  let tokens = tokenize string
  print tokens

testExprParse :: String -> Maybe Expr -> BenchTest
testExprParse expr expected = benchTestCase ("parse expr `" ++ expr ++ "`") $ do
  let lexed = tokenize expr
  let parsed = parseCircomExpr lexed
  case expected of
    Nothing -> pure ()
    Just r -> if r == parsed then pure () else error $ "Error: expected " ++ (show r) ++ " but got " ++ (show parsed)
  print parsed

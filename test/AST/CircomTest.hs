module AST.CircomTest where
import           BenchUtils
import           Control.Monad (unless)
import           Data.Either   (fromLeft, isRight)
import qualified Data.Map.Strict as Map
import           AST.Circom
import           Utils

signalTerm :: String -> Term
signalTerm s = Linear (Map.fromList [(SigLocal s [], 1)], 0)

circomGenTests :: BenchTest
circomGenTests = benchTestGroup "Circom generator tests"
    [ cGenTest Map.empty (NumLit 5) (Scalar 5)
    , cGenTest Map.empty (BinExpr Shl (NumLit 5) (NumLit 2)) (Scalar 20)
    , cGenTest (Map.fromList [("in", signalTerm "in")]) (LValue $ Ident "in") (signalTerm "in")
    , cGenTest (Map.fromList [("in", signalTerm "in")])
               (BinExpr Add (LValue $ Ident "in") (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 2)], 0))
    , cGenTest (Map.fromList [("in", signalTerm "in")])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (NumLit 5)) (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 6)], 0))
    , cGenTest (Map.fromList [("in", signalTerm "in")])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          )
    , cGenTest (Map.fromList [("in", signalTerm "in")])
               (BinExpr Sub (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], -1)], 0)
                          )
    , cGenTest (Map.fromList [("in", signalTerm "in")])
               (BinExpr Mul (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               Other
    ]

cGenTest :: CGenCtx -> Expr -> Term -> BenchTest
cGenTest ctx e t = benchTestCase ("eval " ++ show e) $ do
    let p = cGenExpr ctx e
    unless (snd p == t) $ error $ "Expected\n\t" ++ show e ++ "\nto evaluate to\n\t" ++ show t ++ "\nbut it evaluated to\n\t" ++ show (snd p) ++ "\n"
    pure ()


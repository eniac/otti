module AST.CircomTest where
import           AST.Circom
import           BenchUtils
import           Control.Monad   (unless)
import           Data.Either     (fromLeft, isRight)
import qualified Data.Map.Strict as Map
import           Utils

signalTerm :: String -> Term
signalTerm s = Linear (Map.fromList [(SigLocal s [], 1)], 0)

cGenCtxWithSignals :: [String] -> CGenCtx
cGenCtxWithSignals sigNames = CGenCtx { env = Map.fromList (map (\s -> (s, signalTerm s)) sigNames), constraints = [] }

circomGenTests :: BenchTest
circomGenTests = benchTestGroup "Circom generator tests"
    [ cGenTest (cGenCtxWithSignals []) (NumLit 5) (Scalar 5)
    , cGenTest (cGenCtxWithSignals []) (BinExpr Shl (NumLit 5) (NumLit 2)) (Scalar 20)
    , cGenTest (cGenCtxWithSignals ["in"]) (LValue $ Ident "in") (signalTerm "in")
    , cGenTest (cGenCtxWithSignals ["in"])
               (BinExpr Add (LValue $ Ident "in") (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 2)], 0))
    , cGenTest (cGenCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (NumLit 5)) (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 6)], 0))
    , cGenTest (cGenCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          )
    , cGenTest (cGenCtxWithSignals ["in"])
               (BinExpr Sub (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], -1)], 0)
                          )
    , cGenTest (cGenCtxWithSignals ["in"])
               (BinExpr Mul (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               Other
    , cGenTest (cGenCtxWithSignals [])
               (UnExpr UnPos (ArrayLit [NumLit 5, NumLit 6, NumLit 7]))
               (Scalar 3)
    , cGenTest (cGenCtxWithSignals []) (UnExpr UnNeg (NumLit 5)) (Scalar (-5))
    , ctxStoreGetTest
        "integer"
        (CGenCtx {env = Map.fromList [("in", Array [Scalar 5])], constraints = []})
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array to integer"
        (CGenCtx {env = Map.fromList [("in", Array [Scalar 5])], constraints = []})
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array"
        (CGenCtx {env = Map.fromList [("in", Array [Scalar 5])], constraints = []})
        (LTermIdx (LTermIdent "in") 0)
        (Scalar 6)
        (LTermIdent "in")
        (Array [Scalar 6])
    , ctxStoreGetTest
        "struct in array"
        (CGenCtx {env = Map.fromList [("in", Array [Struct $ Map.fromList [("a", Scalar 5)]])], constraints = []})
        (LTermPin (LTermIdx (LTermIdent "in") 0) "a")
        (Scalar 6)
        (LTermIdent "in")
        (Array [Struct $ Map.fromList [("a", Scalar 6)]])
    ]

cGenTest :: CGenCtx -> Expr -> Term -> BenchTest
cGenTest ctx e t = benchTestCase ("eval " ++ show e) $ do
    let p = cGenExpr ctx e
    unless (snd p == t) $ error $ "Expected\n\t" ++ show e ++ "\nto evaluate to\n\t" ++ show t ++ "\nbut it evaluated to\n\t" ++ show (snd p) ++ "\n"
    return ()

ctxStoreGetTest :: String -> CGenCtx -> LTerm -> Term -> LTerm -> Term -> BenchTest
ctxStoreGetTest name ctx sLoc sVal gLoc gVal = benchTestCase ("store/get test: " ++ name) $ do
    let ctx' = ctxStore ctx sLoc sVal
    let gVal' = ctxGet ctx' gLoc
    unless (gVal == gVal') $ error $ "After placing\n\t" ++ show sVal ++ "\nat\n\t" ++ show sLoc ++ "\nin\n\t" ++ show ctx ++"\n, expected\n\t" ++ show gVal ++ "\nat\n\t" ++ show gLoc ++ "\nbut found\n\t" ++ show gVal' ++ "\n"
    return ()

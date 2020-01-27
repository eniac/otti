module Codegen.CircomTest where
import           AST.Circom
import           Codegen.Circom
import           BenchUtils
import           Control.Monad   (unless)
import           Data.Either     (fromLeft, isRight)
import qualified Data.Map.Strict as Map
import           Utils

signalTerm :: String -> Term
signalTerm s = Linear (Map.fromList [(SigLocal s [], 1)], 0)

cGenCtxWithSignals :: [String] -> CGenCtx
cGenCtxWithSignals sigNames = ctxWithEnv $ Map.fromList (map (\s -> (s, signalTerm s)) sigNames)

circomGenTests :: BenchTest
circomGenTests = benchTestGroup "Circom generator tests"
    [ cGenExprTest (cGenCtxWithSignals []) (NumLit 5) (Scalar 5)
    , cGenExprTest (cGenCtxWithSignals []) (BinExpr Shl (NumLit 5) (NumLit 2)) (Scalar 20)
    , cGenExprTest (cGenCtxWithSignals ["in"]) (LValue $ Ident "in") (signalTerm "in")
    , cGenExprTest (cGenCtxWithSignals ["in"])
               (BinExpr Add (LValue $ Ident "in") (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 2)], 0))
    , cGenExprTest (cGenCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (NumLit 5)) (LValue $ Ident "in"))
               (Linear (Map.fromList [(SigLocal "in" [], 6)], 0))
    , cGenExprTest (cGenCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          )
    , cGenExprTest (cGenCtxWithSignals ["in"])
               (BinExpr Sub (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], -1)], 0)
                          )
    , cGenExprTest (cGenCtxWithSignals ["in"])
               (BinExpr Mul (BinExpr Mul (LValue $ Ident "in") (LValue $ Ident "in")) (LValue $ Ident "in"))
               Other
    , cGenExprTest (cGenCtxWithSignals [])
               (UnExpr UnPos (ArrayLit [NumLit 5, NumLit 6, NumLit 7]))
               (Scalar 3)
    , cGenExprTest (cGenCtxWithSignals []) (UnExpr UnNeg (NumLit 5)) (Scalar (-5))
    , ctxStoreGetTest
        "integer"
        (ctxWithEnv (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array to integer"
        (ctxWithEnv (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array"
        (ctxWithEnv (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdx (LTermIdent "in") 0)
        (Scalar 6)
        (LTermIdent "in")
        (Array [Scalar 6])
    , ctxStoreGetTest
        "struct in array"
        (ctxWithEnv (Map.fromList [("in", Array [Struct (Map.fromList [("a", Scalar 5)]) []])]))
        (LTermPin (LTermIdx (LTermIdent "in") 0) "a")
        (Scalar 6)
        (LTermIdent "in")
        (Array [Struct (Map.fromList [("a", Scalar 6)]) []])
    , cGenStatementTest
        "equal"
        (cGenCtxWithSignals ["a", "b"])
        (Constrain (LValue (Ident "a")) (LValue (Ident "b")))
        (ctxAddConstraint (cGenCtxWithSignals ["a", "b"]) (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -1)], 0)))
    , cGenStatementTest
        "twice (assign & constrain)"
        (cGenCtxWithSignals ["a", "b"])
        (AssignConstrain (Ident "a") (BinExpr Mul (NumLit 2) (LValue (Ident "b"))))
        (ctxAddConstraint (cGenCtxWithSignals ["a", "b"]) (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -2)], 0)))
    ]

cGenExprTest :: CGenCtx -> Expr -> Term -> BenchTest
cGenExprTest ctx e t = benchTestCase ("eval " ++ show e) $ do
    let p = cGenExpr ctx e
    unless (snd p == t) $ error $ "Expected\n\t" ++ show e ++ "\nto evaluate to\n\t" ++ show t ++ "\nbut it evaluated to\n\t" ++ show (snd p) ++ "\n"
    return ()

ctxStoreGetTest :: String -> CGenCtx -> LTerm -> Term -> LTerm -> Term -> BenchTest
ctxStoreGetTest name ctx sLoc sVal gLoc gVal = benchTestCase ("store/get test: " ++ name) $ do
    let ctx' = ctxStore ctx sLoc sVal
    let gVal' = ctxGet ctx' gLoc
    unless (gVal == gVal') $ error $ "After placing\n\t" ++ show sVal ++ "\nat\n\t" ++ show sLoc ++ "\nin\n\t" ++ show ctx ++"\n, expected\n\t" ++ show gVal ++ "\nat\n\t" ++ show gLoc ++ "\nbut found\n\t" ++ show gVal' ++ "\n"
    return ()

cGenStatementTest :: String -> CGenCtx -> Statement -> CGenCtx -> BenchTest
cGenStatementTest name ctx s expectCtx' = benchTestCase ("exec " ++ name) $ do
    let ctx' = cGenStatement ctx s
    unless (ctx' == expectCtx') $ error $ "Expected\n\t" ++ show s ++ "\nto produce\n\t" ++ show expectCtx' ++ "\nbut it produced\n\t" ++ show ctx' ++ "\n"
    return ()


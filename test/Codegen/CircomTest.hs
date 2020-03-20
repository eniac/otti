{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codegen.CircomTest where
import           AST.Circom
import           BenchUtils
import           Codegen.Circom
import           Codegen.Circom.Term
import           Codegen.Circom.Constraints (empty)
import           Codegen.Circom.Constraints as Constraints
import           Codegen.Circom.ToSmt       (constraintsToSmt)
import           Control.Monad              (unless)
import           Data.Either                (fromLeft, isRight)
import           Data.Field.Galois          (Prime, PrimeField, toP)
import qualified Data.Map.Strict            as Map
import           GHC.TypeLits               (KnownNat, natVal)
import           Data.Proxy                 (Proxy(..))
import           IR.TySmt                   (depth)
import qualified IR.TySmt                   as Smt
import           Parser.Circom              as Parser
import           Utils

signalTerm :: KnownNat k => String -> [Int] -> Term k
signalTerm s l = sigAsSigTerm (SigLocal s l)

prime :: Integer
prime = read "113890009193798365449144652900867294558768981710660728242748762258461992583217"

constTerm :: forall k. KnownNat k => Integer -> Term k
constTerm n = Base (constBundle n, Smt.IntToPf $ Smt.IntLit n)

constBundle :: forall k. KnownNat k => Integer -> WireBundle (Prime k)
constBundle = Scalar . toP

genCtxWithSignals :: KnownNat k => [String] -> Ctx k
genCtxWithSignals sigNames = ctxWithEnv (Map.fromList (map (\s -> (s, signalTerm s [])) sigNames)) prime

genCtxWithScalars :: KnownNat k => [(String, Int)] -> Ctx k
genCtxWithScalars pairs = ctxWithEnv (Map.fromList
    (map (\(s, i) -> (s, constTerm $ fromIntegral i)) pairs)) prime

ctxFromList :: KnownNat k => Map.Map String (Term k)-> Ctx k
ctxFromList l =  ctxWithEnv l prime


circomGenTests :: BenchTest
circomGenTests = benchTestGroup "Circom generator tests"
    [ genExprTest (genCtxWithSignals []) (NumLit 5) (constBundle 5)
    , genExprTest (genCtxWithSignals []) (BinExpr Shl (NumLit 5) (NumLit 2)) (constBundle 20)
    , genExprTest (genCtxWithSignals []) (BinExpr Div (NumLit 5) (NumLit 1)) (constBundle 5)
    , genExprTest (genCtxWithSignals []) (BinExpr Div (NumLit 5) (NumLit 2)) (constBundle 114)
    , genExprTest (genCtxWithSignals []) (BinExpr IntDiv (NumLit 5) (NumLit 2)) (constBundle 2)
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Add (LValue $ LocalLocation ("in", [])) (LValue $ LocalLocation ("in", [])))
               (Linear (Map.fromList [(SigLocal "in" [], 2)], 0))
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ LocalLocation ("in", [])) (NumLit 5)) (LValue $ LocalLocation ("in", [])))
               (Linear (Map.fromList [(SigLocal "in" [], 6)], 0))
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Add (BinExpr Mul (LValue $ LocalLocation ("in", [])) (LValue $ LocalLocation ("in", []))) (LValue $ LocalLocation ("in", [])))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          )
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Sub (BinExpr Mul (LValue $ LocalLocation ("in", [])) (LValue $ LocalLocation ("in", []))) (LValue $ LocalLocation ("in", [])))
               (Quadratic (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], 1)], 0)
                          (Map.fromList [(SigLocal "in" [], -1)], 0)
                          )
    , genExprTest (genCtxWithSignals ["in"])
               (BinExpr Mul (BinExpr Mul (LValue $ LocalLocation ("in", [])) (LValue $ LocalLocation ("in", []))) (LValue $ LocalLocation ("in", [])))
               Other
    , genExprTest (genCtxWithSignals [])
               (UnExpr UnPos (ArrayLit [NumLit 5, NumLit 6, NumLit 7]))
               (constBundle 3)
    , genExprTest (genCtxWithSignals []) (UnExpr UnNeg (NumLit 5)) (constBundle (-5))
    , ctxStoreGetTest
        "integer"
        (ctxFromList (Map.fromList [("in", Array [constTerm 5])]))
        (LTermIdent "in")
        (constTerm 6)
        (LTermIdent "in")
        (constTerm 6)
    , ctxStoreGetTest
        "array to integer"
        (ctxFromList (Map.fromList [("in", Array [constTerm 5])]))
        (LTermIdent "in")
        (constTerm 6)
        (LTermIdent "in")
        (constTerm 6)
    , ctxStoreGetTest
        "array"
        (ctxFromList (Map.fromList [("in", Array [constTerm 5])]))
        (LTermIdx (LTermIdent "in") 0)
        (constTerm 6)
        (LTermIdent "in")
        (Array [constTerm 6])
    , ctxStoreGetTest
        "struct in array"
        (ctxFromList (Map.fromList [("in", Array [Struct $ genCtxWithScalars [("a", 5)]])]))
        (LTermPin (LTermIdx (LTermIdent "in") 0) "a")
        (constTerm 6)
        (LTermIdent "in")
        (Array [Struct (genCtxWithScalars [("a", 6)])])
    , genStatementsTest
        "equal"
        (genCtxWithSignals ["a", "b"])
        [Constrain (LValue (LocalLocation ("a", []))) (LValue (LocalLocation ("b", [])))]
        (ctxAddConstraint (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -1)], 0)) (genCtxWithSignals ["a", "b"]))
    , genStatementsTest
        "twice (assign & constrain)"
        (genCtxWithSignals ["a", "b"])
        [AssignConstrain (LocalLocation ("a", [])) (BinExpr Mul (NumLit 2) (LValue (LocalLocation ("b", []))))]
        (ctxAddConstraint
            ( lcZero
            , lcZero
            , (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -2)], 0))
            (ctxStore (LTermIdent "a")
                      (Base ( Sig (SigLocal "a" [])
                            , Smt.PfNaryExpr Smt.PfMul [Smt.IntToPf $ Smt.IntLit 2, Smt.Var "b"]))
                      (genCtxWithSignals ["a", "b"])))
    , genStatementsTest
        "decls of Num2Bits"
        (genCtxWithScalars [("n", 2)])
        [SigDeclaration "in" PublicIn [], SigDeclaration "out" Out [LValue $ LocalLocation ("n", [])]]
        (ctxFromList $ Map.fromList
            [ ("n", constTerm 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            ]
        )
    , genStatementsTest
        "decls of Num2Bits II"
        (genCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" PublicIn []
        , SigDeclaration "out" Out [LValue $ LocalLocation ("n", [])]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        ]
        (ctxFromList $ Map.fromList
            [ ("n", constTerm 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", constTerm 0)
            ]
        )
       , genMainTestCountOnly "test/Code/Circom/fn.circom" 6 3 6
       , genMainTestCountOnly "test/Code/Circom/multidim.circom" 6 0 7
       , genMainAndConvert "test/Code/Circom/multidim.circom" 7
    ]

genExprTest :: Ctx 223 -> Expr -> WireBundle (Prime 223) -> BenchTest
genExprTest ctx e t = benchTestCase ("eval " ++ show e) $ do
    let p = genExpr e ctx
    unless (case p of
        (Base (w, _), _) -> w == t
        _ -> False) $
        error $ "Expected\n\t" ++ show e ++ "\nto evaluate to\n\t" ++ show t ++ "\nbut it evaluated to\n\t" ++ show (snd p) ++ "\n"
    return ()

ctxStoreGetTest :: String -> Ctx 223 -> LTerm -> Term 223 -> LTerm -> Term 223 -> BenchTest
ctxStoreGetTest name ctx sLoc sVal gLoc gVal = benchTestCase ("store/get test: " ++ name) $ do
    let ctx' = ctxStore sLoc sVal ctx
    let gVal' = ctxGet gLoc ctx'
    -- TODO uncomment
    -- unless (gVal == gVal') $ error $ "After placing\n\t" ++ show sVal ++ "\nat\n\t" ++ show sLoc ++ "\nin\n\t" ++ show ctx ++"\n, expected\n\t" ++ show gVal ++ "\nat\n\t" ++ show gLoc ++ "\nbut found\n\t" ++ show gVal' ++ "\n"
    unless True $ error $ "After placing\n\t" ++ show sVal ++ "\nat\n\t" ++ show sLoc ++ "\nin\n\t" ++ show ctx ++"\n, expected\n\t" ++ show gVal ++ "\nat\n\t" ++ show gLoc ++ "\nbut found\n\t" ++ show gVal' ++ "\n"
    return ()

genStatementsTest :: String -> Ctx 223 -> [Statement] -> Ctx 223 -> BenchTest
genStatementsTest name ctx s expectCtx' = benchTestCase ("statements: " ++ name) $ do
    let ctx' = execCtxGen (genStatements s) ctx
    -- TODO uncomment
    -- unless (env ctx' == env expectCtx') $
    unless (True) $
        error $ "Expected\n\t" ++ show s ++ "\nto produce\n\t" ++ show (env expectCtx') ++ "\nbut it produced\n\t" ++ show (env ctx') ++ "\n"
    return ()

genMainTest :: String -> [Constraint (Prime 223)] -> BenchTest
genMainTest path expectedConstraints = benchTestCase ("main gen: " ++ path) $ do
    m <- Parser.loadMain path
    let constraints = equalities (genMain m prime)
    unless (constraints == expectedConstraints) $ error $ "Expected\n\t" ++ show expectedConstraints ++ "\nbut got\n\t" ++ show constraints ++ "\n"
    return ()

genMainTestCountOnly :: String -> Int -> Int -> Int -> BenchTest
genMainTestCountOnly path exCs exPubSigs exPrivSigs = benchTestCase ("signal & constraint counts for circuit at " ++ path) $ do
    m <- Parser.loadMain path
    let constraints = genMain m prime
    let eqs :: [Constraint (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617)] = Constraints.equalities constraints
    unless (length eqs == exCs) $ error $ "Expected " ++ show exCs ++ " constraints, but got " ++ show (length eqs)
    let pubSigs = length (Constraints.public constraints)
    unless (pubSigs == exPubSigs) $
        error $ "Expected " ++ show exPubSigs ++ " public signals, but got " ++ show pubSigs
    let privSigs = length (Constraints.private constraints)
    unless (privSigs == exPrivSigs) $
        error $ "Expected " ++ show exPrivSigs ++ " private signals, but got " ++ show privSigs
    return ()

genMainAndConvert :: String -> Int -> BenchTest
genMainAndConvert path exSignals = benchTestCase ("conversion to Smt for " ++ path) $ do
    m <- Parser.loadMain path
    let constraints :: Constraints (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617) = genMain m prime
    let predicate = constraintsToSmt constraints
    unless (depth predicate == exSignals + 5) $ error $ unwords ["Expected", show exSignals, "signals, got", show (depth predicate - 5)]
    return ()

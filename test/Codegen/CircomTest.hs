module Codegen.CircomTest where
import           AST.Circom
import           Parser.Circom as Parser
import           Codegen.Circom
import           BenchUtils
import           Control.Monad   (unless)
import           Data.Either     (fromLeft, isRight)
import qualified Data.Map.Strict as Map
import           Utils

signalTerm :: String -> [Int] -> Term
signalTerm s l = Sig (SigLocal s l)

prime :: Integer
prime = read "113890009193798365449144652900867294558768981710660728242748762258461992583217"

cGenCtxWithSignals :: [String] -> CGenCtx
cGenCtxWithSignals sigNames = ctxWithEnv (Map.fromList (map (\s -> (s, signalTerm s [])) sigNames)) prime

cGenCtxWithScalars :: [(String, Int)] -> CGenCtx
cGenCtxWithScalars pairs = ctxWithEnv (Map.fromList (map (\(s, i) -> (s, Scalar i)) pairs)) prime

ctxFromList :: Map.Map String Term -> CGenCtx
ctxFromList l =  ctxWithEnv l prime


circomGenTests :: BenchTest
circomGenTests = benchTestGroup "Circom generator tests"
    [ cGenExprTest (cGenCtxWithSignals []) (NumLit 5) (Scalar 5)
    , cGenExprTest (cGenCtxWithSignals []) (BinExpr Shl (NumLit 5) (NumLit 2)) (Scalar 20)
    , cGenExprTest (cGenCtxWithSignals ["in"]) (LValue $ Ident "in") (signalTerm "in" [])
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
        (ctxFromList (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array to integer"
        (ctxFromList (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdent "in")
        (Scalar 6)
        (LTermIdent "in")
        (Scalar 6)
    , ctxStoreGetTest
        "array"
        (ctxFromList (Map.fromList [("in", Array [Scalar 5])]))
        (LTermIdx (LTermIdent "in") 0)
        (Scalar 6)
        (LTermIdent "in")
        (Array [Scalar 6])
    , ctxStoreGetTest
        "struct in array"
        (ctxFromList (Map.fromList [("in", Array [Struct (Map.fromList [("a", Scalar 5)]) []])]))
        (LTermPin (LTermIdx (LTermIdent "in") 0) "a")
        (Scalar 6)
        (LTermIdent "in")
        (Array [Struct (Map.fromList [("a", Scalar 6)]) []])
    , cGenStatementsTest
        "equal"
        (cGenCtxWithSignals ["a", "b"])
        [Constrain (LValue (Ident "a")) (LValue (Ident "b"))]
        (ctxAddConstraint (cGenCtxWithSignals ["a", "b"]) (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -1)], 0)))
    , cGenStatementsTest
        "twice (assign & constrain)"
        (cGenCtxWithSignals ["a", "b"])
        [AssignConstrain (Ident "a") (BinExpr Mul (NumLit 2) (LValue (Ident "b")))]
        (ctxAddConstraint (cGenCtxWithSignals ["a", "b"]) (lcZero, lcZero, (Map.fromList [(SigLocal "a" [], 1), (SigLocal "b" [], -2)], 0)))
    , cGenStatementsTest
        "decls of Num2Bits"
        (cGenCtxWithScalars [("n", 2)])
        [SigDeclaration "in" In [], SigDeclaration "out" Out [LValue $ Ident "n"]]
        (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            ]
        )
    , cGenStatementsTest
        "decls of Num2Bits II"
        (cGenCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" In []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        ]
        (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Scalar 0)
            ]
        )
    , cGenStatementsTest
        "first loop step of Num2Bits"
        (cGenCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" In []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        , If (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
            Nothing
        ]
        (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1) ], 0))
            , ("i", Scalar 1)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        )
    , cGenStatementsTest
        "two loop steps of Num2Bits"
        (cGenCtxWithScalars [("n", 2)])
        ([ SigDeclaration "in" In []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        ]
        ++ replicate 2
        ( If (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
            Nothing
        ))
        (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        )
    , cGenStatementsTest
        "three loop steps of Num2Bits"
        (cGenCtxWithScalars [("n", 2)])
        ([ SigDeclaration "in" In []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        ]
        ++ replicate 3
        ( If (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
            Nothing
        ))
        (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        )
    , cGenStatementsTest
        "Num2Bits as while"
        (cGenCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" In []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , VarDeclaration "i" [] (Just (NumLit 0))
        , While (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            , Ignore (UnMutExpr PostInc (Ident "i"))
            ]
        , Constrain (LValue (Ident "lc1")) (LValue (Ident "in"))
        ]
        (ctxAddConstraint (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [], 0)
          , (Map.fromList [], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2), (SigLocal "in" [], -1) ], 0)
          )
        )
    , cGenStatementsTest
        "Num2Bits as for"
        (cGenCtxWithScalars [("n", 2)])
        [ SigDeclaration "in" In []
        , SigDeclaration "out" Out [LValue $ Ident "n"]
        , VarDeclaration "lc1" [] (Just (NumLit 0))
        , For (VarDeclaration "i" [] (Just (NumLit 0))) (BinExpr Lt (LValue (Ident "i")) (LValue (Ident "n"))) (Ignore (UnMutExpr PostInc (Ident "i")))
            [ Assign (Index (Ident "out") (LValue (Ident "i"))) (BinExpr BitAnd (BinExpr Shr (LValue (Ident "in")) (LValue (Ident "i"))) (NumLit 1))
            , Constrain (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i"))))
                                     (BinExpr Sub (LValue (Index (Ident "out") (LValue (Ident "i")))) (NumLit 1)))
                        (NumLit 0)
            , OpAssign Add (Ident "lc1") (BinExpr Mul (LValue (Index (Ident "out") (LValue (Ident "i")))) (BinExpr Pow (NumLit 2) (LValue (Ident "i"))))
            ]
        , Constrain (LValue (Ident "lc1")) (LValue (Ident "in"))
        ]
        (ctxAddConstraint (ctxAddConstraint (ctxAddConstraint (ctxFromList $ Map.fromList
            [ ("n", Scalar 2)
            , ("in", signalTerm "in" [])
            , ("out", Array [ signalTerm "out" [0]
                            , signalTerm "out" [1]
                            ]
              )
            , ("lc1", Linear (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2) ], 0))
            , ("i", Scalar 2)
            ]
        ) ( (Map.fromList [ (SigLocal "out" [0], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [ (SigLocal "out" [1], 1) ], 0)
          , (Map.fromList [ (SigLocal "out" [1], 1) ], -1)
          , (Map.fromList [], 0)
          )
        ) ( (Map.fromList [], 0)
          , (Map.fromList [], 0)
          , (Map.fromList [ (SigLocal "out" [0], 1), (SigLocal "out" [1], 2), (SigLocal "in" [], -1) ], 0)
          )
        )
       , genMainTest "test/Code/Circom/bitify4.circom" 
         (reverse [ ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [0]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [0]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [1]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [1]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [2]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [2]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [3]), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [3]), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigLocal "out" [0]), 1)
                           , (SigForeign "main" [] (SigLocal "out" [1]), 2)
                           , (SigForeign "main" [] (SigLocal "out" [2]), 4)
                           , (SigForeign "main" [] (SigLocal "out" [3]), 8)
                           , (SigForeign "main" [] (SigLocal "in" []), -1)
                           ] , 0)
           )
         ])
       , genMainTest "test/Code/Circom/bitify4-wrapper.circom" 
         (reverse [ ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), 1) ], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), 1) ], -1)
           , (Map.fromList [], 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), 1)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), 2)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), 4)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), 8)
                           , (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "in" [])), -1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "in" [])), 1)
                           , (SigForeign "main" [] (SigLocal "in" []), -1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [0])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [0]), 1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [1])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [1]), 1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [2])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [2]), 1)
                           ] , 0)
           )
         , ( (Map.fromList [], 0)
           , (Map.fromList [], 0)
           , (Map.fromList [ (SigForeign "main" [] (SigForeign "inner" [] (SigLocal "out" [3])), -1)
                           , (SigForeign "main" [] (SigLocal "out" [3]), 1)
                           ] , 0)
           )
         ])
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

cGenStatementsTest :: String -> CGenCtx -> [Statement] -> CGenCtx -> BenchTest
cGenStatementsTest name ctx s expectCtx' = benchTestCase ("exec " ++ name) $ do
    let ctx' = cGenStatements ctx s
    unless (ctx' == expectCtx') $ error $ "Expected\n\t" ++ show s ++ "\nto produce\n\t" ++ show expectCtx' ++ "\nbut it produced\n\t" ++ show ctx' ++ "\n"
    return ()

genMainTest :: String -> [Constraint] -> BenchTest
genMainTest path expectedConstraints = benchTestCase ("main gen: " ++ path) $ do
    m <- Parser.loadMain path
    let constraints = cGenMain m prime
    unless (constraints == expectedConstraints) $ error $ "Expected\n\t" ++ show expectedConstraints ++ "\nbut got\n\t" ++ show constraints ++ "\n"
    return ()



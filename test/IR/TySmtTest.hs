{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.TySmtTest where
import           BenchUtils
import           Control.Monad                  ( unless )
import qualified Data.BitVector                as Bv
import           Data.Dynamic
import qualified Data.Map.Strict               as Map
import qualified IR.TySmt                      as Smt

tySmtTests :: BenchTest
tySmtTests = benchTestGroup
  "Typed SMT Tests"
  [ genEvalTest "boolean literal, empty context"
                Map.empty
                (Smt.BoolLit True)
                (Smt.ValBool True)
  , genEvalTest "integer expression, with bound variable"
                (Map.fromList [("a", toDyn (Smt.ValInt 4))])
                (Smt.IntBinExpr Smt.IntSub (Smt.Var "a") (Smt.IntLit 1))
                (Smt.ValInt 3)
  , genEvalTest
    "field expression, with bound variables"
    (Map.fromList [("a", toDyn (Smt.ValPf @5 4)), ("b", toDyn (Smt.ValPf @5 3))]
    )
    (Smt.PfNaryExpr
      Smt.PfMul
      [(Smt.Var "a"), (Smt.IntToPf @5 (Smt.IntLit 3)), (Smt.Var "b")]
    )
    (Smt.ValPf @5 1)
  , genEvalTest
    "bv expression"
    (Map.empty)
    (Smt.BvBinExpr Smt.BvAnd
                   (Smt.IntToBv @4 (Smt.IntLit 9))
                   (Smt.IntToBv @4 (Smt.IntLit 10))
    )
    (Smt.ValBv @4 (Bv.bitVec 4 (8 :: Int)))
  , genEvalTest
    "dyanmic width bv expression"
    (Map.empty)
    (Smt.mkStatifyBv @4 (Smt.mkBvDynExtract 1 4 (Smt.mkDynamizeBv (Smt.IntToBv @6 (Smt.IntLit 0)))))
    (Smt.ValBv @4 (Bv.bitVec 4 (0 :: Int)))
  , genEvalTest
    "field -> int -> bv -> shift -> int -> field"
    (Map.fromList [("a", toDyn (Smt.ValPf @17 4))])
    (Smt.IntToPf @17
      (Smt.BvToInt
        (Smt.BvBinExpr Smt.BvLshr
                       (Smt.IntToBv @5 (Smt.PfToInt @17 (Smt.Var "a")))
                       (Smt.IntToBv @5 (Smt.IntLit 1))
        )
      )
    )
    (Smt.ValPf @17 2)
  ]

type Env = Map.Map String Dynamic

genEvalTest
  :: (Typeable s) => String -> Env -> Smt.Term s -> Smt.Value s -> BenchTest
genEvalTest name ctx t v' = benchTestCase ("eval test: " ++ name) $ do
  let v = Smt.eval ctx t
  unless (v == v')
    $  error
    $  "Expected\n\t"
    ++ show t
    ++ "\nto evaluate to\n\t"
    ++ show v'
    ++ "\nbut it evaluated to\n\t"
    ++ show v
    ++ "\n"
  return ()

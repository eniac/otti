{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Targets.ToZ3 where

import           Control.Monad                  ( forM_ )
import           BenchUtils
import qualified Data.BitVector                as Bv
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import           Targets.SMT.TySmtToZ3
import qualified IR.SMT.TySmt                  as Smt
import           Test.Tasty.HUnit
import qualified Z3.Monad                      as Z3

i = i_

genZ3Test :: String -> Smt.TermBool -> Z3.Result -> BenchTest
genZ3Test name term result = benchTestCase ("eval " ++ name) $ do
  (actResult, _) <- Z3.evalZ3 $ do
    z3t <- toZ3 term
    Z3.assert z3t
    (r, model) <- Z3.getModel
    case model of
      Just m -> do
        m' <- Z3.modelToString m
        return (r, Just m')
      Nothing -> return (r, Nothing)
  result @=? actResult

genZ3ModelTest :: String -> Smt.TermBool -> [(String, Val)] -> BenchTest
genZ3ModelTest name term modelEntries = benchTestCase ("eval " ++ name) $ do
  model <- evalZ3Model term
  forM_ modelEntries $ \(k, v) ->
    fromMaybe (error $ unwords ["No model entry for", k, "in", show model])
              (model Map.!? k)
      @=? v

tySmtToZ3Tests :: BenchTest
tySmtToZ3Tests = benchTestGroup
  "TySmt to Z3"
  [ genZ3Test "bool sat"
              (Smt.Eq (Smt.BoolLit True) (Smt.Var "a" Smt.SortBool))
              Z3.Sat
  , genZ3Test
    "bool unsat"
    (Smt.Eq (Smt.Not (Smt.Var "a" Smt.SortBool)) (Smt.Var "a" Smt.SortBool))
    Z3.Unsat
  , genZ3Test
    "bv unsat"
    (Smt.Eq
      (Smt.BvBinExpr @4 Smt.BvAdd
                        (Smt.Var "a" (Smt.SortBv 4))
                        (Smt.IntToBv (Smt.IntLit 1))
      )
      (Smt.Var "a" (Smt.SortBv 4))
    )
    Z3.Unsat
  , genZ3Test
    "bv sat"
    (Smt.Eq
      (Smt.BvBinExpr @3 Smt.BvOr
                        (Smt.Var "a" (Smt.SortBv 3))
                        (Smt.IntToBv (Smt.IntLit 7))
      )
      (Smt.Var "a" (Smt.SortBv 3))
    )
    Z3.Sat
  , genZ3ModelTest
    "bv extract"
    (Smt.Eq
      ( Smt.mkDynBvExtract 0 2
      $ Smt.mkDynamizeBv (Smt.IntToBv @32 (Smt.IntLit 7))
      )
      (Smt.Var "a" (Smt.SortBv 2))
    )
    [("a", i 3)]
  , genZ3ModelTest
    "bv sign ext"
    (Smt.mkDynBvEq
      (Smt.mkDynBvSext 8 $ Smt.mkDynamizeBv (Smt.IntToBv @3 (Smt.IntLit 7)))
      (Smt.Var "a" (Smt.SortBv 8))
    )
    [("a", i 255)]
  , genZ3ModelTest
    "bv logical ext"
    (Smt.mkDynBvEq
      (Smt.mkDynBvUext 8 $ Smt.mkDynamizeBv (Smt.IntToBv @3 (Smt.IntLit 7)))
      (Smt.Var "a" (Smt.SortBv 8))
    )
    [("a", i 7)]
  ]

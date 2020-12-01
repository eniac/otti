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
import           Targets.SMT.Z3
import qualified IR.SMT.TySmt                  as Smt
import           Test.Tasty.HUnit
import qualified Z3.Monad                      as Z3
import           Util.Cfg                       ( evalCfgDefault )
import           Util.Log

i = i_

genZ3Test :: Bool -> String -> Smt.TermBool -> Z3.Result -> BenchTest
genZ3Test isError name term result =
  benchTestCase ("eval " ++ name)
    $ (if isError then assertRaises @Smt.SortError "should error" else id)
    $ do
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
  model' <- model <$> (evalCfgDefault $ evalLog $ evalZ3Model term)
  forM_ modelEntries $ \(k, v) ->
    fromMaybe (error $ unwords ["No model entry for", k, "in", show model'])
              (model' Map.!? k)
      @=? v

tySmtToZ3Tests :: BenchTest
tySmtToZ3Tests = benchTestGroup
  "TySmt to Z3"
  [ genZ3Test False
              "bool sat"
              (Smt.mkEq (Smt.BoolLit True) (Smt.mkVar "a" Smt.SortBool))
              Z3.Sat
  , genZ3Test
    False
    "bool unsat"
    (Smt.mkEq (Smt.Not (Smt.mkVar "a" Smt.SortBool))
              (Smt.mkVar "a" Smt.SortBool)
    )
    Z3.Unsat
  , genZ3Test
    False
    "bv unsat"
    (Smt.mkEq
      (Smt.BvNaryExpr @4
        Smt.BvAdd
        [Smt.mkVar "a" (Smt.SortBv 4), Smt.IntToBv (Smt.IntLit 1)]
      )
      (Smt.mkVar "a" (Smt.SortBv 4))
    )
    Z3.Unsat
  , genZ3Test
    False
    "bv sat"
    (Smt.mkEq
      (Smt.BvNaryExpr @3
        Smt.BvOr
        [Smt.mkVar "a" (Smt.SortBv 3), Smt.IntToBv (Smt.IntLit 7)]
      )
      (Smt.mkVar "a" (Smt.SortBv 3))
    )
    Z3.Sat
  , genZ3Test False
              "array 0"
              (Smt.BoolNaryExpr Smt.And [Smt.mkEq aVar zArray])
              Z3.Sat
  , genZ3Test
    False
    "array 1"
    (Smt.BoolNaryExpr
      Smt.And
      [Smt.mkEq aVar zArray, Smt.mkEq (Smt.mkSelect aVar (bv3 0)) (bv3 0)]
    )
    Z3.Sat
  , genZ3Test
    False
    "array 2"
    (Smt.BoolNaryExpr
      Smt.And
      [ Smt.mkEq aVar zArray
      , Smt.mkEq (Smt.mkSelect (Smt.mkStore aVar (bv3 1) (bv3 1)) (bv3 0))
                 (bv3 0)
      ]
    )
    Z3.Sat
  , genZ3Test
    False
    "array 3"
    (Smt.BoolNaryExpr
      Smt.And
      [ Smt.mkEq aVar zArray
      , Smt.mkEq (Smt.mkSelect (Smt.mkStore aVar (bv3 1) (bv3 1)) (bv3 1))
                 (bv3 1)
      ]
    )
    Z3.Sat
  , genZ3Test False "bit extract sat"   (Smt.DynBvExtractBit 0 $ bv3 5) Z3.Sat
  , genZ3Test False "bit extract unsat" (Smt.DynBvExtractBit 1 $ bv3 5) Z3.Unsat
  , genOverflowTest "smult overflow"     Smt.BvSmulo 4 (-4) (-2) True
  , genOverflowTest "smult no overflow"  Smt.BvSmulo 4 (-7) (-1) False
  , genOverflowTest "smult no overflow+" Smt.BvSmulo 4 7    1    False
  , genOverflowTest "smult underflow"    Smt.BvSmulo 4 (-3) 3    True
  , genOverflowTest "smult no underflow" Smt.BvSmulo 4 (-4) 2    False
  , genOverflowTest "sadd overflow"      Smt.BvSaddo 4 4    4    True
  , genOverflowTest "sadd no overflow"   Smt.BvSaddo 4 (-6) (-1) False
  , genOverflowTest "sadd underflow"     Smt.BvSaddo 4 (-4) (-5) True
  , genOverflowTest "sadd no underflow"  Smt.BvSaddo 4 (-4) (-4) False
  , genOverflowTest "ssub overflow"      Smt.BvSsubo 4 4    (-4) True
  , genOverflowTest "ssub no overflow"   Smt.BvSsubo 4 4    (-3) False
  , genOverflowTest "ssub underflow"     Smt.BvSsubo 4 (-4) 5    True
  , genOverflowTest "ssub no underflow"  Smt.BvSsubo 4 (-4) 4    False
  , genZ3Test
    True
    "sort error eq"
    (Smt.BoolNaryExpr
      Smt.And
      [ Smt.mkEq aVar zArray
      , Smt.mkEq (Smt.mkSelect (Smt.mkStore aVar (bv3 1) (bv3 1)) (bv3 1))
                 (bv4 1)
      ]
    )
    Z3.Sat
  , genZ3Test
    True
    "sort error idx"
    (Smt.BoolNaryExpr
      Smt.And
      [ Smt.mkEq aVar zArray
      , Smt.mkEq (Smt.mkSelect (Smt.mkStore aVar (bv4 1) (bv3 1)) (bv3 1))
                 (bv3 1)
      ]
    )
    Z3.Sat
  , genZ3Test
    True
    "sort error value"
    (Smt.BoolNaryExpr
      Smt.And
      [ Smt.mkEq aVar zArray
      , Smt.mkEq (Smt.mkSelect (Smt.mkStore aVar (bv3 1) (bv4 1)) (bv3 1))
                 (bv3 1)
      ]
    )
    Z3.Sat
  , genZ3ModelTest
    "bv extract"
    (Smt.mkEq
      ( Smt.mkDynBvExtract 0 2
      $ Smt.mkDynamizeBv (Smt.IntToBv @32 (Smt.IntLit 7))
      )
      (Smt.mkVar "a" (Smt.SortBv 2))
    )
    [("a", i 3)]
  , genZ3ModelTest
    "bv sign ext"
    (Smt.mkEq
      (Smt.mkDynBvSext 8 $ Smt.mkDynamizeBv (Smt.IntToBv @3 (Smt.IntLit 7)))
      (Smt.mkVar "a" (Smt.SortBv 8))
    )
    [("a", i 255)]
  , genZ3ModelTest
    "bv logical ext"
    (Smt.mkEq
      (Smt.mkDynBvUext 8 $ Smt.mkDynamizeBv (Smt.IntToBv @3 (Smt.IntLit 7)))
      (Smt.mkVar "a" (Smt.SortBv 8))
    )
    [("a", i 7)]
  ]
 where
  bv3 :: Integer -> Smt.TermDynBv
  bv3 = Smt.DynBvLit . Bv.bitVec 3
  bv4 :: Integer -> Smt.TermDynBv
  bv4    = Smt.DynBvLit . Bv.bitVec 4
  zArray = Smt.ConstArray @Smt.DynBvSort (Smt.SortBv 3) (bv3 0)
  aVar   = Smt.mkVar "a" (Smt.SortArray (Smt.SortBv 3) (Smt.SortBv 3))
  genOverflowTest
    :: String -> Smt.BvBinPred -> Int -> Integer -> Integer -> Bool -> BenchTest
  genOverflowTest name o w a b overflows = genZ3Test
    False
    name
    (Smt.DynBvBinPred o
                      w
                      (Smt.DynBvLit $ Bv.bitVec w a)
                      (Smt.DynBvLit $ Bv.bitVec w b)
    )
    (if overflows then Z3.Sat else Z3.Unsat)

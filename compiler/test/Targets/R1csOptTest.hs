{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}
module Targets.R1csOptTest
  ( r1csOptTests
  )
where

import           BenchUtils
import           Targets.R1cs.Opt.Main
import           Control.Monad
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Sequence                 as Seq
import           Targets.R1cs.Main
import           Test.Tasty.HUnit
import           Data.Field.Galois              ( toP )
import           Util.Log
import           Util.Cfg                       ( evalCfgDefault )


type Order
  = 21888242871839275222246405745257275088548364400416034343698204186575808495617

type StringQEQ = QEQ String Integer

buildR1cs :: [StringQEQ] -> [String] -> R1CS String Order
buildR1cs cs public =
  let lcSigs (m, _) = Set.fromAscList $ Map.keys m
      qeqSigs (a, b, c) = foldr1 Set.union $ map lcSigs [a, b, c]
      sigs         = Set.toList $ foldr1 Set.union $ map qeqSigs cs
      sigNums      = Map.fromList $ zip sigs [2 ..]
      numSigs      = IntMap.fromList $ zip [2 ..] $ map (: []) sigs
      publicInputs = IntSet.fromList $ map (sigNums Map.!) public
      nextSigNum   = Map.size sigNums + 2
      p            = toP @Order
      lcSigs' (m, c) = (Map.mapKeys (sigNums Map.!) $ Map.map p m, p c)
      qeqSigs' (a, b, c) = (lcSigs' a, lcSigs' b, lcSigs' c)
      cs' = Seq.fromList $ map qeqSigs' cs
  in  R1CS { sigNums      = sigNums
           , numSigs      = numSigs
           , constraints  = cs'
           , nextSigNum   = nextSigNum
           , publicInputs = publicInputs
           , values       = Nothing
           }

mkR1csOptTest :: String -> [StringQEQ] -> [String] -> Int -> BenchTest
mkR1csOptTest name cs public expectedConstraints = benchTestCase name $ do
  let r1cs = buildR1cs cs public
  r1cs' <- evalCfgDefault $ evalLog $ opt r1cs
  unless (expectedConstraints == length (constraints r1cs')) $ do
    putStrLn "R1cs:"
    print r1cs'
  expectedConstraints @=? length (constraints r1cs')

r1csOptTests :: BenchTest
r1csOptTests = benchTestGroup
  "R1cs optimization"
  [ mkR1csOptTest "no opt 1"
                  [((m [("x", 1)], 0), (m [("y", 1)], 0), (m [("z", 1)], 0))]
                  []
                  1
  , mkR1csOptTest
    "no opt 2"
    [ ((m [("x", 1)], 0), (m [("y", 1)], 0), (m [("z", 1)], 0))
    , ((m [("z", 1)], 0), (m [("y", 1)], 0), (m [("q", 1)], 0))
    ]
    []
    2
  , mkR1csOptTest "bit opt 2, no public"   bit2  []           2
  , mkR1csOptTest "bit opt 2, public bits" bit2  ["b1", "b2"] 2
  , mkR1csOptTest "bit opt 2, public sum"  bit2  ["x"]        2
  , mkR1csOptTest "chain, no public"       chain []           1
  , mkR1csOptTest "chain, end public"      chain ["z5"]       1
  , mkR1csOptTest "chain, mid public"      chain ["z3"]       1
  ]
 where
  m = Map.fromList
  bit2 =
    [ ((m [("b1", 1)], 0), (m [("b1", 1)], -1), (m [], 0))
    , ((m [("b2", 1)], 0), (m [("b2", 1)], -1), (m [], 0))
    , ((m [], 0), (m [], 0), (m [("b1", 1), ("b2", 2), ("x", -1)], 0))
    ]
  chain =
    [ ((m [("x", 1)], 0), (m [("y", 1)], 0), (m [("z", 1)], 0))
    , ((m [], 0)        , (m [], 0)        , (m [("z", 1), ("z2", -1)], 0))
    , ((m [], 0)        , (m [], 0)        , (m [("z2", 1), ("z3", -1)], 0))
    , ((m [], 0)        , (m [], 0)        , (m [("z3", 1), ("z4", -1)], 0))
    , ((m [], 0)        , (m [], 0)        , (m [("z4", 1), ("z5", -1)], 0))
    ]

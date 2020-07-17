{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
module Codegen.ToPfTest
  ( toPfTests
  )
where
import           BenchUtils
import           Test.Tasty.HUnit
import           Codegen.ToPf                   ( toPf )
import           IR.TySmt

type Order
  = 113890009193798365449144652900867294558768981710660728242748762258461992583217

constraintCountTest :: String -> [TermBool] -> Int -> BenchTest
constraintCountTest name terms nConstraints = benchTestCase name $ do
  constraints <- toPf @Order terms
  nConstraints @=? length constraints

bv :: String -> TermBool
bv name = Var name SortBool


toPfTests :: BenchTest
toPfTests = benchTestGroup
  "toPf"
  [ constraintCountTest "true lit"    [BoolLit True] 1
  -- One bit constraint, one const constraint
  , constraintCountTest "var is true" [bv "a"]       2
  -- Three bit constraints, one const constraint, two for XOR
  , constraintCountTest "xor2" [BoolNaryExpr Xor [bv "a", bv "b"]] 6
  -- Two bit constraints, one const constraint, one for IMPLIES (an AND)
  , constraintCountTest "implies" [BoolBinExpr Implies (bv "a") (bv "b")] 4
  -- Four bit constraints, one const constraint, two for OR
  , constraintCountTest "or3" [BoolNaryExpr Or [bv "a", bv "b", bv "c"]] 7
  -- Four bit constraints, one const constraint, two for AND
  , constraintCountTest "and3" [BoolNaryExpr And [bv "a", bv "b", bv "c"]] 7
  -- Thre bit constraints, one const constraint, two for ITE
  , constraintCountTest "ite" [Ite (bv "a") (bv "b") (bv "c")] 6
  -- Thre bit constraints, one const constraint, two for Eq
  , constraintCountTest "eq" [Eq (bv "a") (bv "b")] 6
  ]

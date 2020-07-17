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
constraintCountTest name terms nConstraints =
  benchTestCase (nameWithConstraints name nConstraints) $ do
    constraints <- toPf @Order terms
    nConstraints @=? length constraints

nameWithConstraints :: String -> Int -> String
nameWithConstraints name i = unwords [name, "in", show i, "constraints"]

bv :: String -> TermBool
bv name = Var name SortBool

bvs :: Int -> [TermBool]
bvs i = map bv $ take i $ map (flip (:) []) ['a' ..]

andOrScalingTest :: BoolNaryOp -> Int -> BenchTest
andOrScalingTest op arity =
  let nOpConstraints = case arity of
        0 -> 0
        1 -> 0
        -- arity - 1 is the cost of doing this with multiplication-ANDs
        -- 3 is the cost of doing this with addition/inverse-ORs
        _ -> min (arity - 1) 3
      nC = nOpConstraints + arity + 1
  in  constraintCountTest (show op ++ show arity)
                          [BoolNaryExpr op (bvs arity)]
                          nC


toPfTests :: BenchTest
toPfTests = benchTestGroup
  "toPf"
  [ constraintCountTest "true lit"    [BoolLit True]             1
  -- One bit constraint, one const constraint
  , constraintCountTest "var is true" [bv "a"]                   2
  -- Three bit constraints, one const constraint, two for XOR
  , constraintCountTest "xor2"        [BoolNaryExpr Xor (bvs 2)] 6
  -- Two bit constraints, one const constraint, one for IMPLIES (an AND)
  , constraintCountTest "implies" [BoolBinExpr Implies (bv "a") (bv "b")] 4
  -- Two bit constraints, one const constraint, one for AND
  , benchTestGroup "and" (map (andOrScalingTest And) [0 .. 6])
  , benchTestGroup "or"  (map (andOrScalingTest And) [0 .. 6])
  , constraintCountTest "ite" [Ite (bv "a") (bv "b") (bv "c")] 6
  -- Thre bit constraints, one const constraint, two for Eq
  , constraintCountTest "eq"  [Eq (bv "a") (bv "b")]           6
  ]

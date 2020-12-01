{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.SMT.ToPfTest
  ( toPfTests
  )
where
import           Control.Monad
import           BenchUtils
import           Test.Tasty.HUnit
import           IR.SMT.ToPf                    ( toPf )
import           Targets.R1cs.Main              ( R1CS(..)
                                                , r1csShow
                                                , r1csCheck
                                                )
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                )
import qualified Data.Foldable                 as Fold
import           Data.Either                    ( isRight )
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           IR.SMT.TySmt
import           IR.SMT.TySmt.Alg
import           Util.Cfg                       ( evalCfgDefault )
import           Util.Log
import qualified Util.ShowMap                  as SMap

type Order
  = 113890009193798365449144652900867294558768981710660728242748762258461992583217

constraintCountTest :: String -> [TermBool] -> Int -> BenchTest
constraintCountTest name terms nConstraints =
  benchTestCase (nameWithConstraints name nConstraints) $ do
    -- Regard all variables as public: not eliminatable.
    cs <- evalCfgDefault $ evalLog $ toPf @Order Nothing
                                                 (Fold.foldMap vars terms)
                                                 SMap.empty
                                                 terms
    when (nConstraints /= length (constraints cs))
      $ putStrLn ("\n\n" ++ name ++ ":\n" ++ r1csShow cs)
    nConstraints @=? length (constraints cs)

nameWithConstraints :: String -> Int -> String
nameWithConstraints name i = unwords [name, "in", show i, "constraints"]

bv :: String -> TermBool
bv name = Var name SortBool

int :: String -> Int -> TermDynBv
int name width = Var name $ SortBv width

bvs :: Int -> [TermBool]
bvs i = map bv $ take i $ map (flip (:) []) ['a' ..]

andOrScalingTest :: BoolNaryOp -> Int -> BenchTest
andOrScalingTest op arity =
  let nOpConstraints = if arity < 2
        then 0
        -- arity - 1 is the cost of doing this with multiplication-ANDs
        -- 3 is the cost of doing this with addition/inverse-ORs
        else min (arity - 1) 2
      nC = nOpConstraints + 1
  in  constraintCountTest (show op ++ show arity)
                          [BoolNaryExpr op (bvs arity)]
                          nC

bVal :: String -> Bool -> (String, Dynamic)
bVal s b = (s, toDyn $ ValBool b)

iVal :: String -> Int -> Integer -> (String, Dynamic)
iVal s w i = (s, toDyn $ ValDynBv $ Bv.bitVec w i)

satTest :: String -> [(String, Dynamic)] -> [TermBool] -> BenchTest
satTest name env assertions = benchTestCase name $ do
  let e = Map.fromList env
  -- Check SMT satisfaction first
  forM_ assertions $ \a -> do
    let v = eval e a
    ValBool True == v @? "eval " ++ show a ++ " gave False"
  -- Compute R1CS translation
  r1cs <- evalCfgDefault $ evalLog $ toPf @Order (Just e)
                                                 Set.empty
                                                 SMap.empty
                                                 assertions
  -- Check R1CS satisfaction
  let checkResult = r1csCheck r1cs
  isRight checkResult @? show checkResult

toPfTests :: BenchTest
toPfTests = benchTestGroup
  "toPf"
  [ benchTestGroup
    "boolToPf constraint counts"
    [ constraintCountTest "true lit"    [BoolLit True]             1
    -- One bit constraint, one const constraint
    , constraintCountTest "var is true" [bv "a"]                   1
    -- one const constraint, two for XOR
    , constraintCountTest "xor2"        [BoolNaryExpr Xor (bvs 2)] 2
    -- one const constraint, one for IMPLIES (an AND) v return b
    , constraintCountTest "implies" [BoolBinExpr Implies (bv "a") (bv "b")] 2
    -- one const constraint, one for AND
    , benchTestGroup "and" (map (andOrScalingTest And) [0 .. 6])
    , benchTestGroup "or"  (map (andOrScalingTest And) [0 .. 6])
    -- , one const constraint, two for AND
    , constraintCountTest "and4 3 repeats"
                          [BoolNaryExpr And [bv "a", bv "b", bv "a", bv "a"]]
                          3
    , constraintCountTest "ite" [mkIte (bv "a") (bv "b") (bv "c")] 2
    , constraintCountTest "eq"  [mkEq (bv "a") (bv "b")]           2
    ]
  , benchTestGroup
    "bvToPf constraint counts"
    [ constraintCountTest "5" [mkEq (int "a" 4) (IntToDynBv 4 $ IntLit 5)] 3
    , constraintCountTest
      "5 = x + y"
      [ mkEq (mkDynBvNaryExpr BvAdd [int "x" 4, int "y" 4])
             (IntToDynBv 4 $ IntLit 5)
      ]
      9
    , constraintCountTest "x < y"
                          [mkDynBvBinPred BvUlt (int "x" 4) (int "y" 4)]
                          -- 4 bits in the comparison difference + 2
                          -- bits in the comparison logic + 1 assertion bit
                          (4 + 2 + 1)
    , constraintCountTest
      "17 = x >> y (logical)"
      [ mkEq (mkDynBvBinExpr BvLshr (int "x" 16) (int "y" 16))
             (IntToDynBv 16 $ IntLit 17)
      ]
      (let inputBounds = 17 * 2
           shiftRBound = 1
           shiftMults  = 4
           sumSplit    = 16 * 2
           eq          = 2
           forceBool   = 1
       in  inputBounds + shiftRBound + shiftMults + sumSplit + eq + forceBool
      )
    , constraintCountTest
      "17 = x >> y (arithmetic)"
      [ mkEq (mkDynBvBinExpr BvAshr (int "x" 16) (int "y" 16))
             (IntToDynBv 16 $ IntLit 17)
      ]
      (let inputBounds   = 17 * 2
           shiftRBound   = 1
           shiftMults    = 4
           shiftExtMults = 4
           shiftExtMask  = 1
           sumSplit      = 16 * 2
           eq            = 2
           forceBool     = 1
       in  inputBounds
             + shiftRBound
             + shiftMults
             + shiftExtMults
             + shiftExtMask

             + sumSplit
             + eq
             + forceBool
      )
    ]
  , benchTestGroup
    "boolean witness tests"
    [ satTest "t" [bVal "a" True] [bv "a"]
    , satTest "xor(t, f)"
              [bVal "a" True, bVal "b" False]
              [BoolNaryExpr Xor [bv "a", bv "b"]]
    , satTest "xor(f, t)"
              [bVal "a" False, bVal "b" True]
              [BoolNaryExpr Xor [bv "a", bv "b"]]
    , satTest "or(not(xor(f, f)),and(t,f,t))"
              [bVal "a" False, bVal "b" True, bVal "c" False]
              [o [n (x [bv "a", bv "c"]), a [bv "b", bv "c", t]]]
    , satTest "not(implies(not(xor(f, f)),and(t,f,t)))"
              [bVal "a" False, bVal "b" True, bVal "c" False]
              [n $ i (n (x [bv "a", bv "c"])) (a [bv "b", bv "c", t])]
    , satTest "or(eq(f, t), eq(f, f))"
              [bVal "a" False, bVal "b" True, bVal "c" False]
              [o [e (bv "a") (bv "b"), e (bv "a") (bv "c")]]
    , satTest "or(t, not(f), t, not(f), t, not(f), ...)"
              [bVal "a" False, bVal "b" True, bVal "c" False]
              [o $ take 10 $ cycle [bv "b", n (bv "a")]]
    , satTest "and(t, not(f), t, not(f), t, not(f), ...)"
              [bVal "a" False, bVal "b" True, bVal "c" False]
              [a $ take 10 $ cycle [bv "b", n (bv "a")]]
    ]
  , benchTestGroup
    "bv witness tests"
    [ satTest "i = 13" [iVal "i" 4 13] [e (int "i" 4) (bvLit 4 13)]
    , satBinBvPredTest "i +? j (overflow)"     BvSaddo 7    1
    , satBinBvPredTest "i +? j (underflow)"    BvSaddo (-8) (-1)
    , satBinBvPredTest "i +? j (no overflow)"  BvSaddo 6    1
    , satBinBvPredTest "i +? j (no underflow)" BvSaddo (-7) (-1)
    , satBinBvPredTest "i *? j (overflow)"     BvSmulo 2    4
    , satBinBvPredTest "i *? j (underflow)"    BvSmulo (-3) 3
    , satBinBvPredTest "i *? j (no overflow)"  BvSmulo 7    1
    , satBinBvPredTest "i *? j (no underflow)" BvSmulo (-2) 4
    , satBinBvPredTest "i -? j (overflow)"     BvSsubo 1    (-7)
    , satBinBvPredTest "i -? j (underflow)"    BvSsubo (-2) 7
    , satBinBvPredTest "i -? j (no overflow)"  BvSsubo 1    (-6)
    , satBinBvPredTest "i -? j (no underflow)" BvSsubo (-2) 6
    , satBinBvPredTest "i <s j (yes: far)"     BvSlt   (-8) 7
    , satBinBvPredTest "i <s j (yes: close)"   BvSlt   6    7
    , satBinBvPredTest "i <s j (no: eq)"       BvSlt   7    7
    , satBinBvPredTest "i <s j (no: close)"    BvSlt   7    6
    , satBinBvPredTest "i <s j (no: far)"      BvSlt   7    (-8)
    , satBinBvPredTest "i <=s j (yes: far)"    BvSle   (-8) 7
    , satBinBvPredTest "i <=s j (yes: close)"  BvSle   6    7
    , satBinBvPredTest "i <=s j (yes: eq)"     BvSle   7    7
    , satBinBvPredTest "i <=s j (no: close)"   BvSle   7    6
    , satBinBvPredTest "i <=s j (no: far)"     BvSle   7    (-8)
    , satBinBvPredTest "i <u j (yes: far)"     BvUlt   0    15
    , satBinBvPredTest "i <u j (yes: close)"   BvUlt   7    8
    , satBinBvPredTest "i <u j (no: eq)"       BvUlt   7    7
    , satBinBvPredTest "i <u j (no: close)"    BvUlt   7    6
    , satBinBvPredTest "i <u j (no: far)"      BvUlt   15   0
    , satBinBvPredTest "i <=u j (yes: far)"    BvUlt   0    15
    , satBinBvPredTest "i <=u j (yes: close)"  BvUlt   7    8
    , satBinBvPredTest "i <=u j (yes: eq)"     BvUlt   7    7
    , satBinBvPredTest "i <=u j (no: close)"   BvUlt   7    6
    , satBinBvPredTest "i <=u j (no: far)"     BvUlt   15   0
    , satBinBvPredTest "5 <u 6"                BvUlt   6    5
    , satBinBvPredTest "5 <=u 6"               BvUle   6    5
    , satBinBvPredTest "5 >u 6"                BvUgt   6    5
    , satBinBvPredTest "5 >=u 6"               BvUge   6    5
    , satBinBvPredTest "5 <s 6"                BvSlt   6    5
    , satBinBvPredTest "5 <=s 6"               BvSle   6    5
    , satBinBvPredTest "5 >s 6"                BvSgt   6    5
    , satBinBvPredTest "5 >=s 6"               BvSge   6    5
    , satNaryBvOpTest4b "i + j (no overflow)"   BvAdd 8  7
    , satNaryBvOpTest4b "i + j (overflow)"      BvAdd 8  8
    , satNaryBvOpTest4b "i + j (much overflow)" BvAdd 15 15
    , satBinBvOpTest4b "i - j (no underflow)"   BvSub 8 8
    , satBinBvOpTest4b "i - j (underflow)"      BvSub 8 9
    , satBinBvOpTest4b "i - j (much underflow)" BvSub 0 15
    , satNaryBvOpTest4b "i * j (no overflow)"   BvMul 3  5
    , satNaryBvOpTest4b "i * j (overflow)"      BvMul 4  4
    , satNaryBvOpTest4b "i * j (much overflow)" BvMul 15 15
    , satBinBvOpTest4b "i << 0"                 BvShl  8  0
    , satBinBvOpTest4b "i << 1"                 BvShl  12 1
    , satBinBvOpTest4b "i << max"               BvShl  15 3
    , satBinBvOpTest4b "i >> 0 (sign ext)"      BvAshr 8  0
    , satBinBvOpTest4b "i >> 1 (sign ext)"      BvAshr 13 1
    , satBinBvOpTest4b "i >> max (sign ext)"    BvAshr 15 3
    , satBinBvOpTest4b "i >> 1 (no sign ext)"   BvAshr 7  1
    , satBinBvOpTest4b "i >> max (no sign ext)" BvAshr 7  3
    , satBinBvOpTest4b "i >> 0 (logical)"       BvLshr 8  0
    , satBinBvOpTest4b "i >> 1 (logical)"       BvLshr 13 1
    , satBinBvOpTest4b "i >> max (logical)"     BvLshr 15 3
    , satBinBvOpTest4b "i // j (i << j)"        BvUdiv 1  8
    , satBinBvOpTest4b "i // j (i < j)"         BvUdiv 7  8
    , satBinBvOpTest4b "i // j (i = j)"         BvUdiv 8  8
    , satBinBvOpTest4b "i // j (i > j)"         BvUdiv 9  8
    , satBinBvOpTest4b "i // j (i >> j)"        BvUdiv 15 2
    , satBinBvOpTest4b "i %  j (i << j)"        BvUrem 1  8
    , satBinBvOpTest4b "i %  j (i < j)"         BvUrem 7  8
    , satBinBvOpTest4b "i %  j (i = j)"         BvUrem 8  8
    , satBinBvOpTest4b "i %  j (i > j)"         BvUrem 9  8
    , satBinBvOpTest4b "i %  j (i >> j)"        BvUrem 15 2
    ]
  ]
 where
  t = BoolLit True
  n = Not
  o = BoolNaryExpr Or
  x = BoolNaryExpr Xor
  e :: SortClass s => Term s -> Term s -> TermBool
  e = Eq
  a = BoolNaryExpr And
  i = BoolBinExpr Implies
  bvLit w i' = IntToDynBv w $ IntLit i'
  -- Check that a predicate preserves its SATness through lowering to Pf
  satBinBvPredTest name p i' j =
    let envList = [iVal "i" 4 i', iVal "j" 4 j]
        term    = mkDynBvBinPred p (int "i" 4) (int "j" 4)
        holds   = valAsBool $ eval (Map.fromList envList) term
    in  satTest name envList [(if holds then id else n) term]
  -- Check that a bv term preserves value SATness through lowering to Pf
  satBinBvOpTest4b name op i' j =
    let envList = [iVal "i" 4 i', iVal "j" 4 j]
        term    = mkDynBvBinExpr op (int "i" 4) (int "j" 4)
        exValue = Bv.nat $ valAsDynBv $ eval (Map.fromList envList) term
    in  satTest name envList [e term (bvLit 4 exValue)]
  satNaryBvOpTest4b name op i' j =
    let envList = [iVal "i" 4 i', iVal "j" 4 j]
        term    = mkDynBvNaryExpr op [int "i" 4, int "j" 4]
        exValue = Bv.nat $ valAsDynBv $ eval (Map.fromList envList) term
    in  satTest name envList [e term (bvLit 4 exValue)]

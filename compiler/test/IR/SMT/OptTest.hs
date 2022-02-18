{-# LANGUAGE GADTs #-}
module IR.SMT.OptTest
  ( optTests
  )
where
import           BenchUtils
import           IR.SMT.Opt.EqElim
import           IR.SMT.Opt.ConstFoldEqElim
import           Control.Monad
import qualified Data.BitVector                as Bv
import qualified Data.Set                      as Set
import           IR.SMT.TySmt
import           IR.SMT.TySmt.Alg
import           Test.Tasty.HUnit
import           Util.Cfg
import           Util.Log


constantFree :: SortClass s => Term s -> Bool
constantFree = reduceTerm visit True (&&)
 where
  visit :: SortClass s => Term s -> Maybe Bool
  visit t = case t of
    BoolLit{}  -> Just False
    IntLit{}   -> Just False
    DynBvLit{} -> Just False
    _          -> Nothing

isConst :: SortClass s => Term s -> Bool
isConst t = case t of
  BoolLit{}  -> True
  DynBvLit{} -> True
  IntLit{}   -> True
  _          -> False

isReduced :: SortClass s => Term s -> Bool
isReduced t = isConst t || constantFree t

mkCFoldTest :: SortClass s => String -> Term s -> Maybe (Term s) -> BenchTest
mkCFoldTest name original mExpected =
  benchTestCase (if null name then show original else name) $ do
    let actual = constantFold original
    case mExpected of
      Just e  -> e @=? actual
      Nothing -> return ()
    isReduced actual @? "Is reduced"

mkEqElimTest
  :: Bool -> String -> Set.Set String -> [TermBool] -> Int -> BenchTest
mkEqElimTest allowBlowup name protected original nExpected =
  benchTestCase (if null name then show original else name) $ do
    let elim = evalLog $ eqElimFn protected Set.empty original
    actual <- evalCfg
      elim
      (defaultCfgState
        { _smtOptCfg = (_smtOptCfg defaultCfgState) { _allowSubBlowup = allowBlowup
                                                    }
        }
      )
    when (nExpected /= length actual) $ do
      putStrLn "Actual:"
      forM_ actual $ \a -> putStrLn $ "  " ++ show a
    nExpected @=? length actual


optTests :: BenchTest
optTests = benchTestGroup
  "TySmt optimization"
  [ benchTestGroup
    "constant folding"
    [ mkCFoldTest "" (n (n t)) (Just t)
    , mkCFoldTest "not and"
                  (n (a [t, t, v "a", v "b"]))
                  (Just $ n (a [v "a", v "b"]))
    , mkCFoldTest "not and -> false" (n (a [t, f, v "a", v "b"])) (Just t)
    , mkCFoldTest "xor -> not xor"
                  (x [t, f, t, t, v "a", v "b"])
                  (Just $ n (x [v "a", v "b"]))
    , mkCFoldTest "big"
                  (i (o [t, f, t, t, v "a", v "b"]) (x [t, v "b"]))
                  Nothing
    , mkCFoldTest "bv add"  (binN BvAdd (bv 5) (bv 16))   (Just $ bv 21)
    , mkCFoldTest "bv sub"  (bin BvSub (bv 5) (bv 16))    (Just $ bv (-11))
    , mkCFoldTest "bv udiv" (bin BvUdiv (bv 16) (bv 5))   (Just $ bv 3)
    , mkCFoldTest "bv mul"  (binN BvMul (bv 5) (bv 16))   (Just $ bv 80)
    , mkCFoldTest "bv urem" (bin BvUrem (bv 5) (bv 16))   (Just $ bv 5)
    , mkCFoldTest "bv lshr" (bin BvLshr (bv (-1)) (bv 1)) (Just $ bv bvSMax)
    , mkCFoldTest "bv ashr" (bin BvAshr (bv (-1)) (bv 1)) (Just $ bv (-1))
    , mkCFoldTest "bv shl"  (bin BvShl (bv (-1)) (bv 1))  (Just $ bv (-2))
    , mkCFoldTest "bv or"   (binN BvOr (bv 5) (bv 16))    (Just $ bv 21)
    , mkCFoldTest "bv and"  (binN BvAnd (bv 5) (bv 16))   (Just $ bv 0)
    , mkCFoldTest "bv xor"  (binN BvXor (bv 5) (bv 16))   (Just $ bv 21)
    , mkCFoldTest "partial bv add"
                  (binN BvAdd (bv 0) (bvV "x"))
                  (Just $ bvV "x")
    , mkCFoldTest "partial bv sub" (bin BvSub (bvV "y") (bv 0)) (Just $ bvV "y")
    , mkCFoldTest "partial bv mul (0)"
                  (binN BvMul (bvV "x") (bv 0))
                  (Just $ bv 0)
    , mkCFoldTest "partial bv mul"
                  (binN BvMul (bvV "x") (bv 1))
                  (Just $ bvV "x")
    , mkCFoldTest "bv ugt 0" (binP BvUgt (bv bvUMax) (bv bvUMax)) (Just f)
    , mkCFoldTest "bv ugt 1" (binP BvUgt (bv bvUMax) (bv $ bvUMax + 1)) (Just t)
    , mkCFoldTest "bv ugt 2" (binP BvUgt (bv bvUMax) (bv $ bvUMax - 1)) (Just t)
    , mkCFoldTest "bv ult 0" (binP BvUlt (bv bvUMax) (bv bvUMax)) (Just f)
    , mkCFoldTest "bv ult 1" (binP BvUlt (bv bvUMax) (bv $ bvUMax + 1)) (Just f)
    , mkCFoldTest "bv ult 2" (binP BvUlt (bv bvUMax) (bv $ bvUMax - 1)) (Just f)
    , mkCFoldTest "bv uge 0" (binP BvUge (bv bvUMax) (bv bvUMax)) (Just t)
    , mkCFoldTest "bv uge 1" (binP BvUge (bv bvUMax) (bv $ bvUMax + 1)) (Just t)
    , mkCFoldTest "bv uge 2" (binP BvUge (bv bvUMax) (bv $ bvUMax - 1)) (Just t)
    , mkCFoldTest "bv ule 0" (binP BvUle (bv bvUMax) (bv bvUMax)) (Just t)
    , mkCFoldTest "bv ule 1" (binP BvUle (bv bvUMax) (bv $ bvUMax + 1)) (Just f)
    , mkCFoldTest "bv ule 2" (binP BvUle (bv bvUMax) (bv $ bvUMax - 1)) (Just f)
    , mkCFoldTest "bv sgt 0" (binP BvSgt (bv bvSMax) (bv bvSMax)) (Just f)
    , mkCFoldTest "bv sgt 1" (binP BvSgt (bv bvSMax) (bv $ bvSMax + 1)) (Just t)
    , mkCFoldTest "bv sgt 2" (binP BvSgt (bv bvSMax) (bv $ bvSMax - 1)) (Just t)
    , mkCFoldTest "bv slt 0" (binP BvSlt (bv bvSMax) (bv bvSMax)) (Just f)
    , mkCFoldTest "bv slt 1" (binP BvSlt (bv bvSMax) (bv $ bvSMax + 1)) (Just f)
    , mkCFoldTest "bv slt 2" (binP BvSlt (bv bvSMax) (bv $ bvSMax - 1)) (Just f)
    , mkCFoldTest "bv sge 0" (binP BvSge (bv bvSMax) (bv bvSMax)) (Just t)
    , mkCFoldTest "bv sge 1" (binP BvSge (bv bvSMax) (bv $ bvSMax + 1)) (Just t)
    , mkCFoldTest "bv sge 2" (binP BvSge (bv bvSMax) (bv $ bvSMax - 1)) (Just t)
    , mkCFoldTest "bv sle 0" (binP BvSle (bv bvSMax) (bv bvSMax)) (Just t)
    , mkCFoldTest "bv sle 1" (binP BvSle (bv bvSMax) (bv $ bvSMax + 1)) (Just f)
    , mkCFoldTest "bv sle 2" (binP BvSle (bv bvSMax) (bv $ bvSMax - 1)) (Just f)
    , mkCFoldTest "bv saddo 0" (binP BvSaddo (bv bvSMax) (bv 1)) (Just t)
    , mkCFoldTest "bv saddo 1" (binP BvSaddo (bv bvSMax) (bv 0)) (Just f)
    , mkCFoldTest "bv saddo 2" (binP BvSaddo (bv bvSMax) (bv (-1))) (Just f)
    , mkCFoldTest "bv ssubo 0" (binP BvSsubo (bv bvSMax) (bv 1)) (Just f)
    , mkCFoldTest "bv ssubo 1" (binP BvSsubo (bv bvSMax) (bv 0)) (Just f)
    , mkCFoldTest "bv ssubo 2" (binP BvSsubo (bv bvSMax) (bv (-1))) (Just t)
    , mkCFoldTest "bv smulo 0" (binP BvSmulo (bv bvSMax) (bv 1)) (Just f)
    , mkCFoldTest "bv smulo 1" (binP BvSmulo (bv bvSMax) (bv 2)) (Just t)
    , mkCFoldTest "bv smulo 2" (binP BvSmulo (bv bvSMax) (bv 0)) (Just f)
    , mkCFoldTest "bv smulo 3" (binP BvSmulo (bv bvSMin) (bv (-1))) (Just t)
    ]
  , benchTestGroup
    "equality elimination"
    [ mkEqElimTest True
                   "one"
                   (Set.fromList ["b", "c", "d"])
                   [Eq (v "a") (v "b"), Eq (v "a") (o [v "d", v "c"])]
                   1
    , mkEqElimTest
      True
      "larger"
      (Set.fromList ["b", "d"])
      [ Eq (v "a") (o [v "d", v "c"])
      , Eq (v "a") (x [v "a", v "c"])
      , Eq (v "d") (v "c")
      ]
      1
    , mkEqElimTest
      True
      "larger2"
      (Set.fromList ["b", "c"])
      [ Eq (v "a") (o [v "d", v "c"])
      , Eq (v "a") (x [v "a", v "c"])
      , Eq (v "d") (v "c")
      ]
      1
    , mkEqElimTest
      False
      "larger2 no blowup"
      (Set.fromList ["b", "c"])
      [ Eq (v "a") (o [v "d", v "e"])
      , Eq (v "a") (x [v "a", v "c"])
      , Eq (v "d") (v "c")
      ]
      2
    ]
  ]
 where
  v s = Var s SortBool :: Term BoolSort
  t   = BoolLit True
  n   = Not
  f   = BoolLit False
  o   = BoolNaryExpr Or
  x   = BoolNaryExpr Xor
  a   = BoolNaryExpr And
  i   = BoolBinExpr Implies
  bvW = 16
  bv :: Int -> TermDynBv
  bv = DynBvLit . Bv.bitVec bvW
  bvV s = Var s (SortBv bvW) :: TermDynBv
  bin = flip DynBvBinExpr bvW
  binN o x y = DynBvNaryExpr o bvW [x, y]
  binP   = flip DynBvBinPred bvW
  bvSMax = 2 ^ (bvW - 1) - 1
  bvUMax = 2 ^ bvW - 1
  bvSMin = negate $ 2 ^ (bvW - 1)

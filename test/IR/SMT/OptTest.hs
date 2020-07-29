{-# LANGUAGE GADTs #-}
module IR.SMT.OptTest
  ( optTests
  )
where
import           BenchUtils
import           IR.SMT.Opt
import           Control.Monad
import qualified Data.Set                      as Set
import           IR.SMT.TySmt
import           Test.Tasty.HUnit


constantFree :: SortClass s => Term s -> Bool
constantFree = reduceTerm visit True (&&)
 where
  visit :: SortClass s => Term s -> Maybe Bool
  visit t = case t of
    BoolLit{} -> Just False
    IntLit{}  -> Just False
    _         -> Nothing

isBoolConst :: TermBool -> Bool
isBoolConst t = case t of
  BoolLit{} -> True
  _         -> False

isReduced :: TermBool -> Bool
isReduced t = isBoolConst t || constantFree t

mkTest :: String -> TermBool -> Maybe TermBool -> BenchTest
mkTest name original mExpected =
  benchTestCase (if length name == 0 then show original else name) $ do
    let actual = constantFold original
    case mExpected of
      Just e  -> e @=? actual
      Nothing -> return ()
    isReduced actual @? "Is reduced"

mkEqElimTest :: String -> Set.Set String -> [TermBool] -> Int -> BenchTest
mkEqElimTest name protected original nExpected =
  benchTestCase (if length name == 0 then show original else name) $ do
    let actual = eqElim protected original
    when (nExpected /= length actual) $ forM_ actual print
    nExpected @=? length actual


optTests :: BenchTest
optTests = benchTestGroup
  "TySmt optimization"
  [ benchTestGroup
    "constant folding"
    [ mkTest "" (n (n t)) (Just t)
    , mkTest "not and"
             (n (a [t, t, v "a", v "b"]))
             (Just $ n (a [v "a", v "b"]))
    , mkTest "not and -> false" (n (a [t, f, v "a", v "b"])) (Just t)
    , mkTest "xor -> not xor"
             (x [t, f, t, t, v "a", v "b"])
             (Just $ n (x [v "a", v "b"]))
    , mkTest "big" (i (o [t, f, t, t, v "a", v "b"]) (x [t, v "b"])) Nothing
    ]
  , benchTestGroup
    "equality elimination"
    [ mkEqElimTest "one"
                   (Set.fromList ["b", "c", "d"])
                   [Eq (v "a") (v "b"), Eq (v "a") (o [v "d", v "c"])]
                   1
    , mkEqElimTest "larger"
                   (Set.fromList ["b", "d"])
                   [ Eq (v "a") (o [v "d", v "c"])
                   , Eq (v "a") (x [v "a", v "c"])
                   , Eq (v "d") (v "c")
                   ]
                   1
    , mkEqElimTest "larger2"
                   (Set.fromList ["b", "c"])
                   [ Eq (v "a") (o [v "d", v "c"])
                   , Eq (v "a") (x [v "a", v "c"])
                   , Eq (v "d") (v "c")
                   ]
                   1
    ]
  ]
 where
  v s = Var s SortBool :: Term BoolSort
  t = BoolLit True
  n = Not
  f = BoolLit False
  o = BoolNaryExpr Or
  x = BoolNaryExpr Xor
  a = BoolNaryExpr And
  i = BoolBinExpr Implies

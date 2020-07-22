{-# LANGUAGE GADTs #-}
module Codegen.FoldTest
  ( constantFoldingTests
  )
where
import           BenchUtils
import           Codegen.Fold
import           IR.TySmt
import           Test.Tasty.HUnit


constantFree :: Term s -> Bool
constantFree = reduceTerm visit True (&&)
 where
  visit :: Term s -> Maybe Bool
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


constantFoldingTests :: BenchTest
constantFoldingTests = benchTestGroup
  "constant folding tests"
  [ mkTest "" (n (n t)) (Just t)
  , mkTest "not and" (n (a [t, t, v "a", v "b"])) (Just $ n (a [v "a", v "b"]))
  , mkTest "not and -> false" (n (a [t, f, v "a", v "b"])) (Just t)
  , mkTest "xor -> not xor"
           (x [t, f, t, t, v "a", v "b"])
           (Just $ n (x [v "a", v "b"]))
  , mkTest "big"
           (i (o [t, f, t, t, v "a", v "b"]) (x [t, v "b"]))
           Nothing
  ]
 where
  v s = Var s SortBool
  t = BoolLit True
  n = Not
  f = BoolLit False
  o = BoolNaryExpr Or
  x = BoolNaryExpr Xor
  a = BoolNaryExpr And
  i = BoolBinExpr Implies

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}


module Codegen.Circom.TestSmt ( smtToR1csLines
                              , writeToR1csFile
                              ) where

import qualified Codegen.Circom.Term        as Term
import qualified Codegen.Circom.Context     as C
import qualified Codegen.Circom.Constraints as CS
import           Data.Field.Galois          (Prime, fromP, toP)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe                 as Maybe
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set                   as Set
import           GHC.TypeLits               (KnownNat, natVal)
import qualified IR.TySmt                   as S
import qualified Digraph

getOrder :: S.Term S.BoolSort -> Maybe Integer
getOrder = S.reduceTerm visit Nothing join
  where
    visit :: S.Term t -> Maybe (Maybe Integer)
    visit t = case t of
      S.IntToPf i -> error "NYI"

    join :: Maybe Integer -> Maybe Integer -> Maybe Integer
    join i j =
      case length ls of
        0 -> Nothing
        1 -> Just $ head ls
        2 -> if head ls == ls !! 1 then Just (head ls) else error $ "Two different orders:\n\t" ++ show (head ls) ++ "\nand\n\t" ++ show (ls !! 1)
      where ls = Maybe.catMaybes [i, j]

collectVars :: S.Term s -> Set.Set String
collectVars = S.reduceTerm visit Set.empty Set.union
  where
    visit :: S.Term t -> Maybe (Set.Set String)
    visit t = case t of
      S.Var v -> Just $ Set.singleton v
      _ -> Nothing

extractConstraints :: S.Term s -> [S.Term S.BoolSort]
extractConstraints = S.reduceTerm visit [] (++)
  where
    visit :: S.Term t -> Maybe [S.Term S.BoolSort]
    visit t = case t of
      S.BoolNaryExpr S.And l -> Just [t]
      _ -> Nothing

countExistentials :: S.Term s -> Integer
countExistentials = S.reduceTerm visit 0 (+)
  where
    visit :: S.Term t -> Maybe Integer
    visit t = case t of
      S.Exists _ _ t' -> Just $ 1 + countExistentials t'
      _ -> Nothing

countLets :: S.Term s -> Integer
countLets = S.reduceTerm visit 0 (+)
  where
    visit :: S.Term t -> Maybe Integer
    visit t = case t of
      S.Let _ t' t'' -> Just $ 1 + countLets t' + countLets t''
      _ -> Nothing

type VarIndices = Map.Map String Integer

smtLcTermToR1csPair :: VarIndices -> Integer -> S.Term (S.PfSort k) -> (Integer, Integer)
smtLcTermToR1csPair varIndices order term = case term of
    S.IntToPf (S.IntLit i) -> (i, 1)
    S.PfNaryExpr S.PfMul [S.IntToPf (S.IntLit i), S.Var n] -> (i `rem` order, varIndices Map.! n)
    S.PfNaryExpr S.PfMul [S.Var n, S.IntToPf (S.IntLit i)] -> (i `rem` order, varIndices Map.! n)
    S.PfNaryExpr S.PfMul [S.Var n] -> (1, varIndices Map.! n)

smtLcToR1csLine :: VarIndices -> Integer -> S.Term (S.PfSort k) -> [Integer]
smtLcToR1csLine varIndices order term = case term of
    S.PfNaryExpr S.PfAdd list -> n : ls
        where
            pairs :: [(Integer, Integer)]
            pairs = map (smtLcTermToR1csPair varIndices order) list
            ls = foldr (\(a, b) l -> if a == 0 then l else a : b : l) [] pairs
            n = fromIntegral (length ls) `div` 2


pfPredToR1csLines :: VarIndices -> Integer -> S.Term S.BoolSort -> [[Integer]]
pfPredToR1csLines varIndices order term = case term of
    S.PfBinPred S.PfEq (S.PfNaryExpr S.PfMul [a, b]) c -> [[], f a, f b, f c]
        where
            f = smtLcToR1csLine varIndices order


constraintsToR1csLines :: VarIndices -> Integer -> S.Term S.BoolSort -> [[Integer]]
constraintsToR1csLines varIndices order term = case term of
    S.BoolNaryExpr S.And l -> concatMap (pfPredToR1csLines varIndices order) l



smtToR1csLines :: Integer -> S.Term S.BoolSort -> [[Integer]]
smtToR1csLines order term =
    [nExistentials, nLets, nConstraints] : bodyLines
  where
    vars = Set.toList $ collectVars term
    varIndices :: VarIndices
    varIndices = Map.fromList $ zip vars [0,1..]
    allConstraints = extractConstraints term
    constraints = if length allConstraints == 1 then head allConstraints else error $ "Should be a single constraint set" ++ show allConstraints
    nExistentials = countExistentials term
    nLets = countLets term
    bodyLines = constraintsToR1csLines varIndices order constraints
    nConstraints = fromIntegral (length bodyLines `div` 4)

writeToR1csFile :: Integer -> S.Term S.BoolSort -> String -> IO ()
writeToR1csFile order term path = do
    let lines = smtToR1csLines order term
    let string = intercalate "\n" $ map (unwords . map show) lines
    writeFile path string

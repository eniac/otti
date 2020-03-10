{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs #-}


module Codegen.Circom.TestSmt ( smtToR1csLines
                              , writeToR1csFile
                              , extendInputsToAssignment
                              ) where

import qualified Codegen.Circom.Term        as Term
import qualified Codegen.Circom.Constraints as CS
import           Data.Field.Galois          (Prime, fromP, toP)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import qualified Data.Maybe                 as Maybe
import           Data.Proxy                 (Proxy (Proxy))
import           Data.Dynamic               (Dynamic, toDyn, fromDyn)
import qualified Data.Set                   as Set
import           GHC.TypeLits               (KnownNat, natVal)
import qualified IR.TySmt                   as S
import qualified Digraph

type Env = Map.Map String Dynamic

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

collectVars :: S.Term s -> [String]
collectVars = S.reduceTerm visit [] (++)
  where
    visit :: S.Term t -> Maybe [String]
    visit t = case t of
      S.Exists n _ t' -> Just $ n : collectVars t'
      S.Let n _ t' -> Just $ n : collectVars t'
      _ -> Just []

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

listExistentials :: S.Term s -> [String]
listExistentials = S.reduceTerm visit [] (++)
  where
    visit :: S.Term t -> Maybe [String]
    visit t = case t of
      S.Exists n _ t'-> Just $ n : listExistentials t'
      _ -> Just []

listLets :: S.Term s -> [String]
listLets = S.reduceTerm visit [] (++)
  where
    visit :: S.Term t -> Maybe [String]
    visit t = case t of
      S.Exists _ _ t'-> Just $ listLets t'
      S.Let n _ t'-> Just $ n : listLets t'
      _ -> Just []

extendEnvToLets :: Env -> S.Term s -> Env
extendEnvToLets e = S.reduceTerm visit Map.empty Map.union
  where
    visit :: S.Term t -> Maybe Env
    visit t = case t of
      S.Exists _ _ t'-> Just $ extendEnvToLets e t'
      S.Let n s' t'-> Just $ extendEnvToLets e' t'
        where
          v = S.eval e s'
          e' = Map.insert n (toDyn v) e
      _ -> Just e

countLets :: S.Term s -> Integer
countLets = S.reduceTerm visit 0 (+)
  where
    visit :: S.Term t -> Maybe Integer
    visit t = case t of
      S.Let _ t' t'' -> Just $ 1 + countLets t' + countLets t''
      _ -> Nothing

extendInputsToAssignment :: forall k. KnownNat k => Proxy k -> [Integer] -> S.Term S.BoolSort -> [Integer]
extendInputsToAssignment orderProxy inputs smt =
    map getAsFp (existNames ++ letNames)
  where
    order = natVal orderProxy
    existNames = listExistentials smt
    letNames = listLets smt
    pfInputs = map (toDyn . S.ValPf @k . ( `rem` order)) inputs
    e = if length pfInputs == length existNames then
        Map.fromList $ zip existNames pfInputs
    else
        error "Wrong number of inputs"
    e' = extendEnvToLets e smt
    getAsFp name = case pfVal of
        S.ValPf i -> i
      where
        entry = Maybe.fromMaybe (error $ "Missing assignment variable: " ++ name) (e' Map.!? name)
        pfVal :: S.Value (S.PfSort k) = fromDyn entry (error $ "Wrong type of variable: " ++ name)


type VarIndices = Map.Map String Integer

smtLcTermToR1csPair :: forall k. KnownNat k => VarIndices -> S.Term (S.PfSort k) -> (Integer, Integer)
smtLcTermToR1csPair varIndices term = case term of
    S.IntToPf (S.IntLit i) -> (i, 1)
    S.PfNaryExpr S.PfMul [S.IntToPf (S.IntLit i), S.Var n] -> (i `rem` order, varIndices Map.! n)
    S.PfNaryExpr S.PfMul [S.Var n, S.IntToPf (S.IntLit i)] -> (i `rem` order, varIndices Map.! n)
    S.PfNaryExpr S.PfMul [S.Var n] -> (1, varIndices Map.! n)
  where order = natVal (Proxy :: Proxy k)

smtLcToR1csLine :: VarIndices -> S.Term (S.PfSort k) -> [Integer]
smtLcToR1csLine varIndices term = case term of
    S.PfNaryExpr S.PfAdd list -> n : ls
        where
            pairs :: [(Integer, Integer)]
            pairs = map (smtLcTermToR1csPair varIndices) list
            ls = foldr (\(a, b) l -> if a == 0 then l else a : b : l) [] pairs
            n = fromIntegral (length ls) `div` 2


pfPredToR1csLines :: VarIndices -> S.Term S.BoolSort -> [[Integer]]
pfPredToR1csLines varIndices term = case term of
    S.PfBinPred S.PfEq (S.PfNaryExpr S.PfMul [a, b]) c -> [[], f a, f b, f c]
        where
            f = smtLcToR1csLine varIndices


constraintsToR1csLines :: VarIndices -> S.Term S.BoolSort -> [[Integer]]
constraintsToR1csLines varIndices term = case term of
    S.BoolNaryExpr S.And l -> concatMap (pfPredToR1csLines varIndices) l



smtToR1csLines :: S.Term S.BoolSort -> [[Integer]]
smtToR1csLines term =
    [nExistentials, nLets, nConstraints] : bodyLines
  where
    vars = collectVars term
    varIndices :: VarIndices
    varIndices = Map.fromList $ zip vars [2,3..]
    allConstraints = extractConstraints term
    constraints = if length allConstraints == 1 then head allConstraints else error $ "Should be a single constraint set" ++ show allConstraints
    nExistentials = countExistentials term
    nLets = countLets term
    bodyLines = constraintsToR1csLines varIndices constraints
    nConstraints = fromIntegral (length bodyLines `div` 4)

writeToR1csFile :: S.Term S.BoolSort -> String -> IO ()
writeToR1csFile term path = do
    let lines = smtToR1csLines term
    let string = intercalate "\n" $ map (unwords . map show) lines
    writeFile path string

type PartialAssignment = Map.Map String Integer

extendAssignment = error "NYI"

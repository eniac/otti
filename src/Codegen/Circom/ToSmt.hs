{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Codegen.Circom.ToSmt ( constraintsToSmt
                            , ctxToSmt
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

collectVars :: S.Term s -> Set.Set String
collectVars = S.reduceTerm visit Set.empty Set.union
  where
    visit :: S.Term t -> Maybe (Set.Set String)
    visit t = case t of
      S.Var v -> Just $ Set.singleton v
      _ -> Nothing


ctxToSmt :: forall k. KnownNat k => C.Ctx k -> S.Term S.BoolSort
ctxToSmt c = quantified
    where
        sigSort = S.SortPf $ natVal (Proxy :: Proxy k)
        cs = C.constraints c

        -- Conjoin the constraints
        conj = S.BoolNaryExpr S.And $ map constraintToSmt (CS.equalities cs)

        -- Build a graph of variable dependencies
        graph = Digraph.graphFromEdgedVerticesOrd $ map (\s -> case C.ctxGet c (Term.sigLocation s) of
            Term.Base (_, smt) -> Digraph.DigraphNode smt (show s) (Set.toList $ collectVars smt)
            other -> error $ "Cannot have signal like " ++ show other ++ " because that is not a field element"
          ) $ Set.toList (CS.private cs)

        -- Let bind all private variables
        letted = foldr (\n f -> case n of
            Digraph.DigraphNode value name _ -> S.Let name value f
          ) conj (Digraph.topologicalSortG graph)

        -- existentially quantify all public variables
        quantified = Set.fold (\s f -> S.Exists (show s) sigSort f) letted (CS.public cs)


constraintsToSmt :: forall k. KnownNat k =>  CS.Constraints (Prime k) -> S.Term S.BoolSort
constraintsToSmt c = quantified
    where
        conj = S.BoolNaryExpr S.And $ map constraintToSmt (CS.equalities c)
        sigs = Set.union (CS.private c) (CS.public c)
        sigSort = S.SortPf $ natVal (Proxy :: Proxy k)
        quantified = Set.fold (\s f -> S.Exists (show s) sigSort f) conj sigs

constraintToSmt :: KnownNat k => CS.Constraint (Prime k) -> S.Term S.BoolSort
constraintToSmt (a, b, c) =
    S.PfBinPred S.PfEq (S.PfNaryExpr S.PfMul [lcToSmt a, lcToSmt b]) (lcToSmt c)

lcToSmt :: forall k. KnownNat k => CS.LC (Prime k) -> S.Term (S.PfSort k)
lcToSmt (m, c) = S.PfNaryExpr S.PfAdd (cTerm : mTerms)
    where
        constToTerm = S.IntToPf . S.IntLit . fromP
        cTerm = constToTerm c
        mTerms = map (uncurry (\s c -> S.PfNaryExpr S.PfMul [S.Var (show s), constToTerm c]))
            $ Map.toList m

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Codegen.Circom.ToSmt ( constraintsToSmt
                            ) where

import qualified Codegen.Circom.Constraints as CS
import           Data.Either                (either)
import           Data.Field.Galois          (Prime, fromP, toP)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set                   as Set
import           GHC.TypeLits               (KnownNat, natVal)
import qualified IR.TySmt                   as S


constraintsToSmt :: forall k. KnownNat k =>  CS.Constraints (Prime k) -> S.BoolTerm
constraintsToSmt c = quantified
    where
        conj = S.BoolNaryExpr S.BoolAnd $ map constraintToSmt (CS.equalities c)
        sigs = Set.union (CS.private c) (CS.public c)
        sigSort = S.Pf $ natVal (Proxy :: Proxy k)
        quantified = Set.fold (\s f -> S.BoolExists (show s) sigSort f) conj sigs

constraintToSmt :: KnownNat k => CS.Constraint (Prime k) -> S.BoolTerm
constraintToSmt (a, b, c) =
    S.PfPred S.PfEq (S.PfNaryExpr S.PfMul [lcToSmt a, lcToSmt b]) (lcToSmt c)

lcToSmt :: forall k. KnownNat k => CS.LC (Prime k) -> S.PfTerm
lcToSmt (m, c) = S.PfNaryExpr S.PfAdd (cTerm : mTerms)
    where
        constToTerm = S.PfLit (natVal (Proxy :: Proxy k)) . fromP
        cTerm = constToTerm c
        mTerms = map (uncurry (\s c -> S.PfNaryExpr S.PfMul [S.PfVar (show s), constToTerm c]))
            $ Map.toList m

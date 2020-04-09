{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- Because of out KnownNat1 instance for the Log2 family...
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Codegen.Circom.CompTypes.WitComp
  ( WitBaseTerm(..)
  , WitBaseCtx(..)
  , nSmtNodes
  )
where

import           AST.Circom                     ( UnOp(..)
                                                , BinOp(..)
                                                , SignalKind(..)
                                                )
import           Codegen.Circom.CompTypes       ( BaseTerm(..)
                                                , BaseCtx(..)
                                                , LTerm(..)
                                                , sigToLterm
                                                )
import qualified Codegen.Circom.Signal         as Sig
import           Codegen.Circom.Utils           ( spanE
                                                , mapGetE
                                                )
import qualified IR.TySmt                      as Smt
import qualified Data.Either                   as Either
import           Data.Field.Galois              ( Prime
                                                , fromP
                                                )
import qualified Data.Foldable                 as Fold
import           Data.Proxy                     ( Proxy(Proxy) )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import qualified Digraph
import           GHC.TypeLits.KnownNat
import           GHC.TypeNats

intLog2 :: Integral a => a -> a
intLog2 n = if n <= fromInteger 1
  then fromInteger 0
  else fromInteger 1 + intLog2 (n `div` fromInteger 2)

-- XXX(HACK): Log2 0 is actually undefined, but who wants to deal with that?
--            we treat it as 0, even though the type systems rejects it.
instance KnownNat x => KnownNat1 $(nameToSymbol ''Log2) x where
  natSing1 = SNatKn (intLog2 (natVal (Proxy @x)))
  {-# INLINE natSing1 #-}


newtype WitBaseTerm n = WitBaseTerm (Smt.Term (Smt.PfSort n)) deriving (Show)

instance KnownNat n => Num (WitBaseTerm n) where
  (WitBaseTerm s) + (WitBaseTerm t) =
    WitBaseTerm $ Smt.PfNaryExpr Smt.PfAdd [s, t]
  (WitBaseTerm s) * (WitBaseTerm t) =
    WitBaseTerm $ Smt.PfNaryExpr Smt.PfMul [s, t]
  negate (WitBaseTerm s) = WitBaseTerm $ Smt.PfUnExpr Smt.PfNeg s
  abs _ = error "ndef"
  signum _ = error "ndef"
  fromInteger = WitBaseTerm . Smt.IntToPf . Smt.IntLit

instance KnownNat n => Fractional (WitBaseTerm n) where
  recip (WitBaseTerm s) = WitBaseTerm $ Smt.PfUnExpr Smt.PfRecip s
  fromRational _ = error "NYI"

instance KnownNat n => BaseTerm (WitBaseTerm n) (Prime n) where
  fromConst  = fromInteger . fromP
  fromSignal = WitBaseTerm . Smt.Var . show
  nonArithBinOp o = case o of
    IntDiv -> liftIntToPf (Smt.IntBinExpr Smt.IntDiv)
    Mod    -> liftIntToPf (Smt.IntBinExpr Smt.IntMod)
    Lt     -> liftIntPredToPf (Smt.IntBinPred Smt.IntLt)
    Gt     -> liftIntPredToPf (Smt.IntBinPred Smt.IntGt)
    Le     -> liftIntPredToPf (Smt.IntBinPred Smt.IntLe)
    Ge     -> liftIntPredToPf (Smt.IntBinPred Smt.IntGe)
    Eq     -> liftIntPredToPf (Smt.IntBinPred Smt.IntEq)
    Ne     -> liftIntPredToPf (Smt.IntBinPred Smt.IntNe)
    And    -> liftBoolToPf (\a b -> Smt.BoolNaryExpr Smt.And [a, b])
    Or     -> liftBoolToPf (\a b -> Smt.BoolNaryExpr Smt.Or [a, b])
    BitAnd -> liftBvToPf (Smt.BvBinExpr Smt.BvAnd)
    BitOr  -> liftBvToPf (Smt.BvBinExpr Smt.BvOr)
    BitXor -> liftBvToPf (Smt.BvBinExpr Smt.BvXor)
    Pow    -> liftIntToPf (Smt.IntBinExpr Smt.IntPow)
    Shl    -> liftBvToPf (Smt.BvBinExpr Smt.BvShl)
    Shr    -> liftBvToPf (Smt.BvBinExpr Smt.BvLshr)
    _      -> error "Unreachable"
   where
    liftIntToPf f (WitBaseTerm a) (WitBaseTerm b) =
      WitBaseTerm $ Smt.IntToPf $ f (Smt.PfToInt a) (Smt.PfToInt b)
    liftBvToPf f = liftIntToPf
      (\a b -> Smt.BvToInt @(Log2 n + 1)
        (f (Smt.IntToBv @(Log2 n + 1) a) (Smt.IntToBv @(Log2 n + 1) b))
      )
    liftIntPredToPf f = liftIntToPf (\a b -> Smt.BoolToInt $ f a b)
    liftBoolToPf f = liftIntPredToPf
      (\a b -> f (Smt.IntBinPred Smt.IntNe (Smt.IntLit 0) a)
                 (Smt.IntBinPred Smt.IntNe (Smt.IntLit 0) b)
      )
  nonNegUnOp o = case o of
    BitNot ->
      error "Bitwise negation has unclear semantics for prime field elements"
    Not -> \(WitBaseTerm a) ->
      WitBaseTerm $ Smt.IntToPf $ Smt.BoolToInt $ Smt.Not $ Smt.PfBinPred
        Smt.PfNe
        z
        a
      where z = Smt.IntToPf $ Smt.IntLit 0
    UnPos -> id
    UnNeg -> error "Unreachable"

data WitBaseCtx n = WitBaseCtx { signalTerms :: Map.Map LTerm (WitBaseTerm n)
                               -- The order in which signals and components are written to
                               -- lefts are signals, rights are components
                               -- Initialize unordered, then ordered in finalize.
                               , assignmentOrder :: [Either LTerm Sig.IndexedIdent]
                               } deriving (Show)
instance KnownNat n => BaseCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n) where
  assert _ = id
  emptyCtx = WitBaseCtx Map.empty []
  storeCtx span_ _kind sig term ctx = if Map.member sig (signalTerms ctx)
    then spanE span_ $ "Signal " ++ show sig ++ " has already been assigned to"
    else ctx { signalTerms     = Map.insert sig term $ signalTerms ctx
             , assignmentOrder = Left sig : assignmentOrder ctx
             }
  getCtx kind sig c = if kind == Out
    then case sig of
      LTermLocal{} -> c
      LTermForeign cLoc _ ->
        c { assignmentOrder = Right cLoc : assignmentOrder c }
    else c
  ignoreCompBlock = const False
  finalize c =
    let
      keys = assignmentOrder c

      collectSigs :: Smt.Term s -> Set.Set Sig.Signal
      collectSigs = Smt.reduceTerm visit Set.empty Set.union
       where
        visit :: Smt.Term t -> Maybe (Set.Set Sig.Signal)
        visit t = case t of
          Smt.Var v -> Just $ Set.singleton $ read v
          _         -> Nothing

      asLterm :: Either LTerm Sig.IndexedIdent -> LTerm
      asLterm = either id LTermLocal

      outputComponent o = case o of
        LTermForeign a _ -> LTermLocal a
        LTermLocal _     -> o

      dependencies :: Either LTerm Sig.IndexedIdent -> [LTerm]
      dependencies assignment = case assignment of
        Left signal ->
          map (outputComponent . sigToLterm)
            $ Fold.toList
            $ collectSigs
            $ (let WitBaseTerm s = mapGetE
                     ("Signal " ++ show signal ++ " has no term")
                     signal
                     (signalTerms c)
               in  s
              )
        Right componentLoc -> filter inputToComponent $ Either.lefts keys
         where
          inputToComponent l = case l of
            LTermForeign x _ | x == componentLoc -> True
            _ -> False

      graph
        :: Digraph.Graph (Digraph.Node LTerm (Either LTerm Sig.IndexedIdent))
      graph = Digraph.graphFromEdgedVerticesOrd $ map
        (\assignment -> Digraph.DigraphNode assignment
                                            (asLterm assignment)
                                            (dependencies assignment)
        )
        keys
    in
      c
        { assignmentOrder = map Digraph.node_payload
                              $ Digraph.topologicalSortG graph
        }

nSmtNodes :: WitBaseCtx n -> Int
nSmtNodes =
  Map.foldr ((+) . (\(WitBaseTerm a) -> Smt.nNodes a)) 0 . signalTerms


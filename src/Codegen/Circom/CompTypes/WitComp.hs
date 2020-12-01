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
import qualified IR.SMT.TySmt                  as Smt
import qualified IR.SMT.TySmt.Alg              as SAlg
import           Data.Coerce                    ( coerce )
import qualified Data.Either                   as Either
import           Data.Field.Galois              ( Prime
                                                , fromP
                                                )
import qualified Data.Foldable                 as Fold
import           Data.Proxy                     ( Proxy(Proxy) )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Digraph
import           Text.Read                      ( readMaybe )
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

instance KnownNat n => BaseTerm (WitBaseTerm n) (Prime n) where
  fromConst = WitBaseTerm . Smt.IntToPf . Smt.IntLit . fromP
  fromSignal =
    WitBaseTerm
      . flip Smt.Var (Smt.SortPf $ fromIntegral $ natVal $ Proxy @n)
      . show
  binOp o = case o of
    Add    -> liftPf (\a b -> Smt.PfNaryExpr Smt.PfAdd [a, b])
    Sub    -> \a b -> binOp Add a $ unOp UnNeg b
    Mul    -> liftPf (\a b -> Smt.PfNaryExpr Smt.PfMul [a, b])
    Div    -> \a b -> binOp Mul a $ liftPfUn (Smt.PfUnExpr Smt.PfRecip) b
    IntDiv -> liftInt (Smt.IntBinExpr Smt.IntDiv)
    Mod    -> liftInt (Smt.IntBinExpr Smt.IntMod)
    Lt     -> liftIntPred (Smt.IntBinPred Smt.IntLt)
    Gt     -> liftIntPred (Smt.IntBinPred Smt.IntGt)
    Le     -> liftIntPred (Smt.IntBinPred Smt.IntLe)
    Ge     -> liftIntPred (Smt.IntBinPred Smt.IntGe)
    Eq     -> liftIntPred Smt.Eq
    Ne     -> liftIntPred (\a b -> Smt.Not $ Smt.Eq a b)
    And    -> liftBool (\a b -> Smt.BoolNaryExpr Smt.And [a, b])
    Or     -> liftBool (\a b -> Smt.BoolNaryExpr Smt.Or [a, b])
    BitAnd -> liftBv (binBv $ Smt.BvNaryExpr Smt.BvAnd)
    BitOr  -> liftBv (binBv $ Smt.BvNaryExpr Smt.BvOr)
    BitXor -> liftBv (binBv $ Smt.BvNaryExpr Smt.BvXor)
    Pow    -> liftInt (Smt.IntBinExpr Smt.IntPow)
    Shl    -> liftBv (Smt.BvBinExpr Smt.BvShl)
    Shr    -> liftBv (Smt.BvBinExpr Smt.BvLshr)
   where
    binBv f x y = f [x, y]
    liftPfUn f = WitBaseTerm . f . coerce
    liftPf f (WitBaseTerm a) (WitBaseTerm b) = WitBaseTerm (f a b)
    liftInt f (WitBaseTerm a) (WitBaseTerm b) =
      WitBaseTerm $ Smt.IntToPf $ f (Smt.PfToInt a) (Smt.PfToInt b)
    liftBv f = liftInt
      (\a b -> Smt.BvToInt @(Log2 n + 1)
        (f (Smt.IntToBv @(Log2 n + 1) a) (Smt.IntToBv @(Log2 n + 1) b))
      )
    liftIntPred f = liftInt (\a b -> Smt.BoolToInt $ f a b)
    liftBool f = liftIntPred
      (\a b -> f (Smt.Not $ Smt.Eq (Smt.IntLit 0) a)
                 (Smt.Not $ Smt.Eq (Smt.IntLit 0) b)
      )
  unOp o = case o of
    BitNot ->
      error "Bitwise negation has unclear semantics for prime field elements"
    Not -> \(WitBaseTerm a) ->
      WitBaseTerm $ Smt.IntToPf $ Smt.BoolToInt $ Smt.Eq z a
      where z = Smt.IntToPf $ Smt.IntLit 0
    UnPos -> id
    UnNeg -> WitBaseTerm . Smt.PfUnExpr Smt.PfNeg . coerce
  ite c t f = WitBaseTerm
    $ Smt.Ite (Smt.Not $ Smt.Eq (coerce c) z) (coerce t) (coerce f)
    where z = Smt.IntToPf $ Smt.IntLit 0

data WitBaseCtx n = WitBaseCtx { signalTerms :: !(Map.Map LTerm (WitBaseTerm n))
                               -- The signals and components are written to
                               -- lefts are signals, rights are components
                               , assignmentSet :: !(Set.Set (Either LTerm Sig.IndexedIdent))
                               -- Initiallly empty, then finalize puts them in order...
                               , assignmentOrder :: ![Either LTerm Sig.IndexedIdent]
                               } deriving (Show)

storeInstantiation :: LTerm -> WitBaseCtx n -> WitBaseCtx n
storeInstantiation sig c = case sig of
  LTermLocal{} -> c
  LTermForeign cLoc _ ->
    c { assignmentSet = Set.insert (Right cLoc) $ assignmentSet c }

instance KnownNat n => BaseCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n) where
  assert _ = id
  emptyCtx = WitBaseCtx Map.empty Set.empty []
  storeCtx span_ _kind sig term ctx = if Map.member sig (signalTerms ctx)
    then spanE span_ $ "Signal " ++ show sig ++ " has already been assigned to"
    else storeInstantiation sig $ ctx
      { signalTerms   = Map.insert sig term $ signalTerms ctx
      , assignmentSet = Set.insert (Left sig) $ assignmentSet ctx
      }
  getCtx _kind = storeInstantiation
  ignoreCompBlock = const False
  finalize c =
    let
      keys = Fold.toList $ assignmentSet c

      collectSigs :: Smt.SortClass s => Smt.Term s -> Set.Set Sig.Signal
      collectSigs = SAlg.reduceTerm visit Set.empty Set.union
       where
        visit :: Smt.Term t -> Maybe (Set.Set Sig.Signal)
        visit t = case t of
          Smt.Var v _ ->
            Just
              $ Set.singleton
              $ fromMaybe (error $ "Cannot read signal: " ++ show v)
              $ readMaybe v
          _ -> Nothing

      asLterm :: Either LTerm Sig.IndexedIdent -> LTerm
      asLterm = either id LTermLocal

      outputComponent o = case o of
        LTermForeign a _ -> LTermLocal a
        LTermLocal _     -> o

      dependencies :: Either LTerm Sig.IndexedIdent -> [LTerm]
      dependencies assignment = case assignment of
        Left signal ->
          let WitBaseTerm s = mapGetE
                ("Signal " ++ show signal ++ " has no term")
                signal
                (signalTerms c)
          in  map (outputComponent . sigToLterm) $ Fold.toList $ collectSigs s
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
        , assignmentSet   = Set.empty
        }

nSmtNodes :: KnownNat n => WitBaseCtx n -> Int
nSmtNodes =
  Map.foldr ((+) . (\(WitBaseTerm a) -> SAlg.nNodes a)) 0 . signalTerms

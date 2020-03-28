{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Codegen.Circom.Linking
  ( link
  , linkMain
  , R1CS
  )
where

import           Codegen.Circom.Signal
import qualified AST.Circom                    as AST
import           Codegen.Circom.Compilation    as Comp
import           Data.Field.Galois              ( Prime )
import           GHC.TypeLits                   ( KnownNat )
import qualified Data.Array                    as Arr
import qualified Data.Sequence                 as Seq
import qualified Data.Map.Strict               as Map

type R1CS n = Seq.Seq (Comp.QEQ GlobalSignal (Prime n))

type Namespace = GlobalSignal

joinName :: Namespace -> Signal -> GlobalSignal
joinName n s = case s of
  SigLocal a     -> a : n
  SigForeign a b -> b : a : n

sigMapLc :: (Ord s, Ord t) => (s -> t) -> LC s n -> LC t n
sigMapLc f (m, c) = (Map.mapKeys f m, c)

sigMapQeq :: (Ord s, Ord t) => (s -> t) -> QEQ s n -> QEQ t n
sigMapQeq f (a, b, c) = (sigMapLc f a, sigMapLc f b, sigMapLc f c)

extractComponents
  :: [Int]
  -> String
  -> Comp.Term k
  -> Seq.Seq (IndexedIdent, Comp.TemplateInvocation)
extractComponents idxs name term = case term of
  Comp.Base      _ -> Seq.empty
  Comp.Const     _ -> Seq.empty
  Comp.Component i -> Seq.singleton ((name, idxs), i)
  Comp.Array a ->
    foldMap (\(i, t) -> extractComponents (i : idxs) name t) $ Arr.assocs a


-- TODO: Reader?
link
  :: KnownNat n
  => Namespace
  -> Comp.TemplateInvocation
  -> Comp.CompCtx (Prime n)
  -> R1CS n
link namespace invocation ctx =
  let c = cache ctx Map.! invocation
      cons =
          Seq.fromList $ map (sigMapQeq $ joinName namespace) $ Comp.constraints c
      components =
          foldMap (uncurry $ extractComponents []) $ Map.assocs $ Comp.lowDegEnv c
  in  mappend
        cons
        (foldMap (\(loc, inv) -> link (loc : namespace) inv ctx) components)

linkMain :: forall k . KnownNat k => AST.MainCircuit -> R1CS k
linkMain m =
  let c = Comp.compMainCtx m
  in  case AST.main m of
        AST.Call name args ->
          let (tArgs, _) =
                  Comp.runCompState (Comp.compExprs @k args) Comp.empty
              iArgs :: [Integer] = map termAsNum tArgs
              invocation         = (name, iArgs)
          in  link [("main", [])] invocation c
        e -> error $ "Invalid main expression: " ++ show e

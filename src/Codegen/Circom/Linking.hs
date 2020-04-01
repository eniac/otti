{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Codegen.Circom.Linking
  ( R1CS(..)
  , r1csCountVars
  , linkMain
  , writeToR1csFile
  )
where

import           Codegen.Circom.Signal
import qualified AST.Circom                    as AST
import           Control.Monad.State.Strict
import qualified Codegen.Circom.Compilation    as Comp
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , fromP
                                                )
import qualified Data.Foldable                 as Fold
import           GHC.TypeLits                   ( KnownNat )
import qualified Data.Array                    as Arr
import qualified Data.Sequence                 as Seq
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map

data R1CS n = R1CS { sigNums :: Map.Map GlobalSignal Int
                   , constraints :: Seq.Seq (Comp.QEQ Int (Prime n))
                   , nextSigNum :: Int
                   , nPublicInputs :: Int
                   }

emptyR1cs :: R1CS n
emptyR1cs = R1CS Map.empty Seq.empty 2 0

newtype LinkState n a = LinkState (State (R1CS n) a)
    deriving (Functor, Applicative, Monad, MonadState (R1CS n))

r1csCountVars :: KnownNat n => R1CS n -> Int
r1csCountVars = foldr ((+) . qeqSize) 0 . constraints
  where qeqSize ((a, _), (b, _), (c, _)) = Map.size a + Map.size b + Map.size c

type Namespace = GlobalSignal

joinName :: Namespace -> Signal -> GlobalSignal
joinName n s = case s of
  SigLocal a     -> a : n
  SigForeign a b -> b : a : n

sigMapLc :: (Ord s, Ord t) => (s -> t) -> Comp.LC s n -> Comp.LC t n
sigMapLc f (m, c) = (Map.mapKeys f m, c)

sigMapQeq :: (Ord s, Ord t) => (s -> t) -> Comp.QEQ s n -> Comp.QEQ t n
sigMapQeq f (a, b, c) = (sigMapLc f a, sigMapLc f b, sigMapLc f c)

extractComponents
  :: [Int]
  -> String
  -> Comp.LowDegTerm k
  -> Seq.Seq (IndexedIdent, Comp.TemplateInvocation)
extractComponents idxs name term = case term of
  Comp.Base      _ -> Seq.empty
  Comp.Const     _ -> Seq.empty
  Comp.Component i -> Seq.singleton ((name, idxs), i)
  Comp.Array a ->
    foldMap (\(i, t) -> extractComponents (i : idxs) name t) $ Arr.assocs a


-- TODO: Reader?
link
  :: forall n
   . KnownNat n
  => Namespace
  -> Comp.TemplateInvocation
  -> Comp.LowDegCompCtx (Prime n)
  -> LinkState n ()
link namespace invocation ctx =
  let
    c = Comp.cache ctx Map.! invocation
    -- Expand a signal name and dimensions into a list of signals
    expandSig sigName dims =
      map (\idx -> SigLocal (sigName, idx)) $ mapM (\d -> take d [0 ..]) dims

    newSignals :: [Signal]
    newSignals =
      -- We sort the signals in public-inputs-first order. By starting with
      -- signal number 2, this ensures that signals numbered 2...n will be the
      -- public inputs, which is what our r1cs format requires
      map snd
        $ List.sort
        $ concatMap (\(n, (k, d)) -> map (k, ) $ expandSig n d)
        $ Map.toAscList
        $ Comp.signals c

    newConstraints :: R1CS n -> Seq.Seq (Comp.QEQ Int (Prime n))
    newConstraints ls =
      Seq.fromList
        $ map (sigMapQeq ((sigNums ls Map.!) . joinName namespace))
        $ Comp.constraints
        $ Comp.baseCtx c

    components :: Seq.Seq (IndexedIdent, Comp.TemplateInvocation)
    components =
      foldMap (uncurry $ extractComponents []) $ Map.assocs $ Comp.env c
  in
    do
      -- add our signals
      modify
        (\ls -> ls
          { sigNums    =
            Map.union
                (Map.fromList
                  (zip (map (joinName namespace) $ Fold.toList newSignals)
                       [(nextSigNum ls) ..]
                  )
                )
              $ sigNums ls
          , nextSigNum = nextSigNum ls + length newSignals
          }
        )
      -- link sub-modules
      mapM_ (\(loc, inv) -> link (loc : namespace) inv ctx) components
      -- add our constraints
      modify
        (\ls -> ls { constraints = newConstraints ls Seq.>< constraints ls })
      return ()

execLink :: KnownNat n => LinkState n a -> R1CS n -> R1CS n
execLink (LinkState s) = execState s

linkMain :: forall k . KnownNat k => AST.MainCircuit -> R1CS k
linkMain m =
  let c = Comp.compMainCtx m
  in  case AST.main m of
        AST.Call name args ->
          let (tArgs, _) =
                  Comp.runLowDegCompState @k (Comp.compExprs args) Comp.empty
              iArgs :: [Integer] = map Comp.termAsNum tArgs
              invocation         = (name, iArgs)
              mainCtx            = Comp.cache c Map.! invocation
          in  (execLink @k (link [("main", [])] invocation c) emptyR1cs)
                { nPublicInputs = Comp.nPublicInputs mainCtx
                }
        e -> error $ "Invalid main expression: " ++ show e

lcToR1csLine :: PrimeField n => Comp.LC Int n -> [Integer]
lcToR1csLine (m, c) =
  let pairs          = Map.toAscList m
      augmentedPairs = if fromP c == 0 then pairs else (1, c) : pairs
      nPairs         = fromIntegral (length augmentedPairs)
  in  nPairs : concatMap (\(x, f) -> [fromP f, fromIntegral x]) augmentedPairs

qeqToR1csLines :: PrimeField n => Comp.QEQ Int n -> [[Integer]]
qeqToR1csLines (a, b, c) = [] : map lcToR1csLine [a, b, c]

r1csAsLines :: KnownNat n => R1CS n -> [[Integer]]
r1csAsLines r1cs =
  let nPubIns         = fromIntegral $ nPublicInputs r1cs
      nWit            = fromIntegral (Map.size $ sigNums r1cs) - nPubIns
      nConstraints    = fromIntegral $ Seq.length $ constraints r1cs
      constraintLines = concatMap qeqToR1csLines $ constraints r1cs
  in  [nPubIns, nWit, nConstraints] : constraintLines

writeToR1csFile :: KnownNat n => R1CS n -> FilePath -> IO ()
writeToR1csFile r1cs path =
  writeFile path $ unlines $ map (unwords . map show) $ r1csAsLines r1cs

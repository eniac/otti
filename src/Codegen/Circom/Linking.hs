{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Codegen.Circom.Linking
  ( R1CS(..)
  , r1csCountVars
  , linkMain
  , computeWitnesses
  , r1csStats
  , nPublicInputs
  )
where

import           Codegen.Circom.Signal
import qualified AST.Circom                    as AST
import qualified IR.SMT.TySmt                  as Smt
import qualified IR.SMT.TySmt.Alg              as SAlg
import           Data.Bifunctor
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Strict
import qualified Codegen.Circom.Compilation    as Comp
import           Targets.R1cs.Main       hiding ( values )
import qualified Codegen.Circom.CompTypes      as CompT
import qualified Codegen.Circom.CompTypes.LowDeg
                                               as LD
import qualified Codegen.Circom.CompTypes.WitComp
                                               as Wit
import           Data.Field.Galois              ( Prime
                                                , fromP
                                                )
import           GHC.TypeLits                   ( KnownNat )
import           Data.Aeson                     ( encode )
import qualified Data.Array                    as Arr
import qualified Data.ByteString.Lazy.Char8    as Char8
import qualified Data.Foldable                 as Fold
import qualified Data.Sequence                 as Seq
import qualified Data.Maybe                    as Maybe
import qualified Data.Map.Strict               as Map
import qualified Data.IntSet                   as IntSet
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )
import           Util.Log

newtype LinkState s n a = LinkState (StateT (R1CS s n) Log a)
    deriving (Functor, Applicative, Monad, MonadState (R1CS s n), MonadIO, MonadLog)

type Namespace = GlobalSignal

joinName :: Namespace -> Signal -> GlobalSignal
joinName (GlobalSignal n) s = case s of
  SigLocal a     -> GlobalSignal $ a : n
  SigForeign a b -> GlobalSignal $ b : a : n

prependNamespace :: IndexedIdent -> Namespace -> Namespace
prependNamespace i (GlobalSignal n) = GlobalSignal $ i : n

extractComponents
  :: [Int]
  -> String
  -> LD.LowDegTerm k
  -> Seq.Seq (IndexedIdent, Comp.TemplateInvocation k)
extractComponents idxs name term = case term of
  CompT.Base      _ -> Seq.empty
  CompT.Const     _ -> Seq.empty
  CompT.Component i -> Seq.singleton ((name, reverse idxs), i)
  CompT.Array a ->
    foldMap (\(i, t) -> extractComponents (i : idxs) name t) $ Arr.assocs a


-- TODO: Reader?
link
  :: forall n
   . KnownNat n
  => Namespace
  -> Comp.TemplateInvocation (Prime n)
  -> LD.LowDegCompCtx (Prime n)
  -> LinkState GlobalSignal n ()
link namespace invocation ctx =
  let
    c =
      Maybe.fromMaybe
          (error $ "Missing invocation " ++ show invocation ++ " from cache")
        $      CompT.cache ctx
        Map.!? invocation

    newSignals :: [Signal]
    newSignals = map SigLocal $ CompT.ctxOrderedSignals c

    newConstraints :: Seq.Seq (LD.QEQ GlobalSignal (Prime n))
    newConstraints =
      Seq.reverse
        $ Seq.fromList
        $ map (sigMapQeq (joinName namespace))
        $ LD.constraints
        $ CompT.baseCtx c

    components :: Seq.Seq (IndexedIdent, Comp.TemplateInvocation (Prime n))
    components =
      Seq.reverse
        $ foldMap (uncurry $ extractComponents [])
        $ Map.assocs
        $ CompT.env c
  in
    do
      logIf "r1cs::link::namespace" $ "Linking namespace: " ++ show namespace
      -- add our signals
      modify (r1csAddSignals (map (joinName namespace) newSignals))
      -- link sub-modules
      mapM_ (\(loc, inv) -> link (prependNamespace loc namespace) inv ctx)
            components
      -- add our constraints
      logIf "r1cs::link::cons"
        $ unlines
        $ map (Char8.unpack . encode)
        $ Fold.toList newConstraints
      modify $ r1csAddConstraints newConstraints
      return ()

execLink :: KnownNat n => LinkState s n a -> R1CS s n -> Log (R1CS s n)
execLink (LinkState s) = execStateT s

linkMain
  :: forall k . KnownNat k => AST.SMainCircuit -> Log (R1CS GlobalSignal k)
linkMain m = do
  c          <- Comp.compMainCtx m
  invocation <- Comp.getMainInvocation (Proxy @k) m
  let mainCtx =
        Maybe.fromMaybe
            (error $ "Missing invocation " ++ show invocation ++ " from cache")
          $      CompT.cache c
          Map.!? invocation
      n         = CompT.nPublicInputs mainCtx
      namespace = GlobalSignal [("main", [])]
  s <- execLink @k (link namespace invocation c) emptyR1cs
  return s { publicInputs = IntSet.fromAscList $ take n [2 ..] }

type GlobalValues = Map.Map GlobalSignal Integer
data LocalValues = LocalValues
  { stringValues :: !(Map.Map String Dynamic)
  , values       :: !(Map.Map Signal Dynamic)
  }
localValuesFromValues :: Map.Map Signal Dynamic -> LocalValues
localValuesFromValues m =
  LocalValues (Map.fromList $ map (first show) $ Map.toList m) m
type ExtValues = Map.Map IndexedIdent Dynamic

newtype WitCompWriter a = WitCompWriter (WriterT GlobalValues Log a)
    deriving (Functor, Applicative, Monad, MonadWriter GlobalValues, MonadIO, MonadLog)

computeWitnessesIn
  :: forall n
   . KnownNat n
  => Comp.WitCompCtx n
  -> Namespace
  -> Comp.TemplateInvocation (Prime n)
  -> ExtValues
  -> WitCompWriter ExtValues
computeWitnessesIn ctx namespace invocation inputs =
  let
    c =
      Maybe.fromMaybe
          (error $ "Missing invocation " ++ show invocation ++ " from cache")
        $      CompT.cache ctx
        Map.!? invocation
    extractOutputs :: LocalValues -> ExtValues
    extractOutputs ctx' =
      Map.fromList
        $ Maybe.mapMaybe
            (\(sig, v) -> case sig of
              SigLocal sLoc
                | AST.Out == fst
                  (      Maybe.fromMaybe (error $ "No signal " ++ fst sLoc)
                  $      CompT.signals c
                  Map.!? fst sLoc
                  )
                -> Just (sLoc, v)
              _ -> Nothing
            )
        $ Map.toList (values ctx')
    expandInputs :: ExtValues -> LocalValues
    expandInputs vs = localValuesFromValues $ Map.mapKeys SigLocal vs
    evalExprs      = Wit.signalTerms $ CompT.baseCtx c
    instantiations = reverse $ Wit.assignmentOrder $ CompT.baseCtx c
    emmigrateInputs :: IndexedIdent -> LocalValues -> ExtValues
    emmigrateInputs loc ctx' =
      Map.fromList
        $ Maybe.mapMaybe
            (\(sig, val) -> case sig of
              SigForeign cLoc sLoc | cLoc == loc -> Just (sLoc, val)
              _ -> Nothing
            )
        $ Map.toList
        $ values ctx'
    immigrateOuputs :: IndexedIdent -> ExtValues -> LocalValues
    immigrateOuputs cLoc vs =
      localValuesFromValues
        $ Map.fromList
        $ map (first (SigForeign cLoc))
        $ Map.toList vs
    folder
      :: LocalValues
      -> Either CompT.LTerm IndexedIdent
      -> WitCompWriter LocalValues
    folder localCtx step = case step of
      Left lterm -> do
        let sig = Comp.ltermToSig lterm
            Wit.WitBaseTerm smt =
              Maybe.fromMaybe (error $ "No term for " ++ show lterm)
                $      evalExprs
                Map.!? lterm
            value  = SAlg.eval (stringValues localCtx) smt
            dValue = toDyn value
            name   = joinName namespace sig
        tell $ Map.singleton name (Smt.valAsPf value)
        return $ localCtx
          { stringValues = Map.insert (show sig) dValue $ stringValues localCtx
          , values       = Map.insert sig dValue $ values localCtx
          }
      Right loc -> do
        let callCtx       = emmigrateInputs loc localCtx
            callNamespace = prependNamespace loc namespace
            term          = CompT.LTermLocal loc
            instr         = CompT.load AST.nullSpan term
        result <- liftLog $ fst <$> CompT.runCompState instr c
        let call = case result of
              CompT.Component i -> i
              t -> error $ "Non-component " ++ show t ++ " at " ++ show loc
        retCtx <- computeWitnessesIn ctx callNamespace call callCtx
        let
          retLocal  = immigrateOuputs loc retCtx
          newValues = Map.union (values localCtx) (values retLocal)
          newStringValues =
            Map.union (stringValues localCtx) (stringValues retLocal)
        return $ LocalValues newStringValues newValues
  in
    do
      postC <- foldM folder (expandInputs inputs) instantiations
      return $ extractOutputs postC

computeWitnesses
  :: forall n
   . KnownNat n
  => Proxy n
  -> AST.SMainCircuit
  -> Map.Map IndexedIdent (Prime n)
  -> Log (Map.Map GlobalSignal Integer)
computeWitnesses order main inputs = do
  let dynInputs = Map.map (toDyn . Smt.ValPf @n . fromP) inputs
  c          <- Comp.compMainWitCtx @n main
  invocation <- Comp.getMainInvocation order main
  let namespace       = GlobalSignal [("main", [])]
      WitCompWriter w = computeWitnessesIn c namespace invocation dynInputs
  a <- execWriterT w
    -- Add the input signals to the global map :P
  return $ Map.union a $ Map.map fromP $ Map.mapKeys
    (joinName namespace . SigLocal)
    inputs

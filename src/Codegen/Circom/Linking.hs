{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Codegen.Circom.Linking
  ( R1CS(..)
  , r1csCountVars
  , linkMain
  , writeToR1csFile
  , computeWitnesses
  , parseSignalsFromFile
  )
where

import           Codegen.Circom.Signal
import qualified AST.Circom                    as AST
import qualified IR.TySmt                      as Smt
import           Data.Bifunctor
import           Control.Monad.State.Strict
import           Control.Monad.Writer.Lazy
import qualified Codegen.Circom.Compilation    as Comp
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import           GHC.TypeLits                   ( KnownNat )
import qualified Data.Array                    as Arr
import qualified Data.Sequence                 as Seq
import qualified Data.List.Split               as Split
import qualified Data.Maybe                    as Maybe
import qualified Data.Tuple                    as Tuple
import qualified Data.Map.Strict               as Map
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                )
import           Data.Proxy                     ( Proxy(Proxy) )

data R1CS n = R1CS { sigNums :: Map.Map GlobalSignal Int
                   , numSigs :: Map.Map Int GlobalSignal
                   , constraints :: Seq.Seq (Comp.QEQ Int (Prime n))
                   , nextSigNum :: Int
                   , nPublicInputs :: Int
                   } deriving (Show)

r1csAddSignals :: [GlobalSignal] -> R1CS n -> R1CS n
r1csAddSignals sigs r1cs =
  let zipped = zip sigs [(nextSigNum r1cs)..]
  in  r1cs { sigNums = Map.union (Map.fromList zipped) (sigNums r1cs)
           , numSigs = Map.union (Map.fromList $ map Tuple.swap zipped) (numSigs r1cs)
           , nextSigNum = length zipped + (nextSigNum r1cs)
           }

emptyR1cs :: R1CS n
emptyR1cs = R1CS Map.empty Map.empty Seq.empty 2 0

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

    newSignals :: [Signal]
    newSignals = map SigLocal $ Comp.ctxOrderedSignals c

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
      modify (r1csAddSignals (map (joinName namespace) newSignals))
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
  let c          = Comp.compMainCtx m
      invocation = Comp.getMainInvocation (Proxy @k) m
      mainCtx    = Comp.cache c Map.! invocation
  in  (execLink @k (link [("main", [])] invocation c) emptyR1cs)
        { nPublicInputs = Comp.nPublicInputs mainCtx
        }

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

type GlobalValues = Map.Map GlobalSignal Integer
data LocalValues = LocalValues { stringValues :: Map.Map String Dynamic
                               , values :: Map.Map Signal Dynamic
                               }
localValuesFromValues :: Map.Map Signal Dynamic -> LocalValues
localValuesFromValues m =
  LocalValues (Map.fromList $ map (first show) $ Map.toList m) m
type ExtValues = Map.Map IndexedIdent Dynamic

newtype WitCompWriter a = WitCompWriter (Writer GlobalValues a)
    deriving (Functor, Applicative, Monad, MonadWriter GlobalValues)

ltermToSig :: Comp.LTerm -> Signal
ltermToSig l = case l of
  Comp.LTermLocal a     -> SigLocal a
  Comp.LTermForeign a b -> SigForeign a b

computeWitnessesIn
  :: forall n
   . KnownNat n
  => Comp.WitCompCtx n
  -> Namespace
  -> Comp.TemplateInvocation
  -> ExtValues
  -> WitCompWriter ExtValues
computeWitnessesIn ctx namespace invocation inputs =
  let
    c = Comp.cache ctx Map.! invocation
    extractOutputs :: LocalValues -> ExtValues
    extractOutputs ctx' =
      Map.fromList
        $ Maybe.mapMaybe
            (\(sig, v) -> case sig of
              SigLocal sLoc | AST.Out == fst (Comp.signals c Map.! fst sLoc) ->
                Just (sLoc, v)
              _ -> Nothing
            )
        $ Map.toList (values ctx')
    expandInputs :: ExtValues -> LocalValues
    expandInputs vs = localValuesFromValues $ Map.mapKeys SigLocal vs
    evalExprs      = Comp.signalTerms $ Comp.baseCtx c
    instantiations = reverse $ Comp.assignmentOrder $ Comp.baseCtx c
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
      -> Either Comp.LTerm IndexedIdent
      -> WitCompWriter LocalValues
    folder localCtx step = case step of
      Left lterm -> do
        let sig                  = ltermToSig lterm
        let Comp.WitBaseTerm smt = evalExprs Map.! lterm
        let value                = Smt.eval (stringValues localCtx) smt
        let dValue               = toDyn value
        tell $ Map.singleton (joinName namespace sig) (Smt.valAsPf value)
        return $ localCtx
          { stringValues = Map.insert (show sig) dValue $ stringValues localCtx
          , values       = Map.insert sig dValue $ values localCtx
          }
      Right loc ->
        let
          callCtx       = emmigrateInputs loc localCtx
          callNamespace = loc : namespace
          call =
            case
                fst $ Comp.runCompState (Comp.load (Comp.LTermLocal loc)) ctx
              of
                Comp.Component i -> i
                t -> error $ "Non-component " ++ show t ++ " at " ++ show loc
        in
          do
            retCtx <- computeWitnessesIn ctx callNamespace call callCtx
            let retLocal = immigrateOuputs loc retCtx
            return $ LocalValues
              { values       = Map.union (values localCtx) (values retLocal)
              , stringValues = Map.union (stringValues localCtx)
                                         (stringValues retLocal)
              }
  in
    do
      postC <- foldM folder (expandInputs inputs) instantiations
      return $ extractOutputs postC

computeWitnesses
  :: forall n
   . KnownNat n
  => Proxy n
  -> AST.MainCircuit
  -> Map.Map IndexedIdent (Prime n)
  -> Map.Map GlobalSignal Integer
computeWitnesses order main inputs =
  let
    dynInputs       = Map.map (toDyn . Smt.ValPf @n . fromP) inputs
    c               = Comp.compMainWitCtx @n main
    invocation      = Comp.getMainInvocation order main
    namespace       = [("main", [])]
    WitCompWriter w = computeWitnessesIn c namespace invocation dynInputs
  in
    -- Add the input signals to the global map :P
    Map.union (execWriter w) $ Map.map fromP $ Map.mapKeys (joinName namespace . SigLocal) inputs

parseIndexedIdent :: String -> IndexedIdent
parseIndexedIdent s =
  let parts = filter (not . null) $ Split.splitOneOf "[]" s
  in  (head parts, map (read @Int) $ tail parts)

parseSignalsFromFile
  :: forall n
   . KnownNat n
  => Proxy n
  -> FilePath
  -> IO (Map.Map IndexedIdent (Prime n))
parseSignalsFromFile _order path = do
  contents <- readFile path
  return
    $ Map.fromList
    $ map
        (\l -> case words l of
          [a, b] -> (parseIndexedIdent a, toP $ read b)
          _      -> error $ "Invalid input line: " ++ show l
        )
    $ lines contents

module IR.R1cs
  ( R1CS(..)
  , r1csStats
  , sigNumLookup
  , r1csAddSignal
  , r1csEnsureSignal
  , r1csAddSignals
  , r1csPublicizeSignal
  , r1csAddConstraint
  , r1csAddConstraints
  , emptyR1cs
  , nPublicInputs
  , r1csCountVars
  , writeToR1csFile
  , lcToR1csLine
  , qeqToR1csLines
  , sigMapQeq
  , sigMapLc
  )
where

import qualified Codegen.Circom.CompTypes.LowDeg
                                               as LD
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , fromP
                                                , toP
                                                )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import qualified Data.Tuple                    as Tuple
import           GHC.TypeLits                   ( KnownNat )

data R1CS s n = R1CS { sigNums :: !(Map.Map s Int)
                     , numSigs :: !(IntMap.IntMap s)
                     , constraints :: !(Seq.Seq (LD.QEQ Int (Prime n)))
                     , nextSigNum :: !Int
                     , publicInputs :: !IntSet.IntSet
                     } deriving (Show)

r1csStats :: R1CS s n -> String
r1csStats r = unlines
  [ "Signals: " ++ show (IntMap.size $ numSigs r)
  , "Constraints: " ++ show (length $ constraints r)
  ]


sigNumLookup :: (Show s, Ord s) => R1CS s n -> s -> Int
sigNumLookup r1cs s = Map.findWithDefault
  (  error
  $  "No signal number for "
  ++ show s
  ++ "\nSignal numbers for these signals:\n"
  ++ unlines (map (("  " ++) . show) $ Map.keys m)
  )
  s
  m
  where m = sigNums r1cs

r1csAddSignals :: Ord s => [s] -> R1CS s n -> R1CS s n
r1csAddSignals sigs r1cs =
  let zipped = zip sigs [(nextSigNum r1cs) ..]
  in  r1cs
        { sigNums    = Map.union (Map.fromList zipped) (sigNums r1cs)
        , numSigs    = IntMap.union (IntMap.fromAscList $ map Tuple.swap zipped)
                                    (numSigs r1cs)
        , nextSigNum = length zipped + nextSigNum r1cs
        }

r1csEnsureSignal :: Ord s => s -> R1CS s n -> R1CS s n
r1csEnsureSignal sig r1cs =
  if Map.member sig (sigNums r1cs) then r1cs else r1csAddSignal sig r1cs

r1csAddSignal :: Ord s => s -> R1CS s n -> R1CS s n
r1csAddSignal sig = r1csAddSignals [sig]

r1csPublicizeSignal :: (Show s, Ord s) => s -> R1CS s n -> R1CS s n
r1csPublicizeSignal sig r1cs = r1cs
  { publicInputs = IntSet.insert (sigNumLookup r1cs sig) $ publicInputs r1cs
  }

r1csAddConstraint
  :: (Show s, Ord s, KnownNat n) => LD.QEQ s (Prime n) -> R1CS s n -> R1CS s n
r1csAddConstraint c = r1csAddConstraints (Seq.singleton c)

r1csAddConstraints
  :: (Show s, Ord s, KnownNat n)
  => Seq.Seq (LD.QEQ s (Prime n))
  -> R1CS s n
  -> R1CS s n
r1csAddConstraints c r1cs = r1cs
  { constraints = fmap (sigMapQeq (sigNumLookup r1cs)) c Seq.>< constraints r1cs
  }

emptyR1cs :: R1CS s n
emptyR1cs = R1CS Map.empty IntMap.empty Seq.empty 2 IntSet.empty

nPublicInputs :: R1CS s n -> Int
nPublicInputs = IntSet.size . publicInputs

r1csCountVars :: KnownNat n => R1CS s n -> Int
r1csCountVars = foldr ((+) . qeqSize) 0 . constraints
  where qeqSize ((a, _), (b, _), (c, _)) = Map.size a + Map.size b + Map.size c

sigMapLc :: (Ord s, Ord t) => (s -> t) -> LD.LC s n -> LD.LC t n
sigMapLc f (m, c) = (Map.mapKeys f m, c)

sigMapQeq :: (Ord s, Ord t) => (s -> t) -> LD.QEQ s n -> LD.QEQ t n
sigMapQeq f (a, b, c) = (sigMapLc f a, sigMapLc f b, sigMapLc f c)

lcToR1csLine :: PrimeField n => LD.LC Int n -> [Integer]
lcToR1csLine (m, c) =
  let pairs          = Map.toAscList m
      augmentedPairs = if fromP c == 0 then pairs else (1, c) : pairs
      nPairs         = fromIntegral (length augmentedPairs)
  in  nPairs : concatMap (\(x, f) -> [fromP f, fromIntegral x]) augmentedPairs

qeqToR1csLines :: PrimeField n => LD.QEQ Int n -> [[Integer]]
qeqToR1csLines (a, b, c) = [] : map lcToR1csLine [a, b, c]

r1csAsLines :: KnownNat n => R1CS s n -> [[Integer]]
r1csAsLines r1cs =
  let nPubIns         = fromIntegral $ IntSet.size $ publicInputs r1cs
      nWit            = fromIntegral (Map.size $ sigNums r1cs) - nPubIns
      nConstraints    = fromIntegral $ Seq.length $ constraints r1cs
      constraintLines = concatMap qeqToR1csLines $ constraints r1cs
  in  [nPubIns, nWit, nConstraints] : constraintLines

writeToR1csFile :: KnownNat n => R1CS s n -> FilePath -> IO ()
writeToR1csFile r1cs path =
  writeFile path $ unlines $ map (unwords . map show) $ r1csAsLines r1cs

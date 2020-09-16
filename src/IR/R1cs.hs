{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module IR.R1cs
  ( LC
  , QEQ
  , lcSig
  , lcScale
  , lcShift
  , lcZero
  , lcAdd
  , qeqLcAdd
  , qeqScale
  , qeqShift
  , R1CS(..)
  , r1csStats
  , sigNumLookup
  , r1csAddSignal
  , r1csEnsureSignal
  , r1csAddSignals
  , r1csPublicizeSignal
  , r1csIsPublicSignal
  , r1csAddConstraint
  , r1csAddConstraints
  , r1csShow
  , qeqShow
  , lcShow
  , primeShow
  , emptyR1cs
  , nPublicInputs
  , r1csCountVars
  , writeToR1csFile
  , lcToR1csLine
  , qeqToR1csLines
  , sigMapQeq
  , sigMapLc
  , r1csCheck
  )
where

import           Control.Monad
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import qualified Data.Map.Merge.Strict         as MapMerge
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Sequence                 as Seq
import qualified Data.Tuple                    as Tuple
import qualified Data.List                     as List
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Data.Proxy                     ( Proxy(..) )

type LC s n = (Map.Map s n, n) -- A linear combination of signals and gen-time constants
type QEQ s n = (LC s n, LC s n, LC s n)

lcZero :: GaloisField k => LC s k
lcZero = (Map.empty, 0)

-- For each pair of matching coefficients, add them, dropping the coefficient if 0
lcAdd :: (Ord s, GaloisField k) => LC s k -> LC s k -> LC s k
lcAdd (sm, sc) (tm, tc) =
  ( MapMerge.merge
    MapMerge.preserveMissing
    MapMerge.preserveMissing
    (MapMerge.zipWithMaybeMatched
      (\_ a b -> let s = a + b in if s == 0 then Nothing else Just s)
    )
    sm
    tm
  , sc + tc
  )

lcSig :: (Ord s, GaloisField k) => s -> LC s k
lcSig s = (Map.fromList [(s, 1)], 0)

lcScale :: GaloisField k => k -> LC s k -> LC s k
lcScale c (sm, sc) =
  if c == 0 then (Map.empty, 0) else (Map.map (* c) sm, c * sc)

lcShift :: GaloisField k => k -> LC s k -> LC s k
lcShift c (sm, sc) = (sm, c + sc)

qeqLcAdd :: (Ord s, GaloisField k) => QEQ s k -> LC s k -> QEQ s k
qeqLcAdd (a1, b1, c1) l = (a1, b1, lcAdd c1 l)

qeqScale :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqScale k (a2, b2, c2) = (lcScale k a2, lcScale k b2, lcScale k c2)

qeqShift :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqShift k (a2, b2, c2) = (lcShift k a2, lcShift k b2, lcShift k c2)


data R1CS s n = R1CS { sigNums :: !(Map.Map s Int)
                     , numSigs :: !(IntMap.IntMap [s])
                     , constraints :: !(Seq.Seq (QEQ Int (Prime n)))
                     , nextSigNum :: !Int
                     , publicInputs :: !IntSet.IntSet
                     } deriving (Show)

r1csStats :: R1CS s n -> String
r1csStats r = unlines
  [ "Signals: " ++ show (IntMap.size $ numSigs r)
  , "Constraints: " ++ show (length $ constraints r)
  ]


r1csShow :: (KnownNat n, Show s, Ord s) => R1CS s n -> String
r1csShow r1cs =
  List.intercalate "" $ map (\qeq -> "  " ++ qeqShow qeq ++ "\n") $ r1csQeqs
    r1cs

primeShow :: forall n . KnownNat n => Prime n -> String
primeShow p =
  let v  = fromP p
      o  = natVal $ Proxy @n
      ho = o `div` 2
  in  if v < ho then show v else show (v - o)

qeqShow :: (KnownNat n, Show s) => QEQ s (Prime n) -> String
qeqShow (a, b, c) =
  unwords ["(" ++ lcShow a ++ ")", "*", "(" ++ lcShow b ++ ")", "=", lcShow c]

lcShow :: (KnownNat n, Show s) => LC s (Prime n) -> String
lcShow (m, c) =
  let list =
          map (\(x, v) -> primeShow v ++ " " ++ show x) (Map.toList m)
            ++ [ primeShow c | c /= toP 0 ]
  in  List.intercalate " + " (if null list then ["0"] else list)

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

-- TODO: get the "best" signal
r1csExternLc :: Ord s => R1CS s n -> LC Int (Prime n) -> LC s (Prime n)
r1csExternLc r1cs (m, c) = (Map.mapKeys (head . (numSigs r1cs IntMap.!)) m, c)

r1csExternQeq :: Ord s => R1CS s n -> QEQ Int (Prime n) -> QEQ s (Prime n)
r1csExternQeq r1cs (a, b, c) = let e = r1csExternLc r1cs in (e a, e b, e c)

r1csQeqs :: Ord s => R1CS s n -> [QEQ s (Prime n)]
r1csQeqs r1cs = map (r1csExternQeq r1cs) $ Fold.toList $ constraints r1cs

r1csAddSignals :: Ord s => [s] -> R1CS s n -> R1CS s n
r1csAddSignals sigs r1cs =
  let zipped = zip sigs [(nextSigNum r1cs) ..]
  in  r1cs
        { sigNums    = Map.union (Map.fromList zipped) (sigNums r1cs)
        , numSigs    = IntMap.union
                         (IntMap.fromAscList $ map (\(a, b) -> (b, [a])) zipped)
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

-- Replace `b` with `a`
r1csMergeSignals :: (Show s, Ord s) => s -> s -> R1CS s n -> R1CS s n
r1csMergeSignals a b r1cs = 
  let aN = sigNums r1cs Map.! a
      bN = sigNums r1cs Map.! b
      bSigs = numSigs r1cs IntMap.! bN
      numSigs' = IntMap.adjust (++ bSigs) aN (numSigs r1cs)
      sigNums' = Map.insert b aN (sigNums r1cs)
      constraints' = fmap (sigMapQeq (\i -> if i == bN then aN else i)) (constraints r1cs)
  in r1cs
     {  numSigs = numSigs'
     , sigNums = sigNums'
     , constraints = constraints'
     }

r1csIsPublicSignal :: (Show s, Ord s) => s -> R1CS s n -> Bool
r1csIsPublicSignal sig r1cs = case Map.lookup sig (sigNums r1cs) of
  Just n  -> IntSet.member n (publicInputs r1cs)
  Nothing -> False

r1csAddConstraint
  :: (Show s, Ord s, KnownNat n) => QEQ s (Prime n) -> R1CS s n -> R1CS s n
r1csAddConstraint c = r1csAddConstraints (Seq.singleton c)

r1csAddConstraints
  :: (Show s, Ord s, KnownNat n)
  => Seq.Seq (QEQ s (Prime n))
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

sigMapLc :: (Ord s, Ord t) => (s -> t) -> LC s n -> LC t n
sigMapLc f (m, c) = (Map.mapKeys f m, c)

sigMapQeq :: (Ord s, Ord t) => (s -> t) -> QEQ s n -> QEQ t n
sigMapQeq f (a, b, c) = (sigMapLc f a, sigMapLc f b, sigMapLc f c)

lcToR1csLine :: PrimeField n => LC Int n -> [Integer]
lcToR1csLine (m, c) =
  let pairs          = Map.toAscList m
      augmentedPairs = if fromP c == 0 then pairs else (1, c) : pairs
      nPairs         = fromIntegral (length augmentedPairs)
  in  nPairs : concatMap (\(x, f) -> [fromP f, fromIntegral x]) augmentedPairs

qeqToR1csLines :: PrimeField n => QEQ Int n -> [[Integer]]
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

getVal :: (Show s, Ord s) => s -> Map.Map s k -> k
getVal k m =
  fromMaybe (error $ "Could not find r1cs var: " ++ show k) $ m Map.!? k

lcEval :: (Show s, Ord s, Num k) => Map.Map s k -> LC s k -> k
lcEval env (m, c) = c + sum (map (\(k, v) -> v * getVal k env) $ Map.toList m)

qeqEval :: (Show s, Ord s, Num k) => Map.Map s k -> QEQ s k -> k
qeqEval env (a, b, c) = let e = lcEval env in e a * e b - e c

r1csCheck
  :: (Show s, Ord s, KnownNat n)
  => Map.Map s (Prime n)
  -> R1CS s n
  -> Either String ()
r1csCheck env r1cs = forM_ (r1csQeqs r1cs) $ \c ->
  let v = qeqEval env c
  in  if 0 == fromP v
        then Right ()
        else Left $ unwords
          ["The constraint", qeqShow c, "evaluated to", primeShow v, "not 0"]

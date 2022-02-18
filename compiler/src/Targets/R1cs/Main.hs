{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Targets.R1cs.Main
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
  , qeqZero
  , R1CS(..)
  , sigNumLookup
  , r1csAddSignal
  , r1csSetSignalVal
  , r1csInitSigVals
  , r1csEnsureSignal
  , r1csAddSignals
  , r1csPublicizeSignal
  , r1csIsPublicSignal
  , r1csAddConstraint
  , r1csAddConstraints
  , r1csMergeSignals
  , r1csMergeSignalNums
  , r1csExternLc
  , r1csExternQeq
  , r1csQeqs
  , lcSigs
  , qeqSigs
  , emptyR1cs
  , nPublicInputs
  , r1csCountVars
  , lcToR1csLine
  , qeqToR1csLines
  , sigMapQeq
  , sigMapLc
  , r1csNumValue
  )
where

import           Control.DeepSeq                ( deepseq
                                                , NFData
                                                )
import           Data.Field.Galois              ( Prime
                                                , GaloisField
                                                , fromP
                                                )
import qualified Data.Foldable                 as Fold
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map.Strict               as Map
import qualified Data.Map.Merge.Strict         as MapMerge
import qualified Data.Sequence                 as Seq
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Data.Proxy                     ( Proxy(..) )
-- Faster IO?
-- import qualified Data.Text.IO                  as TextIO
-- import           System.IO                      ( openFile
--                                                 , hClose
--                                                 , IOMode(WriteMode)
--                                                )

type LC s n = (Map.Map s n, n) -- A linear combination of signals and gen-time constants
type QEQ s n = (LC s n, LC s n, LC s n)

lcZero :: GaloisField k => LC s k
lcZero = (Map.empty, 0)

qeqZero :: GaloisField k => QEQ s k
qeqZero = (lcZero, lcZero, lcZero)

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
qeqScale k (a2, b2, c2) = (lcScale k a2, b2, lcScale k c2)

qeqShift :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqShift k (a2, b2, c2) = (a2, b2, lcShift k c2)


data R1CS s n = R1CS
  { sigNums      :: !(Map.Map s Int)
  , numSigs      :: !(IntMap.IntMap [s])
  , constraints  :: !(Seq.Seq (QEQ Int (Prime n)))
  , nextSigNum   :: !Int
  , publicInputs :: !IntSet.IntSet
  , values       :: !(Maybe (IntMap.IntMap (Prime n)))
  }

primeToSignedInt :: forall n . KnownNat n => Prime n -> Integer
primeToSignedInt p =
  let v  = fromP p
      o  = natVal $ Proxy @n
      ho = o `div` 2
  in  if v < ho then v else v - o

lcSigs :: LC s k -> [s]
lcSigs = Map.keys . fst

qeqSigs :: QEQ s k -> [s]
qeqSigs (a, b, c) = lcSigs a ++ lcSigs b ++ lcSigs c

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

r1csSetSignalVal :: Ord s => s -> Prime n -> R1CS s n -> R1CS s n
r1csSetSignalVal sig val r1cs =
  let r1cs' = r1csEnsureSignal sig r1cs
  in  r1cs'
        { values = IntMap.insert (sigNums r1cs' Map.! sig) val <$> values r1cs'
        }

r1csAddSignal :: Ord s => s -> R1CS s n -> R1CS s n
r1csAddSignal sig = r1csAddSignals [sig]

r1csPublicizeSignal :: (Show s, Ord s) => s -> R1CS s n -> R1CS s n
r1csPublicizeSignal sig r1cs = r1cs
  { publicInputs = IntSet.insert (sigNumLookup r1cs sig) $ publicInputs r1cs
  }

-- Replace `b` with `a`
r1csMergeSignalNums
  :: (Show s, Ord s, KnownNat n) => Int -> Int -> R1CS s n -> R1CS s n
r1csMergeSignalNums !aN !bN !r1cs =
  let bSigs    = numSigs r1cs IntMap.! bN
      numSigs' = IntMap.adjust (++ bSigs) aN (numSigs r1cs)
      sigNums' = foldr (flip Map.insert aN) (sigNums r1cs) bSigs
      constraints' =
          fmap (sigMapQeq (\i -> if i == bN then aN else i)) (constraints r1cs)
  in  r1cs { numSigs     = numSigs'
           , sigNums     = sigNums'
           , constraints = constraints'
           , values      = IntMap.delete bN <$> values r1cs
           }
r1csMergeSignals
  :: (Show s, Ord s, KnownNat n) => s -> s -> R1CS s n -> R1CS s n
r1csMergeSignals !a !b !r1cs =
  let aN = sigNums r1cs Map.! a
      bN = sigNums r1cs Map.! b
  in  r1csMergeSignalNums aN bN r1cs

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
emptyR1cs = R1CS Map.empty IntMap.empty Seq.empty 2 IntSet.empty Nothing

r1csInitSigVals :: R1CS s n -> R1CS s n
r1csInitSigVals r = r { values = Just IntMap.empty }

nPublicInputs :: R1CS s n -> Int
nPublicInputs = IntSet.size . publicInputs

r1csCountVars :: KnownNat n => R1CS s n -> Int
r1csCountVars = foldr ((+) . qeqSize) 0 . constraints
  where qeqSize ((a, _), (b, _), (c, _)) = Map.size a + Map.size b + Map.size c

sigMapLc
  :: forall s t n
   . (NFData t, Ord s, Ord t, Eq n, Num n, NFData n)
  => (s -> t)
  -> LC s n
  -> LC t n
sigMapLc !f (!m, !c) =
  let m' :: Map.Map t n =
          Map.filter (/= fromInteger 0) $ Map.mapKeysWith (+) f m
  in  m' `deepseq` (m', c)

sigMapQeq
  :: (NFData t, Ord s, Ord t, Eq n, Num n, NFData n)
  => (s -> t)
  -> QEQ s n
  -> QEQ t n
sigMapQeq !f (!a, !b, !c) = (sigMapLc f a, sigMapLc f b, sigMapLc f c)

lcToR1csLine :: KnownNat n => LC Int (Prime n) -> [Integer]
lcToR1csLine (m, c) =
  let pairs          = Map.toAscList m
      augmentedPairs = if fromP c == 0 then pairs else (1, c) : pairs
      nPairs         = fromIntegral (length augmentedPairs)
  in  nPairs : concatMap (\(x, f) -> [fromP f, fromIntegral x]) augmentedPairs

qeqToR1csLines :: KnownNat n => QEQ Int (Prime n) -> [[Integer]]
qeqToR1csLines (a, b, c) = [] : map lcToR1csLine [a, b, c]

r1csNumValue :: (KnownNat n, Show s) => R1CS s n -> Int -> Prime n
r1csNumValue r1cs i =
  fromMaybe
      (error $ "Could not find r1cs var: " ++ show i ++ ": " ++ show
        (numSigs r1cs IntMap.!? i)
      )
    $         fromMaybe (error "No r1cs values!") (values r1cs)
    IntMap.!? i

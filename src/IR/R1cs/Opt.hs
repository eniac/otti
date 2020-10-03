module IR.R1cs.Opt
  ( opt
  )
where

import           Control.Monad.Reader           ( asks )
import           GHC.TypeLits                   ( KnownNat )
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Foldable                 as Fold
import qualified Data.Sequence                 as Seq
import           IR.R1cs
import           IR.R1cs.Opt.RedLin             ( reduceLinearities )
import           IR.R1cs.Opt.Fold               ( foldEqs )
import           Util.Cfg                       ( MonadCfg(..)
                                                , _optR1cs
                                                )
import           Util.Log


opt :: (KnownNat n, Ord s, Show s) => R1CS s n -> Log (R1CS s n)
opt r1cs = do
  logIf "r1csOpt" $ "Constraints before r1csOpt: " ++ show
    (Seq.length $ constraints r1cs)
  logIf "r1csOpt" $ "Public inputs: " ++ show (publicInputs r1cs)
  optLevel <- liftCfg $ asks _optR1cs
  r1cs     <- (if optLevel >= 1 then foldEqs else pure) r1cs
  r1cs     <- (if optLevel >= 2 then reduceLinearities else pure) r1cs
  let r1cs' =
        (if optLevel >= 1 then compactifySigNums . removeDeadSignals else id)
          r1cs
  logIf "r1csOpt" $ "Constraints after r1csOpt: " ++ show
    (Seq.length $ constraints r1cs')
  return r1cs'

-- Remove signals not involved in constraints
removeDeadSignals :: R1CS s n -> R1CS s n
removeDeadSignals r1cs =
  let liveSigs = liveSignalIntsR1cs r1cs `IntSet.union` publicInputs r1cs
  in
    r1cs
      { numSigs    = IntMap.filterWithKey (\k _ -> IntSet.member k liveSigs)
                       $ numSigs r1cs
      , sigNums    = Map.filter (`IntSet.member` liveSigs) $ sigNums r1cs
      , nextSigNum = 2 + IntSet.size liveSigs
      , values     = IntMap.filterWithKey (\k _ -> IntSet.member k liveSigs)
                       $ values r1cs
      }
 where
  liveSignalIntsLc (m, _) = IntSet.fromDistinctAscList $ Map.keys m
  liveSignalIntsQeq (a, b, c) =
    foldl1 IntSet.union $ map liveSignalIntsLc [a, b, c]
  liveSignalIntsR1cs =
    Fold.foldr IntSet.union IntSet.empty . fmap liveSignalIntsQeq . constraints

-- Given a set of constraints, ensures that the signal numbers are in the range
-- [2..(1+n)], where n is the number of signals
compactifySigNums :: (Show s, KnownNat n) => R1CS s n -> R1CS s n
compactifySigNums r1cs =
  let usedNums = IntMap.keys $ numSigs r1cs
      numMap   = IntMap.fromDistinctAscList $ zip usedNums [2 ..]
      remap s i =
          Maybe.fromMaybe
              (  error
              $  "Could not find signal number "
              ++ show i
              ++ " when remapping signal numbers.\n\nContext: "
              ++ s
              ++ "\n\n numSigs: "
              ++ show (numSigs r1cs)
              ++ "\n"
              )
            $         numMap
            IntMap.!? i
  in  R1CS
        { sigNums      = Map.map (remap "sigNums") $ sigNums r1cs
        , numSigs = IntMap.mapKeysMonotonic (remap "numSigs") $ numSigs r1cs
        , constraints  = sigMapQeq (remap "constraints") <$> constraints r1cs
        , publicInputs = IntSet.map (remap "publicInputs") $ publicInputs r1cs
        , values       = IntMap.mapKeysMonotonic (remap "values") $ values r1cs
        , nextSigNum   = 2 + IntMap.size numMap
        }

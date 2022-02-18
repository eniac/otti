{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Targets.R1cs.Opt.RedLin
  ( reduceLinearities
  )
where

import           Control.Monad.State.Strict
import           Data.Bifunctor                 ( second )
import qualified Data.IntMap.Strict            as IntMap
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntSet                   as IntSet
import           Data.IntSet                    ( IntSet )
import           Data.Field.Galois              ( Prime )
import qualified Data.Foldable                 as Fold
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import           Data.Maybe                     ( fromMaybe )
import           GHC.TypeLits                   ( KnownNat )
import           Targets.R1cs.Main
import           Targets.R1cs.Opt.Util          ( normalize
                                                , constantlyTrue
                                                , asLinearSub
                                                , subLcInQeq
                                                )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import qualified Util.Cfg                      as Cfg
import           Util.Control
import           Util.Log

-- A constraint, together with its set of variables.
type Constraint n = (QEQ Int (Prime n), IntSet)
type ConstrId = Int
data RedLinState n = RedLinState
  { _cs     :: IntMap (Constraint n)
  , _uses   :: IntMap IntSet
  , _queue  :: Seq Int
  , _queued :: IntSet
  }


$(makeLenses ''RedLinState)

newtype RedLin n a = RedLin (StateT (RedLinState n) Log a)
  deriving (Functor, Applicative, Monad, MonadState (RedLinState n), MonadIO, MonadLog, Cfg.MonadCfg)

shouldVisit :: KnownNat n => Constraint n -> Bool
shouldVisit ((a, b, _), _) = a == lcZero || b == lcZero

getC :: ConstrId -> RedLin n (Constraint n)
getC id' = gets
  ( fromMaybe (error $ "No constraint at: " ++ show id')
  . IntMap.lookup id'
  . view cs
  )

-- Add id to the queue, if it's not there already
enqueue :: KnownNat n => ConstrId -> RedLin n ()
enqueue id' = do
  there <- gets (IntSet.member id' . view queued)
  unless there $ do
    modify $ over queued $ IntSet.insert id'
    modify $ over queue (Seq.|> id')

updateC :: ConstrId -> Constraint n -> RedLin n ()
updateC id' c = do
  oldVars <- snd <$> getC id'
  let newVars = snd c
  -- Unregister vars that are now unused
  forM_ (IntSet.toList $ oldVars IntSet.\\ newVars)
    $ \v -> modify $ over uses $ IntMap.adjust (IntSet.delete id') v
  -- Re-register vars that are now used
  forM_ (IntSet.toList $ newVars IntSet.\\ oldVars)
    $ \v -> modify $ over uses $ IntMap.adjust (IntSet.insert id') v
  modify $ over cs $ IntMap.insert id' c


popFromQueue :: RedLin n (Maybe ConstrId)
popFromQueue = do
  q <- gets (view queue)
  case q of
    Seq.Empty     -> return Nothing
    (Seq.:<|) h t -> do
      modify (set queue t)
      modify (over queued $ IntSet.delete h)
      return (Just h)

run :: (Ord s, Show s, KnownNat n) => R1CS s n -> RedLin n ()
run r1cs = do
  ids <- gets (IntMap.keys . view cs)
  forM_ ids $ \id' -> do
    c <- getC id'
    when (shouldVisit c) $ enqueue id'
  -- For `id` in queue
  whileJustM popFromQueue $ \id' -> do
    c <- getC id'
    -- If it is a linear sub, `v` -> `t`
    whenJust (asLinearSub (publicInputs r1cs) (fst c)) $ \(v, t) -> do
      logIf "r1cs::opt::lin::sub" $ show $ map show (numSigs r1cs IntMap.! v)
      -- Get constraints that use `v`
      vUses <- gets (IntSet.toList . (IntMap.! v) . view uses)
      -- Set constraint to zero
      updateC id' (qeqZero, IntSet.empty)
      -- For constraints that use `v`
      forM_ vUses $ \useId -> do
        -- Do substitution
        useC <- getC useId
        let useC' = attachSigs $ normalize $ subLcInQeq v t $ fst useC
        updateC useId useC'
        -- Enqueue if result looks promising
        when (shouldVisit useC') $ enqueue useId

-- TODO: be careful to never reconstruct the variable sets.
qSigs (a, b, c) = IntSet.union (lSigs a) (IntSet.union (lSigs b) (lSigs c))
lSigs (m, _) = IntSet.fromDistinctAscList $ Map.keys m
attachSigs qeq = (qeq, qSigs qeq)

initRedLinState :: (Show s, Ord s, KnownNat n) => R1CS s n -> RedLinState n
initRedLinState r1cs =
  let
    cs' =
      IntMap.fromDistinctAscList
        $ zip [0 ..]
        $ map (attachSigs . normalize)
        $ Fold.toList
        $ constraints r1cs
    usePairs =
      [ (v, id') | (id', (_, vs)) <- IntMap.toList cs', v <- IntSet.toList vs ]
    uses' =
      IntMap.fromListWith IntSet.union $ map (second IntSet.singleton) usePairs
  in
    RedLinState { _cs     = cs'
                , _uses   = uses'
                , _queue  = Seq.empty
                , _queued = IntSet.empty
                }

reduceLinearities :: (Show s, Ord s, KnownNat n) => R1CS s n -> Log (R1CS s n)
reduceLinearities r1cs = do
  let (RedLin runAction) = run r1cs
  logIf "r1cs::opt::lin" $ "Public: " ++ show (publicInputs r1cs)
  st <- execStateT runAction $ initRedLinState r1cs
  let constraints' =
        Seq.fromList
          $ filter (not . constantlyTrue)
          $ map fst
          $ IntMap.elems
          $ view cs st
  logIf "r1cs::opt::lin" $ "Constraints: " ++ show (Seq.length constraints')
  return $ r1cs { constraints = constraints' }

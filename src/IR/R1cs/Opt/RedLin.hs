{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module IR.R1cs.Opt.RedLin
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
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import           Data.Maybe                     ( fromMaybe )
import           GHC.TypeLits                   ( KnownNat )
import           IR.R1cs
import           IR.R1cs.Opt.Util               ( normalize
                                                , constantlyTrue
                                                , asLinearSub
                                                , subLcInQeq
                                                )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Util.Log

type Constraint n = QEQ Int (Prime n)
type ConstrId = Int
data RedLinState n = RedLinState { _cs :: IntMap (Constraint n)
                                 , _uses :: IntMap IntSet
                                 , _queue :: Seq Int
                                 , _queued :: IntSet
                                 }


$(makeLenses ''RedLinState)

newtype RedLin n a = RedLin (StateT (RedLinState n) Log a)
  deriving (Functor, Applicative, Monad, MonadState (RedLinState n), MonadIO, MonadLog)

shouldVisit :: KnownNat n => Constraint n -> Bool
shouldVisit (a, b, _) = a == lcZero || b == lcZero

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

whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust test body = maybe (return ()) body test

updateC :: ConstrId -> Constraint n -> RedLin n ()
updateC id' c = do
  oldVars <- IntSet.fromList . qeqSigs <$> getC id'
  let newVars = IntSet.fromList $ qeqSigs c
  -- Unregister vars that are now unused
  forM_ (IntSet.toList $ oldVars IntSet.\\ newVars)
    $ \v -> modify $ over uses $ IntMap.adjust (IntSet.delete id') v
  -- Re-register vars that are now used
  forM_ (IntSet.toList $ newVars IntSet.\\ oldVars)
    $ \v -> modify $ over uses $ IntMap.adjust (IntSet.insert id') v
  modify $ over cs $ IntMap.insert id' c

whileJustM :: Monad m => m (Maybe a) -> (a -> m ()) -> m ()
whileJustM test body = do
  t <- test
  case t of
    Just v  -> body v >> whileJustM test body
    Nothing -> return ()

popFromQueue :: RedLin n (Maybe ConstrId)
popFromQueue = do
  q <- gets (view queue)
  case q of
    Seq.Empty     -> return Nothing
    (Seq.:<|) h t -> do
      modify (set queue t)
      modify (over queued $ IntSet.delete h)
      return (Just h)

run :: (Show s, KnownNat n) => R1CS s n -> RedLin n ()
run r1cs = do
  ids <- gets (IntMap.keys . view cs)
  forM_ ids $ \id' -> do
    c <- getC id'
    when (shouldVisit c) $ enqueue id'
  -- For `id` in queue
  whileJustM popFromQueue $ \id' -> do
    c <- getC id'
    -- If it is a linear sub, `v` -> `t`
    whenJust (asLinearSub (publicInputs r1cs) c) $ \(v, t) -> do
      liftLog $ logIf "r1cs::opt::lin::sub" $ show $ map
        show
        (numSigs r1cs IntMap.! v)
      -- Get constraints that use `v`
      vUses <- gets (IntSet.toList . (IntMap.! v) . view uses)
      -- Set constraint to zero
      updateC id' qeqZero
      -- For constraints that use `v`
      forM_ vUses $ \useId -> do
        -- Do substitution
        useC <- getC useId
        let useC' = normalize $ subLcInQeq v t useC
        updateC useId useC'
        -- Enqueue if result looks promising
        when (shouldVisit useC') $ enqueue useId

initRedLinState :: KnownNat n => R1CS s n -> RedLinState n
initRedLinState r1cs =
  let
    cs' =
      IntMap.fromDistinctAscList
        $ zip [0 ..]
        $ map normalize
        $ Fold.toList
        $ constraints r1cs
    usePairs = [ (v, id') | (id', qeq) <- IntMap.toList cs', v <- qeqSigs qeq ]
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
  st <- execStateT runAction $ initRedLinState r1cs
  let constraints' =
        Seq.fromList $ filter (not . constantlyTrue) $ IntMap.elems $ view cs st
  logIf "r1cs::opt::lin" $ "Constraints: " ++ show (Seq.length constraints')
  return $ r1cs { constraints = constraints' }

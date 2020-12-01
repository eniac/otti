{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Targets.R1cs.Opt.Fold
  ( foldEqs
  )
where

import           Control.DeepSeq
import           Control.Monad.State.Strict
import qualified Data.IntMap.Strict            as IntMap
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntSet                   as IntSet
import           Data.IntSet                    ( IntSet )
import qualified Data.Map.Strict               as Map
import qualified Data.Sequence                 as Seq
import           GHC.TypeLits                   ( KnownNat )
import           Targets.R1cs.Main
import           Targets.R1cs.Opt.Util          ( asEqOrConst
                                                , Eliminatable(..)
                                                , normalize
                                                , constantlyTrue
                                                )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , view
                                                )
import           Util.Log

data FoldState = FoldState
  { _reps    :: !(IntMap Int)
  , _classes :: !(IntMap IntSet)
  }

emptyFoldState :: FoldState
emptyFoldState = FoldState IntMap.empty IntMap.empty

$(makeLenses ''FoldState)

newtype Fold a = Fold (StateT FoldState Log a)
  deriving (Functor, Applicative, Monad, MonadState FoldState, MonadIO, MonadLog)

ensure :: Int -> Fold ()
ensure x = do
  present <- gets (IntMap.member x . view reps)
  unless present $ do
    modify $ over reps $ IntMap.insert x x
    modify $ over classes $ IntMap.insert x (IntSet.singleton x)

equate :: Int -> Int -> Fold ()
equate x y = do
  logIf "r1cs::opt::fold::debug" $ "equate " ++ show x ++ " with " ++ show y
  ensure x
  ensure y
  xRep   <- gets $ (IntMap.! x) . view reps
  yRep   <- gets $ (IntMap.! y) . view reps
  xClass <- gets $ (IntMap.! xRep) . view classes
  yClass <- gets $ (IntMap.! yRep) . view classes
  -- Replacing bRep with aRep
  let (aR, aC, bR, bC) = if False
  --let (aR, aC, bR, bC) = if IntSet.size xClass < IntSet.size yClass
        then (yRep, yClass, xRep, xClass)
        else (xRep, xClass, yRep, yClass)
  -- reassign members of bRep's class
  forM_ (IntSet.toList bC) $ \z -> modify $ over reps $ IntMap.insert z aR
  -- update aRep's class
  modify $ over classes $ IntMap.insert aR $ IntSet.union aC bC
  -- remove bRep's class
  modify $ over classes $ IntMap.delete bR

foldEqs :: (Show s, Ord s, KnownNat n) => R1CS s n -> Log (R1CS s n)
foldEqs r1cs = do
  let (Fold action) = do
        forM_ (IntMap.keys $ numSigs r1cs) ensure
        forM_ (constraints r1cs) $ \c ->
          case asEqOrConst (publicInputs r1cs) (normalize c) of
            Eq a b -> equate a b
            _      -> return ()
  s <- execStateT action emptyFoldState
  logIf "r1cs::opt::fold" $ "Eq classes: " ++ show
    (IntMap.size $ view classes s)
  -- (replaced, replacement) pairs
  let representatives =
        [ (j, i)
        | (i, js) <- IntMap.toList (view classes s)
        , j       <- IntSet.toList js
        ]
  let replacements = filter (uncurry (/=)) representatives
  logIf "r1cs::opt::fold" $ "Constraints before: " ++ show
    (Seq.length $ constraints r1cs)
  let constraints' =
        force
          $   Seq.filter (not . constantlyTrue)
          $   sigMapQeq (IntMap.fromList representatives IntMap.!)
          <$> constraints r1cs
  logIf "r1cs::opt::fold" $ "Constraints after : " ++ show
    (Seq.length constraints')
  let r1cs' = foldr (uncurry $ flip mergeInNumSigMaps)
                    r1cs { constraints = constraints' }
                    replacements
  return r1cs'
 where
  -- replaces bN with aN, updating signal maps, but not constraints
  mergeInNumSigMaps
    :: (Show s, Ord s, KnownNat n) => Int -> Int -> R1CS s n -> R1CS s n
  mergeInNumSigMaps aN bN r1cs' =
    let bSigs    = numSigs r1cs' IntMap.! bN
        numSigs' = IntMap.adjust (++ bSigs) aN (numSigs r1cs')
        sigNums' = foldr (flip Map.insert aN) (sigNums r1cs') bSigs
    in  r1cs' { numSigs = numSigs', sigNums = sigNums' }

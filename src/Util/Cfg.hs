{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Util.Cfg
  ( Cfg(..)
  , CfgState(..)
  , SmtOptCfg(..)
  , ToPfCfg(..)
  , MonadCfg(..)
  , setFromEnv
  , defaultCfgState
  , evalCfg
  , evalCfgDefault
  )
where

import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.List                      ( intercalate )
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                , Proxy(Proxy)
                                                )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )
import           Data.List.Split                ( splitOn )
import           Lens.Simple

data SmtOptCfg = SmtOptCfg { _allowSubBlowup :: Bool
                           , _cFoldInSub     :: Bool
                           , _smtOpts        :: [String]
                           } deriving (Show)

defaultSmtOptCfg :: SmtOptCfg
defaultSmtOptCfg = SmtOptCfg { _allowSubBlowup = False
                             , _cFoldInSub     = True
                             , _smtOpts        = ["cfee", "ee"]
                             }

$(makeLenses ''SmtOptCfg)

data ToPfCfg = ToPfCfg { _assumeNoOverflow :: Bool
                       , _optEqs           :: Bool
                       , _assumeInputsInRange :: Bool
                       } deriving (Show)
defaultToPfCfg :: ToPfCfg
defaultToPfCfg = ToPfCfg { _assumeNoOverflow    = False
                         , _optEqs              = True
                         , _assumeInputsInRange = True
                         }

$(makeLenses ''ToPfCfg)

data CfgState = CfgState { _optR1cs :: Bool
                         , _toPfCfg :: ToPfCfg
                         , _smtOptCfg :: SmtOptCfg
                         , _streams :: [String]
                         , _loopBound :: Int
                         } deriving (Show)

defaultCfgState :: CfgState
defaultCfgState = CfgState { _optR1cs   = True
                           , _toPfCfg   = defaultToPfCfg
                           , _smtOptCfg = defaultSmtOptCfg
                           , _streams   = []
                           , _loopBound = 5
                           }

$(makeLenses ''CfgState)

newtype Cfg a = Cfg (ReaderT CfgState IO a)
    deriving (Functor, Applicative, Monad, MonadReader CfgState, MonadIO)

class Monad m => MonadCfg m where
  liftCfg :: Cfg a -> m a
instance MonadCfg Cfg where
  liftCfg = id
instance (MonadCfg m) => MonadCfg (StateT s m) where
  liftCfg = lift . liftCfg

evalCfg :: Cfg a -> CfgState -> IO a
evalCfg (Cfg action) = runReaderT action

evalCfgDefault :: Cfg a -> IO a
evalCfgDefault (Cfg action) = runReaderT action defaultCfgState

data CfgOption = CfgOption { optLens :: Lens' CfgState String
                           , optName :: String
                           , optDesc :: String
                           , optDetail :: String
                           , optDefault :: String
                           }

showReadLens :: (Typeable a, Show a, Read a) => Lens' a String
showReadLens = lens show (const read')
 where
  read' :: forall a . (Read a, Typeable a) => String -> a
  read' s =
    maybe
        (error $ "Cannot deserialize " ++ show s ++ " as a " ++ show
          (typeRep $ Proxy @a)
        )
        id
      $ readMaybe s

commaListLens :: Lens' [String] String
commaListLens =
  lens (intercalate ",") (const (filter (not . null) . splitOn ","))

options :: [CfgOption]
options =
  [ CfgOption (optR1cs . showReadLens)
              "opt-r1cs"
              "Optimize the rank-1 constraint system"
              "Eliminates linear equations through substitution."
              "True"
  , CfgOption
    (toPfCfg . assumeNoOverflow . showReadLens)
    "no-overflow"
    "Assume C arithmetic never overflow"
    "Omits wrapping constraints from bit-vector arithmetic when lowering from Smt to R1cs"
    "False"
  , CfgOption
    (toPfCfg . optEqs . showReadLens)
    "to-pf-opt-eq"
    "Optimize equalities when lower Smt to R1cs"
    "Unconditional equalities are lowered as *aliases*, rather than encoded as equalities in R1cs"
    "True"
  , CfgOption
    (toPfCfg . assumeInputsInRange . showReadLens)
    "inputs-in-range"
    "Assume that r1cs public inputs are in range"
    "Does not emit constraints forcing the field representation of public bitvector inputs to be in-range. The outer system is assumed to check this."
    "True"
  , CfgOption (smtOptCfg . allowSubBlowup . showReadLens)
              "smt-sub-blowup"
              "Allow Smt substitutions which increase formula size"
              ""
              "False"
  , CfgOption (smtOptCfg . cFoldInSub . showReadLens)
              "smt-cfold-in-sub"
              "Perform constant folding during Smt substitutions"
              ""
              "False"
  , CfgOption (smtOptCfg . smtOpts . commaListLens)
              "smt-opts"
              "Optimizations to perform over the Smt formula"
              "A comma-separated list. Options: {cfee, ee, cf}"
              "cfee,ee"
  , CfgOption (streams . commaListLens)
              "streams"
              "Debug streams to emit"
              "A comma-separated list"
              ""
  , CfgOption (loopBound . showReadLens)
              "loop-bound"
              "How many iterations loops are unrolled for"
              ""
              "5"
  ]

setFromEnv :: CfgState -> IO CfgState
setFromEnv cfg = foldM setOptFromEnv cfg options
 where
  setOptFromEnv :: CfgState -> CfgOption -> IO CfgState
  setOptFromEnv st o = do
    v <- lookupEnv ("C-" ++ optName o)
    return $ case v of
      Just s  -> set (optLens o) s st
      Nothing -> st

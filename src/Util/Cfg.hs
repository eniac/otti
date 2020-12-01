{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module Util.Cfg
  ( Cfg(..)
  , CfgState(..)
  , SmtOptCfg(..)
  , CCfg(..)
  , ToPfCfg(..)
  , R1csCfg(..)
  , MonadCfg(..)
  , CfgOption(..)
  , setFromEnv
  , setFromArgs
  , defaultCfgState
  , evalCfg
  , evalCfgDefault
  , options
  )
where

import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Functor.Identity          ( Identity(runIdentity) )
import           Data.List                      ( intercalate )
import           Data.Typeable                  ( Typeable
                                                , typeRep
                                                , Proxy(Proxy)
                                                )
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Map                      as M
import           System.Environment             ( lookupEnv )
import           System.Exit                    ( exitSuccess )
import           Text.Read                      ( readMaybe )
import           Data.List.Split                ( splitOn )
import           Lens.Simple

data SmtOptCfg = SmtOptCfg
  { _allowSubBlowup :: Bool
  , _cFoldInSub     :: Bool
  , _smtOpts        :: [String]
  , _optForZ3       :: Bool
  , _checkOpts      :: Bool
  , _benesThresh    :: Int
  , _subBvAdd       :: Bool
  }
  deriving Show

defaultSmtOptCfg :: SmtOptCfg
defaultSmtOptCfg = SmtOptCfg
  { _allowSubBlowup = False
  , _cFoldInSub     = True
  , _smtOpts        = [ "cfee"
                      , "ee"
                      , "nary"
                      , "cfee"
                      , "arrayElim"
                      , "flattenAnds"
                      , "cfee"
                      , "ee"
                      , "mem"
                      , "flattenAnds"
                      , "cfee"
                      , "ee"
                      ]
  , _optForZ3       = False
  , _checkOpts      = False
  , _benesThresh    = 50
  , _subBvAdd       = True
  }

$(makeLenses ''SmtOptCfg)

data ToPfCfg = ToPfCfg
  { _assumeNoOverflow    :: Bool
  , _optEqs              :: Bool
  , _assumeInputsInRange :: Bool
  }
  deriving Show
defaultToPfCfg :: ToPfCfg
defaultToPfCfg = ToPfCfg { _assumeNoOverflow    = False
                         , _optEqs              = True
                         , _assumeInputsInRange = True
                         }
$(makeLenses ''ToPfCfg)

data CCfg = CCfg
  { _printfOutput  :: Bool
  , _svExtensions  :: Bool
  , _pequinIo      :: Bool
  , _constLoops    :: Bool
  , _smtBoundLoops :: Bool
  }
  deriving Show

defaultCCfg = CCfg { _printfOutput  = True
                   , _svExtensions  = False
                   , _pequinIo      = False
                   , _constLoops    = True
                   , _smtBoundLoops = False
                   }

$(makeLenses ''CCfg)

data R1csCfg = R1csCfg
  { _optLevel      :: Int
  , _checkR1csOpts :: Bool
  }
  deriving Show

$(makeLenses ''R1csCfg)

defaultR1csCfg = R1csCfg { _optLevel = 2, _checkR1csOpts = False }


data CfgState = CfgState
  { _r1csCfg          :: R1csCfg
  , _toPfCfg          :: ToPfCfg
  , _smtOptCfg        :: SmtOptCfg
  , _streams          :: [String]
  , _loopBound        :: Int
  , _loopFlatten      :: Bool
  , _loopMaxIteration :: Int
  , _cCfg             :: CCfg
  , _help             :: Bool
  }
  deriving Show

defaultCfgState :: CfgState
defaultCfgState = CfgState { _r1csCfg          = defaultR1csCfg
                           , _toPfCfg          = defaultToPfCfg
                           , _smtOptCfg        = defaultSmtOptCfg
                           , _streams          = []
                           , _loopBound        = 5
                           , _loopFlatten      = True
                           , _loopMaxIteration = 10000
                           , _help             = False
                           , _cCfg             = defaultCCfg
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
evalCfg (Cfg action) cfg = setFromEnv cfg >>= runReaderT action

--TODO: This is helpful for getting stream output from tests. Worth it?
evalCfgDefault :: Cfg a -> IO a
evalCfgDefault (Cfg action) = setFromEnv defaultCfgState >>= runReaderT action

data CfgOption = CfgOption
  { optLens    :: Lens' CfgState String
  , optName    :: String
  , optDesc    :: String
  , optDetail  :: String
  , optDefault :: String
  }

showReadLens :: (Typeable a, Show a, Read a) => Lens' a String
showReadLens = lens show (const read')
 where
  read' :: forall a . (Read a, Typeable a) => String -> a
  read' s =
    fromMaybe
        (error $ "Cannot deserialize " ++ show s ++ " as a " ++ show
          (typeRep $ Proxy @a)
        )
      $ readMaybe s

commaListLens :: Lens' [String] String
commaListLens =
  lens (intercalate ",") (const (filter (not . null) . splitOn ","))

cfgEnvName :: CfgOption -> String
cfgEnvName o = replace '-' '_' $ "C_" ++ optName o

options :: [CfgOption]
options =
  [ CfgOption
    (r1csCfg . optLevel . showReadLens)
    "opt-r1cs"
    "Level of optimization to apply to the R1CS"
    "0: None (not recommended), 1: eliminate equalities, 2: eliminate all linear constraints"
    "2"
  , CfgOption (r1csCfg . checkR1csOpts . showReadLens)
              "check-opt-r1cs"
              "Check each R1cs optimization"
              ""
              "False"
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
              "True"
  , CfgOption
    (smtOptCfg . smtOpts . commaListLens)
    "smt-opts"
    "Optimizations to perform over the Smt formula"
    "A comma-separated list. Options: {cfee, ee, cf, arrayElim, flattenAnds, mem}"
    "cfee,ee,nary,cfee,arrayElim,flattenAnds,cfee,ee,mem,flattenAnds,cfee,ee"
  , CfgOption (smtOptCfg . optForZ3 . showReadLens)
              "opt-z3"
              "Optimize the z3 inputs"
              ""
              "False"
  , CfgOption (smtOptCfg . checkOpts . showReadLens)
              "smt-check-opts"
              "Check the SMT system after each optimization"
              "Takes additional time"
              "False"
  , CfgOption
    (smtOptCfg . benesThresh . showReadLens)
    "smt-benes-thresh"
    "The array size at which a benes network is used instead of a linear scan"
    ""
    "50"
  , CfgOption
    (smtOptCfg . subBvAdd . showReadLens)
    "smt-sub-bv-add"
    "Always substitute/eliminate variables equal to bit-vector additions"
    ""
    "True"
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
  , CfgOption (loopFlatten . showReadLens)
              "loop-flatten"
              "Enable nested loop flattening optimization"
              ""
              "True"
  , CfgOption (loopMaxIteration . showReadLens)
              "loop-max-iteration"
              "Maximum iterator bound for loop flattening optimization"
              ""
              "10000"
  , CfgOption (cCfg . printfOutput . showReadLens)
              "c-printf"
              "Handle printf specially"
              "It is a bug to print something undefined"
              "True"
  , CfgOption
    (cCfg . svExtensions . showReadLens)
    "c-sv"
    "Apply sv conventions"
    "Handle __VERIFIER_error, __VERIFIER_assert, __VERIFIER_assume, __VERIFIER_nondet_<type> in accordance with the competition"
    "False"
  , CfgOption (cCfg . pequinIo . showReadLens)
              "pequin-io"
              "Use pequin IO conventions"
              "Input is a struct In *, output is a struct Out *"
              "False"
  , CfgOption (cCfg . constLoops . showReadLens)
              "const-loops"
              "Detect constant-iteration loops and special-case them"
              "Detects incrementally increasing loops"
              "True"
  , CfgOption (cCfg . smtBoundLoops . showReadLens)
              "smt-bound-loops"
              "Use an SMT solver to only unroll loops as much as necessary."
              "The solver checks whether the post-condition path is feasible."
              "False"
  , CfgOption (help . showReadLens)
              "help"
              "Prints cfg help and exits"
              ""
              "False"
  ]

replace :: Char -> Char -> String -> String
replace f t s = case s of
  []     -> []
  c : s' -> (if c == f then t else c) : replace f t s'

setFromEnv :: CfgState -> IO CfgState
setFromEnv cfg = do
  cfg' <- foldM setOptFromEnv cfg options
  when (_help cfg') $ do
    forM_ options $ \o ->
      putStrLn
        $  optName o
        ++ " (env: "
        ++ cfgEnvName o
        ++ ") : "
        ++ optDesc o
        ++ "\n\t"
        ++ optDetail o
        ++ "\n\tDefault: "
        ++ optDefault o
    exitSuccess
  return cfg'
 where
  setOptFromEnv :: CfgState -> CfgOption -> IO CfgState
  setOptFromEnv st o = do
    v <- lookupEnv (cfgEnvName o)
    return $ case v of
      Just s  -> set (optLens o) s st
      Nothing -> st

setFromArgs :: [String] -> CfgState -> ([String], CfgState)
setFromArgs args st = runIdentity $ runStateT (go args) st
 where
  go :: [String] -> StateT CfgState Identity [String]
  go args = case args of
    []                  -> return []
    karg : varg : args' -> case M.lookup karg m of
      Just o  -> (modify $ set (optLens o) varg) >> go args'
      Nothing -> (karg :) <$> go (varg : args')
    [a] -> return [a]
  m = M.fromList [ ("--" ++ optName o, o) | o <- options ]

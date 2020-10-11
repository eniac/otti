{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
module IR.SMT.Opt
  ( opt
  , constantFold
  , eqElim
  , OptMetadata(..)
  , newOptMetadata
  )
where

import           IR.SMT.TySmt
import           IR.SMT.OblivArray              ( elimOblivArrays )
import           IR.SMT.Opt.ConstFoldEqElim     ( constantFold
                                                , constFoldEqElim
                                                )
import           IR.SMT.Opt.EqElim              ( eqElim )

import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import           Util.Cfg                       ( MonadCfg(..)
                                                , _smtOpts
                                                , _smtOptCfg
                                                )
import           Util.Log
import           Util.ShowMap                   ( ShowMap )

type ArraySizes = ShowMap (Term (ArraySort DynBvSort DynBvSort)) Int

data OptMetadata = OptMetadata
  { protected      :: !(Set.Set String)
  , arraySizes     :: !ArraySizes
  } deriving (Show)

newOptMetadata :: Set.Set String -> ArraySizes -> OptMetadata
newOptMetadata p s = OptMetadata { protected = p, arraySizes = s }

data Opt = Opt
  { fn   :: OptMetadata -> [TermBool] -> Log [TermBool]
  , name :: String
  }

constantFoldOpt :: Opt
constantFoldOpt = Opt { fn = const (return . map constantFold), name = "cf" }

constantFoldEqOpt :: Opt
constantFoldEqOpt = Opt { fn = constFoldEqElim . protected, name = "cfee" }

eqElimOpt :: Opt
eqElimOpt = Opt { fn = eqElim . protected, name = "ee" }

arrayElimOpt :: Opt
arrayElimOpt = Opt { fn = elimOblivArrays . arraySizes, name = "arrayElim" }

-- | Flattens ANDs
flattenAnds :: TermBool -> [TermBool]
flattenAnds t = case t of
  BoolNaryExpr And conjuncts -> concatMap flattenAnds conjuncts
  _                          -> [t]

flattenAndsOpt :: Opt
flattenAndsOpt =
  Opt { fn = \_ ts -> return $ concatMap flattenAnds ts, name = "flattenAnds" }

opts :: Map.Map String Opt
opts = Map.fromList
  [ (name o, o)
  | o <-
    [ eqElimOpt
    , constantFoldOpt
    , constantFoldEqOpt
    , arrayElimOpt
    , flattenAndsOpt
    ]
  ]

logAssertions :: String -> [TermBool] -> Log ()
logAssertions context as = logIfM "smt::opt" $ do
  liftIO $ putStrLn $ context ++ ":"
  forM_ as $ \a -> liftIO $ putStrLn $ "  " ++ show a
  return $ show (length as) ++ " assertions"


-- Optimize, ensuring that the variables in `p` continue to exist.
opt :: ArraySizes -> Set.Set String -> [TermBool] -> Log [TermBool]
opt sizes p ts = do
  let m' = OptMetadata { protected = p, arraySizes = sizes }
  optsToRun <- liftCfg $ asks (_smtOpts . _smtOptCfg)
  logAssertions "initial" ts
  foldM
    (\a oname -> do
      let o =
            fromMaybe (error $ "No optimization named: " ++ oname)
              $ Map.lookup oname opts
      a' <- fn o m' a
      logAssertions ("Post " ++ show oname) a'
      return a'
    )
    ts
    optsToRun

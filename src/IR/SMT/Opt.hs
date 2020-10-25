{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
module IR.SMT.Opt
  ( opt
  , OptMetadata(..)
  , newOptMetadata
  )
where

import           IR.SMT.TySmt
import           IR.SMT.Opt.Mem.OblivArray      ( elimOblivArrays )
import           IR.SMT.Opt.Mem.NonObliv        ( memPass )
import           IR.SMT.Opt.ConstFoldEqElim     ( constantFold
                                                , constFoldEqElim
                                                )
import           IR.SMT.Opt.EqElim              ( eqElim )
import           IR.SMT.Opt.Nary                ( flattenNary )
import qualified IR.SMT.Opt.Assert             as OA
import qualified IR.SMT.Assert                 as A

import           Control.Monad.State.Strict     ( forM_
                                                , liftM2
                                                )
import           Control.Monad.Reader           ( asks )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import           Util.Cfg                       ( MonadCfg(..)
                                                , _smtOpts
                                                , _smtOptCfg
                                                , _checkOpts
                                                )
import           Util.Control                   ( whenM )
import           Util.Log
import           Util.ShowMap                   ( ShowMap )

type ArraySizes = ShowMap (Term (ArraySort DynBvSort DynBvSort)) Int

data OptMetadata = OptMetadata
  { protected  :: !(Set.Set String)
  , arraySizes :: !ArraySizes
  }
  deriving Show

newOptMetadata :: Set.Set String -> ArraySizes -> OptMetadata
newOptMetadata p s = OptMetadata { protected = p, arraySizes = s }

data Opt = Opt
  { fn   :: OA.Assert ()
  , name :: String
  }

constantFoldOpt :: Opt
constantFoldOpt =
  Opt { fn = OA.modifyAssertions (return . map constantFold), name = "cf" }

constantFoldEqOpt :: Opt
constantFoldEqOpt = Opt { fn = constFoldEqElim, name = "cfee" }

eqElimOpt :: Opt
eqElimOpt = Opt { fn = eqElim, name = "ee" }

arrayElimOpt :: Opt
arrayElimOpt = Opt { fn = elimOblivArrays, name = "arrayElim" }

memOpt :: Opt
memOpt = Opt { fn = memPass, name = "mem" }

-- | Flattens ANDs
flattenAnds :: TermBool -> [TermBool]
flattenAnds t = case t of
  BoolNaryExpr And conjuncts -> concatMap flattenAnds conjuncts
  _                          -> [t]

flattenAndsOpt :: Opt
flattenAndsOpt = Opt
  { fn   = OA.modifyAssertions (return . concatMap flattenAnds)
  , name = "flattenAnds"
  }

flattenNaryOpt :: Opt
flattenNaryOpt = Opt { fn = flattenNary, name = "nary" }

opts :: Map.Map String Opt
opts = Map.fromList $ map
  (\o -> (name o, o))
  [ eqElimOpt
  , constantFoldOpt
  , constantFoldEqOpt
  , arrayElimOpt
  , flattenAndsOpt
  , memOpt
  , flattenNaryOpt
  ]


-- Optimize, ensuring that the variables in `p` continue to exist.
opt :: A.AssertState -> Log OA.AssertState
opt a = do
  let a' = OA.fromAssertState a
  optsToRun <- liftCfg $ asks (_smtOpts . _smtOptCfg)
  let optimize = do
        OA.logAssertions "smt::opt" "initial"
        forM_ optsToRun $ \oname -> do
          let o =
                fromMaybe (error $ "No optimization named: " ++ oname)
                  $ Map.lookup oname opts
          fn o
          OA.logAssertions "smt::opt" ("Post " ++ show oname)
          whenM
              (liftM2 (&&)
                      OA.isStoringValues
                      (liftCfg $ asks (_checkOpts . _smtOptCfg))
              )
            $ do
                logIf "smt::opt" "Checking system"
                r <- OA.check
                case r of
                  Left  m  -> error m
                  Right () -> return ()
                logIf "smt::opt" "Checked"
  OA.execAssert optimize a'

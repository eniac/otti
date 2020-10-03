{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.C.CToR1cs
  ( fnToR1cs
  , FnTrans
  )
where

-- This module glues the C circification pipeline to the Smt -> R1cs pipeline.

import           Control.Monad
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import qualified Language.C.Syntax.AST         as AST
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify                ( initValues
                                                , liftCircify
                                                )
import qualified Codegen.Circify               as Circify
import qualified IR.R1cs.Opt                   as R1csOpt
import           Codegen.C                      ( codegenFn
                                                , runC
                                                , assertBug
                                                )
import           Codegen.C.CUtils               ( InMap )
import           IR.SMT.ToPf                    ( toPf )
import qualified IR.SMT.Opt                    as SmtOpt
import           IR.R1cs                        ( R1CS(..)
                                                , r1csShow
                                                )
import           Data.Maybe                     ( isJust )
import           Data.Dynamic                   ( Dynamic )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import qualified Data.IntMap.Strict            as IntMap
import           GHC.TypeNats                   ( KnownNat )
import           Util.Log
import           Util.Cfg                       ( liftCfg )
import qualified Util.ShowMap                  as SMap

data FnTrans = FnTrans { assertions :: [Ty.TermBool]
                       , public :: [String]
                       , vals   :: Maybe (Map.Map String Dynamic)
                       , arraySizes :: SMap.ShowMap (Ty.TermArray Ty.DynBvSort Ty.DynBvSort) Int
                       }

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
fnToSmt :: Bool -> Maybe InMap -> AST.CTranslUnit -> String -> Log FnTrans
fnToSmt findBugs inVals tu name = do
  let init = when (isJust inVals) $ liftCircify initValues
  (((_, _), circState, memState), assertState) <-
    liftCfg $ Assert.runAssert $ runC inVals findBugs $ do
      init
      r <- codegenFn tu name
      when findBugs assertBug
      return r
  let public' = map snd $ Circify.inputs circState
  liftLog $ logIf "cToSmt" $ "Public: " ++ show public'
  return $ FnTrans { assertions = Assert.asserted assertState
                   , public     = public'
                   , vals       = Assert.vals assertState
                   , arraySizes = Mem.sizes memState
                   }

fnToR1cs
  :: forall n
   . KnownNat n
  => Bool -- ^ Find bugs?
  -> Maybe InMap -- ^ Optional input map
  -> AST.CTranslUnit -- ^ C file
  -> String -- ^ c function name
  -> Log (R1CS String n)
fnToR1cs findBugs inVals tu fnName = do
  fn <- fnToSmt findBugs inVals tu fnName
  let pubVars = Set.fromList $ if findBugs then [] else public fn
  logIf "inputs" $ "Public inputs: " ++ show pubVars
  newSmt <- SmtOpt.opt (arraySizes fn) pubVars (assertions fn)
  r      <- toPf @n (vals fn) pubVars (arraySizes fn) newSmt
  logIf "inputs" $ "R1cs: " ++ unlines
    (map (\(n, s) -> show n ++ ": " ++ show s) $ IntMap.toList $ numSigs r)
  R1csOpt.opt r

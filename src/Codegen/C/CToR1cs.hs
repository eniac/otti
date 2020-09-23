{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.C.CToR1cs
  ( fnToR1cs
  , fnToR1csWithWit
  , FnTrans
  )
where

-- This module glues the C circification pipeline to the Smt -> R1cs pipeline.

import           Control.Monad
import           Control.Monad.IO.Class
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import qualified Language.C.Syntax.AST         as AST
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify                ( evalCodegen
                                                , runCodegen
                                                , initValues
                                                , liftCircify
                                                )
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LC
                                                , QEQ
                                                )
import qualified IR.R1cs.Opt                   as R1csOpt
import           Codegen.C                      ( codegenFn
                                                , runC
                                                )
import           Codegen.C.CUtils               ( CTerm
                                                , InMap
                                                )
import           IR.SMT.ToPf                    ( toPf
                                                , toPfWithWit
                                                )
import qualified IR.SMT.Opt                    as SmtOpt
import           IR.R1cs                        ( R1CS(..)
                                                , r1csStats
                                                )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                , fromMaybe
                                                )
import           Data.Dynamic                   ( Dynamic )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           GHC.TypeNats                   ( KnownNat )
import           Data.Field.Galois              ( Prime
                                                , toP
                                                )
import           Util.Log
import           Util.Cfg                       ( liftCfg )
import qualified Util.ShowMap                  as SMap

data FnTrans = FnTrans { assertions :: [Ty.TermBool]
                       , inputs :: [String]
                       , output :: String
                       , vals   :: Maybe (Map.Map String Dynamic)
                       , arraySizes :: SMap.ShowMap (Ty.TermArray Ty.DynBvSort Ty.DynBvSort) Int
                       }

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
fnToSmt :: Maybe InMap -> AST.CTranslUnit -> String -> Log FnTrans
fnToSmt inVals tu name = do
  let init = when (isJust inVals) $ liftCircify initValues
  (((inputs, output), compState, memState), assertState) <-
    liftCfg $ Assert.runAssert $ runC False $ init >> codegenFn tu name inVals
  return $ FnTrans
    { assertions = Assert.asserted assertState
    , inputs     = inputs
    , output     = fromMaybe (error "No return value in fnToSmt") output
    , vals       = Assert.vals assertState
    , arraySizes = Mem.sizes memState
    }

fnToR1cs
  :: forall n . KnownNat n => AST.CTranslUnit -> String -> Log (R1CS String n)
fnToR1cs tu fnName = do
  fn <- fnToSmt Nothing tu fnName
  let pubVars = Set.insert (output fn) $ Set.fromList $ inputs fn
  newSmt <- SmtOpt.opt pubVars (assertions fn)
  r      <- toPf @n pubVars (arraySizes fn) newSmt
  R1csOpt.opt r

fnToR1csWithWit
  :: forall n
   . KnownNat n
  => InMap
  -> AST.CTranslUnit
  -> String
  -> Log (R1CS String n, Map.Map String (Prime n))
fnToR1csWithWit inVals tu fnName = do
  fn <- fnToSmt (Just inVals) tu fnName
  let vs = fromJust $ vals fn
  forM_ (assertions fn) $ \a -> do
    let v = Ty.eval vs a
    unless (Ty.ValBool True == v) $ error $ "eval " ++ show a ++ " gave False"
  let pubVars = Set.insert (output fn) $ Set.fromList $ inputs fn
  newSmt <- SmtOpt.opt pubVars (assertions fn)
  (r, w) <- toPfWithWit @n vs pubVars (arraySizes fn) newSmt
  r'     <- R1csOpt.opt r
  return (r', w)

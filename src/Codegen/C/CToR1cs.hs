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
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import qualified Language.C.Syntax.AST         as AST
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.Circify                ( initValues
                                                , liftCircify
                                                )
import qualified IR.R1cs.Opt                   as R1csOpt
import           Codegen.C                      ( codegenFn
                                                , runC
                                                , assertBug
                                                )
import           Codegen.C.CUtils               ( InMap )
import           IR.SMT.ToPf                    ( toPf
                                                , toPfWithWit
                                                )
import qualified IR.SMT.Opt                    as SmtOpt
import           IR.R1cs                        ( R1CS(..) )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                , fromMaybe
                                                )
import           Data.Dynamic                   ( Dynamic )
import qualified Data.Set                      as Set
import qualified Data.Map.Strict               as Map
import           GHC.TypeNats                   ( KnownNat )
import           Data.Field.Galois              ( Prime )
import           Util.Log
import           Util.Cfg                       ( liftCfg )
import qualified Util.ShowMap                  as SMap

data FnTrans = FnTrans { assertions :: [Ty.TermBool]
                       , inputs :: [String]
                       , output :: String
                       , vals   :: Maybe (Map.Map String Dynamic)
                       , arraySizes :: SMap.ShowMap (Ty.TermArray Ty.DynBvSort Ty.DynBvSort) Int
                       }

fnTransPublicIns :: FnTrans -> Set.Set String
fnTransPublicIns f = Set.fromList (output f : inputs f)

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
fnToSmt :: Bool -> Maybe InMap -> AST.CTranslUnit -> String -> Log FnTrans
fnToSmt findBugs inVals tu name = do
  let init = when (isJust inVals) $ liftCircify initValues
  (((inputs, output), _, memState), assertState) <-
    liftCfg $ Assert.runAssert $ runC findBugs $ do
      init
      r <- codegenFn tu name inVals
      when findBugs assertBug
      return r
  return $ FnTrans
    { assertions = Assert.asserted assertState
    , inputs     = inputs
    , output     = fromMaybe (error "No return value in fnToSmt") output
    , vals       = Assert.vals assertState
    , arraySizes = Mem.sizes memState
    }

fnToR1cs
  :: forall n
   . KnownNat n
  => Bool
  -> AST.CTranslUnit
  -> String
  -> Log (R1CS String n)
fnToR1cs findBugs tu fnName = do
  fn <- fnToSmt findBugs Nothing tu fnName
  let pubVars = if findBugs then Set.empty else fnTransPublicIns fn
  newSmt <- SmtOpt.opt (arraySizes fn) pubVars (assertions fn)
  r      <- toPf @n pubVars (arraySizes fn) newSmt
  R1csOpt.opt r

fnToR1csWithWit
  :: forall n
   . KnownNat n
  => Bool
  -> InMap
  -> AST.CTranslUnit
  -> String
  -> Log (R1CS String n, Map.Map String (Prime n))
fnToR1csWithWit findBugs inVals tu fnName = do
  fn <- fnToSmt findBugs (Just inVals) tu fnName
  let vs = fromJust $ vals fn
  forM_ (assertions fn) $ \a -> do
    let v = Ty.eval vs a
    logIf "fnToR1csWithWit" $ "Assert: " ++ show a
    unless (Ty.ValBool True == v) $ error $ "eval " ++ show a ++ " gave False"
  let pubVars = if findBugs then Set.empty else fnTransPublicIns fn
  newSmt <- SmtOpt.opt (arraySizes fn) pubVars (assertions fn)
  (r, w) <- toPfWithWit @n vs pubVars (arraySizes fn) newSmt
  r'     <- R1csOpt.opt r
  return (r', w)

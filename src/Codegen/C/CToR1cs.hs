{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.C.CToR1cs
  ( fnToR1cs
  , fnToR1csWithWit
  , FnTrans
  )
where

import Control.Monad
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.Assert                 as Assert
import qualified Language.C.Syntax.AST         as AST
import           Codegen.Circify                ( evalCodegen
                                                , runCodegen
                                                , initValues
                                                , values
                                                , liftCircify
                                                )
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LC
                                                , QEQ
                                                )
import qualified IR.R1cs.Opt                   as Opt
import           Codegen.C                      ( codegenFn, runC )
import           IR.SMT.ToPf                    ( toPf
                                                , toPfWithWit
                                                )
import           IR.SMT.Opt                     ( constantFold
                                                , eqElim
                                                )
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

data FnTrans = FnTrans { assertions :: [Ty.TermBool]
                       , inputs :: [String]
                       , output :: String
                       , vals   :: Maybe (Map.Map String Dynamic)
                       }

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
fnToSmt
  :: Maybe (Map.Map String Integer) -> AST.CTranslUnit -> String -> IO FnTrans
fnToSmt inVals tu name = do
  let init = when (isJust inVals) $ liftCircify initValues
  (((inputs, output), compState), assertState) <-
    Assert.runAssert $ runC False $ init >> codegenFn tu name inVals
  return $ FnTrans { assertions = Assert.asserted assertState
                   , inputs     = inputs
                   , output     = fromMaybe (error "No return value in fnToSmt") output
                   , vals       = values compState
                   }

fnToR1cs
  :: forall n
   . KnownNat n
  => Bool
  -> AST.CTranslUnit
  -> String
  -> IO (R1CS String n)
fnToR1cs opt tu fnName = do
  fn <- fnToSmt Nothing tu fnName
  let pubVars   = Set.insert (output fn) $ Set.fromList $ inputs fn
  let smtOptFn  = if opt then eqElim pubVars . map constantFold else id
  let r1csOptFn = if opt then Opt.opt else id
  -- TODO: Use R1CS for optimization
  r <- toPf @n pubVars $ smtOptFn $ assertions fn
  return $ r1csOptFn r

fnToR1csWithWit
  :: forall n
   . KnownNat n
  => Map.Map String Integer
  -> Bool
  -> AST.CTranslUnit
  -> String
  -> IO (R1CS String n, Map.Map String (Prime n))
fnToR1csWithWit inVals opt tu fnName = do
  fn <- fnToSmt (Just inVals) tu fnName
  let vs        = fromJust $ vals fn
  forM_ (assertions fn) $ \a -> do
    let v = Ty.eval vs a
    unless (Ty.ValBool True == v) $
      error $ "eval " ++ show a ++ " gave False"
  let pubVars = Set.insert (output fn) $ Set.fromList $ inputs fn
  let smtOptFn = if opt then eqElim pubVars . map constantFold else id
  let r1csOptFn = if opt then Opt.opt else id
  -- TODO: Use R1CS for optimization
  (r, w) <- toPfWithWit @n vs pubVars $ smtOptFn $ assertions fn
  return (r1csOptFn r, w)

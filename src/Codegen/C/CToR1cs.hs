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
import Control.Monad.IO.Class
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
import           IR.SMT.Opt                     ( opt
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
  :: Maybe (Map.Map String Integer) -> AST.CTranslUnit -> String -> Log FnTrans
fnToSmt inVals tu name = do
  let init = when (isJust inVals) $ liftCircify initValues
  (((inputs, output), compState), assertState) <-
    liftIO $ Assert.runAssert $ runC False $ init >> codegenFn tu name inVals
  return $ FnTrans { assertions = Assert.asserted assertState
                   , inputs     = inputs
                   , output     = fromMaybe (error "No return value in fnToSmt") output
                   , vals       = values compState
                   }

smtMaybeOpt :: Bool -> Set.Set String -> [Ty.TermBool] -> Log [Ty.TermBool]
smtMaybeOpt doOpt protect assertions =
  if doOpt then opt protect assertions
           else return assertions

r1csMaybeOpt :: KnownNat n => Bool -> R1CS String n -> Log (R1CS String n)
r1csMaybeOpt doOpt r1cs =
  if doOpt then Opt.opt r1cs
           else return r1cs

fnToR1cs
  :: forall n
   . KnownNat n
  => Bool
  -> AST.CTranslUnit
  -> String
  -> Log (R1CS String n)
fnToR1cs opt tu fnName = do
  fn <- fnToSmt Nothing tu fnName
  let pubVars   = Set.insert (output fn) $ Set.fromList $ inputs fn
  newSmt <- smtMaybeOpt opt pubVars $ assertions fn
  -- TODO: Use R1CS for optimization
  r <- toPf @n pubVars newSmt
  r1csMaybeOpt opt r

fnToR1csWithWit
  :: forall n
   . KnownNat n
  => Map.Map String Integer
  -> Bool
  -> AST.CTranslUnit
  -> String
  -> Log (R1CS String n, Map.Map String (Prime n))
fnToR1csWithWit inVals opt tu fnName = do
  fn <- fnToSmt (Just inVals) tu fnName
  let vs        = fromJust $ vals fn
  forM_ (assertions fn) $ \a -> do
    let v = Ty.eval vs a
    unless (Ty.ValBool True == v) $
      error $ "eval " ++ show a ++ " gave False"
  let pubVars = Set.insert (output fn) $ Set.fromList $ inputs fn
  newSmt <- smtMaybeOpt opt pubVars $ assertions fn
  -- TODO: Use R1CS for optimization
  (r, w) <- toPfWithWit @n vs pubVars newSmt
  r' <- r1csMaybeOpt opt r
  return (r', w)

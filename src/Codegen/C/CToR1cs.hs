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
import qualified IR.SMT.Opt.Assert             as OptAssert
import qualified Language.C.Syntax.AST         as AST
import qualified Codegen.Circify.Memory        as Mem
import qualified IR.R1cs.Opt                   as R1csOpt
import           Codegen.C                      ( runC
                                                , assertBug
                                                , genFn
                                                )
import           Codegen.C.Term                 ( InMap )
import           IR.SMT.ToPf                    ( toPf )
import qualified IR.SMT.Opt                    as SmtOpt
import           IR.R1cs                        ( R1CS(..) )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Maybe                     ( isJust )
import qualified Data.Set                      as Set
import           GHC.TypeNats                   ( KnownNat )
import           Util.Log
import           Util.Cfg                       ( liftCfg )
import qualified Util.ShowMap                  as SMap

data FnTrans = FnTrans { system :: Assert.AssertState
                       , arraySizes :: SMap.ShowMap (Ty.TermArray Ty.DynBvSort Ty.DynBvSort) Int
                       }

-- Can a fn exhibit undefined behavior?
-- Returns a string describing it, if so.
fnToSmt :: Bool -> Maybe InMap -> AST.CTranslUnit -> String -> Log FnTrans
fnToSmt findBugs inVals tu name = do
  (((), _, memState), assertState) <-
    liftCfg $ Assert.runAssert $ runC inVals findBugs $ do
      when (isJust inVals) $ Assert.liftAssert Assert.initValues
      genFn tu name
      when findBugs assertBug
  when (isJust inVals) $ case Assert.check assertState of
    Left  e -> error e
    Right _ -> return ()
  let public' = Set.toList $ Assert.public assertState
  logIf "cToSmt" $ "Public: " ++ show public'
  return $ FnTrans { system = assertState, arraySizes = Mem.sizes memState }

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
  let sys     = system fn
  let sys' = if findBugs then sys { Assert.public = Set.empty } else sys
  let pubVars = Assert.public sys'
  logIf "inputs" $ "Public inputs: " ++ show pubVars
  newSmt <- SmtOpt.opt (arraySizes fn) (system fn)
  r      <- toPf @n (OptAssert._vals newSmt)
                    pubVars
                    (arraySizes fn)
                    (OptAssert.listAssertions newSmt)
  logIf "r1csvars" $ "R1cs: " ++ unlines
    (map (\(n, s) -> show n ++ ": " ++ show s) $ IntMap.toList $ numSigs r)
  R1csOpt.opt r

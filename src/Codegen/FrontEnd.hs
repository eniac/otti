{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Codegen.FrontEnd
  ( FrontEndInputs(..)
  )
where

import qualified IR.SMT.Assert                 as Assert
import           Targets.R1cs.Main              ( R1CS(..) )
import qualified Targets.R1cs.Opt.Main         as R1csOpt
import qualified IR.SMT.Opt                    as SmtOpt
import qualified IR.SMT.Opt.Assert             as OptAssert
import qualified IR.SMT.ToPf                   as ToPf
import           GHC.TypeNats                   ( KnownNat
                                                , Nat
                                                )
import           Util.Log
import qualified Util.Cfg                      as Cfg


class FrontEndInputs i where
  compile :: i -> Assert.Assert ()
  compileToR1cs :: forall (n :: Nat). KnownNat n => i -> Log (R1CS String n)
  compileToR1cs i = do
    assert <- Cfg.liftCfg $ Assert.execAssert (compile i)
    newSmt <- SmtOpt.opt assert
    r      <- ToPf.toPf @n (OptAssert._vals newSmt)
                      (OptAssert._public newSmt)
                      (OptAssert._sizes newSmt)
                      (OptAssert.listAssertions newSmt)
    R1csOpt.opt r

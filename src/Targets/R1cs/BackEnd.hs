{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# OPTIONS_GHC -Wno-orphans     #-}
module Targets.R1cs.BackEnd
  ( R1CS(..)
  )
where

import           Targets.R1cs.Main              ( R1CS(..) )
import qualified Targets.R1cs.Opt.Main         as R1csOpt
import           Targets.BackEnd
import qualified IR.SMT.Opt                    as SmtOpt
import qualified IR.SMT.Opt.Assert             as OptAssert
import qualified IR.SMT.ToPf                   as ToPf
import           GHC.TypeNats                   ( KnownNat )

instance KnownNat n => BackEnd (R1CS String n) where
  target a = do
    newSmt <- SmtOpt.opt a
    r      <- ToPf.toPf @n (OptAssert._vals newSmt)
                      (OptAssert._public newSmt)
                      (OptAssert._sizes newSmt)
                      (OptAssert.listAssertions newSmt)
    R1csOpt.opt r

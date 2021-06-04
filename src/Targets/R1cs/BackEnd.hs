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

import           Control.Monad                  ( )
import           Control.Monad.State.Strict
import           Targets.R1cs.Main              ( R1CS(..) )
import qualified Targets.R1cs.Opt.Main         as R1csOpt
import           Targets.R1cs.Output            ( r1csShow )
import           Targets.BackEnd
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.ToPf                   as ToPf
import           GHC.TypeNats                   ( KnownNat )
import           Debug.Trace

instance KnownNat n => BackEnd (R1CS String n) where
  target a = do
    --    newSmt <- SmtOpt.opt a
    newSmt <- return a
    liftIO . putStrLn $ "====== Old R1cs"
    liftIO . putStrLn . show $ newSmt
    liftIO . putStrLn $ "====== Values"
    liftIO . putStrLn . show $ Assert.vals newSmt
    r <- ToPf.toPf @n (Assert.vals newSmt)
                      (Assert.public newSmt)
                      (Assert.arraySizes newSmt)
                      (Assert.listAssertions newSmt)
    liftIO . putStrLn $ "====== New R1cs"
    liftIO . putStrLn . r1csShow $ r
    trace ("R1csopt before " ++ show r) $ R1csOpt.opt r

module IR.SMT.Opt.Mem.NonObliv
  ( memPass
  )
where

import           Control.Monad.Reader           ( asks )
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           IR.SMT.TySmt
import qualified IR.SMT.Opt.Assert             as OA
import           IR.SMT.Opt.Assert              ( Assert )
import           IR.SMT.Opt.Mem.Util
import           IR.SMT.Opt.Mem.Benes           ( benesPass )
import           IR.SMT.Opt.Mem.LinearArray     ( linearizeArrays )
import qualified Util.Cfg                      as Cfg
import           Util.Log
import qualified Util.ShowMap                  as SMap

type NamesToSizes = Map String Int

getNamesAndSizes :: Assert NamesToSizes
getNamesAndSizes = do
  as <- gets OA.listAssertions
  execStateT (mapM (runMemReplacePass visit) as) Map.empty

 where
  visit = (defaultMemReplacePass :: MemReplacePass (StateT NamesToSizes Assert)
          ) { visitVar = visitVar'
            }

  visitVar' :: String -> Sort -> StateT NamesToSizes Assert ()
  visitVar' name sort = do
    s <- OA.liftAssert $ gets (SMap.lookup (Var name sort) . OA._sizes)
    forM_ s $ \size -> modify $ Map.insert name size

memPass :: Assert ()
memPass = do
  namesToSizes <- getNamesAndSizes
  logIf "smt::opt::mem" $ "Sizes: " ++ show namesToSizes
  benesThresh <- Cfg.liftCfg $ asks (Cfg._benesThresh . Cfg._smtOptCfg)
  logIf "smt::opt::mem" $ "Benes Threshold: " ++ show benesThresh
  let small = Map.keysSet $ Map.filter (<= benesThresh) namesToSizes
  logIf "smt::opt::mem" $ "Linearizing: " ++ show small
  -- Replaces the small arrays
  linearizeArrays small
  -- Replaces the remaining arrays
  benesPass


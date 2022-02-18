module Main where

import           SubCmd                         ( runCmd )
import           Options                        ( parseCmd )
import qualified Util.Cfg                      as Cfg
import           Util.Log                       ( evalLog )
import           System.Environment             ( getArgs )

main :: IO ()
main = do
  cfg  <- Cfg.setFromEnv Cfg.defaultCfgState
  args <- getArgs
  let (args', cfg') = Cfg.setFromArgs args cfg
  cmd <- parseCmd args'
  Cfg.evalCfg (evalLog $ runCmd cmd) cfg'

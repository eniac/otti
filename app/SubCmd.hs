{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
module SubCmd
  ( runCmd
  )
where

import qualified Codegen.C.Main                as C
import qualified Codegen.Circom.Linking        as Link
import           Codegen.FrontEnd               ( compile )
import           Codegen.LangVal                ( parseToMap
                                                , modelMapToExtMap
                                                )
import qualified Codegen.Zokrates.Main         as ZGen
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Field.Galois              ( toP
                                                , Prime
                                                )
import qualified Data.IntMap                   as IntMap
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy(..) )
import qualified IR.SMT.Assert                 as Assert
import           Options
import qualified Parser.C                      as CParse
import qualified Parser.Circom                 as CircomParse
import qualified Parser.Circom.Inputs          as CircomIParse
import qualified Parser.Zokrates               as ZokratesParse
import           System.Exit                    ( ExitCode(..)
                                                , exitFailure
                                                )
import           System.IO                      ( IOMode(..)
                                                , openFile
                                                )
import           System.Process                 ( readProcessWithExitCode )
import           Targets.BackEnd                ( target )
import           Targets.R1cs.BackEnd           ( )
import qualified Targets.R1cs.Main             as R1cs
import qualified Targets.R1cs.Opt.Main         as R1csOpt
import qualified Targets.SMT.Z3                as Z3
import           Util.Cfg                       ( liftCfg )
import           Util.Log

checkProcess :: FilePath -> [String] -> String -> IO ()
checkProcess pgm args input = do
  putStrLn $ "Running: " ++ pgm ++ " " ++ unwords args
  (code, stdout, stderr) <- readProcessWithExitCode pgm args input
  unless (code == ExitSuccess) $ do
    putStrLn "STDOUT"
    putStrLn stdout
    putStrLn "END STDOUT"
    putStrLn "STDERR"
    putStrLn stderr
    putStrLn "END STDERR"
    putStrLn $ pgm ++ " return code " ++ show code ++ ", see above."
    exitFailure

type Order
  = 21888242871839275222246405745257275088548364400416034343698204186575808495617

runProofAction
  :: (Ord s, Show s) => R1cs.R1CS s Order -> ProofOpts -> ProofAction -> IO ()
runProofAction r1cs o a = case a of
  EmitR1cs -> R1cs.writeToR1csFile (asJson o) r1cs (r1csPath o)
  Setup    -> do
    R1cs.writeToR1csFile False r1cs (r1csPath o)
    checkProcess (libPath o)
                 ["setup", "-V", vkPath o, "-P", pkPath o, "-C", r1csPath o]
                 ""
  Prove -> do
    case R1cs.r1csCheck r1cs of
      Right _ -> return ()
      Left  e -> putStrLn e >> exitFailure
    R1cs.r1csWriteAssignments r1cs (xPath o) (wPath o)
    runLibsnarkProve o

runLibsnarkProve :: ProofOpts -> IO ()
runLibsnarkProve o = checkProcess
  (libPath o)
  [ "prove"
  , "-V"
  , vkPath o
  , "-P"
  , pkPath o
  , "-x"
  , xPath o
  , "-w"
  , wPath o
  , "-p"
  , pfPath o
  ]
  ""

runVerify :: ProofOpts -> IO ()
runVerify o = checkProcess
  (libPath o)
  ["verify", "-V", vkPath o, "-x", xPath o, "-p", pfPath o]
  ""

runBackend :: BackEnd -> Assert.AssertState -> Log ()
runBackend b a = case b of
  Solve -> do
    satRes <- target a
    liftIO $ if Z3.sat satRes
      then do
        putStrLn "SAT"
        forM_ (Map.toList $ Z3.model satRes)
          $ \(k, v) -> putStrLn $ unwords [k, ":", show v]
      else putStrLn "UNSAT"
  Proof o act -> do
    r1cs <- target @(R1cs.R1CS String Order) a
    liftIO $ runProofAction r1cs o act

runFrontend :: (Maybe FilePath) -> FrontEnd -> Log Assert.AssertState
runFrontend inPath fe = do
  inMap <- forM inPath $ \i -> liftIO $ parseToMap <$> readFile i
  a     <- liftCfg $ case fe of
    C fn path bugs -> do
      tu <- liftIO $ CParse.parseC path
      return $ compile $ C.CInputs tu fn bugs inMap
    Zokrates fn path -> do
      ast <- liftIO $ ZokratesParse.loadFilesRecursively path
      return $ compile @(ZGen.ZokratesInputs Order) $ ZGen.ZokratesInputs
        fn
        path
        ast
        inMap
  liftCfg $ Assert.execAssert a

runCmd :: Cmd -> Log ()
runCmd c = case c of
  Composite mInPath fe be    -> runFrontend mInPath fe >>= runBackend be
  Verify o                   -> liftIO $ runVerify o
  CCheckProve fn path pfOpts -> do
    eqc    <- runFrontend Nothing $ C fn path True
    satRes <- target eqc
    if Z3.sat satRes
      then do
        let inMap = modelMapToExtMap $ Z3.model satRes
        tu   <- liftIO $ CParse.parseC path
        eqc' <- liftCfg $ Assert.execAssert $ compile $ C.CInputs tu
                                                                  fn
                                                                  True
                                                                  (Just inMap)
        runBackend (Proof pfOpts Prove) eqc'
      else liftIO $ do
        putStrLn "No bug found"
        exitFailure
  Circom path mInPath pfOpts pfAct -> do
    m    <- liftIO $ CircomParse.loadMain path
    r1cs <- Link.linkMain @Order m >>= R1csOpt.opt
    if pfAct == Prove
      then do
        inputFile     <- liftIO $ openFile (Maybe.fromJust mInPath) ReadMode
        inputsSignals <- liftIO
          $ CircomIParse.parseSignalsFromFile (Proxy @Order) inputFile
        allSignals <- Link.computeWitnesses (Proxy @Order) m inputsSignals
        let getOr m_ k =
              Maybe.fromMaybe (error $ "Missing sig: " ++ show k) $ m_ Map.!? k
        let getOrI m_ k =
              Maybe.fromMaybe (error $ "Missing sig num: " ++ show k)
                $         m_
                IntMap.!? k
        let lookupSignalVal :: Int -> Prime Order
            lookupSignalVal =
              toP . getOr allSignals . head . getOrI (Link.numSigs r1cs)
        liftIO $ R1cs.emitAssignment
          (map lookupSignalVal [2 .. (1 + Link.nPublicInputs r1cs)])
          (xPath pfOpts)
        liftIO $ R1cs.emitAssignment
          (map lookupSignalVal
               [(2 + Link.nPublicInputs r1cs) .. (Link.nextSigNum r1cs - 1)]
          )
          (wPath pfOpts)
        liftIO $ runLibsnarkProve pfOpts
      else liftIO $ runProofAction r1cs pfOpts pfAct

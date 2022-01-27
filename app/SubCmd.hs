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
                                                , stderr
                                                , hPutStr
                                                )
import           System.Process                 ( readProcessWithExitCode )
import           Targets.BackEnd                ( target )
import           Targets.R1cs.BackEnd           ( )
import           Targets.R1cs.Main              ( R1CS )
import qualified Targets.R1cs.Output           as R1cs
import qualified Targets.R1cs.Opt.Main         as R1csOpt
import qualified Targets.SMT.Z3                as Z3
import           Util.Cfg                       ( Cfg
                                                , liftCfg
                                                )
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
  = 7237005577332262213973186563042994240857116359379907606001950938285454250989

runProofAction
  :: (Ord s, Show s) => R1CS s Order -> ProofOpts -> ProofAction -> Cfg ()
runProofAction r1cs o a = case a of
  EmitR1cs -> R1cs.writeToR1csFile r1cs $ r1csPath o
  Setup    -> do
    R1cs.writeToR1csFile r1cs $ r1csPath o
    liftIO $ putStrLn $ "Done"
--      (libPath o)
--      ["setup", "-V", vkPath o, "-P", pkPath o, "-C", r1csPath o]
--      ""

  Prove -> do
    case R1cs.r1csCheck r1cs of
      Right _ -> return ()
      Left  e -> liftIO (putStrLn e >> exitFailure)
    R1cs.r1csWriteAssignments r1cs (xPath o) (wPath o)
    liftIO $ runLibsnarkProve o

runLibsnarkProve :: ProofOpts -> IO ()
runLibsnarkProve o = return ()

runVerify :: ProofOpts -> IO ()
runVerify o = return ()

runBackend :: BackEnd -> Assert.AssertState -> Log ()
runBackend b a = do
  logIf "basic" $ "Running backend..."
  case b of
    Solve -> do
      liftIO $ hPutStr stderr "Running Z3...\n"
      satRes <- target a
      liftIO $ if Z3.sat satRes
        then do
          let inputs = Assert.inputs a
          liftIO $ hPutStr stderr "SAT\n"
          forM_ (Map.toList $ modelMapToExtMap $ Z3.model satRes) $ \(k, v) ->
            forM_ (inputs Map.!? k) $ \kk -> putStrLn $ unwords [kk, show v]
        else putStrLn "UNSAT"
    Proof o act -> do
      r1cs <- target @(R1CS String Order) a
      liftCfg $ runProofAction r1cs o act

runFrontend :: Maybe FilePath -> FrontEnd -> Log Assert.AssertState
runFrontend inPath fe = do
  logIf "basic" $ "Parsing C file " ++ (show inPath)
  inMap <- forM inPath $ \i -> liftIO $ parseToMap <$> readFile i
  a     <- liftCfg $ case fe of
    C fn path bugs -> do
      tu <- liftCfg $ CParse.parseC path
      return $ compile $ C.CInputs tu fn bugs inMap
    Zokrates fn path -> do
      ast <- liftIO $ ZokratesParse.loadFilesRecursively path
      return $ compile @(ZGen.ZokratesInputs Order) $ ZGen.ZokratesInputs
        fn
        path
        ast
        inMap
  logIf "basic" $ "Building assertions " ++ (show inPath)
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
        tu   <- liftCfg $ CParse.parseC path
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
      else liftCfg $ runProofAction r1cs pfOpts pfAct

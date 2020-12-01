{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
module Codegen.CircomTest where
import           BenchUtils
import           Test.Tasty.HUnit
import qualified Codegen.Circom.Linking        as Link
import qualified Parser.Circom.Inputs          as Parse
import qualified Targets.R1cs.Main             as R1cs
import qualified Data.Map                      as Map
import qualified Data.IntMap                   as IntMap
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy(..) )
import           System.Directory
import           System.FilePath
import           System.IO.Temp
import           System.IO
import           Parser.Circom                 as Parser
import           Util.Cfg
import           Util.Log

type Order
  = 113890009193798365449144652900867294558768981710660728242748762258461992583217

checkR1cs :: FilePath -> Maybe Int -> BenchTest
checkR1cs circuitPath constraintCount = benchTestCase circuitPath $ do
  pgm      <- loadMain circuitPath
  r1cs     <- evalCfgDefault $ evalLog $ Link.linkMain @Order pgm
  tempDir  <- getTemporaryDirectory
  tempPath <- emptyTempFile tempDir "circom-test-check.ext"
  _        <- R1cs.writeToR1csFile False r1cs tempPath
  case constraintCount of
    Just n  -> length (Link.constraints r1cs) @?= n
    Nothing -> pure ()
  _ <- removeFile tempPath
  return ()

emitAssignment :: [Integer] -> FilePath -> IO ()
emitAssignment xs path = do
  handle <- openFile path WriteMode
  hPutStr handle
    $ concatMap (\i -> show i ++ "\n") (fromIntegral (length xs) : xs)
  hClose handle

checkWitComp :: FilePath -> Map.Map String Int -> BenchTest
checkWitComp circuitPath inputs = benchTestCase circuitPath $ do
  m <- loadMain circuitPath
  withSystemTempFile
    ("circom-test-" ++ takeFileName circuitPath ++ "-in.ext")
    (\inPath i -> withSystemTempFile
      ("circom-test-" ++ takeFileName circuitPath ++ "-w.ext")
      (\wPath w -> withSystemTempFile
        ("circom-test-" ++ takeFileName circuitPath ++ "-x.ext")
        (\xPath x -> do
          _ <- hClose w
          _ <- hClose x
          _ <- hPutStr
            i
            (unlines $ map (\(n, v) -> n ++ " " ++ show v) $ Map.toList inputs)
          _             <- hClose i
          inFile        <- openFile inPath ReadMode
          inputsSignals <- Parse.parseSignalsFromFile (Proxy @Order) inFile
          allSignals    <- evalCfgDefault $ evalLog $ Link.computeWitnesses
            (Proxy @Order)
            m
            inputsSignals
          r1cs <- evalCfgDefault $ evalLog $ Link.linkMain @Order m
          let
            getOr m_ k =
              Maybe.fromMaybe (error $ "Missing key: " ++ show k) $ m_ Map.!? k
          let getOrI m_ k =
                Maybe.fromMaybe (error $ "Missing sig num: " ++ show k)
                  $         m_
                  IntMap.!? k
          let lookupSignalVal =
                getOr allSignals . head . getOrI (Link.numSigs r1cs)
          emitAssignment
            (map lookupSignalVal [2 .. (1 + Link.nPublicInputs r1cs)])
            xPath
          emitAssignment
            (map lookupSignalVal
                 [(2 + Link.nPublicInputs r1cs) .. (Link.nextSigNum r1cs - 1)]
            )
            wPath
        )
      )
    )

circomGenTests :: BenchTest
circomGenTests = benchTestGroup
  "Circom"
  [ benchTestGroup
    "R1CS emission"
    [ checkR1cs "test/Code/Circom/bitify4.circom"         (Just 5)
    , checkR1cs "test/Code/Circom/inout.circom"           Nothing
    , checkR1cs "test/Code/Circom/bitify4-wrapper.circom" (Just 10)
    , checkR1cs "test/Code/Circom/multidim.circom"        Nothing
    , checkR1cs "test/Code/Circom/bitify-main.circom"     (Just 17)
    , checkR1cs "test/Code/Circom/poseidon.circom"        Nothing
    ]
  , benchTestGroup
    "Witness computation"
    [ checkWitComp "test/Code/Circom/bitify4.circom" (Map.fromList [("in", 13)])
    , checkWitComp "test/Code/Circom/bitify-main.circom"
                   (Map.fromList [("in", 34)])
    , checkWitComp "test/Code/Circom/poseidon.circom"
                   (Map.fromList [("inputs[0]", 0), ("inputs[1]", 1)])
    ]
  ]

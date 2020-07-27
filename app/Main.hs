{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module Main where

import           Codegen.C                  (transFn, checkFn, FnTrans(..))
import           Codegen.ToPf               (toPf)
import           Codegen.Opt                (constantFold, eqElim)
import qualified Codegen.Circom.Compilation as Comp
import qualified Codegen.Circom.CompTypes.WitComp
                                            as Wit
import qualified Codegen.Circom.CompTypes   as CompT
import qualified Codegen.Circom.Linking     as Link
import qualified Codegen.Circom.Opt         as Opt
import           Control.Monad              (forM_)
import qualified Data.Map                   as Map
import qualified Data.IntMap                as IntMap
import qualified Data.Maybe                 as Maybe
import qualified Data.Set                   as Set
import           Data.Proxy                 (Proxy(..))
import           Parser.C                   (parseC)
import           Parser.Circom              (loadMain)
import           System.Environment         (getArgs)
import           System.IO                  (openFile, hGetContents, hPutStr, IOMode(..), hClose)
import           System.Console.Docopt
import           System.Process


patterns :: Docopt
patterns = [docopt|
Usage:
  compiler emit-r1cs [options]
  compiler count-terms [options]
  compiler setup [options]
  compiler prove [options]
  compiler verify [options]
  compiler (-h | --help)
  compiler c-check <fn-name> <path>
  compiler c-r1cs <fn-name> <path>

Options:
  -h, --help         Display this message
  --circom <path>    Read the circom circuit from this path [default: main.circom]
  -C <path>          Write the R1CS circuit to this path [default: C]
  -V <path>          Write the verifier key to this path [default: vk]
  -P <path>          Write the prover key to this path [default: pk]
  -i <path>          Read all inputs from this path [default: i]
  -x <path>          Write the public input to this path [default: x]
  -w <path>          Write the the auxiliary input to this path [default: w]
  -p <path>          Write/Read the proof at this path [default: pf]
  --libsnark <path>  Location of the libsnark binary [default: libsnark-frontend/build/src/main]
  -o --opt           Optimize the circuit

Commands:
  prove            Run the prover
  verify           Run the verifier
  setup            Run the setup
  emit-r1cs        Emit R1CS
  count-terms      Compile at the fn-level only
  c-check          Check a C function using an SMT solver
  c-r1cs           Convert a C program to R1CS
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns


-- input parsing/de-parsing
parseInputs :: FilePath -> IO [Integer]
parseInputs path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let r = tail $ map read $ words contents
    return r

emitAssignment :: [Integer] -> FilePath -> IO ()
emitAssignment xs path = do
    handle <- openFile path WriteMode
    hPutStr handle $ concatMap (\i -> show i ++ "\n") (fromIntegral (length xs) : xs)
    hClose handle

-- libsnark functions
runSetup :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runSetup libsnarkPath circuitPath pkPath vkPath = do
    _ <- readProcess libsnarkPath ["setup", "-V", vkPath, "-P", pkPath, "-C", circuitPath] ""
    return ()

runProve :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runProve libsnarkPath pkPath vkPath xPath wPath pfPath = do
    callProcess libsnarkPath ["prove", "-V", vkPath, "-P", pkPath, "-x", xPath, "-w", wPath, "-p", pfPath]
    return ()

runVerify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runVerify libsnarkPath vkPath xPath pfPath  = do
    _ <- readProcess libsnarkPath ["verify", "-V", vkPath, "-x", xPath, "-p", pfPath] ""
    return ()

order :: Integer
order = 21888242871839275222246405745257275088548364400416034343698204186575808495617
type Order = 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- order = 17
-- type Order = 17
-- type OrderCtx = Ctx Order

cmdEmitR1cs :: Bool -> FilePath -> FilePath -> IO ()
cmdEmitR1cs opt circomPath r1csPath = do
    print "Loading circuit"
    m <- loadMain circomPath
    let optFn = if opt then Opt.opt else id
    let r1cs = optFn $ Link.linkMain @Order m
    putStrLn $ Link.r1csStats r1cs
    Link.writeToR1csFile r1cs r1csPath

cmdCountTerms :: FilePath -> IO ()
cmdCountTerms circomPath = do
    m <- loadMain circomPath
    let c = Comp.compMainWitCtx @Order m
    print $ count c + sum (Map.map count $ CompT.cache c)
    print c
    where
     count = Wit.nSmtNodes . CompT.baseCtx


cmdSetup :: Bool -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdSetup opt libsnarkPath circomPath r1csPath pkPath vkPath = do
    cmdEmitR1cs opt circomPath r1csPath
    print "Running libsnark"
    runSetup libsnarkPath r1csPath pkPath vkPath

cmdProve :: Bool -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdProve opt libsnarkPath pkPath vkPath inPath xPath wPath pfPath circomPath = do
    m <- loadMain circomPath
    inputsSignals <- Link.parseSignalsFromFile (Proxy @Order) inPath
    let allSignals = Link.computeWitnesses (Proxy @Order) m inputsSignals
    let optFn = if opt then Opt.opt else id
    let r1cs = optFn $ Link.linkMain @Order m
    let getOr m_ k = Maybe.fromMaybe (error $ "Missing sig: " ++ show k) $ m_ Map.!? k
    let getOrI m_ k = Maybe.fromMaybe (error $ "Missing sig num: " ++ show k) $ m_ IntMap.!? k
    let lookupSignalVal :: Int -> Integer = getOr allSignals . getOrI (Link.numSigs r1cs)
    emitAssignment (map lookupSignalVal [2..(1 + Link.nPublicInputs r1cs)]) xPath
    emitAssignment (map lookupSignalVal [(2 + Link.nPublicInputs r1cs)..(Link.nextSigNum r1cs - 1)]) wPath
    runProve libsnarkPath pkPath vkPath xPath wPath pfPath

cmdVerify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdVerify = runVerify

cmdCCheck :: String -> FilePath -> IO ()
cmdCCheck name path = do
  result <- parseC path
  case result of
    Right tu -> do
      r <- checkFn tu name
      case r of
        Just s -> putStrLn s
        Nothing -> pure ()
    Left p -> do
      putStrLn "Parse error"
      print p



cmdCR1cs :: String -> FilePath -> IO ()
cmdCR1cs name path = do
  result <- parseC path
  case result of
    Right tu -> do
      trans <- transFn tu name
      putStrLn "Assertions:"
      forM_ (assertions trans) $ \v -> do
        putStr "  "
        print v
      r <- toPf @Order $ assertions trans
      putStrLn $ "R1CS: " ++ show (length r)
      --forM_ (map constantFold assertions) print
      r' <- toPf @Order $ map constantFold $ assertions trans
      putStrLn $ "R1CS: " ++ show (length r')
      let ioVars = Set.insert (output trans) (Set.fromList $ inputs trans)
      putStrLn "Safe:"
      forM_ (Set.toList ioVars) $ \s -> putStr "  " >> putStrLn s
      r'' <- toPf @Order $ eqElim ioVars $ map constantFold $ assertions trans
      putStrLn $ "R1CS: " ++ show (length r'')
      forM_ (eqElim ioVars $ map constantFold $ assertions trans) print
    Left p -> do
      putStrLn "Parse error"
      print p

defaultR1cs :: String
defaultR1cs = "C"

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    let optimize = args `isPresent` longOption "opt"
    if args `isPresent` command "emit-r1cs" then do
        circomPath <- args `getArgOrExit` longOption "circom"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        cmdEmitR1cs optimize circomPath r1csPath
    else if args `isPresent` command "count-terms" then do
        circomPath <- args `getArgOrExit` longOption "circom"
        cmdCountTerms circomPath
    else if args `isPresent` command "setup" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        circomPath <- args `getArgOrExit` longOption "circom"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        pkPath <- args `getArgOrExit` shortOption 'P'
        vkPath <- args `getArgOrExit` shortOption 'V'
        cmdSetup optimize libsnarkPath circomPath r1csPath pkPath vkPath
    else if args `isPresent` command "prove" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        circomPath <- args `getArgOrExit` longOption "circom"
        pkPath <- args `getArgOrExit` shortOption 'P'
        vkPath <- args `getArgOrExit` shortOption 'V'
        inPath <- args `getArgOrExit` shortOption 'i'
        xPath <- args `getArgOrExit` shortOption 'x'
        wPath <- args `getArgOrExit` shortOption 'w'
        pfPath <- args `getArgOrExit` shortOption 'p'
        cmdProve optimize libsnarkPath pkPath vkPath inPath xPath wPath pfPath circomPath
    else if args `isPresent` command "verify" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        vkPath <- args `getArgOrExit` shortOption 'V'
        xPath <- args `getArgOrExit` shortOption 'x'
        pfPath <- args `getArgOrExit` shortOption 'p'
        cmdVerify libsnarkPath vkPath xPath pfPath
    else if args `isPresent` command "c-check" then do
        fnName <- args `getArgOrExit` argument "fn-name"
        path <- args `getArgOrExit` argument "path"
        cmdCCheck fnName path
    else if args `isPresent` command "c-r1cs" then do
        fnName <- args `getArgOrExit` argument "fn-name"
        path <- args `getArgOrExit` argument "path"
        cmdCR1cs fnName path
    else
        exitWithUsageMessage patterns "Missing command!"

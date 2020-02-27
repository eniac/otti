{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Codegen.Circom             (genMainCtx)
import           Codegen.Circom.Constraints (equalities)
import           Codegen.Circom.Context     (Ctx (..))
import           Codegen.Circom.Term        (Constraint)
import           Codegen.Circom.ToSmt       (ctxToSmt)
import           Codegen.Circom.TestSmt     (writeToR1csFile)
import           Data.Field.Galois          (Prime)
import           Parser.Circom              (loadMain)
import           Control.Monad              (when)
import           System.Environment         (getArgs)
import           System.Console.Docopt
import           System.Process


patterns :: Docopt
patterns = [docopt|
Usage:
  compiler print-smt [options]
  compiler emit-r1cs [options]
  compiler setup [options]
  compiler prove [options]
  compiler verify [options]
  compiler (-h | --help)

Options:
  -h, --help         Display this message
  --circom <path>    Write the verifier key to this path [default: main.circom]
  -C <path>          Write the verifier key to this path [default: C]
  -V <path>          Write the verifier key to this path [default: vk]
  -P <path>          Write the prover key to this path [default: pk]
  -x <path>          Read the primary input from this path [default: x]
  -w <path>          Read the auxiliary input from this path [default: w]
  -p <path>          Write/Read the proof at this path [default: pf]
  --libsnark <path>  Location of the libsnark binary

Commands:
  prove            Run the prover
  verify           Run the verifier
  setup            Run the setup
  print-smt        Print the smt
  emit-r1cs        Emit the R1CS
|]

getArgOrExit = getArgOrExitWith patterns


-- libsnark functions
runSetup :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runSetup libsnarkPath circuitPath pkPath vkPath = do
    readProcess libsnarkPath ["setup", "-V", vkPath, "-P", pkPath, "-C", circuitPath] ""
    return ()

runProve :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runProve libsnarkPath pkPath vkPath xPath wPath pfPath = do
    readProcess libsnarkPath ["prove", "-V", vkPath, "-P", pkPath, "-x", xPath, "-w", wPath, "-p", pfPath] ""
    return ()

runVerify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runVerify libsnarkPath vkPath xPath pfPath  = do
    readProcess libsnarkPath ["verify", "-V", vkPath, "-x", xPath, "-p", pfPath] ""
    return ()

order = 21888242871839275222246405745257275088548364400416034343698204186575808495617

type OrderCtx = Ctx 21888242871839275222246405745257275088548364400416034343698204186575808495617

-- Our commands
cmdPrintSmt :: FilePath -> IO ()
cmdPrintSmt circomPath = do
    m <- loadMain circomPath
    print (ctxToSmt (genMainCtx m order :: OrderCtx))

cmdEmitR1cs :: FilePath -> FilePath -> IO ()
cmdEmitR1cs circomPath r1csPath = do
    m <- loadMain circomPath
    writeToR1csFile (ctxToSmt (genMainCtx m order :: OrderCtx)) r1csPath

cmdSetup :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdSetup libsnarkPath circomPath r1csPath pkPath vkPath = do
    m <- loadMain circomPath
    writeToR1csFile (ctxToSmt (genMainCtx m order :: OrderCtx)) r1csPath
    runSetup libsnarkPath r1csPath pkPath vkPath

cmdProve :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdProve = runProve

cmdVerify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdVerify = runVerify

defaultR1cs = "C"

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    if args `isPresent` command "print-smt" then do
        circomPath <- args `getArgOrExit` longOption "circom"
        cmdPrintSmt circomPath
    else if args `isPresent` command "emit-r1cs" then do
        circomPath <- args `getArgOrExit` longOption "circom"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        cmdEmitR1cs circomPath r1csPath
    else if args `isPresent` command "setup" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        circomPath <- args `getArgOrExit` longOption "circom"
        r1csPath <- args `getArgOrExit` shortOption 'C'
        pkPath <- args `getArgOrExit` shortOption 'P'
        vkPath <- args `getArgOrExit` shortOption 'V'
        cmdSetup libsnarkPath circomPath r1csPath pkPath vkPath
    else if args `isPresent` command "prove" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        pkPath <- args `getArgOrExit` shortOption 'P'
        vkPath <- args `getArgOrExit` shortOption 'V'
        xPath <- args `getArgOrExit` shortOption 'x'
        wPath <- args `getArgOrExit` shortOption 'w'
        pfPath <- args `getArgOrExit` shortOption 'p'
        cmdProve libsnarkPath pkPath vkPath xPath wPath pfPath
    else if args `isPresent` command "verify" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        vkPath <- args `getArgOrExit` shortOption 'V'
        xPath <- args `getArgOrExit` shortOption 'x'
        pfPath <- args `getArgOrExit` shortOption 'p'
        cmdVerify libsnarkPath vkPath xPath pfPath
    else
        exitWithUsageMessage patterns "Missing command!"

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

import           Codegen.Circom             (genMainCtx)
import           Codegen.Circom.Constraints (equalities)
import           Codegen.Circom.Context     (Ctx (..))
import           Codegen.Circom.Term        (Constraint)
import           Codegen.Circom.ToSmt       (ctxToSmt)
import           Codegen.Circom.TestSmt     (writeToR1csFile, extendInputsToAssignment)
import           Data.Field.Galois          (Prime)
import           Data.Proxy                 (Proxy(..))
import           GHC.TypeNats
import           GHC.TypeLits.KnownNat
import qualified IR.TySmt                   as Smt
import           Parser.Circom              (loadMain)
import           Control.Monad              (when)
import           System.Environment         (getArgs)
import           System.IO                  (openFile, hGetContents, hPutStr, IOMode(..), hClose)
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

extendInput :: FilePath -> FilePath -> FilePath -> Smt.Term Smt.BoolSort -> IO ()
extendInput publicInputPath privateInputPath assignmentPath term = do
    publicInputs <- parseInputs publicInputPath
    privateInputs <- parseInputs privateInputPath
    let inputs = publicInputs ++ privateInputs
    let assignment = extendInputsToAssignment (Proxy :: Proxy Order) inputs term
    let witnessAssignment = drop (length publicInputs) assignment
    print witnessAssignment
    emitAssignment witnessAssignment assignmentPath

-- libsnark functions
runSetup :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runSetup libsnarkPath circuitPath pkPath vkPath = do
    readProcess libsnarkPath ["setup", "-V", vkPath, "-P", pkPath, "-C", circuitPath] ""
    return ()

runProve :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runProve libsnarkPath pkPath vkPath xPath wPath pfPath = do
    callProcess libsnarkPath ["prove", "-V", vkPath, "-P", pkPath, "-x", xPath, "-w", wPath, "-p", pfPath]
    return ()

runVerify :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
runVerify libsnarkPath vkPath xPath pfPath  = do
    readProcess libsnarkPath ["verify", "-V", vkPath, "-x", xPath, "-p", pfPath] ""
    return ()

order = 21888242871839275222246405745257275088548364400416034343698204186575808495617
type Order = 21888242871839275222246405745257275088548364400416034343698204186575808495617
type OrderCtx = Ctx Order

-- order = 17
-- type Order = 17
-- type OrderCtx = Ctx Order

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
    print "Loading circuit"
    m <- loadMain circomPath
    print "Generating main"
    let ctx = genMainCtx m order :: OrderCtx
    print $ "Constraints: " ++ show (length $ equalities $ constraints ctx) ++ ". Converting to smt..."
    let smt = ctxToSmt ctx
    print $ "Smt depth: " ++ show (Smt.depth smt) ++ ". Serializing smt..."
    writeToR1csFile smt r1csPath
    print "Running libsnark"
    runSetup libsnarkPath r1csPath pkPath vkPath

cmdProve :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
cmdProve libsnarkPath pkPath vkPath xPath wPath pfPath circomPath = do
    print ("order is: "  ++ show (natVal $ Proxy @Order))
    print ("order is: "  ++ show (natVal $ Proxy @(Log2 Order + 1)))
    m <- loadMain circomPath
    let term = ctxToSmt (genMainCtx m order :: OrderCtx)
    extendInput xPath wPath assignmentPath term
    runProve libsnarkPath pkPath vkPath xPath assignmentPath pfPath
  where
    assignmentPath = wPath ++ ".full"

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
        circomPath <- args `getArgOrExit` longOption "circom"
        pkPath <- args `getArgOrExit` shortOption 'P'
        vkPath <- args `getArgOrExit` shortOption 'V'
        xPath <- args `getArgOrExit` shortOption 'x'
        wPath <- args `getArgOrExit` shortOption 'w'
        pfPath <- args `getArgOrExit` shortOption 'p'
        cmdProve libsnarkPath pkPath vkPath xPath wPath pfPath circomPath
    else if args `isPresent` command "verify" then do
        libsnarkPath <- args `getArgOrExit` longOption "libsnark"
        vkPath <- args `getArgOrExit` shortOption 'V'
        xPath <- args `getArgOrExit` shortOption 'x'
        pfPath <- args `getArgOrExit` shortOption 'p'
        cmdVerify libsnarkPath vkPath xPath pfPath
    else
        exitWithUsageMessage patterns "Missing command!"

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


patterns :: Docopt
patterns = [docopt|
Usage:
  compiler [options] <path>
  compiler (-h | --help)

Options:
  -h, --help     Display this message
  --smt          Print the smt
  --r1cs <path>  Write the R1CS to this path
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    path <- args `getArgOrExit` argument "path"
    let smt = args `isPresent` longOption "smt"
    m <- loadMain path
    let order = 21888242871839275222246405745257275088548364400416034343698204186575808495617
    let r :: (Ctx 21888242871839275222246405745257275088548364400416034343698204186575808495617) = genMainCtx m order
    let cs = equalities (constraints r)
    print (length cs)
    let smt = ctxToSmt r
    when (args `isPresent` longOption "smt") $ print smt
    case args `getArg` longOption "r1cs" of
        Just r1cs -> writeToR1csFile order smt r1cs
        Nothing -> pure ()

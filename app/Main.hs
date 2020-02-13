{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Codegen.Circom             (genMainCtx)
import           Codegen.Circom.Constraints (equalities)
import           Codegen.Circom.Context     (Ctx (..))
import           Codegen.Circom.Term        (Constraint)
import           Data.Field.Galois          (Prime)
import           Parser.Circom              (loadMain)
import           System.Environment         (getArgs)
import           System.Console.Docopt

import           Data.Char                  (toUpper)


patterns :: Docopt
patterns = [docoptFile|
Usage:
  compiler [options] <file>
  compiler (-h | --help)

Options:
  -h, --help    Display this message
  --ast         Print the AST
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    path <- args `getArgOrExit` (argument "path")
    genPath path


genPath :: String -> IO ()
genPath path = do
    m <- loadMain path
    let r :: (Ctx (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617)) = genMainCtx m 7
    let cs = equalities (constraints r)
    print (length cs)
    print r
    return ()

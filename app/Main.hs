{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Codegen.Circom             (genMainCtx)
import           Codegen.Circom.Constraints (equalities)
import           Codegen.Circom.Context     (Ctx (..))
import           Codegen.Circom.Term        (Constraint)
import           Codegen.Circom.ToSmt       (ctxToSmt)
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
  -h, --help    Display this message
  --ctx         Print the context
|]

getArgOrExit = getArgOrExitWith patterns

main :: IO ()
main = do
    args <- parseArgsOrExit patterns =<< getArgs
    path <- args `getArgOrExit` argument "path"
    let ctx = args `isPresent` longOption "ctx"
    genPath ctx path


genPath :: Bool -> String -> IO ()
genPath ctx path = do
    m <- loadMain path
    let r :: (Ctx 21888242871839275222246405745257275088548364400416034343698204186575808495617) = genMainCtx m 21888242871839275222246405745257275088548364400416034343698204186575808495617
    let cs = equalities (constraints r)
    print (length cs)
    when ctx $ print $ ctxToSmt r
    return ()

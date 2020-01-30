{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Environment (getArgs)
import Parser.Circom (loadMain)
import Codegen.Circom (genMain)
import Codegen.Circom.Term (Constraint)
import Data.Field.Galois (Prime)

main :: IO ()
main = do
    args <- getArgs
    genPath (head args)


genPath :: String -> IO ()
genPath path = do
    m <- loadMain path
    let constraints :: [Constraint (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617)] = genMain m 7
    print(length constraints)
    return ()

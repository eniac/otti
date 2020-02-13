{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Codegen.Circom             (genMainCtx)
import           Codegen.Circom.Constraints (equalities)
import           Codegen.Circom.Context     (Ctx (..))
import           Codegen.Circom.Term        (Constraint)
import           Data.Field.Galois          (Prime)
import           Parser.Circom              (loadMain)
import           System.Environment         (getArgs)

main :: IO ()
main = do
    args <- getArgs
    genPath (head args)


genPath :: String -> IO ()
genPath path = do
    m <- loadMain path
    let r :: (Ctx (Prime 21888242871839275222246405745257275088548364400416034343698204186575808495617)) = genMainCtx m 7
    let cs = equalities (constraints r)
    print (length cs)
    print r
    return ()

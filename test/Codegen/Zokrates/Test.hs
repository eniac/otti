{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.Zokrates.Test
  ( zTests
  )
where
import           BenchUtils
import           Codegen.Zokrates.Main
import           Codegen.FrontEnd
import           Control.Monad
import           Data.Either                    ( isRight )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust )
import           Targets.R1cs.Main              ( r1csCheck )
import           IR.SMT.Assert                  ( AssertState(..)
                                                , asserted
                                                , execAssert
                                                , vals
                                                )
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.TySmt.Alg              as SAlg
import qualified Parser.Zokrates               as ZParse
import           Test.Tasty.HUnit
import           Util.Log
import           Util.Cfg                       ( evalCfgDefault )

type Order
  = 113890009193798365449144652900867294558768981710660728242748762258461992583217

bothTest :: String -> String -> FilePath -> M.Map String Integer -> BenchTest
bothTest name fnName path inMap = benchTestGroup
  name
  [ benchTestCase "SMT" $ do
    files <- ZParse.loadFilesRecursively path
    let inputs = ZokratesInputs @Order fnName path files (Just inMap)
    cs <-
      evalCfgDefault $ evalLog $ compileToR1cs @(ZokratesInputs Order) @Order
        inputs
    -- Check R1CS satisfaction
    let checkResult = r1csCheck cs
    isRight checkResult @? show checkResult
  , benchTestCase "R1CS" $ do
    files <- ZParse.loadFilesRecursively path
    let inputs = ZokratesInputs @Order fnName path files (Just inMap)
    assertState <- evalCfgDefault $ execAssert $ compile $ inputs
    let assertions = asserted assertState
    let env        = fromJust $ vals assertState
    forM_ assertions $ \a -> do
      unless (Ty.ValBool True == SAlg.eval env a)
        $  putStrLn
        $  "Unsat constraint: "
        ++ show a
      Ty.ValBool True @=? SAlg.eval env a
  ]

zTests = benchTestGroup
  "Zokrates"
  [ bothTest "sum" "main" "test/Code/Zokrates/sum.zok"
    $ M.fromList [("a", 1), ("b", 2), ("c", 3), ("d", 12312), ("e", 1)]
  , bothTest "sha t1" "main" "test/Code/Zokrates/sha_t1.zok" $ M.fromList
    [("e", 1), ("f", 2), ("g", 3), ("k", 12312), ("h", 1), ("k", 3), ("w", 0)]
  , bothTest "arrays" "main" "test/Code/Zokrates/idx.zok" $ M.fromList
    [("x.0", 1), ("x.1", 2), ("x.2", 3), ("x.3", 12312), ("x.4", 1)]
  , bothTest
      "mux3"
      "main"
      "test/Code/Zokrates/stdlib/utils/multiplexer/lookup3bitSigned.zok"
    $ M.fromList
        [ ("b.0", 1)
        , ("b.1", 0)
        , ("b.2", 1)
        , ("c.0", 1)
        , ("c.1", 2)
        , ("c.2", 3)
        , ("c.3", 4)
        ]
  ]

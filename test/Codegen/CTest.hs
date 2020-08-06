{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
module Codegen.CTest
  ( cTests
  )
where
import           AST.Simple
import           BenchUtils
import           Control.Monad
import           Codegen.C
import           Codegen.C.CompilerMonad
import           Data.Either                    ( isRight )
import qualified Data.Map                      as M
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import qualified Data.Set                      as Set
import           IR.SMT.Assert                  ( execAssert
                                                , AssertState(..)
                                                , runAssert
                                                , asserted
                                                )
import           IR.R1cs                        ( R1CS(..)
                                                , r1csShow
                                                , r1csCheck
                                                )
import           IR.SMT.ToPf                    ( toPfWithWit )
import qualified IR.SMT.TySmt                  as Ty
import           Parser.C
import           Test.Tasty.HUnit
import           Utils

type Order
  = 113890009193798365449144652900867294558768981710660728242748762258461992583217

cTests :: BenchTest
cTests = benchTestGroup
  "C codegen test"
  [toSmtTests, ubTests, satSmtCircuitTests, satR1csTests]


constraintCountTest :: String -> FilePath -> Int -> BenchTest
constraintCountTest name path constraints = benchTestCase name $ do
  tu         <- parseC path
  assertions <- execAssert $ evalCodegen True $ codegenAll tu
  constraints @=? length (asserted assertions)

toSmtTests = benchTestGroup
  "SMT conversion"
  [ constraintCountTest "basic"         "test/Code/C/add.c"     8
  , constraintCountTest "loop"          "test/Code/C/loop.c"    26
  , constraintCountTest "if"            "test/Code/C/if.c"      6
  , constraintCountTest "return"        "test/Code/C/init.c"    4
  , constraintCountTest "function call" "test/Code/C/fn_call.c" 13
  ]

ubCheckTest :: String -> String -> FilePath -> Bool -> BenchTest
ubCheckTest name fnName path undef = benchTestCase name $ do
  tu         <- parseC path
  assertions <- execAssert $ evalCodegen True $ codegenFn tu fnName Nothing
  r          <- Ty.evalZ3 $ Ty.BoolNaryExpr Ty.And (asserted assertions)
  undef @=? isJust r

ubTests = benchTestGroup
  "UB Checks"
  [ ubCheckTest "signed overflow" "inner" "test/Code/C/fn_call.c" True
  , ubCheckTest "signed overflow in fn" "outer" "test/Code/C/fn_call.c" True
  , ubCheckTest "unsigned overflow" "add" "test/Code/C/add_unsigned.c" False
  , ubCheckTest "constant in if" "sum" "test/Code/C/if.c" False
  , ubCheckTest "constant loop" "sum" "test/Code/C/loop.c" False
  ]

satSmtCircuitTest :: String -> String -> FilePath -> BenchTest
satSmtCircuitTest name fnName path = benchTestCase name $ do
  tu                       <- parseC path
  (compState, assertState) <-
    runAssert
    $  execCodegen False
    $  initValues
    >> setDefaultValueZero
    >> codegenFn tu fnName (Just M.empty) -- Provide an empty map to trigger initialization with default. ew.
  let assertions = asserted assertState
  let env        = fromJust $ values compState
  forM_ assertions $ \a -> Ty.ValBool True @=? Ty.eval env a

satSmtCircuitTests = benchTestGroup
  "SAT SMT Circuit checks"
  [ satSmtCircuitTest "constant in if" "sum" "test/Code/C/if.c"
  , satSmtCircuitTest "loop"           "sum" "test/Code/C/loop.c"
  , satSmtCircuitTest "sum"            "add" "test/Code/C/add_unsigned.c"
  ]

satR1csTest :: String -> String -> FilePath -> BenchTest
satR1csTest name fnName path = benchTestCase name $ do
  tu                       <- parseC path
  (compState, assertState) <-
    runAssert
    $  execCodegen False
    $  initValues
    >> setDefaultValueZero
    >> codegenFn tu fnName (Just M.empty) -- Provide an empty map to trigger initialization with default. ew.
  let assertions = asserted assertState
  let env        = fromJust $ values compState
  forM_ assertions $ \a -> Ty.ValBool True @=? Ty.eval env a
  (cs, wit) <- toPfWithWit @Order env Set.empty assertions
  -- Check R1CS satisfaction
  let checkResult = r1csCheck wit cs
  isRight checkResult @? show checkResult

satR1csTests = benchTestGroup
  "SAT R1cs checks"
  [ satR1csTest "constant in if" "sum" "test/Code/C/if.c"
  , satR1csTest "loop"           "sum" "test/Code/C/loop.c"
  , satR1csTest "sum"            "add" "test/Code/C/add_unsigned.c"
  ]

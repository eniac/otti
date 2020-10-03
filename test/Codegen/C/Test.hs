{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.C.Test
  ( cTests
  )
where
import           BenchUtils
import           Codegen.C
import           Control.Monad
import           Data.Either                    ( isRight )
import qualified Data.Map                      as M
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                )
import qualified Data.Set                      as Set
import           IR.R1cs                        ( r1csCheck )
import           IR.SMT.Assert                  ( AssertState(..)
                                                , asserted
                                                , execAssert
                                                , vals
                                                , liftAssert
                                                , initValues
                                                )
import           IR.SMT.ToPf                    ( toPf )
import qualified IR.SMT.TySmt                  as Ty
import           Parser.C
import           Test.Tasty.HUnit
import           Util.Log
import qualified Util.ShowMap                  as SMap
import           Util.Cfg                       ( evalCfgDefault )

type Order
  = 113890009193798365449144652900867294558768981710660728242748762258461992583217

cTests :: BenchTest
cTests = benchTestGroup
  "C codegen test"
  [toSmtTests, ubTests, satSmtCircuitTests, satR1csTests]


-- constraintCountTest :: String -> FilePath -> Int -> BenchTest
-- constraintCountTest name path constraints = benchTestCase name $ do
--   tu         <- parseC path
--   assertions <- evalCfgDefault $ execAssert $ evalC True $ codegenAll tu
--   unless (constraints == length (asserted assertions)) $ do
--     --forM_ (asserted assertions) $ \s -> putStrLn $ name ++ ": " ++ show s
--     return ()
--   constraints @=? length (asserted assertions)

toSmtTests = benchTestGroup "SMT conversion" []
  -- These tests area just too brittle.
  -- [ constraintCountTest "basic"         "test/Code/C/add.c"     9
  -- , constraintCountTest "loop"          "test/Code/C/loop.c"    36
  -- , constraintCountTest "if"            "test/Code/C/if.c"      7
  -- , constraintCountTest "return"        "test/Code/C/init.c"    4
  -- , constraintCountTest "function call" "test/Code/C/fn_call.c" 13
  -- ]

ubCheckTest :: String -> String -> FilePath -> Bool -> BenchTest
ubCheckTest name fnName path undef = benchTestCase name $ do
  tu <- parseC path
  r  <- evalCfgDefault $ checkFn tu fnName
  undef @=? isJust r

ubTests = benchTestGroup
  "UB Checks"
  [ ubCheckTest "signed overflow" "inner" "test/Code/C/fn_call.c" True
  , ubCheckTest "signed overflow in fn" "outer" "test/Code/C/fn_call.c" True
  , ubCheckTest "unsigned overflow" "add" "test/Code/C/add_unsigned.c" False
  , ubCheckTest "constant in if" "sum" "test/Code/C/if.c" False
  , ubCheckTest "constant loop" "sum" "test/Code/C/loop.c" False
  , ubCheckTest "left shift by neg" "lneg" "test/Code/C/shift.c" True
  , ubCheckTest "right shift by neg" "rneg" "test/Code/C/shift.c" True
  , ubCheckTest "right shift overflow" "lover" "test/Code/C/shift.c" True
  , ubCheckTest "right shift overflow" "rover" "test/Code/C/shift.c" True
  , ubCheckTest "left shift sign bit" "signoff" "test/Code/C/shift.c" True
  , ubCheckTest "right shift sign bit" "arith" "test/Code/C/shift.c" True
  , ubCheckTest "shift anything" "any" "test/Code/C/shift.c" True
  , ubCheckTest "shift anything unsigned" "usany" "test/Code/C/shift.c" True
  , ubCheckTest "undefined variable 1" "undef1" "test/Code/C/other.c" True
  , ubCheckTest "undefined variable 2" "undef2" "test/Code/C/other.c" True
  -- TODO: fails
  --, ubCheckTest "null deref" "null" "test/Code/C/other.c" True
  , ubCheckTest "out of bounds load" "oob_load" "test/Code/C/other.c" True
  , ubCheckTest "out of bounds store" "oob_store" "test/Code/C/other.c" True
  -- TODO: fails
  --, ubCheckTest "modify string" "str" "test/Code/C/other.c" True
  -- TODO: fails
  --, ubCheckTest "negate intmin" "neg" "test/Code/C/other.c" True
  , ubCheckTest "divide by zero" "divzero" "test/Code/C/other.c" True
  , ubCheckTest "mod by zero" "modzero" "test/Code/C/other.c" True
  , ubCheckTest "no bounds check" "trivial" "test/Code/C/array_bounds.c" True
  , ubCheckTest "bad bounds check" "very_easy" "test/Code/C/array_bounds.c" True
  , ubCheckTest "bad ashr bounds check" "easy" "test/Code/C/array_bounds.c" True
  , ubCheckTest "lshr bounds check"
                "easy_okay"
                "test/Code/C/array_bounds.c"
                False
  ]

satSmtCircuitTest
  :: String -> String -> FilePath -> M.Map String Integer -> BenchTest
satSmtCircuitTest name fnName path inputs = benchTestCase name $ do
  tu          <- parseC path
  assertState <- evalCfgDefault $ execAssert $ runC (Just inputs) False $ do
    liftAssert initValues
    genFn tu fnName
  let assertions = asserted assertState
  let env        = fromJust $ vals assertState
  forM_ assertions $ \a -> do
    unless (Ty.ValBool True == Ty.eval env a)
      $  putStrLn
      $  "Unsat constraint: "
      ++ show a
    Ty.ValBool True @=? Ty.eval env a

satSmtCircuitTests = benchTestGroup
  "SAT SMT Circuit checks"
  [ satSmtCircuitTest "constant in if"
                      "sum"
                      "test/Code/C/if.c"
                      (M.fromList [("x", 4)])
  , satSmtCircuitTest "constant in if (SMT var set)"
                      "sum"
                      "test/Code/C/if.c"
                      (M.fromList [("f0_foo_lex0__x_v0", 0)])
  , satSmtCircuitTest "ch"
                      "ch"
                      "test/Code/C/sha.c"
                      (M.fromList [("x", 17), ("y", 3), ("z", 4)])
  , satSmtCircuitTest "loop" "sum" "test/Code/C/loop.c" (M.fromList [])
  , satSmtCircuitTest "sum"
                      "add"
                      "test/Code/C/add_unsigned.c"
                      (M.fromList [("x", 1), ("y", 15)])
  ]

satR1csTestInputs
  :: String -> String -> FilePath -> M.Map String Integer -> BenchTest
satR1csTestInputs name fnName path inputs = benchTestCase name $ do
  tu          <- parseC path
  assertState <- evalCfgDefault $ execAssert $ runC (Just inputs) False $ genFn
    tu
    fnName
  let assertions = asserted assertState
  let env        = fromJust $ vals assertState
  forM_ assertions $ \a -> Ty.ValBool True @=? Ty.eval env a
  cs <- evalCfgDefault $ evalLog $ toPf @Order (Just env)
                                               Set.empty
                                               SMap.empty
                                               assertions
  -- Check R1CS satisfaction
  let checkResult = r1csCheck cs
  isRight checkResult @? show checkResult

satR1csTests = benchTestGroup
  "SAT R1cs checks"
  [ satR1csTestInputs "constant in if"
                      "sum"
                      "test/Code/C/if.c"
                      (M.fromList [("x", 4)])
  , satR1csTestInputs "skip if" "sum" "test/Code/C/if.c" (M.fromList [("x", 5)])
  , satR1csTestInputs "loop"    "sum" "test/Code/C/loop.c" (M.fromList [])
  , satR1csTestInputs "add"
                      "add"
                      "test/Code/C/add_unsigned.c"
                      (M.fromList [("x", 1), ("y", 15)])
  , satR1csTestInputs "shifts"
                      "shift_it"
                      "test/Code/C/shifts.c"
                      (M.fromList [("x", 17), ("y", 3)])
  , satR1csTestInputs "fn calls"
                      "outer"
                      "test/Code/C/fn_call.c"
                      (M.fromList [("x", 17), ("y", 3)])
  , satR1csTestInputs "majority"
                      "maj"
                      "test/Code/C/sha.c"
                      (M.fromList [("x", 17), ("y", 3), ("z", 4)])
  , satR1csTestInputs "ch"
                      "ch"
                      "test/Code/C/sha.c"
                      (M.fromList [("x", 17), ("y", 3), ("z", 4)])
  -- Set inputs specially because rotation is not SAT for 0 length (the
  -- default)
  , satR1csTestInputs "rot left"
                      "rotateleft"
                      "test/Code/C/sha.c"
                      (M.fromList [("x", 17), ("n", 30)])
  -- Set inputs specially because rotation is not SAT for 0 length (the
  -- default)
  , satR1csTestInputs "rot right"
                      "rotateright"
                      "test/Code/C/sha.c"
                      (M.fromList [("x", 14), ("n", 1)])
  , satR1csTestInputs "struct input"
                      "foo"
                      "test/Code/C/struct_inputs.c"
                      (M.fromList [("p.x", 14), ("p.y", 1)])
  , satR1csTestInputs
    "nested struct input"
    "nested"
    "test/Code/C/struct_inputs.c"
    (M.fromList [("t.p.x", 14), ("t.p.y", 13), ("t.q.x", 12), ("t.q.y", 11)])
  ]

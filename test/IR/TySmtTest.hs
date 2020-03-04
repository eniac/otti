{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module IR.TySmtTest where
import           AST.Circom
import           BenchUtils
import           Codegen.Circom
import           Codegen.Circom.Term
import           Codegen.Circom.Constraints (empty)
import           Codegen.Circom.Constraints as Constraints
import           Codegen.Circom.Context
import           Codegen.Circom.ToSmt       (constraintsToSmt)
import           Control.Monad              (unless)
import qualified Data.BitVector as Bv
import           Data.Dynamic
import           Data.Either                (fromLeft, isRight)
import           Data.Field.Galois          (Prime, PrimeField, toP)
import qualified Data.Map.Strict            as Map
import           GHC.TypeLits               
import           Data.Proxy                 (Proxy(..))
import           IR.TySmt                   (depth)
import qualified IR.TySmt                   as Smt
import           Parser.Circom              as Parser
import           Utils

tySmtTests :: BenchTest
tySmtTests = benchTestGroup "Typed SMT Tests"
    [ genEvalTest
        "boolean literal, empty context"
        Map.empty
        (Smt.BoolLit True)
        (Smt.ValBool True)
    , genEvalTest
        "integer expression, with bound variable"
        (Map.fromList [("a", toDyn (Smt.ValInt 4))])
        (Smt.IntBinExpr Smt.IntSub (Smt.Var "a") (Smt.IntLit 1))
        (Smt.ValInt 3)
    , genEvalTest
        "field expression, with bound variables"
        (Map.fromList [("a", toDyn (Smt.ValPf @5 4)), ("b", toDyn (Smt.ValPf @5 3))])
        (Smt.PfNaryExpr Smt.PfMul [(Smt.Var "a"), (Smt.IntToPf @5 (Smt.IntLit 3)), (Smt.Var "b")])
        (Smt.ValPf @5 1)
    , genEvalTest
        "bv expression"
        (Map.empty)
        (Smt.BvBinExpr Smt.BvAnd (Smt.IntToBv @4 (Smt.IntLit 9)) (Smt.IntToBv @4 (Smt.IntLit 10)))
        (Smt.ValBv @4 (Bv.bitVec 4 8))
    ]

type Env = Map.Map String Dynamic

genEvalTest :: (Typeable s) => String -> Env -> Smt.Term s -> Smt.Value s -> BenchTest
genEvalTest name ctx t v' = benchTestCase ("eval test: " ++ name) $ do
    let v = Smt.eval ctx t
    unless (v == v') $
        error $ "Expected\n\t" ++ show t ++ "\nto evaluate to\n\t" ++ show v' ++ "\nbut it evaluated to\n\t" ++ show v ++ "\n"
    return ()


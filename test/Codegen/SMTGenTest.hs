module Codegen.SMTGenTest where
import           AST.Simple
import           BenchUtils
import           Codegen.CompilerMonad
import           Codegen.SMTGen
import qualified Data.Map              as M
import           IR.SMT                (initMem)
import           Utils

codegenTests :: BenchTest
codegenTests = benchTestGroup "Codegen tests" [ binOpTest
                                              , ifTest
                                              , callTest
                                              , structTest
                                              , memTest
                                              ]

-- Fix so that declared but not defined variables have undef bit set
binOpTest :: BenchTest
binOpTest = benchTestCase "bin op" $ do

  r <- evalCodegen Nothing $ do
    let result = Var U8 "result"
        one = NumExpr $ INum U8 1
        three = NumExpr $ INum U8 3
        body = [ Decl result
               , Assign result $ Add one three
               , Assign result $ Add three three
               ]
        fun = Function "fun" U8 [] body
    genFunctionSMT fun
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_1", 4)
                       , ("result_1_undef", 0)
                       , ("result_2", 6)
                       , ("result_2_undef", 0)
                       ]

-- Undefined variables should have undef bit set to 1
ifTest :: BenchTest
ifTest = benchTestCase "if" $ do

  r <- evalCodegen Nothing $ do
    let result = Var U8 "result"
        undefResult = Var U8 "undef"
        one = NumExpr $ INum U8 1
        three = NumExpr $ INum U8 3
        body = [ Decl result
               , If (Eq one three) [Assign result three] [Assign result one]
               , If (Eq three three) [Assign result three] [Assign result one]
               , Decl undefResult
               -- undef should be undef
               , If (Eq one three) [Assign undefResult three] []
               -- undef should no longer be undef
               , If (Eq one one) [If (Eq three three) [Assign undefResult three] [] ] []
               ]
    genBodySMT body
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_2", 1)
                       , ("result_3", 3)
                       , ("result_4", 3)
                       , ("undef_2", 3)
                       , ("undef_2_undef", 0)
--                       , ("undef_1_undef", 1)
                       ]


callTest :: BenchTest
callTest = benchTestCase "call" $ do

  r <- evalCodegen Nothing $ do
    let input = VarExpr $ Var U8 "input"
        one = NumExpr $ INum U8 1
        body = [ Return $ Add input one ]
        addOne = Function "addOne" U8 [("input", U8)] body

        two = NumExpr $ INum U8 2
        three = NumExpr $ INum U8 3
        result = Var U8 "result"
        distractor = Var U8 "distractor"
        body2 = [ Decl result
                , Decl distractor
                , Assign result $ Call "addOne" [two]
                , Assign distractor $ Call "addOne" [three]
                , Assign distractor $ Call "addOne" [VarExpr distractor]
                , Return $ VarExpr result
                ]
        funThree = Function "three" U8 [] body2
    registerFunction addOne
    genFunctionSMT funThree
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_1", 3)
                       , ("distractor_1", 4)
                       , ("distractor_2", 5)
                       , ("input_1", 2)
                       , ("input_2", 3)
                       , ("addOne_retVal_2", 3)
                       , ("addOne_retVal_3", 4)
                       , ("three_retVal_1", 3)
                       ]

structTest :: BenchTest
structTest = benchTestCase "structs" $ do

  r <- evalCodegen Nothing $ do
    let structType = Struct [U8, U8, U8]
        two = NumExpr $ INum U8 2
        three = NumExpr $ INum U8 3
        four = NumExpr $ INum U8 4
        zero = NumExpr $ INum U32 0
        -- 00000010 | 00000011 | 00000100 aka 131842
        struct = StructExpr $ StructLit structType [two, three, four]
        structVar = Var structType "struct"
        zeroVar = Var U8 "elemZero"
        oneVar = Var U8 "elemOne"
        -- pointer to struct
        pointerType = Ptr32 structType
        pointerVar = Var pointerType "pointer"
        -- struct with a pointer in it
        structPtrType = Struct [Ptr32 structType]
        structP = StructExpr $ StructLit structPtrType [VarExpr pointerVar]
        ptrTy = Ptr32 structPtrType
        ptrVar = Var ptrTy "sptr"
        two32 = NumExpr $ INum U32 2
        threeVar = Var U8 "elemThree"
        -- nested struct
        nstructType = Struct [Struct [U8, U8]]
        nstruct = StructExpr $ StructLit nstructType
                  [StructExpr $ StructLit (Struct [U8, U8])[two, four]]
        nvar = Var U8 "nvar"
        body = [ Decl structVar
               , Decl zeroVar
               , Assign structVar struct
               , Assign zeroVar $ Access (VarExpr structVar) 0
               , Decl pointerVar
               , Assign pointerVar zero
               , Store (VarExpr pointerVar) (VarExpr structVar)
               , Decl oneVar
               , Assign oneVar $ PtrAccess (VarExpr pointerVar) 1
                 -- Trying nested pointers
               , Decl ptrVar
               , Assign ptrVar two32
               , Store (VarExpr ptrVar) structP
               , Decl threeVar
               , Assign threeVar $ PtrAccess (PtrAccess (VarExpr ptrVar) 0) 2
                 -- Nested structs
               , Decl nvar
               , Assign nvar $ Access (Access nstruct 0) 1
               ]
    liftIR initMem
    genBodySMT body
    runSolverOnSMT

  vtest r $ M.fromList [ ("struct_1", 131844)
                       , ("elemZero_1", 2)
                       , ("elemOne_1", 3)
                       , ("elemThree_1", 4)
                       , ("nvar_1", 4)
                       ]

memTest :: BenchTest
memTest = benchTestCase "memory" $ do

  r <- evalCodegen Nothing $ do
    let structType = Struct [U8, U8, U8]
        pointerType = Ptr32 structType
        one = NumExpr $ INum U8 1
        two = NumExpr $ INum U8 2
        three = NumExpr $ INum U8 3
        structLit = StructExpr $ StructLit structType [one, two, three]
        pointerNum = NumExpr $ INum U32 0
        pointerVar = Var pointerType "pointer"
        resultVar = Var U8 "result"
        -- nested struct
        struct2Type = Struct [U8, Struct [U8]]
        pointer2Type = Ptr32 struct2Type
        number = NumExpr $ INum U8 50
        nestedLit = StructExpr $ StructLit (Struct [U8]) [number]
        struct2Lit = StructExpr $ StructLit struct2Type [number, nestedLit]
        pointer2Num = NumExpr $ INum U32 10
        pointer2Var = Var pointer2Type "pointer2"
        result2Var = Var U8 "result2"
        body = [ Decl pointerVar
               , Decl resultVar
               , Assign pointerVar pointerNum
               , Store (VarExpr pointerVar) structLit
               , Assign resultVar $ PtrAccess (VarExpr pointerVar) 1
               , Store (PtrAccess (VarExpr pointerVar) 1) three
               , Assign resultVar $ PtrAccess (VarExpr pointerVar) 1
               , Decl pointer2Var
               , Decl result2Var
               , Assign pointer2Var pointer2Num
               , Store (Access (PtrAccess (VarExpr pointer2Var) 1) 0) number
               , Assign result2Var $ Access (Access (Load $ VarExpr pointer2Var) 1) 0
               ]
    liftIR $ initMem
    genBodySMT body
    runSolverOnSMT

  vtest r $ M.fromList [ ("result_1", 2)
                       , ("result_2", 3)
                       , ("result2_1", 50)
                       ]

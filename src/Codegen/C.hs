module Codegen.C where
import           Codegen.CompilerMonad
import           Control.Monad.State.Strict      (forM, forM_, liftIO, unless,
                                                  void, when)
import           IR.SMT
import           Language.C.Analysis.AstAnalysis
import           Language.C.Data.Ident
import           Language.C.Syntax.AST
import           Language.C.Syntax.Constants

fieldToInt :: Ident -> Int
fieldToInt = undefined

genVarSMT :: Ident -> Compiler SMTNode
genVarSMT (Ident name _ _) = getNodeFor name

genNumSMT :: CConstant a -> Compiler SMTNode
genNumSMT c = case c of
  CIntConst (CInteger i _ _) _ -> error ""
  CCharConst (CChar c _) _     -> error ""
  CCharConst (CChars c _) _    -> error ""
  CFloatConst (CFloat str) _   -> error ""
  CStrConst (CString str _) _  -> error ""

genExprSMT :: CExpr -> Compiler SMTNode
genExprSMT expr = case expr of
  CVar id _               -> genVarSMT id
  CConst c                -> genNumSMT c
  CAssign op lhs rhs _    -> do
    lhs' <- genExprSMT lhs
    rhs' <- genExprSMT rhs
    getAssignOp op lhs' rhs'
  CBinary op left right _ -> do
    left' <- genExprSMT left
    right' <- genExprSMT right
    getBinOp op left' right'
  CUnary op arg _ -> genExprSMT arg >>= getUnaryOp op
  CIndex arr index _ -> do
    arr' <- genExprSMT arr
    index' <- genExprSMT index
    liftIR $ getIdx arr' index'
  CMember struct field _ _ -> do
    struct' <- genExprSMT struct
    liftIR $ getField struct' $ fieldToInt field
  _                       -> error $ unwords [ "We do not support"
                                             , show expr
                                             , "right now"
                                             ]

getUnaryOp :: CUnaryOp -> SMTNode -> Compiler SMTNode
getUnaryOp op arg = liftIR $ case op of
  CPreIncOp -> error ""
    -- one <- bvNumOfWidth arg 1
    -- cppAdd one arg >>= smtAssign arg
    -- return arg
  -- cpredecop ->
  -- CPostIncOp ->
  -- CPostDecOp ->
  -- CAdrOp ->
  -- The '*' operation
  CIndOp    -> smtLoad arg
  CPlusOp   -> error $ unwords $ ["Do not understand:", show op]
  CMinOp    -> error $ unwords $ ["Do not understand:", show op]
  -- One's complement: NOT CORRECT
  CCompOp   -> cppNeg arg
  -- Logical negation: NOT CORRECT
  CNegOp    -> cppNeg arg

getBinOp :: CBinaryOp -> SMTNode -> SMTNode -> Compiler SMTNode
getBinOp op left right = liftIR $ case op of
  CMulOp -> cppMul left right
  -- CDivOp
  -- CRmdOp
  CAddOp -> cppAdd left right
  CSubOp -> cppSub left right
  CShlOp -> cppShiftLeft left right
  CShrOp -> cppShiftRight left right
  CLeOp  -> cppLt left right
  CGrOp  -> cppGt left right
  CLeqOp -> cppLte left right
  CGeqOp -> cppGte left right
  CEqOp  -> cppEq left right
  CNeqOp -> cppEq left right >>= cppNeg
  CAndOp -> cppAnd left right
  CXorOp -> cppXor left right
  COrOp  -> cppOr left right
  -- CLndOp
  -- CLorOp

-- | Assign operation
-- eg x += 1
-- aka x = x + 1
getAssignOp :: CAssignOp -> SMTNode -> SMTNode -> Compiler SMTNode
getAssignOp op l r = case op of
  CAssignOp -> liftIR $ smtAssign l r >> return l
  CMulAssOp -> liftIR $ do
    result <- cppMul l r
    smtAssign l result >> return l
  -- CDivAssOp -> liftIR $ do
  --   result <- cppDiv l r
  --   smtAssign l result >> return l
  -- CRmdAssOp
  CAddAssOp -> liftIR $ do
    result <- cppAdd l r
    smtAssign l result >> return l
  CSubAssOp -> liftIR $ do
    result <- cppSub l r
    smtAssign l result >> return l
  CShlAssOp -> liftIR $ do
    result <- cppShiftLeft l r
    smtAssign l result >> return l
  CShrAssOp -> liftIR $ do
    result <- cppShiftRight l r
    smtAssign l result >> return l
  CAndAssOp -> liftIR $ do
    result <- cppAnd l r
    smtAssign l result >> return l
  CXorAssOp -> liftIR $ do
    result <- cppXor l r
    smtAssign l result >> return l
  COrAssOp -> liftIR $ do
    result <- cppOr l r
    smtAssign l result >> return l

---
--- Statements
---

genStmtSMT :: CStatement a -> Compiler SMTNode
genStmtSMT stmt = case stmt of
  CCompound{} -> error ""
  CExpr{}     -> error ""
  CIf{}       -> error ""
  CWhile{}    -> error ""
  CFor{}      -> error ""
  _           -> error ""

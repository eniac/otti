module Codegen.SMTGen ( genVarSMT
                      , genNumSMT
                      , genExprSMT
                      , genStmtSMT
                      , genBodySMT
                      , genFunctionSMT
                      ) where
import           AST.Simple
import           Codegen.CompilerMonad
import           Control.Monad.State.Strict (forM, forM_, unless, void)
import           IR.SMT
import           Prelude                    hiding (Num)

{-|

Module that generates SMT for the Simple AST defined in AST.Simple.
Codegen from AST to a circuit consists of the following challenges:
1. SSA all variables
2. Inline all function calls, loops, if statements, etc

-}

genVarSMT :: Var -> Compiler SMTNode
genVarSMT var =
  -- suboptimal
  case var of
    Var{} -> getNodeFor $ varName var
    StructAccess var int -> do
      varSMT <- genVarSMT var
      liftIR $ getField varSMT int
    StructPtrAccess var int -> do
      varSMT <- genVarSMT var
      loadedSMT <- liftIR $ smtLoad varSMT
      liftIR $ getField loadedSMT int
    ArrayAccess var expr -> do
      varSMT <- genVarSMT var
      exprSMT <- genExprSMT expr
      liftIR $ getIdx varSMT exprSMT
    ArrayPtrAccess var expr -> do
      varSMT <- genVarSMT var
      loadedSMT <- liftIR $ smtLoad varSMT
      exprSMT <- genExprSMT expr
      liftIR $ getIdx loadedSMT exprSMT

genNumSMT :: Num -> Compiler SMTNode
genNumSMT num = case num of
                  INum ty _   | isDouble ty -> error "Cannot make int with double val"
                  INum ty val -> liftIR $ newInt ty val
                  FNum ty _   | not $ isDouble ty -> error "Cannot make double with int val"
                  FNum ty val -> liftIR $ newDouble ty val

genExprSMT :: Expr -> Compiler SMTNode
genExprSMT expr =
  case expr of
    VarExpr v  -> genVarSMT v
    NumExpr n  -> genNumSMT n
    Neg n      -> genExprSMT n >>= liftIR . cppNeg
    Not n      -> genExprSMT n >>= liftIR . cppBitwiseNeg
    Eq a b     -> genBinOpSMT a b cppEq
    NEq a b    -> genBinOpSMT a b cppEq >>= liftIR . cppBitwiseNeg
    And a b    -> genBinOpSMT a b cppAnd
    Add a b    -> genBinOpSMT a b cppAdd
    Sub a b    -> genBinOpSMT a b cppSub
    Mul a b    -> genBinOpSMT a b cppMul
    Or a b     -> genBinOpSMT a b cppOr
    XOr a b    -> genBinOpSMT a b cppXor
    Min a b    -> genBinOpSMT a b cppMin
    Max a b    -> genBinOpSMT a b cppMax
    Gt a b     -> genBinOpSMT a b cppGt
    Gte a b    -> genBinOpSMT a b cppGte
    Lt a b     -> genBinOpSMT a b cppLt
    Lte a b    -> genBinOpSMT a b cppLte
    Shl a b    -> genBinOpSMT a b cppShiftLeft
    Shr a b    -> genBinOpSMT a b cppShiftRight
    Tern c t f -> do
      c' <- genExprSMT c
      t' <- genExprSMT t
      f' <- genExprSMT f
      liftIR $ cppCond c' t' f'
    Cast v t -> do
      v' <- genExprSMT v
      liftIR $ cppCast v' t
    Call name args -> genCallSMT name args
    Load e -> do
      addr <- genExprSMT e
      liftIR $ smtLoad addr
    -- Idx arr' idx' -> do
    --   arr <- genExprSMT arr'
    --   idx <- genExprSMT idx'
    --   liftIR $ getIdx arr idx
    -- Access struct' idx -> do
    --   struct <- genExprSMT struct'
    --   liftIR $ getField struct idx
    _          -> error "Unsupported instruction"

genBinOpSMT :: Expr
            -> Expr
            -> (SMTNode -> SMTNode -> IR SMTNode)
            -> Compiler SMTNode
genBinOpSMT e1 e2 op = do
  s1 <- genExprSMT e1
  s2 <- genExprSMT e2
  liftIR $ op s1 s2

genCallSMT name args = do
  -- Get the arguments
  smtArgs <- mapM genExprSMT args
  -- Make a new return value for the function and push it onto the stack
  function <- getFunction name
  returnValName <- getReturnValName name
  returnVal <- liftIR $ newVar (fTy function) returnValName
  pushFunction name returnVal
  -- Get the formal arguments and set them equal to the arguments
  let formalArgs = fArgs function
  unless (length formalArgs == length args) $
    error $ unwords ["Wrong number of args to", name]
  smtFormalArgs <- forM formalArgs $ \(name, ty) -> do
    nextVer name
    getNodeFor name
  forM_ (zip smtArgs smtFormalArgs) $ \(arg, farg) -> liftIR $ smtAssign arg farg
  -- Execute the function
  mapM genStmtSMT $ fBody function
  -- Once done, pop the function back off the stack
  popFunction
  -- Return the return value
  return returnVal

genStmtSMT :: Stmt -> Compiler ()
genStmtSMT stmt =
  case stmt of
    Decl var           -> declareVar (varName var) (varTy var)
    Assign lhs rhs     -> do
      rhsSmt <- genExprSMT rhs
      prevLhs <- genVarSMT lhs
      -- Bump the version number of the LHS to SSA the statement
      nextVer (getVarName lhs)
      newLhs <- genVarSMT lhs
      -- Guard the assignment with the possible conditional context
      guard <- getCurrentGuardNode
      condAssign <- liftIR $ cppCond guard rhsSmt prevLhs
      liftIR $ smtAssign newLhs condAssign
    If c t f           -> do
      trueCond <- genExprSMT c
      falseCond <- liftIR $ cppBitwiseNeg trueCond
      -- Guard the true branch with the true condition
      pushCondGuard trueCond
      mapM_ genStmtSMT t
      popCondGuard
      -- Guard the false branch with the false condition
      pushCondGuard falseCond
      mapM_ genStmtSMT f
      popCondGuard
    While c body       -> error ""
    VoidCall name args -> void $ genCallSMT name args
    Return e           -> do
      guard <- getCurrentGuardNode
      toReturn <- genExprSMT e
      retVal <- getReturnVal
      returnOccurs <- liftIR $ cppEq retVal toReturn
      -- Only set the return value equal to e if the guard is true
      liftIR $ smtImplies guard returnOccurs
    VoidReturn         -> return ()
    Store var expr -> genStoreSMT var expr

genStoreSMT var expr = error ""

genBodySMT :: [Stmt] -> Compiler ()
genBodySMT = mapM_ genStmtSMT

genFunctionSMT :: Function -> Compiler ()
genFunctionSMT fun = do
  returnValName <- getReturnValName $ fName fun
  returnVal <- liftIR $ newVar (fTy fun) returnValName
  pushFunction (fName fun) returnVal
  genBodySMT $ fBody fun




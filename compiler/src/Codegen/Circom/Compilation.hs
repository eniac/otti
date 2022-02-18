{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Codegen.Circom.Compilation
  ( compMainCtx
  , compMainWitCtx
  , TemplateInvocation
  , WitCompCtx
  , runLowDegCompState
  , getMainInvocation
  , ltermToSig
  )
where

import           AST.Circom
import qualified Codegen.Circom.Signal         as Sig
import           Codegen.Circom.Utils           ( spanE )
import           Codegen.Circom.CompTypes
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LowDegCtx
                                                , LowDeg
                                                , LowDegCompCtx
                                                )
import           Codegen.Circom.CompTypes.WitComp
                                                ( WitBaseTerm
                                                , WitBaseCtx
                                                )

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                    as Arr
import           Data.Field.Galois              ( Prime
                                                , fromP
                                                , toP
                                                )
import           Data.Functor
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy )
import           Debug.Trace
import           Util.Control
import           Util.Log
import           GHC.TypeNats



compIndexedIdent
  :: (BaseCtx c b (Prime n), KnownNat n)
  => SIndexedIdent
  -> CompState c b n Sig.IndexedIdent
compIndexedIdent i =
  let (name, dims) = ast i
  in  do
        dimTerms <- compExprs dims
        let dimInts = map (termAsNum $ ann i) dimTerms
        return (ast name, dimInts)

compLoc
  :: (BaseCtx c b (Prime n), KnownNat n) => SLocation -> CompState c b n LTerm
compLoc l = case ast l of
  LocalLocation a -> do
    at <- compIndexedIdent a
    return $ LTermLocal at
  ForeignLocation a b -> do
    at <- compIndexedIdent a
    bt <- compIndexedIdent b
    return $ LTermForeign at bt

compExpr
  :: (BaseCtx c b (Prime n), KnownNat n)
  => SExpr
  -> CompState c b n (Term b (Prime n))
compExpr e = case ast e of
  NumLit   i  -> return $ Const $ fromInteger $ fromIntegral i
  ArrayLit es -> do
    ts <- compExprs es
    return $ Array $ Arr.listArray (0, length ts - 1) ts
  BinExpr op l r -> do
    l' <- compExpr l
    r' <- compExpr r
    return $ binOp op l' r'
  UnExpr op e' -> do
    t <- compExpr e'
    return $ unOp op t
  UnMutExpr op loc -> do
    lval <- compLoc loc
    term <- load (ann loc) lval
    let term' = binOp Add (fromConst $ toP $ opToOffset op) term
    modify (store (ann e) lval term')
    case unMutOpTime op of
      Post -> return term
      Pre  -> return term'
   where
    opToOffset o = case unMutOpOp o of
      Inc -> 1
      Dec -> -1
  Ite c l r -> do
    condT <- compExpr c
    caseT <- compExpr l
    caseF <- compExpr r
    return $ ite condT caseT caseF
  LValue loc -> do
    lt <- compLoc loc
    load (ann loc) lt
  Call name args -> do
    tArgs <- compExprs args
    -- TODO: Allow non-constant arguments
    let constArgs  = map (termAsConst $ ann e) tArgs
    let invocation = (ast name, constArgs)
    c <- get
    let (isFn, formalArgs, code) = Maybe.fromMaybe
          (spanE (ann name) $ "Unknown callable " ++ ast name)
          (callables c Map.!? ast name)
    unless (length args == length formalArgs)
      $  return
      $  spanE (ann e)
      $  "Wrong number of arguments for "
      ++ show name
    let callState = empty { callables = callables c
                          , cache     = cache c
                          , env       = Map.fromList $ zip formalArgs tArgs
                          , ids       = Map.fromList $ map (, IKVar) formalArgs
                          }
    if isFn
      then do
        ((), c') <- liftLog $ runCompState (compStatements $ ast code) callState
        case returning c' of
          Just r -> return r
          Nothing ->
            spanE (ann e) $ "Function " ++ ast name ++ " did not return"
      else do
        unless (Map.member invocation (cache c)) $ do
          ((), c') <- liftLog
            $ runCompState (compStatements $ ast code) callState
          let newCache = cache c'
          let strippedCtx = c'
                { env     = Map.restrictKeys
                              (env c')
                              (Map.keysSet $ Map.filter (== IKComp) (ids c'))
                , cache   = Map.empty
                , baseCtx = finalize $ baseCtx c'
                }
          modify
            (\cc -> cc { cache = Map.insert invocation strippedCtx newCache })
        return $ Component invocation

compExprs
  :: (BaseCtx c b (Prime n), KnownNat n)
  => [SExpr]
  -> CompState c b n [Term b (Prime n)]
compExprs = mapM compExpr

compCondition
  :: (BaseCtx c b (Prime n), KnownNat n) => SExpr -> CompState c b n Bool
compCondition cond = do
  tcond <- compExpr cond
  case tcond of
    Const 0 -> return False
    Const _ -> return True
    _ ->
      spanE (ann cond)
        $  "Invalid conditional term "
        ++ show tcond
        ++ " in while condition "
        ++ show cond

compStatements
  :: (BaseCtx c b (Prime n), KnownNat n) => [SStatement] -> CompState c b n ()
compStatements = void . mapM compStatement

compStatement
  :: (BaseCtx c b (Prime n), KnownNat n) => SStatement -> CompState c b n ()
compStatement s = do
  c <- get
  if Maybe.isJust (returning c) then return () else compStatementNoReturn s

compStatementNoReturn
  :: (BaseCtx c b (Prime n), KnownNat n) => SStatement -> CompState c b n ()
compStatementNoReturn s = case ast s of
  Assign loc expr -> do
    lval <- compLoc loc
    term <- compExpr expr
    modify (store (ann s) lval term)
  OpAssign op loc expr -> do
    lval <- compLoc loc
    base <- load (ann loc) lval
    new  <- compExpr expr
    let base' = binOp op base new
    modify (store (ann s) lval base')
  Constrain l r -> do
    lt <- compExpr l
    rt <- compExpr r
    assert' $ binOp Sub lt rt
  AssignConstrain loc e -> do
    l  <- compLoc loc
    t1 <- compExpr e
    modify (store (ann s) l t1)
    t0 <- load (ann loc) l
    assert' $ binOp Sub t0 t1
  VarDeclaration name dims ini -> do
    ts <- compExprs dims
    modify (alloc name IKVar $ termMultiDimArray (ann s) (Const 0) ts)
    mapM_ (compExpr >=> modify' . store (ann s) (LTermLocal (ast name, []))) ini
  SigDeclaration name kind dims -> do
    ts    <- compExprs dims
    fresh <- gets (not . Map.member (ast name) . ids)
    unless fresh $ spanE (ann name) $ show name ++ " is not fresh"
    modify
      (\c -> c
        { signals = Map.insert (ast name) (kind, map (termAsNum $ ann s) ts)
                      $ signals c
        , ids     = Map.insert (ast name) IKSig $ ids c
        }
      )
  SubDeclaration name dims ini -> do
    ts <- compExprs dims
    modify (alloc name IKComp (termMultiDimArray (ann s) (Const 1) ts))
    mapM_ (compExpr >=> modify' . store (ann s) (LTermLocal (ast name, []))) ini
  If cond true false -> do
    b <- compCondition cond
    if b then compStatements $ ast true else mapM_ (compStatements . ast) false
  While cond block ->
    whileM (compCondition cond) (compStatements $ ast block) $> ()
  For ini cond step block -> do
    compStatement ini
    _ <- whileM (compCondition cond)
                (compStatements (ast block) *> compStatement step)
    return ()
  DoWhile block cond -> compStatements (ast block)
    <* whileM (compCondition cond) (compStatements $ ast block)
  Compute b -> do
    ignore <- gets (ignoreCompBlock . baseCtx)
    if ignore then return () else compStatements (ast b)
  Ignore e -> do
    _ <- compExpr e
    return ()
  Log e -> do
    t <- compExpr e
    return $ trace (show e ++ ": " ++ show t) ()
  Return e -> do

    t <- compExpr e
    modify (\c -> c { returning = Just t })
    return ()
 where
  assert' (Base b) = do
    logIf "circomAssert" $ show b
    modify $ \c -> c { baseCtx = assert b (baseCtx c) }
  assert' t = spanE (ann s) $ "Cannot constain " ++ show t ++ " to zero"

termMultiDimArray
  :: (Show b, KnownNat k)
  => Span
  -> Term b (Prime k)
  -> [Term b (Prime k)]
  -> Term b (Prime k)
termMultiDimArray span_ = foldr
  (\d acc -> case d of
    Const n -> Array $ Arr.listArray (0, i - 1) (replicate i acc)
      where i = fromIntegral $ fromP n
    _ -> spanE span_ $ "Illegal dimension " ++ show d
  )

type WitCompCtx n = CompCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n)

compMain
  :: forall c b k
   . (KnownNat k, BaseCtx c b (Prime k))
  => SMainCircuit
  -> Log (CompCtx c b (Prime k))
compMain m =
  snd
    <$> (runCompState (compStatement $ main m) $ empty
          { callables = Map.union
                          (Map.map (\(p, b) -> (False, p, b)) (templates m))
                          (Map.map (\(p, b) -> (True, p, b)) (functions m))
          }
        )

compMainCtx
  :: KnownNat k
  => SMainCircuit
  -> Log (CompCtx (LowDegCtx (Prime k)) (LowDeg (Prime k)) (Prime k))
compMainCtx = compMain

compMainWitCtx
  :: KnownNat k
  => SMainCircuit
  -> Log (CompCtx (WitBaseCtx k) (WitBaseTerm k) (Prime k))
compMainWitCtx = compMain

runLowDegCompState
  :: KnownNat n
  => CompState (LowDegCtx (Prime n)) (LowDeg (Prime n)) n a
  -> LowDegCompCtx (Prime n)
  -> Log (a, LowDegCompCtx (Prime n))
runLowDegCompState = runCompState


getMainInvocation
  :: forall k
   . KnownNat k
  => Proxy k
  -> SMainCircuit
  -> Log (TemplateInvocation (Prime k))
getMainInvocation _order m = case ast (main m) of
  SubDeclaration _ [] (Just (Annotated (Call name args) _)) -> do
    e <- runLowDegCompState @k (compExprs args) empty
    return (ast name, map (termAsConst nullSpan) $ fst e)
  expr -> spanE (ann $ main m) $ "Invalid main expression " ++ show expr

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- Because of out KnownNat1 instance for the Log2 family...
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Codegen.Circom.Compilation
  ( compMainCtx
  , compMainWitCtx
  , nSmtNodes
  , TemplateInvocation
  , WitBaseCtx(..)
  , WitBaseTerm(..)
  , WitCompCtx
  , runLowDegCompState
  , getMainInvocation
  , ltermToSig
  )
where

import           AST.Circom
import qualified Codegen.Circom.Signal         as Sig
import           Codegen.Circom.Utils           ( spanE
                                                , mapGetE
                                                )
import           Codegen.Circom.CompTypes
import           Codegen.Circom.CompTypes.LowDeg
                                                ( LowDegCtx
                                                , LowDeg
                                                , LowDegCompCtx
                                                )

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                    as Arr
import           Data.Field.Galois              ( Prime
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import           Data.Functor
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Either                   as Either
import qualified Data.Set                      as Set
import           Data.Proxy                     ( Proxy(Proxy) )
import           Debug.Trace                    ( trace )
import qualified Digraph
import qualified IR.TySmt                      as Smt
import           GHC.TypeLits.KnownNat
import           GHC.TypeNats

intLog2 :: Integral a => a -> a
intLog2 n = if n <= fromInteger 1
  then fromInteger 0
  else fromInteger 1 + intLog2 (n `div` fromInteger 2)

-- XXX(HACK): Log2 0 is actually undefined, but who wants to deal with that?
--            we treat it as 0, even though the type systems rejects it.
instance KnownNat x => KnownNat1 $(nameToSymbol ''Log2) x where
  natSing1 = SNatKn (intLog2 (natVal (Proxy @x)))
  {-# INLINE natSing1 #-}


newtype WitBaseTerm n = WitBaseTerm (Smt.Term (Smt.PfSort n)) deriving (Show)

instance KnownNat n => Num (WitBaseTerm n) where
  (WitBaseTerm s) + (WitBaseTerm t) =
    WitBaseTerm $ Smt.PfNaryExpr Smt.PfAdd [s, t]
  (WitBaseTerm s) * (WitBaseTerm t) =
    WitBaseTerm $ Smt.PfNaryExpr Smt.PfMul [s, t]
  negate (WitBaseTerm s) = WitBaseTerm $ Smt.PfUnExpr Smt.PfNeg s
  abs _ = error "ndef"
  signum _ = error "ndef"
  fromInteger = WitBaseTerm . Smt.IntToPf . Smt.IntLit

instance KnownNat n => Fractional (WitBaseTerm n) where
  recip (WitBaseTerm s) = WitBaseTerm $ Smt.PfUnExpr Smt.PfRecip s
  fromRational _ = error "NYI"

instance KnownNat n => BaseTerm (WitBaseTerm n) (Prime n) where
  fromConst  = fromInteger . fromP
  fromSignal = WitBaseTerm . Smt.Var . show
  nonArithBinOp o = case o of
    IntDiv -> liftIntToPf (Smt.IntBinExpr Smt.IntDiv)
    Mod    -> liftIntToPf (Smt.IntBinExpr Smt.IntMod)
    Lt     -> liftIntPredToPf (Smt.IntBinPred Smt.IntLt)
    Gt     -> liftIntPredToPf (Smt.IntBinPred Smt.IntGt)
    Le     -> liftIntPredToPf (Smt.IntBinPred Smt.IntLe)
    Ge     -> liftIntPredToPf (Smt.IntBinPred Smt.IntGe)
    Eq     -> liftIntPredToPf (Smt.IntBinPred Smt.IntEq)
    Ne     -> liftIntPredToPf (Smt.IntBinPred Smt.IntNe)
    And    -> liftBoolToPf (\a b -> Smt.BoolNaryExpr Smt.And [a, b])
    Or     -> liftBoolToPf (\a b -> Smt.BoolNaryExpr Smt.Or [a, b])
    BitAnd -> liftBvToPf (Smt.BvBinExpr Smt.BvAnd)
    BitOr  -> liftBvToPf (Smt.BvBinExpr Smt.BvOr)
    BitXor -> liftBvToPf (Smt.BvBinExpr Smt.BvXor)
    Pow    -> liftIntToPf (Smt.IntBinExpr Smt.IntPow)
    Shl    -> liftBvToPf (Smt.BvBinExpr Smt.BvShl)
    Shr    -> liftBvToPf (Smt.BvBinExpr Smt.BvLshr)
    _      -> error "Unreachable"
   where
    liftIntToPf f (WitBaseTerm a) (WitBaseTerm b) =
      WitBaseTerm $ Smt.IntToPf $ f (Smt.PfToInt a) (Smt.PfToInt b)
    liftBvToPf f = liftIntToPf
      (\a b -> Smt.BvToInt @(Log2 n + 1)
        (f (Smt.IntToBv @(Log2 n + 1) a) (Smt.IntToBv @(Log2 n + 1) b))
      )
    liftIntPredToPf f = liftIntToPf (\a b -> Smt.BoolToInt $ f a b)
    liftBoolToPf f = liftIntPredToPf
      (\a b -> f (Smt.IntBinPred Smt.IntNe (Smt.IntLit 0) a)
                 (Smt.IntBinPred Smt.IntNe (Smt.IntLit 0) b)
      )
  nonNegUnOp o = case o of
    BitNot ->
      error "Bitwise negation has unclear semantics for prime field elements"
    Not -> \(WitBaseTerm a) ->
      WitBaseTerm $ Smt.IntToPf $ Smt.BoolToInt $ Smt.Not $ Smt.PfBinPred
        Smt.PfNe
        z
        a
      where z = Smt.IntToPf $ Smt.IntLit 0
    UnPos -> id
    UnNeg -> error "Unreachable"

data WitBaseCtx n = WitBaseCtx { signalTerms :: Map.Map LTerm (WitBaseTerm n)
                               -- The order in which signals and components are written to
                               -- lefts are signals, rights are components
                               -- Initialize unordered, then ordered in finalize.
                               , assignmentOrder :: [Either LTerm Sig.IndexedIdent]
                               } deriving (Show)

ltermToSig :: LTerm -> Sig.Signal
ltermToSig l = case l of
  LTermLocal a     -> Sig.SigLocal a
  LTermForeign a b -> Sig.SigForeign a b

sigToLterm :: Sig.Signal -> LTerm
sigToLterm l = case l of
  Sig.SigLocal a     -> LTermLocal a
  Sig.SigForeign a b -> LTermForeign a b


instance KnownNat n => BaseCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n) where
  assert _ = id
  emptyCtx = WitBaseCtx Map.empty []
  storeCtx span_ _kind sig term ctx = if Map.member sig (signalTerms ctx)
    then spanE span_ $ "Signal " ++ show sig ++ " has already been assigned to"
    else ctx { signalTerms     = Map.insert sig term $ signalTerms ctx
             , assignmentOrder = Left sig : assignmentOrder ctx
             }
  getCtx kind sig c = if kind == Out
    then case sig of
      LTermLocal{} -> c
      LTermForeign cLoc _ ->
        c { assignmentOrder = Right cLoc : assignmentOrder c }
    else c
  ignoreCompBlock = const False
  finalize c =
    let
      keys = assignmentOrder c

      collectSigs :: Smt.Term s -> Set.Set Sig.Signal
      collectSigs = Smt.reduceTerm visit Set.empty Set.union
       where
        visit :: Smt.Term t -> Maybe (Set.Set Sig.Signal)
        visit t = case t of
          Smt.Var v -> Just $ Set.singleton $ read v
          _         -> Nothing

      asLterm :: Either LTerm Sig.IndexedIdent -> LTerm
      asLterm = either id LTermLocal

      outputComponent o = case o of
        LTermForeign a _ -> LTermLocal a
        LTermLocal _     -> o

      dependencies :: Either LTerm Sig.IndexedIdent -> [LTerm]
      dependencies assignment = case assignment of
        Left signal ->
          map (outputComponent . sigToLterm)
            $ Fold.toList
            $ collectSigs
            $ (let WitBaseTerm s = mapGetE
                     ("Signal " ++ show signal ++ " has no term")
                     signal
                     (signalTerms c)
               in  s
              )
        Right componentLoc -> filter inputToComponent $ Either.lefts keys
         where
          inputToComponent l = case l of
            LTermForeign x _ | x == componentLoc -> True
            _ -> False

      graph
        :: Digraph.Graph (Digraph.Node LTerm (Either LTerm Sig.IndexedIdent))
      graph = Digraph.graphFromEdgedVerticesOrd $ map
        (\assignment -> Digraph.DigraphNode assignment
                                            (asLterm assignment)
                                            (dependencies assignment)
        )
        keys
    in
      c
        { assignmentOrder = map Digraph.node_payload
                              $ Digraph.topologicalSortG graph
        }

nSmtNodes :: WitBaseCtx n -> Int
nSmtNodes =
  Map.foldr ((+) . (\(WitBaseTerm a) -> Smt.nNodes a)) 0 . signalTerms

type WitCompCtx n = CompCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n)


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
    let term' = term + Const (toP $ opToOffset op)
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
    return $ case condT of
      Const 0 -> caseF
      Const _ -> caseT
      -- TODO: allow: Base  _ -> Base HighDegree
      t       -> spanE (ann c) $ "Cannot condition on term " ++ show t
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
      $  (spanE (ann e))
      $  "Wrong number of arguments for "
      ++ show name
    let callState = empty { callables = callables c
                          , cache     = cache c
                          , env       = Map.fromList $ zip formalArgs tArgs
                          , ids       = Map.fromList $ map (, IKVar) formalArgs
                          }
    if isFn
      then do
        let ((), c') = runCompState (compStatements $ ast code) callState
        let returnValue = Maybe.fromMaybe
              (  spanE (ann e)
              $  "Function "
              ++ show (ast name)
              ++ " did not return"
              )
              (returning c')
        return returnValue
      else do
        unless (Map.member invocation (cache c)) $ do
          let ((), c') = runCompState (compStatements $ ast code) callState
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

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ condition step = do
  b <- condition
  if b then step *> whileM_ condition step else pure ()

compStatement
  :: (BaseCtx c b (Prime n), KnownNat n) => SStatement -> CompState c b n ()
compStatement s = do
  ctx <- get
  if Maybe.isJust (returning ctx)
    then return ()
    else case ast s of
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
        -- Construct the zero term
        let zt = lt - rt
        case zt of
          Base b -> modify $ \c -> c { baseCtx = assert b (baseCtx c) }
          _      -> spanE (ann s) $ "Cannot constain " ++ show zt ++ " to zero"
      -- TODO Not quite right: evals location twice
      AssignConstrain l e -> compStatements
        [ Annotated (Assign l e) (ann s)
        , Annotated (Constrain (Annotated (LValue l) (ann l)) e) (ann s)
        ]
      VarDeclaration name dims ini -> do
        ts <- compExprs dims
        modify (alloc name IKVar $ termMultiDimArray (ann s) (Const 0) ts)
        mapM_
          (compExpr >=> modify' . store (ann s) (LTermLocal (ast name, [])))
          ini
      SigDeclaration name kind dims -> do
        ts    <- compExprs dims
        fresh <- gets (not . Map.member (ast name) . ids)
        unless fresh
          $  spanE (ann name)
          $  "Signal name "
          ++ show name
          ++ " is not fresh"
        modify
          (\c -> c
            { signals = Map.insert (ast name) (kind, map (termAsNum $ ann s) ts)
                          $ signals c
            , ids     = Map.insert (ast name) IKSig $ ids ctx
            }
          )
      SubDeclaration name dims ini -> do
        ts <- compExprs dims
        modify (alloc name IKComp (termMultiDimArray (ann s) (Const 1) ts))
        mapM_
          (compExpr >=> modify' . store (ann s) (LTermLocal (ast name, [])))
          ini
      If cond true false -> do
        b <- compCondition cond
        if b
          then compStatements $ ast true
          else mapM_ (compStatements . ast) false
      While cond block ->
        whileM_ (compCondition cond) (compStatements $ ast block) $> ()
      For ini cond step block -> do
        compStatement ini
        _ <- whileM_ (compCondition cond)
                     (compStatements (ast block) *> compStatement step)
        return ()
      DoWhile block cond -> compStatements (ast block)
        <* whileM_ (compCondition cond) (compStatements $ ast block)
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

compMain
  :: forall c b k
   . (KnownNat k, BaseCtx c b (Prime k))
  => SMainCircuit
  -> CompCtx c b (Prime k)
compMain m = snd $ runCompState (compStatement $ main m) $ empty
  { callables = Map.union (Map.map (\(p, b) -> (False, p, b)) (templates m))
                          (Map.map (\(p, b) -> (True, p, b)) (functions m))
  }

compMainCtx
  :: KnownNat k
  => SMainCircuit
  -> CompCtx (LowDegCtx (Prime k)) (LowDeg (Prime k)) (Prime k)
compMainCtx = compMain

compMainWitCtx
  :: KnownNat k
  => SMainCircuit
  -> CompCtx (WitBaseCtx k) (WitBaseTerm k) (Prime k)
compMainWitCtx = compMain

runLowDegCompState
  :: KnownNat n
  => CompState (LowDegCtx (Prime n)) (LowDeg (Prime n)) n a
  -> LowDegCompCtx (Prime n)
  -> (a, LowDegCompCtx (Prime n))
runLowDegCompState = runCompState


getMainInvocation
  :: forall k
   . KnownNat k
  => Proxy k
  -> SMainCircuit
  -> TemplateInvocation (Prime k)
getMainInvocation _order m = case ast (main m) of
  SubDeclaration _ [] (Just (Annotated (Call name args) _)) ->
    ( ast name
    , map (termAsNum nullSpan) $ fst $ runLowDegCompState @k (compExprs args)
                                                             empty
    )
  expr -> spanE (ann $ main m) $ "Invalid main expression " ++ show expr

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
module IR.SMT.Opt
  ( opt
  , constantFold
  , eqElim
  )
where

import           IR.SMT.TySmt

import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDyn
                                                , dynTypeRep
                                                )
import           Data.Functor.Identity
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import           Data.Typeable                  ( typeOf )
import           Util.Log


-- Folds constants (literals) away.
-- The end result is either
--   (a) the whole term is constant or
--   (b) the term is constant-free.
constantFold :: SortClass s => Term s -> Term s
constantFold = mapTerm visit
 where
  visit :: SortClass s => Term s -> Maybe (Term s)
  visit term = case term of
    BoolLit{} -> Just term
    Not a     -> Just $ case constantFold a of
      BoolLit True  -> BoolLit False
      BoolLit False -> BoolLit True
      a'            -> Not a'
    BoolBinExpr Implies a b -> Just $ case constantFold a of
      BoolLit True  -> constantFold b
      BoolLit False -> BoolLit True
      a'            -> case constantFold b of
        BoolLit True  -> BoolLit True
        BoolLit False -> Not a'
        b'            -> BoolBinExpr Implies a' b'
    BoolNaryExpr op xs ->
      Just
        $ let i    = identity op
              xs'  = filter (/= BoolLit i) $ map constantFold xs
              xs'' = filter (/= BoolLit (not i)) xs'
              n    = length xs' - length xs''
              f x = iterate (xfm op) x !! n
          in  f $ case xs'' of
                []  -> BoolLit i
                [x] -> x
                _   -> BoolNaryExpr op xs''
     where
      -- The identity element
      identity Or  = False
      identity And = True
      identity Xor = False
      -- The effect of a non-indentity element on the expression
      xfm Or  = const (BoolLit True)
      xfm And = const (BoolLit False)
      xfm Xor = negateBool
    Eq a b ->
      let a' = constantFold a
          b' = constantFold b
      in  case (a', b') of
            (BoolLit a'', BoolLit b'') -> Just $ BoolLit (a'' == b'')
            _                          -> Just $ Eq a' b'
    IntToDynBv w i ->
      let i' = constantFold i
      in  Just $ case i' of
            IntLit i'' -> DynBvLit $ Bv.bitVec w i''
            _          -> IntToDynBv w i'
    DynBvBinExpr op w a b ->
      let a' = constantFold a
          b' = constantFold b
      in  case (a', b') of
            (DynBvLit a'', DynBvLit b'') -> Just $ DynBvLit $ o a'' b''
            _                            -> Just $ DynBvBinExpr op w a' b'
     where
      bvizeIntOp f x y = Bv.bitVec (Bv.size x) $ f (Bv.uint x) (Bv.uint y)
      o = case op of
        BvAdd  -> bvizeIntOp (+)
        BvSub  -> bvizeIntOp (-)
        BvMul  -> bvizeIntOp (*)
        BvUdiv -> bvizeIntOp div
        BvUrem -> bvizeIntOp rem
        BvShl  -> Bv.shl
        BvAshr -> Bv.ashr
        BvLshr -> Bv.shr
        BvOr   -> (Bv..|.)
        BvAnd  -> (Bv..&.)
        BvXor  -> Bv.xor
    DynBvBinPred op w a b ->
      let a' = constantFold a
          b' = constantFold b
      in  case (a', b') of
            (DynBvLit a'', DynBvLit b'') -> Just $ BoolLit $ o a'' b''
            _                            -> Just $ DynBvBinPred op w a' b'
     where
      outOfRangeAfter
        :: (Integer -> Integer -> Integer) -> Bv.BV -> Bv.BV -> Bool
      outOfRangeAfter f x y =
        let s     = f (Bv.int x) (Bv.int y)
            big   = 2 ^ (Bv.size x - 1)
            small = negate $ 2 ^ (Bv.size x - 1)
        in  not (small <= s && s < big)
      o = case op of
        BvUgt   -> (Bv.>.)
        BvUlt   -> (Bv.<.)
        BvUge   -> (Bv.>=.)
        BvUle   -> (Bv.<=.)
        BvSgt   -> Bv.sgt
        BvSlt   -> Bv.slt
        BvSge   -> Bv.sge
        BvSle   -> Bv.sle
        BvSaddo -> outOfRangeAfter (+)
        BvSsubo -> outOfRangeAfter (-)
        BvSmulo -> outOfRangeAfter (*)
    Ite c t f ->
      Just
        $ let c' = constantFold c
              t' = constantFold t
              f' = constantFold f
          in  case c' of
                BoolLit True  -> t'
                BoolLit False -> f'
                _             -> case t' of
                  BoolLit True -> constantFold $ BoolNaryExpr Or [c', f']
                  BoolLit False ->
                    constantFold $ BoolNaryExpr And [negateBool c', f']
                  _ -> case f' of
                    BoolLit True ->
                      constantFold $ BoolNaryExpr Or [negateBool c', t']
                    BoolLit False -> constantFold $ BoolNaryExpr And [c', t']
                    _             -> Ite c' t' f'
    _ -> Nothing

  negateBool :: TermBool -> TermBool
  negateBool (Not a) = a
  negateBool a       = Not a


-- The equality elimination algorithm is a one-pass sweep over a list of
-- assertions which seeks to eliminate equalities like x = y + z by replacing
-- all instances of x with y + z.
--
-- It processes one assertion at time, maintaining the following state:
--   * A: A list of (already processed) assertions.
--   * S: A mapping from variables to terms that they are equal to.
-- This state respects the following invariants:
--  (1) vars(A) and vars(S) are disjoint
--  (2) forall v == t in A: v \in t.
--  (3) forall v -> t in S: v \notin t
--
-- In the algorithm below, square brackets denote substitution.
-- The invariant-preservation argument is on the right.
--
-- 1. For each assertion, apply S. Then, for each equality v == t // vars(t) _|_ vars(S)
-- 2.  if v in t:
-- 3.    A' = A + (v == t)        // (1) ok b/c 1. (2) ok b/c 2.
-- 4.  else:
-- 5.    S' = S[v\t] + (v\t)     // (1,3) ok b/c 1.
-- 6.    A' = A'[v\t]             // (2,3) ok b/c a lot of things
-- 7.  return (S', A')
--
-- One complication is that we handle a set of protected variables, which cannot be eliminated

data EqElimState = EqElimState { assertions :: [TermBool]
                               , subs :: Map.Map String Dynamic
                               }

newtype EqElim a = EqElim (StateT EqElimState Identity a)
    deriving (Functor, Applicative, Monad, MonadState EqElimState)


sub :: SortClass s => String -> Dynamic -> Term s -> Term s
sub name value = mapTerm visit
 where
  visit :: forall t . SortClass t => Term t -> Maybe (Term t)
  visit term = case term of
    Var name' _ -> if name' == name
      then Just $ fromDyn @(Term t) value (error "wrong sort")
      else Nothing
    _ -> Nothing

dynamize :: (forall s . SortClass s => Term s -> Term s) -> Dynamic -> Dynamic
dynamize f t
  | ty == typeOf (BoolLit True) = toDyn
  $ f (fromDyn t (error "unreachable") :: TermBool)
  | ty == typeOf (IntToDynBv 0 (IntLit 0)) = toDyn
  $ f (fromDyn t (error "unreachable") :: TermDynBv)
  | otherwise = error $ "Cannot dynamize " ++ show ty
  where ty = dynTypeRep t

inTerm :: SortClass s => String -> Term s -> Bool
inTerm name = reduceTerm visit False (||)
 where
  visit :: SortClass t => Term t -> Maybe Bool
  visit term = case term of
    Var name' _ -> Just $ name' == name
    _           -> Nothing

eqElim :: Set.Set String -> [TermBool] -> [TermBool]
eqElim protected terms =
  let EqElim   action = forM_ terms process
      Identity result = execStateT action (EqElimState [] Map.empty)
  in  assertions result
 where
  process :: TermBool -> EqElim ()
  process assertion = do
    a' <- applyStoredSubs assertion
    case a' of
      Eq (Var v s) t | v `Set.notMember` protected ->
        if v `inTerm` t then addAssertion (Eq (Var v s) t) else addSub v t
      Eq t (Var v s) | v `Set.notMember` protected ->
        if v `inTerm` t then addAssertion (Eq (Var v s) t) else addSub v t
      _ -> addAssertion a'

  applyStoredSubs :: SortClass s => Term s -> EqElim (Term s)
  applyStoredSubs term =
    gets (foldr (.) id . map (uncurry sub) . Map.toList . subs) <*> pure term

  addAssertion :: TermBool -> EqElim ()
  addAssertion a = modify $ \s -> s { assertions = a : assertions s }

  addSub :: SortClass s => String -> Term s -> EqElim ()
  addSub v t =
    let t' = toDyn t
    in  modify $ \s -> s
          { assertions = map (sub v t') $ assertions s
          , subs = Map.insert v t' $ Map.map (dynamize $ sub v t') $ subs s
          }

logAssertions :: String -> [TermBool] -> Log ()
logAssertions context as = logIfM "opt" $ do
  liftIO $ putStrLn $ context ++ ":"
  forM_ as $ \a -> liftIO $ putStrLn $ "  " ++ show a
  return $ show (length as) ++ " assertions"

opt :: Set.Set String -> [TermBool] -> Log [TermBool]
opt protected terms = do
  liftIO $ putStrLn "hi"
  logAssertions "initial" terms
  let folded = map constantFold terms
  logAssertions "post fold" folded
  let elimed = eqElim protected folded
  logAssertions "post elim" elimed
  let refolded = map constantFold elimed
  logAssertions "post refold" refolded
  return refolded

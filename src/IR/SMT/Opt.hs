{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE Rank2Types #-}
module IR.SMT.Opt
  ( opt
  , constantFold
  , eqElim
  , OptMetadata(..)
  , newOptMetadata
  )
where

import           IR.SMT.TySmt
import           IR.SMT.OblivArray              ( elimOblivArrays )

import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDyn
                                                , dynTypeRep
                                                , fromDynamic
                                                )
import           Data.List                      ( intercalate )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( isJust
                                                , fromMaybe
                                                )
import qualified Data.Set                      as Set
import           Util.Cfg                       ( Cfg
                                                , MonadCfg(..)
                                                , _smtOptCfg
                                                , _allowSubBlowup
                                                , _cFoldInSub
                                                , _smtOpts
                                                )
import           Util.Log
import           Util.ShowMap                   ( ShowMap )

type ArraySizes = ShowMap (Term (ArraySort DynBvSort DynBvSort)) Int

data OptMetadata = OptMetadata
  { protected      :: !(Set.Set String)
  , eqElimNoBlowup :: !Bool
  , cFoldInEqElim  :: !Bool
  , arraySizes     :: !ArraySizes
  }
newOptMetadata :: Set.Set String -> ArraySizes -> OptMetadata
newOptMetadata p s = OptMetadata { protected      = p
                                 , eqElimNoBlowup = False
                                 , cFoldInEqElim  = False
                                 , arraySizes     = s
                                 }

data Opt = Opt
  { fn   :: OptMetadata -> [TermBool] -> Log [TermBool]
  , name :: String
  , cfg  :: OptMetadata -> Cfg OptMetadata
  }

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

constantFoldOpt :: Opt
constantFoldOpt =
  Opt { fn = const (return . map constantFold), name = "cf", cfg = return }

data ConstFoldEqState = ConstFoldEqState
  { terms  :: [TermBool]
  , consts :: Map.Map String Dynamic
  }

newtype ConstFoldEq a = ConstFoldEq (StateT ConstFoldEqState Log a)
    deriving (Functor, Applicative, Monad, MonadState ConstFoldEqState)


isConst :: SortClass s => Term s -> Bool
isConst t = case t of
  BoolLit{}  -> True
  DynBvLit{} -> True
  IntLit{}   -> True
  _          -> False

constFoldEq :: OptMetadata -> [TermBool] -> Log [TermBool]
constFoldEq meta = go
 where
  -- While processing makes progress, process.
  -- TODO: use progress pass?
  go ts = do
    (continue, ts') <- processAll ts
    if continue then go ts' else return ts
  noElim = protected meta

  -- Returns the result of one pass of constant folding and eq-elim. Also
  -- returns whether any progress was made.
  processAll :: [TermBool] -> Log (Bool, [TermBool])
  processAll allTerms = do
    let ConstFoldEq action = forM_ allTerms process
    result <- execStateT action (ConstFoldEqState [] Map.empty)
    logIf "smt::opt::cfee" $ "Substs:\n  " ++ intercalate
      "\n  "
      (map show $ Map.toList $ consts result)
    return (not $ Map.null $ consts result, terms result)

  process :: TermBool -> ConstFoldEq ()
  process assertion = do
    a' <- applyStoredSubs assertion
    let subst = case a' of
          Eq (Var v _s) t | v `Set.notMember` noElim && isConst t ->
            Just (v, toDyn t)
          Eq t (Var v _s) | v `Set.notMember` noElim && isConst t ->
            Just (v, toDyn t)
          _ -> Nothing
    case subst of
      Just (v, t) -> do
        knownConsts <- gets consts
        case Map.lookup v knownConsts of
          Just{} ->
            error $ "Internal error: variable " ++ v ++ " present after subst"
          Nothing -> addSub v t
      Nothing -> addAssertion a'


  applyStoredSubs :: SortClass s => Term s -> ConstFoldEq (Term s)
  applyStoredSubs term = do
    allSubFn <- gets $ foldr (.) id . map (uncurry sub) . Map.toList . consts
    return $ constantFold $ allSubFn term

  addAssertion :: TermBool -> ConstFoldEq ()
  addAssertion a = modify $ \s -> s { terms = a : terms s }

  addSub :: String -> Dynamic -> ConstFoldEq ()
  addSub v t = modify $ \s -> s
    { terms  = map (sub v t) $ terms s
    , consts = Map.insert v t $ Map.map (dynamize $ sub v t) $ consts s
    }

constantFoldEqOpt :: Opt
constantFoldEqOpt = Opt { fn = constFoldEq, name = "cfee", cfg = return }


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

data EqElimState = EqElimState
  { assertions :: [TermBool]
  , subs       :: Map.Map String Dynamic
  }

newtype EqElim a = EqElim (StateT EqElimState Log a)
    deriving (Functor, Applicative, Monad, MonadState EqElimState, MonadLog)


sub :: SortClass s => String -> Dynamic -> Term s -> Term s
sub name_ value = mapTerm visit
 where
  visit :: forall t . SortClass t => Term t -> Maybe (Term t)
  visit term = case term of
    Var name' _ -> if name' == name_
      then Just $ fromDyn @(Term t) value (error "wrong sort")
      else Nothing
    _ -> Nothing

type TermMem = Term (ArraySort DynBvSort DynBvSort)

dynamize :: (forall s . SortClass s => Term s -> Term s) -> Dynamic -> Dynamic
dynamize f t
  | isJust (fromDynamic @TermBool t) = toDyn
  $ f (fromDyn t (error "unreachable") :: TermBool)
  | isJust (fromDynamic @TermDynBv t) = toDyn
  $ f (fromDyn t (error "unreachable") :: TermDynBv)
  | isJust (fromDynamic @TermMem t) = toDyn
  $ f (fromDyn t (error "unreachable") :: TermMem)
  | otherwise = error $ "Cannot dynamize " ++ show ty
  where ty = dynTypeRep t

inTerm :: SortClass s => String -> Term s -> Bool
inTerm name_ = reduceTerm visit False (||)
 where
  visit :: SortClass t => Term t -> Maybe Bool
  visit term = case term of
    Var name' _ -> Just $ name' == name_
    _           -> Nothing

eqElim :: OptMetadata -> [TermBool] -> Log [TermBool]
eqElim meta ts = do
  let EqElim action = forM_ ts process
  result <- execStateT action (EqElimState [] Map.empty)
  return $ assertions result
 where
  subbable :: SortClass s => String -> Sort -> Term s -> Bool
  subbable v s t =
    (not (eqElimNoBlowup meta) || nNodes t == 1)
      &&              v
      `Set.notMember` noElim
      &&              not (v `inTerm` t)
      &&              not (isArray s)
  noElim = protected meta
  cFoldFn :: SortClass s => Term s -> Term s
  cFoldFn = if cFoldInEqElim meta then constantFold else id

  process :: TermBool -> EqElim ()
  process assertion = do
    a' <- applyStoredSubs assertion
    case a' of
      -- Combining these cases is tough b/c the sorts may differ
      Eq (Var v s) t | subbable v s t -> do
        liftLog $ logIf "smt::opt::ee" $ "Elim: " ++ v ++ " = " ++ show t
        addSub v t
      Eq t (Var v s) | subbable v s t -> do
        liftLog $ logIf "smt::opt::ee" $ "Elim: " ++ v ++ " = " ++ show t
        addSub v t
      _ -> addAssertion a'

  applyStoredSubs :: SortClass s => Term s -> EqElim (Term s)
  applyStoredSubs term = do
    allSubFn <- gets $ foldr (.) id . map (uncurry sub) . Map.toList . subs
    return $ cFoldFn $ allSubFn term

  addAssertion :: TermBool -> EqElim ()
  addAssertion a = modify $ \s -> s { assertions = a : assertions s }

  isArray :: Sort -> Bool
  isArray (SortArray{}) = True
  isArray _             = False

  addSub :: SortClass s => String -> Term s -> EqElim ()
  addSub v t =
    let t' = toDyn $ cFoldFn t
    in  modify $ \s -> s
          { assertions = map (sub v t') $ assertions s
          , subs = Map.insert v t' $ Map.map (dynamize $ sub v t') $ subs s
          }

eqElimCfg :: OptMetadata -> Cfg OptMetadata
eqElimCfg m = do
  o <- asks (_allowSubBlowup . _smtOptCfg)
  c <- asks (_cFoldInSub . _smtOptCfg)
  return $ m { eqElimNoBlowup = o, cFoldInEqElim = c }

eqElimOpt :: Opt
eqElimOpt = Opt { fn = eqElim, name = "ee", cfg = eqElimCfg }

arrayElimOpt :: Opt
arrayElimOpt =
  Opt { fn = elimOblivArrays . arraySizes, name = "arrayElim", cfg = return }

-- | Flattens ANDs
flattenAnds :: TermBool -> [TermBool]
flattenAnds t = case t of
  BoolNaryExpr And conjuncts -> concatMap flattenAnds conjuncts
  _                          -> [t]

flattenAndsOpt :: Opt
flattenAndsOpt = Opt { fn   = \_ ts -> return $ concatMap flattenAnds ts
                     , name = "flattenAnds"
                     , cfg  = return
                     }

opts :: Map.Map String Opt
opts = Map.fromList
  [ (name o, o)
  | o <-
    [ eqElimOpt
    , constantFoldOpt
    , constantFoldEqOpt
    , arrayElimOpt
    , flattenAndsOpt
    ]
  ]

logAssertions :: String -> [TermBool] -> Log ()
logAssertions context as = logIfM "smt::opt" $ do
  liftIO $ putStrLn $ context ++ ":"
  forM_ as $ \a -> liftIO $ putStrLn $ "  " ++ show a
  return $ show (length as) ++ " assertions"


-- Optimize, ensuring that the variables in `p` continue to exist.
opt :: ArraySizes -> Set.Set String -> [TermBool] -> Log [TermBool]
opt sizes p ts = do
  let m0 = OptMetadata { protected      = p
                       , eqElimNoBlowup = False
                       , cFoldInEqElim  = False
                       , arraySizes     = sizes
                       }
  m'        <- liftCfg $ foldM (flip cfg) m0 (Map.elems opts)
  optsToRun <- liftCfg $ asks (_smtOpts . _smtOptCfg)
  logAssertions "initial" ts
  foldM
    (\a oname -> do
      let o =
            fromMaybe (error $ "No optimization named: " ++ oname)
              $ Map.lookup oname opts
      a' <- fn o m' a
      logAssertions ("Post " ++ show oname) a'
      return a'
    )
    ts
    optsToRun

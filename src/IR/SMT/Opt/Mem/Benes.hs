{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{- |

= Interface

Machinery for extracting memrory traces from SMT formulas, and checking them
using Benes routing.

That is, given a conjunction of assertions including

   a. memory (Array from Bv to Bv) sorted variables
   b. that are set equal to const arrays (= v (const ...)) OR
   c. set equal it ITEs of stores of terms recursively meeting this and the
      above condition (= v' (ite c (store v ...) v))
   d. selects on array terms meeting the above conditions (select v ...)

1. Assigns an numberic index to each STORE or SELECT such that the index
   ordering is consistent which the order constraints imposed by the ITE
   assignments.

    var' = ITE(c, STORE(var, addr, value), )

   Note forks in the ITE chain are prohibited.

2. Associates with each SELECT or STORE a *current value*: a new bitvector
   variable.
3. Replaces SELECTs with their current value.
4. Collects an *access array*, where each element corresponds to an access,
   and is a 4-tuple of SMT terms:
      (is_select, idx, addr, val)
   where
     is_select is
        whether this is a select (v. store) OR
        whether the store is disabled by its ITE
     idx is the assigned index
     addr is the address interacted with
     val  is ITE(store & enabled, select value, select value)
5. Removes the ITE assignments.

This *access list* is then checked to be consistent with memory semantics.

* it is address-sorted.
* for each read
   * if this read is first for the address, then the default is read.
   * if the read is not first for the address, the read value agrees with the
     last value from this address.

= Implementation

1. We index the formula by variables.
2. We find all array root variables.
3. For each array root, we trace that array:
   a. Maintain a *tracepoint*: a variable name.
   b. Initialize it to the root variable (which is set to a constant)
   c. Iteratively find SELECTS and ITE stores to the tracepoint
      * First list the SELECTS
      * then the stores
4. For each trace
   a. Extend the trace to a power-of-two length with addr=0 reads.
   b. Create a value variable for each access in the trace, detailed above.
   c. Perfom the substitutions, detailed above.
   d. Compute the access array, detailed above.

-}


module IR.SMT.Opt.Mem.Benes
  ( benesPass
  )
where

import           Control.Monad.State.Strict
import           Data.Bifunctor                 ( first
                                                , second
                                                )
import qualified Data.BitVector                as Bv
import           Data.Functor                   ( (<&>) )
import qualified Data.Foldable                 as Fold
import qualified Data.IntMap.Strict            as IMap
import qualified Data.IntSet                   as ISet
import qualified Data.HashMap.Strict           as HMap
import qualified Data.HashSet                  as HSet
import           Data.List                      ( intercalate
                                                , sortOn
                                                , transpose
                                                , zip4
                                                )
import           Data.Maybe                     ( fromMaybe
                                                , catMaybes
                                                )
import qualified Data.Sequence                 as Seq
import           Data.Sequence                  ( Seq )
import           Data.Typeable                  ( cast )
import           Lens.Simple                    ( over
                                                , view
                                                )
import           IR.SMT.TySmt
import qualified IR.SMT.Opt.Assert             as A
import qualified IR.SMT.Opt.Mem.Route          as MemR
import           IR.SMT.Opt.Mem.Util           as MemU
                                                ( MemReplacePass(visitSelect)
                                                , TMem
                                                , TBv
                                                , defaultMemReplacePass
                                                , runMemReplacePass
                                                )
import           IR.SMT.Opt.Assert              ( Assert )
import qualified Util.ShowMap                  as SMap
import           Util.Show                      ( pShow )
import           Util.Log                       ( logIf )


-- | A root: (name, array sort, array size, index of assertion creating it, default value)
data Root = Root String Sort Int Int TBv deriving Show

-- | Search the assertions for terms of the form @Eq var (ConstArray
-- default ..)@ and remove them, returning a list of [Root]s.
extractRoots :: Assert [Root]
extractRoots = do
  sizes <- gets $ view A.sizes
  as    <- gets $ view A.assertions
  -- Find all roots
  let
    rts = do -- List monad
      (idx, a) <- IMap.toAscList as
      case a of
        Eq (Var n s) c@(ConstArray _ defaultValue) ->
          case cast (c, defaultValue) of
            Just (c', d) ->
              let size =
                      fromMaybe
                          (error $ "extractRoots: No array size for " ++ show c)
                        $ SMap.lookup c' sizes
              in  return $ Root n s size idx d
            Nothing -> mempty
        _ -> mempty
  -- Remove all root-declaring equalities
  forM_ rts $ \(Root name _ _ i _) -> do
    A.deleteAssertion i
    modify $ over A.index $ HMap.adjust (ISet.delete i) name
  -- Return roots
  return rts

-- | A memory access: (is_read, addr, val)
data Access = Access TermBool TBv TBv deriving Show

-- | An indexed memory access: (is_read, idx, addr, val)
-- "index" is the formula/time index of the access.
type IAccess = (TermBool, TBv, TBv, TBv)

type SelectReplace a = StateT (Int, Seq Access) Assert a

-- | Given an SMT @var@, find all selects based on the variable.
-- That is, all terms @(Select (Var var _) addr)@.
-- Replaces the select terms with a new variable. These variables,
-- together with the corresponding select addresses are returned.
extractSelects :: String -> Assert (Seq Access)
extractSelects var = do
  uses <- A.useAssertions var
  let terms = map (findUseTerms . snd) uses
  logIf "smt::opt::trace" $ "Selects: " ++ pShow terms
  (_, accesses) <- execStateT (replaceInAll $ map fst uses) (0, Seq.empty)
  return accesses
 where
  visitSelect :: TMem -> TBv -> TMem -> TBv -> SelectReplace (Maybe TBv)
  visitSelect _ _ a' i' = case a' of
    (Var name _) | name == var -> do
      -- Get next number
      i <- gets fst
      modify $ first (+ 1)
      -- Create read variable
      let n = var ++ "_select_" ++ show i
      t <- A.liftAssert $ A.newVar n (sort $ Select a' i') (Select a' i')
      -- Record access
      modify $ second (Seq.|> Access (BoolLit True) i' t)
      return $ Just t
    _ -> return Nothing
  replaceInAssertion :: TermBool -> SelectReplace TermBool
  replaceInAssertion t = MemU.runMemReplacePass
    (MemU.defaultMemReplacePass { MemU.visitSelect = visitSelect })
    t

  replaceInAll :: [Int] -> SelectReplace ()
  replaceInAll is = forM_ is $ \i -> do
      -- Use the maybe-variant, because we're removing uses without fixing the
      -- index.
    mT <- A.liftAssert $ A.getAssertionM i
    forM_ mT $ \t -> do
      t' <- replaceInAssertion t
      A.liftAssert $ modify $ over A.assertions $ IMap.insert i t'


  findUseTerms :: SortClass s => Term s -> HSet.HashSet (TBv, TBv)
  findUseTerms = reduceTerm visit HSet.empty HSet.union
   where
    visit :: SortClass s => Term s -> Maybe (HSet.HashSet (TBv, TBv))
    visit t = case t of
      s@(Select (Var name _) idx) | name == var -> case cast (s, idx) of
        Just p  -> Just $ HSet.insert p $ findUseTerms idx
        Nothing -> Nothing
      _ -> Nothing

-- | Used inside @extractStores@.
-- Different kinds of stores
data S = Alias String -- ^ An alias for an array term
       | DStore String TBv TBv -- ^ a non-conditional store
       | MStore String TermBool TBv TBv TBv -- ^ a conditional store (new name, isWrite, index, value, oldValue)

-- | Find all stores based on var, removing them in the process, and
-- creating [Access] terms for them.
extractStores :: String -> Assert (Seq (String, Maybe Access))
extractStores var = do
  uses    <- A.useAssertions var
  mStores <- forM uses $ \(i, a) -> do
    s <- asStore a
    return ((i, ) <$> s)
  let stores = catMaybes mStores
  forM_ stores $ \(i, _) -> A.deleteAssertion i
  logIf "smt::opt::trace" $ "Stores: " ++ pShow stores
  return $ Seq.fromList $ map snd stores
 where
  -- Returns either a renaming or a store, or neither
  asStore :: TermBool -> Assert (Maybe (String, Maybe Access))
  asStore a =
    let
      store = case a of
        Eq (Var n0 _) (Var n1 _) | n0 == var -> Just $ Alias n1
        Eq (Var n0 _) (Var n1 _) | n1 == var -> Just $ Alias n0
        Eq (Var n0 _) (Store (Var n1 _) i v) | n1 == var ->
          cast (i, v) <&> uncurry (DStore n0)
        Eq (Store (Var n1 _) i v) (Var n0 _) | n1 == var ->
          cast (i, v) <&> uncurry (DStore n0)
        Eq (Var n0 _) (Ite isWrite (Store ar@(Var n1' _) i v) (Var n1 _))
          | n1' == var && n1 == var -> cast (ar, i, v)
          <&> \(ar', i', v') -> MStore n0 isWrite i' v' $ mkSelect ar' i'
        Eq (Var n0 _) (Ite isWrite (Var n1 _) (Store ar@(Var n1' _) i v))
          | n1' == var && n1 == var -> cast (ar, i, v) <&> \(ar', i', v') ->
            MStore n0 (Not isWrite) i' v' $ mkSelect ar' i'
        Eq (Ite isWrite (Store ar@(Var n1' _) i v) (Var n1 _)) (Var n0 _)
          | n1' == var && n1 == var -> cast (ar, i, v)
          <&> \(ar', i', v') -> MStore n0 isWrite i' v' $ mkSelect ar' i'
        Eq (Ite isWrite (Var n1 _) (Store ar@(Var n1' _) i v)) (Var n0 _)
          | n1' == var && n1 == var -> cast (ar, i, v) <&> \(ar', i', v') ->
            MStore n0 (Not isWrite) i' v' $ mkSelect ar' i'
        _ -> Nothing
    in
      case store of
        Nothing        -> return Nothing
        Just (Alias n) -> return $ Just (n, Nothing)
        Just (DStore n i v) ->
          return $ Just (n, Just $ Access (BoolLit False) i v)
        Just (MStore n isWrite addr v oldV) -> do
          let n' = var ++ "_storeselect"
          altV <- A.newVar n' (sort oldV) oldV
          let val = Ite isWrite v altV
          return $ Just (n, Just $ Access (Not isWrite) addr val)

-- | Find all selects based on var
extractAccesses :: String -> Assert (Maybe String, Seq Access)
extractAccesses var = do
  selects <- extractSelects var
  stores  <- extractStores var
  case stores of
    -- No store
    Seq.Empty -> return (Nothing, selects)
    -- One store
    (var', mA) Seq.:<| Seq.Empty ->
      return (Just var', maybe selects (selects Seq.|>) mA)
    -- more stores!!!
    s ->
      error
        $  "Multiple arrays, {"
        ++ intercalate ", " (map fst $ Fold.toList s)
        ++ "} derived from "
        ++ var

-- | Trace the memory accesses from some variable
--   Removes original select and store terms as it goes.
extractTrace
  :: String -- The variable name to trace from
  -> Assert (Seq Access)
extractTrace = go Seq.empty
 where
  go accessesAcc var = do
    (mVar', accesses) <- extractAccesses var
    let accessesAcc' = accessesAcc Seq.>< accesses
    case mVar' of
      Just var' -> go accessesAcc' var'
      Nothing   -> return accessesAcc'

data Trace = Trace -- ^ Trace of an array through the computation
                   String-- ^ Root name
                          Int   -- ^ Array size
                              TBv   -- ^ Default term
                                  [Access] -- ^ Accesses
  deriving Show

-- Indexed trace
data ITrace = ITrace -- ^ Trace of an array through the computation
                   String-- ^ Root name
                          Int   -- ^ Array size
                              TBv   -- ^ Default term
                                  [IAccess] -- ^ Accesses (isRead, idx, addr, val)
  deriving Show

extractTraces :: Assert [Trace]
extractTraces = do
  rts <- extractRoots
  logIf "smt::opt::trace" $ "Roots: " ++ pShow rts
  ts <- forM rts $ \(Root rootName _ size _ defaultTerm) -> do
    accesses <- extractTrace rootName
    return $ Trace rootName size defaultTerm (Fold.toList accesses)
  -- refresh the index. We may have messed it up.
  A.refresh
  return ts


-- | Given a symbolic memory trace in index-order (time-order), return
-- a list of address-order traces, with appropriate routing.
-- They are not confirmed to be in address order.
benesRoute :: ITrace -> Assert ITrace
benesRoute (ITrace name size def accesses) = do
  logIf "smt::opt::benes"
    $  "benesRoute ("
    ++ show (length accesses)
    ++ ") = "
    ++ name
  storing_vals <- A.isStoringValues
  let evalOr t = if storing_vals
        then valAsDynBv <$> A.eval t
        else return (Bv.bitVec 1 (0 :: Int))
  a' <- forM (zip [(0 :: Int) ..] accesses)
    $ \(idx, (_, _, a, _)) -> (, idx) <$> evalOr a
  -- NB: correctness depends on the stability of sortOn. o.w. proofs break
  let out_ord = map snd $ sortOn fst a'
      in_ord  = take (length out_ord) $ [(0 :: Int) ..]
      buildBenes name inp outp accesses = do
        case length inp of
          len | len < 2 -> return accesses
          2             -> benesLayer2 name (inp /= outp) accesses
          3             -> benesLayer3 name (MemR.benesRoute3 inp outp) accesses
          _             -> do
            -- 0. figure out Benes switch settings
            let (sw_i, sw_o) = MemR.benesRoute inp outp
                (it  , ib  ) = MemR.benesTopBottom sw_i inp
                (ot  , ob  ) = MemR.benesTopBottom sw_o outp
            -- 1. construct input switches
            (accT, accB) <- benesLayerIn name accesses sw_i
            -- 2. construct top and bottom networks
            accTO        <- buildBenes (name ++ "_top") it ot accT
            accBO        <- buildBenes (name ++ "_bot") ib ob accB
            -- 3. construct output switches
            benesLayerOut name accTO accBO sw_o
  liftM (ITrace name size def)
    $ buildBenes (name ++ "_benes") in_ord out_ord accesses

-- | Create one crossbar switch and accompanying constraints
benesSwitch :: String -> Bool -> IAccess -> IAccess -> Assert (IAccess, IAccess)
benesSwitch name swap accIn0 accIn1 = do
  swapVar <- A.newVar (name ++ "_sw") SortBool (BoolLit swap)
  let (r0, i0, a0, v0) = accIn0
      (r1, i1, a1, v1) = accIn1

      toTuple4 r [i, a, v] = (r, i, a, v)
      toTuple4 _ _         = undefined

      toTuple2 [o0, o1] = (o0, o1)
      toTuple2 _        = undefined

      -- generate one crossbar switch (i.e., two ITEs)
      mkSwitch :: SortClass a => (String, Term a, Term a) -> Assert [Term a]
      mkSwitch (suffix, in0, in1) = do
        let ite0 = Ite swapVar in1 in0
            ite1 = Ite swapVar in0 in1
        o0 <- A.newVar (name ++ suffix ++ "0") (sort in0) ite0
        o1 <- A.newVar (name ++ suffix ++ "1") (sort in0) ite1
        A.assign o0 ite0
        A.assign o1 ite1
        return [o0, o1]

  ret1 <- mkSwitch ("_r", r0, r1)
  ret2 <- mapM mkSwitch [("_i", i0, i1), ("_a", a0, a1), ("_v", v0, v1)]
  return . toTuple2 $ zipWith ($) (map toTuple4 ret1) (transpose ret2)

-- | Construct a two-input Benes network (i.e., one switch)
benesLayer2 :: String -> Bool -> [IAccess] -> Assert [IAccess]
benesLayer2 name swap accesses = do
  logIf "smt::opt::benes" $ "benesLayer2 (" ++ show swap ++ ") = " ++ name
  let [acc0, acc1] = accesses
  (o0, o1) <- benesSwitch name swap acc0 acc1
  return [o0, o1]

-- | Construct a 3-input Benes network (i.e., 3 switches)
benesLayer3 :: String -> [Bool] -> [IAccess] -> Assert [IAccess]
benesLayer3 name sw accesses = do
  logIf "smt::opt::benes" $ "benesLayer3 (" ++ show sw ++ ") = " ++ name
  let [acc0, acc1, acc2] = accesses
      [sw_i, sw_m, sw_o] = sw
  (t0, m ) <- benesSwitch (name ++ "_I") sw_i acc0 acc1
  (t1, o2) <- benesSwitch (name ++ "_M") sw_m m acc2
  (o0, o1) <- benesSwitch (name ++ "_O") sw_o t0 t1
  return [o0, o1, o2]

-- | Construct input side of one Benes "shell"
benesLayerIn :: String -> [IAccess] -> [Bool] -> Assert ([IAccess], [IAccess])
benesLayerIn name accesses sw_i = do
  logIf "smt::opt::benes" $ "benesLayerIn=" ++ name
  let (accPairs, accOdd) = toPairs accesses
      loop_in            = zip3 [(0 :: Int) ..] accPairs sw_i
  res <- forM loop_in $ \(idx, (acc0, acc1), swval) ->
    benesSwitch (name ++ "_I" ++ show idx) swval acc0 acc1
  let (accT, accB) = unzip res
  return $ case accOdd of
    Nothing  -> (accT, accB)
    Just odd -> (accT, accB ++ [odd])

-- | Construct output side of one Benes "shell"
benesLayerOut :: String -> [IAccess] -> [IAccess] -> [Bool] -> Assert [IAccess]
benesLayerOut name accT accB sw_o = do
  logIf "smt::opt::benes" $ "benesLayerOut=" ++ name
  let loop_in = zip4 [(0 :: Int) ..] accT accB sw_o
  res <- liftM concat $ forM loop_in $ \(idx, accT, accB, swval) -> do
    (o0, o1) <- benesSwitch (name ++ "_O" ++ show idx) swval accT accB
    return [o0, o1]
  return $ case length accB `mod` 2 == 1 of
    True  -> res ++ [last accB]
    False -> res ++ [last accT, last accB]

-- | Turn a list into successive pairs (not sliding!) and possibly a final value
toPairs :: [a] -> ([(a, a)], Maybe a)
toPairs accesses = (reverse res, rem)
 where
  tPH acc []           = (acc, Nothing)
  tPH acc (l     : []) = (acc, Just l)
  tPH acc (l : m : ls) = tPH ((l, m) : acc) ls
  (res, rem) = tPH [] accesses

-- | A list of pairs of adjacent elements
slidingPairs :: [a] -> [(a, a)]
slidingPairs l = case l of
  x : y : l' -> (x, y) : slidingPairs (y : l')
  _          -> []


-- | Emit constraints checking a trace's validity
checkTrace :: Trace -> Assert ()
checkTrace t@(Trace _ _ _ as) = if null as
  then return ()
  else do
  -- Attach indices to trace
    let it = indexTrace t
    -- Apply routing network
    ITrace _ _ d accesses <- benesRoute it
    -- Check first
    let (r, _, _, v) = head accesses -- safe b/c null check
    A.assert $ BoolBinExpr Implies r $ mkEq v d
    -- Check each pair
    forM_ (slidingPairs accesses) $ \((_, i, a, v), (r, i', a', v')) ->
      let deltaA = mkDynBvBinPred BvUgt a' a
          deltaI = BoolNaryExpr And [mkEq a' a, mkDynBvBinPred BvUgt i' i]
      in  do
        -- Check sort
            A.assert $ BoolNaryExpr Or [deltaA, deltaI]
            -- Check read consistency
            A.assert $ BoolBinExpr Implies r $ mkEq v' $ Ite deltaA d v

indexTrace :: Trace -> ITrace
indexTrace (Trace name size def accesses) =
  let idxWidth =
          ceiling $ logBase (2.0 :: Double) $ fromIntegral $ length accesses
      idxs = map (DynBvLit . Bv.bitVec idxWidth) [(0 :: Int) ..]
  in  ITrace name size def
        $ zipWith (\i (Access r a v) -> (r, i, a, v)) idxs accesses


-- Entry point to the Benes-routing memory transformation
benesPass :: Assert ()
benesPass = do
  ts <- extractTraces
  logIf "smt::opt::trace" $ pShow ts
  forM_ ts checkTrace

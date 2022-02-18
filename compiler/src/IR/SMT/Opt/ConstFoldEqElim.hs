{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module IR.SMT.Opt.ConstFoldEqElim
  ( constantFold
  , constFoldEqElim
  )
where
import           IR.SMT.TySmt
import           IR.SMT.Util
import           IR.SMT.TySmt.Alg               ( mapTerm
                                                , vars
                                                )
import qualified IR.SMT.Opt.Assert             as OA
import           IR.SMT.Opt.Assert              ( Assert )

import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDyn
                                                )
import           Data.Either                    ( partitionEithers )
import           Data.Field.Galois              ( Prime
                                                , toP
                                                , fromP
                                                )
import           Data.List                      ( foldl'
                                                , foldl1'
                                                )
import           Data.Hashable                  ( Hashable )
import qualified Data.HashMap.Strict           as HMap
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq
import           GHC.TypeLits
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Util.Control                   ( whenM
                                                , unlessM
                                                )
import           Util.Log

pattern PfLit :: forall s. () => forall n. (s ~ PfSort n, KnownNat n) => Integer -> Term s
pattern PfLit i = IntToPf (IntLit i)

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
    DynBvExtractBit i b ->
      let b' = constantFold b
      in  Just $ case b' of
            DynBvLit b'' -> BoolLit $ Bv.testBit b'' i
            _            -> DynBvExtractBit i b'
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
            (BoolLit  a'', BoolLit b'' ) -> Just $ BoolLit (a'' == b'')
            (DynBvLit a'', DynBvLit b'') -> Just $ BoolLit (a'' == b'')
            (PfLit    a'', PfLit b''   ) -> Just $ BoolLit (a'' == b'')
            _                            -> Just $ Eq a' b'
    PfToDynBv w p ->
      let p' = constantFold p
      in  Just $ case p' of
            PfLit i -> DynBvLit $ Bv.bitVec w i
            _       -> PfToDynBv w p'
    IntToDynBv w i ->
      let i' = constantFold i
      in  Just $ case i' of
            IntLit i'' -> DynBvLit $ Bv.bitVec w i''
            _          -> IntToDynBv w i'
    DynBvBinExpr op w a b ->
      let a' = constantFold a
          b' = constantFold b
      in  case (a', b') of
            (DynBvLit a'', DynBvLit b'') ->
              Just $ DynBvLit $ opConstConst a'' b''
            (a'', DynBvLit b'') | isJust (opTermConst a'' b'') ->
              opTermConst a'' b''
            (DynBvLit a'', b'') | isJust (opConstTerm a'' b'') ->
              opConstTerm a'' b''
            _ -> Just $ DynBvBinExpr op w a' b'
     where
      bvizeIntOp f x y = Bv.bitVec (Bv.size x) $ f (Bv.uint x) (Bv.uint y)
      -- Operator applied to two contants inputs
      opConstConst = case op of
        BvSub  -> bvizeIntOp (-)
        BvUdiv -> smtDiv
        BvUrem -> smtRem
        BvShl  -> Bv.shl
        BvAshr -> Bv.ashr
        BvLshr -> Bv.shr
      opTermConst :: TermDynBv -> Bv.BV -> Maybe TermDynBv
      opTermConst l r = case op of
        BvSub | r == 0 -> Just l
        BvLshr         -> if rNat < toInteger w
          then Just $ mkDynBvConcat (DynBvLit $ Bv.zeros rInt)
                                    (mkDynBvExtract rInt (w - rInt) l)
          else shiftE
        BvAshr -> if rNat < toInteger w
          then Just $ mkDynBvSext w $ mkDynBvExtract rInt (w - rInt) l
          else shiftE
        BvShl -> if rNat < toInteger w
          then Just $ mkDynBvConcat (mkDynBvExtract 0 (w - rInt) l)
                                    (DynBvLit $ Bv.zeros rInt)
          else shiftE
        _ -> Nothing
       where
        w    = Bv.size r
        rNat = Bv.nat r
        rInt = fromIntegral rNat
        shiftE =
          error
            $  "Shifting "
            ++ show l
            ++ " by "
            ++ show r
            ++ " is an overshift"
      opConstTerm _l _r = Nothing
    DynBvNaryExpr op w a ->
      let a'           = map constantFold a
          (ls, consts) = partitionEithers $ map
            (\case
              DynBvLit l -> Right l
              e          -> Left e
            )
            a'
          c = nOpConsts consts
      in  if null ls then Just $ DynBvLit c else nOpTermsConst ls c
     where
      nOpConsts :: [Bv.BV] -> Bv.BV
      nOpConsts = case op of
        BvOr  -> foldl' (Bv..|.) (Bv.zeros w)
        BvAnd -> foldl' (Bv..&.) (Bv.ones w)
        BvXor -> foldl' Bv.xor (Bv.zeros w)
        BvAdd -> foldl' (bvizeIntOp (+)) (Bv.zeros w)
        BvMul -> foldl' (bvizeIntOp (*)) (Bv.bitVec w (1 :: Integer))
      bvizeIntOp f x y = Bv.bitVec (Bv.size x) $ f (Bv.uint x) (Bv.uint y)
      safeNary ls = if length ls > 1 then DynBvNaryExpr op w ls else head ls
      nOpTermsConst :: [TermDynBv] -> Bv.BV -> Maybe TermDynBv
      nOpTermsConst l r = case op of
        BvAdd | r == 0         -> Just $ safeNary l
        BvMul | r == 0         -> Just (DynBvLit r)
        BvMul | r == 1         -> Just $ safeNary l
        BvAnd | r == Bv.ones w -> Just $ safeNary l
        BvAnd                  -> Just $ bitwise foldAnd
        BvOr | r == 0          -> Just $ safeNary l
        BvOr                   -> Just $ bitwise foldOr
        BvXor | r == 0         -> Just $ safeNary l
        BvXor                  -> Just $ bitwise foldXor
        _                      -> Nothing
       where
        foldAnd, foldOr, foldXor :: Bool -> [TermBool] -> TermBool
        foldAnd cbit = if cbit then BoolNaryExpr And else const (BoolLit False)
        foldOr cbit = if cbit then const (BoolLit True) else BoolNaryExpr Or
        foldXor cbit = (if cbit then Not else id) . BoolNaryExpr Xor
        bitwise f = foldl1' mkDynBvConcat $ map
          (\i -> BoolToDynBv $ f (Bv.testBit r i) (map (mkDynBvExtractBit i) l))
          [0 .. (w - 1)]


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
    p@(PfUnExpr{}  ) -> visitPf p
    p@(PfNaryExpr{}) -> visitPf p
    _                -> Nothing

  negateBool :: TermBool -> TermBool
  negateBool (Not a) = a
  negateBool a       = Not a

  visitPf :: forall n . KnownNat n => TermPf n -> Maybe (TermPf n)
  visitPf t = case t of
    PfUnExpr PfNeg p ->
      Just
        $ let p' = constantFold p
          in  case p' of
                PfLit i -> PfLit (fromP @(Prime n) $ negate $ toP @n i)
                _       -> PfUnExpr PfNeg p'
    PfUnExpr PfRecip p ->
      Just
        $ let p' = constantFold p
          in  case p' of
                PfLit i -> PfLit (fromP @(Prime n) $ recip $ toP @n i)
                _       -> PfUnExpr PfNeg p'
    PfNaryExpr PfAdd ps ->
      Just
        $ let (c, l) =
                foldl'
                    (\(accConst, accList) entry -> case entry of
                      PfLit i -> (toP @n i + accConst, accList)
                      _       -> (accConst, entry : accList)
                    )
                    (0, [])
                  $ map constantFold ps
          in  if null l
                then PfLit (fromP @(Prime n) c)
                else if c == 0
                  then if length l == 1 then head l else PfNaryExpr PfAdd l
                  else PfNaryExpr PfAdd (PfLit (fromP @(Prime n) c) : l)
    PfNaryExpr PfMul ps ->
      Just
        $ let (c, l) =
                foldl'
                    (\(accConst, accList) entry -> case entry of
                      PfLit i -> (toP @n i * accConst, accList)
                      _       -> (accConst, entry : accList)
                    )
                    (1, [])
                  $ map constantFold ps
          in  if null l || c == 0
                then PfLit (fromP @(Prime n) c)
                else if c == 1
                  then if length l == 1 then head l else PfNaryExpr PfMul l
                  else PfNaryExpr PfMul (PfLit (fromP @(Prime n) c) : l)
    _ -> Nothing


data ConstFoldEqState = ConstFoldEqState
  { _terms  :: !(IntMap.IntMap TermBool)
  , _uses   :: !(HMap.HashMap String IntSet.IntSet)
  , _consts :: !(HMap.HashMap String Dynamic)
  , _queue  :: Seq.Seq Int
  , _queued :: IntSet.IntSet
  }
  deriving Show

$(makeLenses ''ConstFoldEqState)

newtype ConstFoldEq a = ConstFoldEq (StateT ConstFoldEqState Log a)
    deriving (Functor, Applicative, Monad, MonadState ConstFoldEqState, MonadIO, MonadLog)

toIntSetMap :: (Eq a, Hashable a) => [(a, Int)] -> HMap.HashMap a IntSet.IntSet
toIntSetMap = foldl'
  (\m (k, v) ->
    HMap.alter (Just . maybe (IntSet.singleton v) (IntSet.insert v)) k m
  )
  HMap.empty

isConst :: SortClass s => Term s -> Bool
isConst t = case t of
  BoolLit{}          -> True
  DynBvLit{}         -> True
  IntLit{}           -> True
  IntToPf (IntLit{}) -> True
  _                  -> False

constFoldEqElim :: Assert ()
constFoldEqElim = do
  noElim <- gets (OA._public)
  OA.modifyAssertions (constFoldEqElimFn noElim)
  OA.refresh

constFoldEqElimFn :: Set.Set String -> [TermBool] -> Log [TermBool]
constFoldEqElimFn noElim ts =
  filter (/= BoolLit True)
    .   map snd
    .   IntMap.toAscList
    .   _terms
    <$> execStateT action initState
 where
  ConstFoldEq action = stepToFixPoint

  ids                = [0 .. (length ts - 1)]
  ts'                = zip ids $ map constantFold ts
  uses' = toIntSetMap [ (v, i) | (i, t) <- ts', v <- Set.toList $ vars t ]


  initState          = ConstFoldEqState { _terms  = IntMap.fromList $ ts'
                                        , _uses   = uses'
                                        , _consts = HMap.empty
                                        , _queue  = Seq.fromList ids
                                        , _queued = IntSet.fromList ids
                                        }

  dequeue :: ConstFoldEq (Maybe Int)
  dequeue = do
    q <- gets (view queue)
    case q of
      Seq.Empty   -> return Nothing
      h Seq.:<| t -> do
        modify (set queue t)
        modify (over queued $ IntSet.delete h)
        return (Just h)

  enqueue :: Int -> ConstFoldEq ()
  enqueue id' = do
    there <- gets (IntSet.member id' . view queued)
    unless there $ do
      modify $ over queued $ IntSet.insert id'
      modify $ over queue (Seq.|> id')

  stepToFixPoint :: ConstFoldEq ()
  stepToFixPoint = step >> unlessM (gets (Seq.null . _queue)) stepToFixPoint

  step :: ConstFoldEq ()
  step = do
    mI <- dequeue
    forM_ mI $ \i -> do
      cs   <- gets $ _consts
      preA <- gets $ (IntMap.! i) . view terms
      logIf "smt::opt::cfee" $ "Checking " ++ show preA
      logIf "smt::opt::cfee" $ "Subbed   " ++ show (subAll cs preA)
      modify $ over terms $ IntMap.adjust (constantFold . subAll cs) i
      a <- gets ((IntMap.! i) . view terms)
      logIf "smt::opt::cfee" $ "Elimed   " ++ show a
      subst <- case a of
        Eq (Var v _s) t | v `Set.notMember` noElim && isConst t -> do
          logIf "smt::opt::cfee" $ "Sub " ++ show v ++ " to " ++ show t
          return $ Just (v, toDyn t)
        Eq t (Var v _s) | v `Set.notMember` noElim && isConst t -> do
          logIf "smt::opt::cfee" $ "Sub " ++ show v ++ " to " ++ show t
          return $ Just (v, toDyn t)
        _ -> return $ Nothing
      forM_ subst $ \(var, val) -> do
        modify $ over consts $ HMap.insert var val
        vUses <- gets (fromMaybe IntSet.empty . HMap.lookup var . view uses)
        modify $ over uses $ HMap.delete var
        modify $ over terms $ IntMap.delete i
        forM_ (IntSet.toList vUses) $ \useI -> do
          when (useI /= i) $ do
            whenM (gets (IntMap.member useI . view terms)) $ enqueue useI
  subAll :: SortClass s => HMap.HashMap String Dynamic -> Term s -> Term s
  subAll subs = mapTerm visit
   where
    visit :: forall t . SortClass t => Term t -> Maybe (Term t)
    visit term = case term of
      Var name' _ -> case HMap.lookup name' subs of
        Just e  -> Just $ fromDyn @(Term t) e (error "wrong sort")
        Nothing -> Nothing
      _ -> Nothing

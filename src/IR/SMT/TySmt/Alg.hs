{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
module IR.SMT.TySmt.Alg
  ( mapTerm
  , mapTermM
  , reduceTerm
  , depth
  , eval
  , nNodes
  , vars
  , nChars
  , checkSortDeep
  , valueToTerm
  )
where

import           Control.Exception              ( throw )
import           Control.Monad                  ( liftM
                                                , liftM2
                                                , liftM3
                                                , forM_
                                                )
import           Data.Bits                     as Bits
import qualified Data.BitVector                as Bv
import           Data.Dynamic                   ( Dynamic
                                                , Typeable
                                                , fromDyn
                                                , toDyn
                                                )
import           Data.List                      ( foldl' )
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Maybe                    as Maybe
import           Data.Proxy                     ( Proxy(..) )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           GHC.TypeLits
import           Prelude                 hiding ( exp )
import           IR.SMT.TySmt
import qualified IR.SMT.TySmt.DefaultMap       as DMap
import           IR.SMT.TySmt.DefaultMap        ( DefaultMap )

-- |
-- Given a function that optionally transforms a term, traverses the term
-- applying that function at every stage. When the function returns something,
-- this is the transformation. When the function does not, the transformation
-- recurses.
-- TODO: Define this in terms of mapTermM, and measure perf.
mapTerm
  :: SortClass s
  => (forall t . SortClass t => Term t -> Maybe (Term t))
  -> Term s
  -> Term s
mapTerm f t = case f t of
  Nothing -> case t of
    BoolLit{}             -> t
    BoolBinExpr o l r     -> BoolBinExpr o (mapTerm f l) (mapTerm f r)
    BoolNaryExpr o as     -> BoolNaryExpr o (map (mapTerm f) as)
    Not s                 -> Not (mapTerm f s)

    Ite c tt ff           -> Ite (mapTerm f c) (mapTerm f tt) (mapTerm f ff)
    Var{}                 -> t
    Exists v s tt         -> Exists v s (mapTerm f tt)
    Let    v s e          -> Let v (mapTerm f s) (mapTerm f e)
    Eq        a b         -> Eq (mapTerm f a) (mapTerm f b)

    BvConcat  a b         -> BvConcat (mapTerm f a) (mapTerm f b)
    BvExtract i s         -> BvExtract i (mapTerm f s)
    BvBinExpr o l r       -> BvBinExpr o (mapTerm f l) (mapTerm f r)
    BvBinPred o l r       -> BvBinPred o (mapTerm f l) (mapTerm f r)
    BvNaryExpr o a        -> BvNaryExpr o (map (mapTerm f) a)
    BvUnExpr   o r        -> BvUnExpr o (mapTerm f r)
    IntToBv   tt          -> IntToBv (mapTerm f tt)
    FpToBv    tt          -> FpToBv (mapTerm f tt)

    StatifyBv t'          -> StatifyBv (mapTerm f t')
    RoundFpToDynBv w s t' -> RoundFpToDynBv w s (mapTerm f t')
    DynBvBinExpr o w a b  -> DynBvBinExpr o w (mapTerm f a) (mapTerm f b)
    DynBvNaryExpr o w a   -> DynBvNaryExpr o w (map (mapTerm f) a)
    DynBvConcat   w a b   -> DynBvConcat w (mapTerm f a) (mapTerm f b)
    DynBvBinPred o w a b  -> DynBvBinPred o w (mapTerm f a) (mapTerm f b)
    DynBvExtract s l b    -> DynBvExtract s l (mapTerm f b)
    DynBvExtractBit i b   -> DynBvExtractBit i (mapTerm f b)
    DynBvUnExpr o w r     -> DynBvUnExpr o w (mapTerm f r)
    DynBvLit bv           -> DynBvLit bv
    DynBvSext w w' r      -> DynBvSext w w' (mapTerm f r)
    DynBvUext w w' r      -> DynBvUext w w' (mapTerm f r)
    DynamizeBv w b        -> DynamizeBv w (mapTerm f b)
    IntToDynBv w i        -> IntToDynBv w (mapTerm f i)
    PfToDynBv  w p        -> PfToDynBv w (mapTerm f p)
    BoolToDynBv b         -> BoolToDynBv (mapTerm f b)


    IntLit{}              -> t
    IntBinExpr o l r      -> IntBinExpr o (mapTerm f l) (mapTerm f r)
    IntNaryExpr o as      -> IntNaryExpr o (map (mapTerm f) as)
    IntUnExpr   o l       -> IntUnExpr o (mapTerm f l)
    IntBinPred o l r      -> IntBinPred o (mapTerm f l) (mapTerm f r)
    BvToInt       tt      -> BvToInt (mapTerm f tt)
    SignedBvToInt tt      -> SignedBvToInt (mapTerm f tt)
    BoolToInt     tt      -> BoolToInt (mapTerm f tt)
    PfToInt       tt      -> PfToInt (mapTerm f tt)

    Fp64Lit{}             -> t
    Fp32Lit{}             -> t
    FpBinExpr o l r       -> FpBinExpr o (mapTerm f l) (mapTerm f r)
    FpUnExpr o l          -> FpUnExpr o (mapTerm f l)
    FpBinPred o l r       -> FpBinPred o (mapTerm f l) (mapTerm f r)
    FpUnPred o l          -> FpUnPred o (mapTerm f l)
    FpFma a b c           -> FpFma (mapTerm f a) (mapTerm f b) (mapTerm f c)
    IntToFp    tt         -> IntToFp (mapTerm f tt)
    FpToFp     tt         -> FpToFp (mapTerm f tt)
    BvToFp     tt         -> BvToFp (mapTerm f tt)
    DynUbvToFp tt         -> DynUbvToFp (mapTerm f tt)
    DynSbvToFp tt         -> DynSbvToFp (mapTerm f tt)

    PfNaryExpr o as       -> PfNaryExpr o (map (mapTerm f) as)
    PfUnExpr   o l        -> PfUnExpr o (mapTerm f l)
    IntToPf tt            -> IntToPf (mapTerm f tt)

    Select a k            -> Select (mapTerm f a) (mapTerm f k)
    Store a k v           -> Store (mapTerm f a) (mapTerm f k) (mapTerm f v)
    ConstArray s i        -> ConstArray s (mapTerm f i)
  Just s -> s

-- |
-- Given a function that monadically maps a term to an optional term, traverses
-- the term (left-to-right), applying the action when appropriate.
--
-- If no term is returned, does a default recursion.
mapTermM
  :: forall m s
   . (Monad m, SortClass s)
  => (forall t . SortClass t => Term t -> m (Maybe (Term t)))
  -> Term s
  -> m (Term s)
mapTermM f t = do
  a <- f t
  case a of
    Nothing -> case t of
      BoolLit{}             -> return t
      BoolBinExpr o l r     -> liftM2 (BoolBinExpr o) (rec l) (rec r)
      BoolNaryExpr o as     -> fmap (BoolNaryExpr o) (mapM rec as)
      Not s                 -> fmap Not (rec s)

      Ite c tt ff           -> liftM3 Ite (rec c) (rec tt) (rec ff)
      Var{}                 -> return t
      Exists v s tt         -> fmap (Exists v s) (rec tt)
      Let    v s e          -> liftM2 (Let v) (rec s) (rec e)
      Eq        a b         -> liftM2 Eq (rec a) (rec b)

      BvConcat  a b         -> liftM2 BvConcat (rec a) (rec b)
      BvExtract i s         -> liftM (BvExtract i) (rec s)
      BvBinExpr o l r       -> liftM2 (BvBinExpr o) (rec l) (rec r)
      BvNaryExpr o l        -> liftM (BvNaryExpr o) (mapM rec l)
      BvBinPred o l r       -> liftM2 (BvBinPred o) (rec l) (rec r)
      BvUnExpr o r          -> liftM (BvUnExpr o) (rec r)
      IntToBv   tt          -> liftM IntToBv (rec tt)
      FpToBv    tt          -> liftM FpToBv (rec tt)

      StatifyBv t'          -> liftM StatifyBv (rec t')
      RoundFpToDynBv w s t' -> liftM (RoundFpToDynBv w s) (rec t')
      DynBvBinExpr o w a b  -> liftM2 (DynBvBinExpr o w) (rec a) (rec b)
      DynBvNaryExpr o w a   -> liftM (DynBvNaryExpr o w) (mapM rec a)
      DynBvConcat   w a b   -> liftM2 (DynBvConcat w) (rec a) (rec b)
      DynBvBinPred o w a b  -> liftM2 (DynBvBinPred o w) (rec a) (rec b)
      DynBvExtract s l b    -> liftM (DynBvExtract s l) (rec b)
      DynBvExtractBit i b   -> liftM (DynBvExtractBit i) (rec b)
      DynBvUnExpr o w r     -> liftM (DynBvUnExpr o w) (rec r)
      DynBvLit bv           -> return $ DynBvLit bv
      DynBvSext w w' r      -> liftM (DynBvSext w w') (rec r)
      DynBvUext w w' r      -> liftM (DynBvUext w w') (rec r)
      DynamizeBv w b        -> liftM (DynamizeBv w) (rec b)
      IntToDynBv w i        -> liftM (IntToDynBv w) (rec i)
      PfToDynBv  w i        -> liftM (PfToDynBv w) (rec i)
      BoolToDynBv i         -> liftM BoolToDynBv (rec i)


      IntLit{}              -> return t
      IntBinExpr o l r      -> liftM2 (IntBinExpr o) (rec l) (rec r)
      IntNaryExpr o as      -> liftM (IntNaryExpr o) (mapM (rec) as)
      IntUnExpr   o l       -> liftM (IntUnExpr o) (rec l)
      IntBinPred o l r      -> liftM2 (IntBinPred o) (rec l) (rec r)
      BvToInt       tt      -> liftM BvToInt (rec tt)
      SignedBvToInt tt      -> liftM SignedBvToInt (rec tt)
      BoolToInt     tt      -> liftM BoolToInt (rec tt)
      PfToInt       tt      -> liftM PfToInt (rec tt)

      Fp64Lit{}             -> return t
      Fp32Lit{}             -> return t
      FpBinExpr o l r       -> liftM2 (FpBinExpr o) (rec l) (rec r)
      FpUnExpr o l          -> liftM (FpUnExpr o) (rec l)
      FpBinPred o l r       -> liftM2 (FpBinPred o) (rec l) (rec r)
      FpUnPred o l          -> liftM (FpUnPred o) (rec l)
      FpFma a b c           -> liftM3 FpFma (rec a) (rec b) (rec c)
      IntToFp    tt         -> liftM IntToFp (rec tt)
      FpToFp     tt         -> liftM FpToFp (rec tt)
      BvToFp     tt         -> liftM BvToFp (rec tt)
      DynUbvToFp tt         -> liftM DynUbvToFp (rec tt)
      DynSbvToFp tt         -> liftM DynSbvToFp (rec tt)

      PfNaryExpr o as       -> liftM (PfNaryExpr o) (mapM (rec) as)
      PfUnExpr   o l        -> liftM (PfUnExpr o) (rec l)
      IntToPf tt            -> liftM IntToPf (rec tt)

      Select a k            -> liftM2 Select (rec a) (rec k)
      Store a k v           -> liftM3 Store (rec a) (rec k) (rec v)
      ConstArray s i        -> liftM (ConstArray s) (rec i)
    Just s -> return $ s
 where
  rec :: SortClass s' => Term s' -> m (Term s')
  rec = mapTermM f

reduceTerm
  :: SortClass s
  => (forall t . SortClass t => Term t -> Maybe k)
  -> k
  -> (k -> k -> k)
  -> Term s
  -> k
reduceTerm mapF i foldF t = case mapF t of
  Nothing -> case t of
    BoolLit{} -> i
    BoolBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BoolNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    Not s             -> reduceTerm mapF i foldF s

    Ite c tt ff       -> foldF
      (foldF (reduceTerm mapF i foldF c) (reduceTerm mapF i foldF tt))
      (reduceTerm mapF i foldF ff)
    Var{} -> i
    Exists _ _ tt -> reduceTerm mapF i foldF tt
    Let _ s e -> foldF (reduceTerm mapF i foldF s) (reduceTerm mapF i foldF e)
    Eq tt ff -> foldF (reduceTerm mapF i foldF tt) (reduceTerm mapF i foldF ff)

    BvConcat a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    BvUnExpr  _ s -> reduceTerm mapF i foldF s
    BvExtract _ s -> reduceTerm mapF i foldF s
    BvBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvNaryExpr _ a -> foldr foldF i (map (reduceTerm mapF i foldF) a)
    BvBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntToBv   tt          -> reduceTerm mapF i foldF tt
    FpToBv    tt          -> reduceTerm mapF i foldF tt

    StatifyBv t'          -> reduceTerm mapF i foldF t'
    RoundFpToDynBv _ _ t' -> reduceTerm mapF i foldF t'
    DynBvBinExpr _ _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvNaryExpr _ _ a -> foldr foldF i (map (reduceTerm mapF i foldF) a)
    DynBvConcat _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvBinPred _ _ a b ->
      foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b)
    DynBvExtract _ _ b  -> reduceTerm mapF i foldF b
    DynBvExtractBit _ b -> reduceTerm mapF i foldF b
    DynBvUnExpr _ _ b   -> reduceTerm mapF i foldF b
    DynBvLit{}          -> i
    DynBvSext _ _ b     -> reduceTerm mapF i foldF b
    DynBvUext _ _ b     -> reduceTerm mapF i foldF b
    DynamizeBv _ b      -> reduceTerm mapF i foldF b
    IntToDynBv _ i'     -> reduceTerm mapF i foldF i'
    PfToDynBv  _ i'     -> reduceTerm mapF i foldF i'
    BoolToDynBv i'      -> reduceTerm mapF i foldF i'


    IntLit{}            -> i
    IntBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    IntNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    IntUnExpr   _ l  -> reduceTerm mapF i foldF l
    IntBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    BvToInt       tt -> reduceTerm mapF i foldF tt
    SignedBvToInt tt -> reduceTerm mapF i foldF tt
    BoolToInt     tt -> reduceTerm mapF i foldF tt
    PfToInt       tt -> reduceTerm mapF i foldF tt

    Fp64Lit{}        -> i
    Fp32Lit{}        -> i
    FpBinExpr _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    FpUnExpr _ l -> reduceTerm mapF i foldF l
    FpBinPred _ l r ->
      foldF (reduceTerm mapF i foldF l) (reduceTerm mapF i foldF r)
    FpUnPred _ l -> reduceTerm mapF i foldF l
    FpFma a b c  -> foldF
      (foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF b))
      (reduceTerm mapF i foldF c)
    IntToFp    tt   -> reduceTerm mapF i foldF tt
    FpToFp     tt   -> reduceTerm mapF i foldF tt
    BvToFp     tt   -> reduceTerm mapF i foldF tt
    DynUbvToFp tt   -> reduceTerm mapF i foldF tt
    DynSbvToFp tt   -> reduceTerm mapF i foldF tt

    PfNaryExpr _ as -> foldr foldF i (map (reduceTerm mapF i foldF) as)
    PfUnExpr   _ l  -> reduceTerm mapF i foldF l
    IntToPf tt      -> reduceTerm mapF i foldF tt

    Select a k -> foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k)
    Store a k v     -> foldF
      (foldF (reduceTerm mapF i foldF a) (reduceTerm mapF i foldF k))
      (reduceTerm mapF i foldF v)
    ConstArray _s v -> reduceTerm mapF i foldF v
  Just s -> s

depth :: SortClass s => Term s -> Int
depth = reduceTerm (const Nothing) 0 (\a b -> 1 + max a b)

nNodes :: SortClass s => Term s -> Int
nNodes = reduceTerm (const Nothing) 1 ((+) . (1 +))

vars :: SortClass s => Term s -> Set String
vars = reduceTerm visit Set.empty Set.union
 where
  visit t = case t of
    Var s _ -> Just $ Set.singleton s
    _       -> Nothing

nChars :: SortClass s => Term s -> Int
nChars = reduceTerm visit 0 (+)
 where
  visit t = case t of
    Var s _ -> Just $ length s
    _       -> Nothing

type Env = Map String Dynamic

boolBinFn :: BoolBinOp -> Bool -> Bool -> Bool
boolBinFn op = case op of
  Implies -> \a b -> not a || b

boolNaryFn :: BoolNaryOp -> [Bool] -> Bool
boolNaryFn op = case op of
  And -> and
  Or  -> or
  Xor -> foldr Bits.xor False

intUnFn :: IntUnOp -> Integer -> Integer
intUnFn op = case op of
  IntNeg -> (0 -)
  IntAbs -> abs

intBinFn :: IntBinOp -> Integer -> Integer -> Integer
intBinFn op = case op of
  IntSub -> (-)
  IntDiv -> div
  IntMod -> rem
  IntShl -> \a b -> Bits.shiftL a (fromIntegral b)
  IntShr -> \a b -> Bits.shiftR a (fromIntegral b)
  IntPow -> (^)

intBinPredFn :: IntBinPred -> Integer -> Integer -> Bool
intBinPredFn op = case op of
  IntGe -> (>=)
  IntLe -> (<=)
  IntGt -> (>)
  IntLt -> (<)

intNaryFn :: IntNaryOp -> [Integer] -> Integer
intNaryFn op = case op of
  IntAdd -> sum
  IntMul -> product

extGCD :: Integer -> Integer -> (Integer, Integer, Integer)
extGCD a 0 = (1, 0, a)     -- Base case
extGCD a b = (x, c - q * x, y)
 where
  q         = a `div` b
  r         = a `rem` b
  (c, x, y) = extGCD b r

invMod :: Integer -> Integer -> Integer
invMod x m = x' where (x', _, _) = extGCD x m

pfUnFn :: PfUnOp -> Integer -> Integer -> Integer
pfUnFn op m = case op of
  PfNeg   -> (m -)
  PfRecip -> flip invMod m


pfNaryFn :: PfNaryOp -> Integer -> [Integer] -> Integer
pfNaryFn op m = case op of
  PfAdd -> foldr (\a b -> (a + b) `rem` m) 0
  PfMul -> foldr (\a b -> (a * b) `rem` m) 1

bvUnFn :: BvUnOp -> Bv.BV -> Bv.BV
bvUnFn op = case op of
  BvNot -> Bv.not
  BvNeg -> negate

bvBinFn :: BvBinOp -> Bv.BV -> Bv.BV -> Bv.BV
bvBinFn op = case op of
  BvShl  -> Bv.shl
  BvLshr -> Bv.shr
  BvAshr -> Bv.ashr
  BvUrem -> rem
  BvUdiv -> div
  BvSub  -> (-)

bvNaryFn :: BvNaryOp -> Bv.BV -> [Bv.BV] -> Bv.BV
bvNaryFn op = case op of
  BvAdd -> foldl' (+)
  BvMul -> foldl' (*)
  BvOr  -> foldl' (Bv..|.)
  BvAnd -> foldl' (Bv..&.)
  BvXor -> foldl' Bv.xor

bvNaryId :: forall n . KnownNat n => [TermBv n] -> BvNaryOp -> Bv.BV
bvNaryId _bs op = case op of
  BvAdd -> Bv.zeros (fromIntegral $ natVal $ Proxy @n)
  BvMul -> Bv.bitVec (fromIntegral $ natVal $ Proxy @n) (1 :: Integer)
  BvOr  -> Bv.zeros (fromIntegral $ natVal $ Proxy @n)
  BvAnd -> Bv.ones (fromIntegral $ natVal $ Proxy @n)
  BvXor -> Bv.zeros (fromIntegral $ natVal $ Proxy @n)

dynBvNaryId :: Int -> BvNaryOp -> Bv.BV
dynBvNaryId w op = case op of
  BvAdd -> Bv.zeros w
  BvMul -> Bv.bitVec w (1 :: Integer)
  BvOr  -> Bv.zeros w
  BvAnd -> Bv.ones w
  BvXor -> Bv.zeros w

bvBinPredFn :: BvBinPred -> Bv.BV -> Bv.BV -> Bool
bvBinPredFn op = case op of
  BvUgt   -> (Bv.>.)
  BvUge   -> (Bv.>=.)
  BvUlt   -> (Bv.<.)
  BvUle   -> (Bv.<=.)
  BvSgt   -> Bv.sgt
  BvSge   -> Bv.sge
  BvSlt   -> Bv.slt
  BvSle   -> Bv.sle
  BvSaddo -> \a b ->
    let s = Bv.size a
        v = Bv.int a + Bv.int b
    in  v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v
  BvSsubo -> \a b ->
    let s = Bv.size a
        v = Bv.int a - Bv.int b
    in  v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v
  BvSmulo -> \a b ->
    let s = Bv.size a
        v = Bv.int a * Bv.int b
    in  v < -2 ^ (s - 1) || 2 ^ (s - 1) - 1 < v

modulus :: forall n . KnownNat n => TermPf n -> Integer
modulus _ = natVal (Proxy :: Proxy n)

size :: forall n . KnownNat n => TermBv n -> Int
size _ = fromIntegral $ natVal $ Proxy @n

eval :: forall s . Typeable s => Env -> Term s -> Value s
eval e t = case t of
  BoolLit b -> ValBool b
  BoolBinExpr o l r ->
    ValBool $ boolBinFn o (valAsBool $ eval e l) (valAsBool $ eval e r)
  BoolNaryExpr o as -> ValBool $ boolNaryFn o (map (valAsBool . eval e) as)
  Not s             -> (ValBool . not . valAsBool . eval e) s

  Ite c tt ff       -> if valAsBool $ eval e c then eval e tt else eval e ff
  Eq  tt ff         -> ValBool $ eval e tt == eval e ff
  Var s  _          -> typed
   where
    entry = Map.findWithDefault
      (error $ "Unknown identifier '" ++ s ++ "'" ++ show e)
      s
      e
    typed = fromDyn
      entry
      (  error
      $  "Indentifier '"
      ++ show s
      ++ "' of wrong sort.\nValue: "
      ++ show entry
      )
  Exists{}   -> error "Cannot evaluate existential quantifiers!"
  Let x s t' -> eval e' t'
   where
    v  = eval e s
    e' = Map.insert x (toDyn v) e

  BvConcat  a      b  -> ValBv $ valAsBv (eval e a) `mappend` valAsBv (eval e b)
  BvUnExpr  o      t' -> ValBv $ bvUnFn o $ valAsBv (eval e t')
  BvExtract start' t' -> bvExtract e start' t'
   where
    bvExtract
      :: forall n i
       . (KnownNat n, KnownNat i, i <= n)
      => Env
      -> Int
      -> TermBv n
      -> Value (BvSort i)
    bvExtract env start term = ValBv $ Bv.extract
      (min (oldSize - 1) (start + newSize - 1))
      start
      (valAsBv $ eval env term)
     where
      oldSize = fromInteger $ natVal (Proxy :: Proxy n)
      newSize = fromInteger $ natVal (Proxy :: Proxy i)
  BvBinExpr o l r ->
    ValBv $ bvBinFn o (valAsBv $ eval e l) (valAsBv $ eval e r)
  BvNaryExpr o rs ->
    ValBv $ bvNaryFn o (bvNaryId rs o) (map (valAsBv . eval e) rs)
  BvBinPred o l r ->
    ValBool $ bvBinPredFn o (valAsBv $ eval e l) (valAsBv $ eval e r)
  IntToBv i  -> ValBv $ Bv.bitVec (size t) $ valAsInt $ eval e i
  FpToBv  tt -> ValBv $ asBits $ asRepr (eval e tt)

  StatifyBv t' ->
    let inner = valAsDynBv $ eval e t'
        width = dynBvWidth t'
    in  if Bv.width inner == width
          then ValBv inner
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  -- TODO: check this
  RoundFpToDynBv w _ t' ->
    let i :: Integer = round $ asRepr $ eval e t' in ValDynBv $ Bv.bitVec w i
  DynBvUnExpr o w t' ->
    let inner = valAsDynBv $ eval e t'
    in  if Bv.width inner == w
          then ValDynBv $ bvUnFn o inner
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynBvLit bv -> ValDynBv bv
  DynBvUext _ wDelta t' ->
    ValDynBv $ Bv.zeroExtend wDelta $ valAsDynBv $ eval e t'
  DynBvSext _ wDelta t' ->
    ValDynBv $ Bv.signExtend wDelta $ valAsDynBv $ eval e t'
  DynBvExtract s w a ->
    let a' = valAsDynBv $ eval e a
    in  if s + w <= Bv.width a'
          then ValDynBv $ Bv.extract (s + w - 1) s a'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynBvExtractBit i a ->
    let a' = valAsDynBv $ eval e a
    in  if i <= Bv.width a'
          then ValBool $ Bv.testBit a' i
          else
            throw $ SortError $ "oob bit mis-match while evaluating " ++ show t
  DynBvBinExpr o w a b ->
    let a' = valAsDynBv $ eval e a
        b' = valAsDynBv $ eval e b
    in  if w == Bv.width a' && w == Bv.width b'
          then ValDynBv $ bvBinFn o a' b'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynBvNaryExpr o w rs ->
    ValDynBv $ bvNaryFn o (dynBvNaryId w o) (map (valAsDynBv . eval e) rs)
  DynBvConcat _ a b ->
    ValDynBv $ Bv.concat [valAsDynBv $ eval e a, valAsDynBv $ eval e b]
  DynBvBinPred o w a b ->
    let a' = valAsDynBv $ eval e a
        b' = valAsDynBv $ eval e b
    in  if w == Bv.width a' && w == Bv.width b'
          then ValBool $ bvBinPredFn o a' b'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  DynamizeBv w a ->
    let a' = valAsBv $ eval e a
    in  if w == Bv.width a'
          then ValDynBv a'
          else
            throw $ SortError $ "bitwidth mis-match while evaluating " ++ show t
  IntToDynBv w i -> ValDynBv $ Bv.bitVec w $ valAsInt $ eval e i
  PfToDynBv  w i -> ValDynBv $ Bv.bitVec w $ valAsPf $ eval e i
  BoolToDynBv i  -> ValDynBv $ Bv.bitVec 1 $ fromEnum $ valAsBool $ eval e i

  IntLit      i  -> ValInt i
  IntUnExpr o t' -> ValInt $ intUnFn o (valAsInt $ eval e t')
  IntBinExpr o l r ->
    ValInt $ intBinFn o (valAsInt $ eval e l) (valAsInt $ eval e r)
  IntNaryExpr o as -> ValInt $ intNaryFn o (map (valAsInt . eval e) as)
  IntBinPred o l r ->
    ValBool $ intBinPredFn o (valAsInt $ eval e l) (valAsInt $ eval e r)
  BvToInt       tt -> ValInt $ Bv.nat $ valAsBv $ eval e tt
  SignedBvToInt tt -> ValInt $ Bv.nat $ valAsBv $ eval e tt
  BoolToInt     t' -> ValInt $ if valAsBool (eval e t') then 1 else 0
  PfToInt       t' -> ValInt $ valAsPf $ eval e t'

  Fp64Lit       d  -> ValDouble d
  Fp32Lit       f  -> ValFloat f
  FpBinExpr o l r  -> evalBin o (eval e l) (eval e r)
  FpUnExpr o l     -> evalUn o (eval e l)
  FpBinPred o l r  -> evalBinPred o (eval e l) (eval e r)
  FpUnPred o l     -> evalUnPred o (eval e l)
  FpFma a b c      -> eval e (FpBinExpr FpAdd (FpBinExpr FpMul a b) c)
  IntToFp    tt    -> evalFromInt (eval e tt)
  FpToFp     tt    -> (fromRepr . asOther . asRepr) (eval e tt)
  BvToFp     tt    -> fromRepr $ fromBits $ valAsBv (eval e tt)
  DynUbvToFp tt    -> fromRepr $ fromIntegral $ Bv.nat $ valAsDynBv (eval e tt)
  DynSbvToFp tt    -> fromRepr $ fromIntegral $ Bv.nat $ valAsDynBv (eval e tt)

  PfUnExpr   o t'  -> ValPf $ pfUnFn o (modulus t') (valAsPf $ eval e t')
  PfNaryExpr o as  -> ValPf $ pfNaryFn o m (map (valAsPf . eval e) as)
    where m = modulus (PfNaryExpr o as)
  IntToPf t' -> ValPf $ valAsInt $ eval e t'

  Select a k -> Maybe.fromMaybe
    (error $ "Array has no entry for " ++ show k')
    (a' DMap.!? k')
   where
    a' = valAsArray $ eval e a
    k' = eval e k
  Store a k v ->
    ValArray $ DMap.insert (eval e k) (eval e v) (valAsArray $ eval e a)
  -- Not quite right
  ConstArray _ v -> newArray t $ Just $ eval e v

newArray
  :: forall k v
   . (Typeable k, Typeable v)
  => Term (ArraySort k v)
  -> Maybe (Value v)
  -> Value (ArraySort k v)
newArray _t v = case v of
  Just v' ->
    ValArray $ (DMap.emptyWithDefault v' :: (DefaultMap (Value k) (Value v)))
  Nothing -> ValArray $ (DMap.empty :: (DefaultMap (Value k) (Value v)))


-- | Traverse a term, sort-checking as you go.
-- Incomplete.
checkSortDeep :: SortClass s => Term s -> Either String (Term s)
checkSortDeep = mapTermM visit
 where
  visit :: SortClass s => Term s -> Either String (Maybe (Term s))
  visit t = case t of
    Eq a b ->
      let as = sort a
          bs = sort b
      in  if as == bs then Right Nothing else e
    DynBvConcat w a b ->
      if dynBvWidth a + dynBvWidth b == w then Right Nothing else e
    DynBvNaryExpr _o w xs ->
      forM_ xs (\x -> if dynBvWidth x == w then Right Nothing else e)
        >> return Nothing
    DynBvUext w dw x -> if dynBvWidth x + dw == w then Right Nothing else e
    -- TODO: finish
    _                -> Right Nothing
   where
    e :: Either String a
    e = Left $ "sort mismatch in " ++ show t

-- | Given a value, returns a term of the same sort that would evaluate to that
-- value, and has no variables.
valueToTerm :: SortClass s => Value s -> Term s
valueToTerm = undefined

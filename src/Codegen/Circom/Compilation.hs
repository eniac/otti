{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
-- Because of out KnownNat1 instance for the Log2 family...
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Codegen.Circom.Compilation
  ( CompCtx(..)
  , compMainCtx
  , compMainWitCtx
  , nSmtNodes
  , LC
  , QEQ
  , TemplateInvocation
  , Term(..)
  , LTerm(..)
  , LowDegTerm
  , CompState(..)
  , LowDegCtx(..)
  , LowDegCompCtx
  , WitBaseCtx(..)
  , WitBaseTerm(..)
  , WitCompCtx
  , empty
  , runLowDegCompState
  , runCompState
  , nPublicInputs
  , ctxOrderedSignals
  , load
  , getMainInvocation
  , ltermToSig
  )
where

import           AST.Circom
import qualified Codegen.Circom.Signal         as Sig
import qualified Codegen.Circom.Typing         as Typing

import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Array                    as Arr
import qualified Data.Bits                     as Bits
import           Data.Ix
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import           Data.Functor
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import qualified Data.Maybe                    as Maybe
import qualified Data.Either                   as Either
import qualified Data.Set                      as Set
import           Data.Proxy                     ( Proxy(Proxy) )
import           Debug.Trace                    ( trace
                                                )
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

mapGetE :: Ord k => String -> k -> Map.Map k v -> v
mapGetE = Map.findWithDefault . error


type LC s n = (Map.Map s n, n) -- A linear combination of signals and gen-time constants
type QEQ s n = (LC s n, LC s n, LC s n)

data LowDeg n = Scalar n
              | Linear (LC Sig.Signal n)
              | Quadratic (QEQ Sig.Signal n)
              | HighDegree
              deriving (Show,Eq,Ord)

spanE :: Span -> String -> a
spanE (Span path s e) m =
  error
    $  m
    ++ "\nLocation:\n\t"
    ++ path
    ++ "\n\tbetween "
    ++ show s
    ++ "\n\t    and "
    ++ show e

lcZero :: GaloisField k => LC s k
lcZero = (Map.empty, 0)

lcAdd :: (Ord s, GaloisField k) => LC s k -> LC s k -> LC s k
lcAdd (sm, sc) (tm, tc) = (Map.unionWith (+) sm tm, sc + tc)

lcSig :: (Ord s, GaloisField k) => s -> LC s k
lcSig s = (Map.fromList [(s, 1)], 0)

lcScale :: GaloisField k => k -> LC s k -> LC s k
lcScale c (sm, sc) = (Map.map (* c) sm, c * sc)

lcShift :: GaloisField k => k -> LC s k -> LC s k
lcShift c (sm, sc) = (sm, c + sc)

qeqLcAdd :: (Ord s, GaloisField k) => QEQ s k -> LC s k -> QEQ s k
qeqLcAdd (a1, b1, c1) l = (a1, b1, lcAdd c1 l)

qeqScale :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqScale k (a2, b2, c2) = (lcScale k a2, lcScale k b2, lcScale k c2)

qeqShift :: GaloisField k => k -> QEQ s k -> QEQ s k
qeqShift k (a2, b2, c2) = (lcShift k a2, lcShift k b2, lcShift k c2)

instance GaloisField n => Num (LowDeg n) where
  s + t = case (s, t) of
    (HighDegree  , _          ) -> HighDegree
    (Linear a    , Linear b   ) -> Linear $ lcAdd a b
    (Linear l    , Quadratic q) -> Quadratic $ qeqLcAdd q l
    (Linear l    , Scalar c   ) -> Linear $ lcShift c l
    (Quadratic{} , Quadratic{}) -> HighDegree
    (Quadratic q , Scalar k   ) -> Quadratic $ qeqShift k q
    (Scalar    c1, Scalar c2  ) -> Scalar $ c1 + c2
    (l           , r          ) -> r + l
  s * t = case (s, t) of
    (HighDegree  , _          ) -> HighDegree
    (Linear l1   , Linear l2  ) -> Quadratic (l1, l2, lcZero)
    (Linear _    , Quadratic{}) -> HighDegree
    (Linear l    , Scalar c   ) -> Linear $ lcScale c l
    (Quadratic{} , Quadratic{}) -> HighDegree
    (Quadratic q , Scalar c   ) -> Quadratic $ qeqScale c q
    (Scalar    c1, Scalar c2  ) -> Scalar $ c1 * c2
    (l           , r          ) -> r * l
  fromInteger n = Scalar $ fromInteger n
  signum _ = Scalar 1
  abs = id
  negate s = fromInteger (-1) * s

instance GaloisField n => Fractional (LowDeg n) where
  fromRational = Scalar . fromRational
  recip t = case t of
    Scalar c1 -> Scalar (recip c1)
    _         -> HighDegree

-- A base term type `b` over constant type `k`
class (Show b, Num b, Fractional b) => BaseTerm b k | b -> k where
  fromConst :: k -> b
  fromSignal :: Sig.Signal -> b
  -- will not be called with arithmetic operations
  nonArithBinOp :: BinOp -> b -> b -> b
  -- will not be called with negation
  nonNegUnOp :: UnOp -> b -> b

  binOp :: BinOp -> b -> b -> b
  binOp o = case o of
    Add -> (+)
    Sub -> (-)
    Div -> (/)
    Mul -> (*)
    _ -> nonArithBinOp o

  unOp :: UnOp -> b -> b
  unOp o = case o of
    UnNeg -> negate
    _ -> nonNegUnOp o

class (Show c, BaseTerm b k) => BaseCtx c b k | c -> b where
  assert :: b -> c -> c
  emptyCtx :: c
  storeCtx :: Span -> SignalKind -> LTerm -> b -> c -> c
  -- Notification that a particular signal was gotten.
  getCtx :: SignalKind -> LTerm -> c -> c
  ignoreCompBlock :: c -> Bool
  -- Called after function exit.
  finalize :: c -> c

newtype LowDegCtx k = LowDegCtx { constraints :: [QEQ Sig.Signal k] } deriving (Show)

primeBinOp :: (KnownNat k) => BinOp -> Prime k -> Prime k -> Prime k
primeBinOp o = case o of
  IntDiv -> liftIntToPrime div
  Mod    -> liftIntToPrime mod
  Lt     -> liftIntPredToPrime (<)
  Gt     -> liftIntPredToPrime (>)
  Le     -> liftIntPredToPrime (<=)
  Ge     -> liftIntPredToPrime (>=)
  Eq     -> liftIntPredToPrime (==)
  Ne     -> liftIntPredToPrime (/=)
  And    -> liftBoolToPrime (&&)
  Or     -> liftBoolToPrime (||)
  BitAnd -> liftIntToPrime (Bits..&.)
  BitOr  -> liftIntToPrime (Bits..|.)
  BitXor -> liftIntToPrime Bits.xor
  Pow    -> liftIntToPrime (^)
  Shl    -> liftIntToPrime (liftShiftToInt Bits.shiftL)
  Shr    -> liftIntToPrime (liftShiftToInt Bits.shiftR)
  Add    -> (+)
  Sub    -> (-)
  Mul    -> (*)
  Div    -> (/)
 where
  liftIntToPrime f a b = toP $ f (fromP a) (fromP b)
  liftIntPredToPrime f =
    liftIntToPrime (\a b -> fromIntegral $ fromEnum (f a b))
  liftBoolToPrime f = liftIntPredToPrime (\a b -> f (a /= 0) (b /= 0))
  liftShiftToInt :: (Integer -> Int -> Integer) -> Integer -> Integer -> Integer
  liftShiftToInt a b c = a b (fromIntegral c)

primeUnOp :: (KnownNat k) => UnOp -> Prime k -> Prime k
primeUnOp o = case o of
  UnNeg -> negate
  BitNot ->
    error "Bitwise negation has unclear semantics for prime field elements"
  Not   -> \a -> if a == 0 then 1 else 0
  UnPos -> id

data Void k deriving (Show,Eq,Ord)

instance Num (Void k) where
instance Fractional (Void k) where
instance BaseTerm (Void k) k where

instance KnownNat k => BaseTerm (LowDeg (Prime k)) (Prime k) where
  fromConst  = Scalar
  fromSignal = Linear . lcSig
  nonNegUnOp o t = case t of
    Scalar f -> Scalar $ primeUnOp o f
    _        -> HighDegree
  nonArithBinOp o s t = case (s, t) of
    (Scalar a, Scalar b) -> Scalar $ primeBinOp o a b
    _                    -> HighDegree

instance KnownNat k => BaseCtx (LowDegCtx (Prime k)) (LowDeg (Prime k)) (Prime k) where
  assert t (LowDegCtx cs) = case t of
    Scalar    0  -> LowDegCtx cs
    Linear    lc -> LowDegCtx ((lcZero, lcZero, lc) : cs)
    Quadratic q  -> LowDegCtx (q : cs)
    _            -> error $ "Cannot constain " ++ show t ++ " to zero"
  emptyCtx = LowDegCtx []
  storeCtx _span _sigKind _signalLoc _term = id
  getCtx _kind _loc = id
  ignoreCompBlock = const True
  finalize        = id

data Term b n = Base b
                 | Array (Arr.Array Int (Term b n))
                 | Component (TemplateInvocation n)
                 | Const n
                 deriving (Show,Eq,Ord)

type LowDegTerm k = Term (LowDeg k) k

termAsNum :: (Show b, Num n, PrimeField k) => Span -> Term b k -> n
termAsNum s t = case t of
  Const n -> fromInteger $ fromP n
  _ -> spanE s $ "term " ++ show t ++ " should be constant integer, but is not"

termAsConst :: (Show b, Show k) => Span -> Term b k -> Term (Void k) k
termAsConst s t = case t of
  Const     n -> Const n
  Array     a -> Array $ fmap (termAsConst s) a
  Component _ -> spanE s "don't pass function calls as arguments"
  Base{} -> spanE s $ "term " ++ show t ++ " should be constant, but is not"

instance (BaseTerm b k, GaloisField k) => Num (Term b k) where
  s + t = case (s, t) of
    (a@Array{}, _) ->
      error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Component{}, _) ->
      error $ "Cannot add component term " ++ show a ++ " to anything"
    (Base  a, Base b ) -> Base $ a + b
    (Const a, Const b) -> Const $ a + b
    (Const a, Base b ) -> Base (b + fromConst a)
    (l      , r      ) -> r + l
  s * t = case (s, t) of
    (a@Array{}, _) ->
      error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Component{}, _) ->
      error $ "Cannot multiply component term " ++ show a ++ " with anything"
    (Base  a, Base b ) -> Base $ a * b
    (Const a, Const b) -> Const $ a * b
    (Const a, Base b ) -> Base (b * fromConst a)
    (l      , r      ) -> r * l
  fromInteger = Const . fromInteger
  signum s = case s of
    Array{}     -> error $ "Cannot get sign of array term " ++ show s
    Component{} -> error $ "Cannot get sign of component term " ++ show s
    Base  a     -> Base $ signum a
    Const k     -> Const $ signum k
  abs s = case s of
    Array a     -> Base $ fromIntegral $ length a
    Component{} -> error $ "Cannot get size of component term " ++ show s
    Base  a     -> Base $ abs a
    Const k     -> Const $ abs k
  negate s = fromInteger (-1) * s

instance (BaseTerm b k, GaloisField k) => Fractional (Term b k) where
  fromRational = Const . fromRational
  recip t = case t of
    a@Array{}     -> error $ "Cannot invert array term " ++ show a
    a@Component{} -> error $ "Cannot invert component term " ++ show a
    Base  a       -> Base $ recip a
    Const a       -> Const $ recip a

instance (BaseTerm b (Prime k), KnownNat k) => BaseTerm (Term b (Prime k)) (Prime k) where
  fromConst  = Const
  fromSignal = Base . fromSignal
  nonNegUnOp o t = case t of
    Const f              -> Const $ primeUnOp o f
    Base  b              -> Base $ nonNegUnOp o b
    Array a | o == UnPos -> Const $ fromInteger $ fromIntegral $ length a
    _ -> error $ "Cannot perform operation " ++ show o ++ " on " ++ show t
  nonArithBinOp o s t = case (s, t) of
    (Const a, Const b) -> Const $ primeBinOp o a b
    (Const a, Base b ) -> Base $ binOp o (fromConst a) b
    (Base  a, Const b) -> Base $ binOp o a (fromConst b)
    (Base  a, Base b ) -> Base $ binOp o a b
    _ ->
      error
        $  "Cannot perform operation "
        ++ show o
        ++ " on "
        ++ show s
        ++ " and "
        ++ show t

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

type TemplateInvocation n = (String, [Term (Void n) n])

data IdKind = IKVar | IKSig | IKComp deriving (Show,Eq,Ord)


data CompCtx c b n = CompCtx { env :: Map.Map String (Term b n)
                             , baseCtx :: c
                             , signals :: Map.Map String (SignalKind, [Int])
                             , type_ :: Typing.InstanceType
                             , ids :: Map.Map String IdKind
                             , returning :: Maybe (Term b n)
                             --                             isFn, frmlArgs, code
                             , callables :: Map.Map String (Bool, [String], SBlock)
                             , cache :: Map.Map (TemplateInvocation n) (CompCtx c b n)
                             } deriving (Show)

ctxOrderedSignals :: CompCtx c b n -> [Sig.IndexedIdent]
ctxOrderedSignals =
      -- We sort the signals in public-inputs-first order. By starting with
      -- signal number 2, this ensures that signals numbered 2...n will be the
      -- public inputs, which is what our r1cs format requires
  map snd
    . List.sort
    . concatMap (\(n, (k, d)) -> map (k, ) $ expandSig n d)
    . Map.toAscList
    . signals
 where
  expandSig :: String -> [Int] -> [Sig.IndexedIdent]
  expandSig sigName dims = map (sigName, ) $ mapM (\d -> take d [0 ..]) dims

type LowDegCompCtx n = CompCtx (LowDegCtx n) (LowDeg n) n

type WitCompCtx n = CompCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n)

empty :: (BaseCtx c b n) => CompCtx c b n
empty = CompCtx { env       = Map.empty
                , baseCtx   = emptyCtx
                , signals   = Map.empty
                , type_     = Typing.emptyType
                , ids       = Map.empty
                , returning = Nothing
                , callables = Map.empty
                , cache     = Map.empty
                }

nPublicInputs :: CompCtx c b n -> Int
nPublicInputs c =
  sum
    $ map (\(_, ds) -> product ds)
    $ filter (isPublic . fst)
    $ Fold.toList
    $ signals c

data LTerm = LTermLocal Sig.IndexedIdent
           | LTermForeign Sig.IndexedIdent Sig.IndexedIdent
           deriving (Show,Eq,Ord,Read)

newtype CompState c b n a = CompState (State (CompCtx c b (Prime n)) a)
    deriving (Functor, Applicative, Monad, MonadState (CompCtx c b (Prime n)))

runCompState
  :: KnownNat n
  => CompState c b n a
  -> CompCtx c b (Prime n)
  -> (a, CompCtx c b (Prime n))
runCompState (CompState s) = runState s

runLowDegCompState
  :: KnownNat n
  => CompState (LowDegCtx (Prime n)) (LowDeg (Prime n)) n a
  -> LowDegCompCtx (Prime n)
  -> (a, LowDegCompCtx (Prime n))
runLowDegCompState = runCompState

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

-- Gets a value from a location
load
  :: forall c b k
   . (BaseCtx c b (Prime k), KnownNat k)
  => Span
  -> LTerm
  -> CompState c b k (Term b (Prime k))
load span_ loc = do
  ctx <- get
  case loc of
    LTermLocal (name, idxs) -> case ids ctx Map.!? name of
      Just IKVar -> return
        $ extract idxs (mapGetE ("Unknown var " ++ show name) name (env ctx))
      Just IKComp -> return $ extract
        idxs
        (mapGetE ("Unknown component " ++ show name) name (env ctx))
      Just IKSig -> do
        modify (\c -> c { baseCtx = getCtx kind loc $ baseCtx c })
        return $ Base $ fromSignal $ either
          (spanE span_)
          (const $ Sig.SigLocal (name, idxs))
          (checkDims idxs dims)
       where
        (kind, dims) =
          mapGetE ("Unknown signal " ++ show name) name (signals ctx)
      Nothing ->
        spanE span_ $ "Unknown identifier `" ++ name ++ "` in " ++ show
          (ids ctx)
    LTermForeign (name, idxs) sigLoc -> case ids ctx Map.!? name of
      Just IKComp ->
        case
            extract
              idxs
              (mapGetE
                (  "Unknown component "
                ++ show name
                ++ " in foreign location "
                ++ show loc
                )
                name
                (env ctx)
              )
          of
            Component invoc ->
              let
                forCtx = mapGetE ("Missing invocation " ++ show invoc)
                                 invoc
                                 (cache ctx)
              in
                case signals forCtx Map.!? fst sigLoc of
                  Just (k, dims) | isVisible k -> do
                    modify (\c -> c { baseCtx = getCtx k loc $ baseCtx c })
                    return $ Base $ fromSignal $ either
                      (spanE span_)
                      (const $ Sig.SigForeign (name, idxs) sigLoc)
                      (checkDims (snd sigLoc) dims)
                  Just (k, _) ->
                    spanE span_
                      $  "Cannot load foreign signal "
                      ++ show (fst sigLoc)
                      ++ " of type "
                      ++ show k
                      ++ " at "
                      ++ show loc
                  _ ->
                    spanE span_ $ "Unknown foreign signal " ++ show (fst sigLoc)
            _ -> spanE span_ "Unreachable: non-component in component id!"
      Just _ ->
        spanE span_ $ "Identifier " ++ show name ++ " is not a component"
      Nothing -> spanE span_ $ "Identifier " ++ show name ++ " is unknown"

subscript :: Show b => Int -> Term b (Prime k) -> Term b (Prime k)
subscript i t = case t of
  Array a -> a Arr.! i
  _       -> error $ "Cannot index term " ++ show t

extract :: Show b => [Int] -> Term b (Prime k) -> Term b (Prime k)
extract = flip $ foldl (flip subscript)

checkDims :: [Int] -> [Int] -> Either String ()
checkDims idxs dims = if length idxs == length dims
  then if all (uncurry (<)) (zip idxs dims)
    then Right ()
    else
      Left
      $  "Indices "
      ++ show idxs
      ++ " out-of-bounds for dimensions "
      ++ show dims
  else
    Left $ "Indices " ++ show idxs ++ " wrong size for dimensions " ++ show dims


-- allocate a name with a term
alloc
  :: forall c b k
   . KnownNat k
  => SString
  -> IdKind
  -> Term b (Prime k)
  -> CompCtx c b (Prime k)
  -> CompCtx c b (Prime k)
-- Stores a term in a location
alloc name kind term ctx = case ids ctx Map.!? ast name of
  Just IKVar  -> ctx'
  Nothing     -> ctx'
  Just IKSig  -> e
  Just IKComp -> e
 where
  ctx' = ctx { env = Map.insert (ast name) term $ env ctx
             , ids = Map.insert (ast name) kind $ ids ctx
             }
  e = spanE (ann name) $ "Identifier " ++ show name ++ " already used"

store
  :: forall c b k
   . (BaseCtx c b (Prime k), KnownNat k)
  => Span
  -> LTerm
  -> Term b (Prime k)
  -> CompCtx c b (Prime k)
  -> CompCtx c b (Prime k)
store span_ loc term ctx = case loc of
  LTermLocal (name, idxs) -> case ids ctx Map.!? name of
    Nothing    -> spanE span_ $ "Unknown identifier `" ++ name ++ "`"
    Just IKSig -> case signals ctx Map.!? name of
      Just (k, dims) ->
        either (spanE span_) (const $ storeSig k loc ctx) (checkDims idxs dims)
      Nothing -> spanE span_ "Unreachable"
    Just _ -> case env ctx Map.!? name of
      Just t ->
        ctx { env = Map.insert name (modifyIn idxs (const term) t) (env ctx) }
      Nothing -> case signals ctx Map.!? name of
        Just _  -> ctx
        Nothing -> spanE span_ $ "Unknown identifier `" ++ name ++ "`"
   where
    arrayUpdate :: Ix i => i -> (a -> a) -> Arr.Array i a -> Arr.Array i a
    arrayUpdate i f a = a Arr.// [(i, f (a Arr.! i))]

    modifyIn
      :: [Int]
      -> (Term b (Prime k) -> Term b (Prime k))
      -> Term b (Prime k)
      -> Term b (Prime k)
    modifyIn is f t = case is of
      []      -> f t
      i : is' -> case t of
        Array a -> Array $ arrayUpdate i (modifyIn is' f) a
        _ ->
          spanE span_
            $  "Cannot update index "
            ++ show i
            ++ " of non-array "
            ++ show t
  LTermForeign (name, idxs) sigLoc -> case ids ctx Map.!? name of
    Just IKComp ->
      case
          extract
            idxs
            (mapGetE
              (  "Unknown component "
              ++ show name
              ++ " in foreign location "
              ++ show loc
              )
              name
              (env ctx)
            )
        of
          Component invoc ->
            let
              forCtx =
                mapGetE ("Missing invocation " ++ show invoc) invoc (cache ctx)
            in  case signals forCtx Map.!? fst sigLoc of
                  Just (k, dims) | isInput k -> either
                    (spanE span_)
                    (const $ storeSig k loc ctx)
                    (checkDims (snd sigLoc) dims)
                  Just (k, _) ->
                    spanE span_
                      $  "Cannot store into foreign signal "
                      ++ show (fst sigLoc)
                      ++ " of type "
                      ++ show k
                  _ ->
                    spanE span_ $ "Unknown foreign signal " ++ show (fst sigLoc)
          _ -> spanE span_ "Unreachable: non-component in component id!"
    Just _  -> spanE span_ $ "Identifier " ++ show name ++ " is not a component"
    Nothing -> spanE span_ $ "Identifier " ++ show name ++ " is unknown"
 where
  storeSig k l c = case term of
    Base  b -> c { baseCtx = storeCtx span_ k l b $ baseCtx c }
    Const b -> c { baseCtx = storeCtx span_ k l (fromConst b) $ baseCtx c }
    _ ->
      spanE span_
        $  "Cannot store non-base term "
        ++ show term
        ++ " in signal "
        ++ show l

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

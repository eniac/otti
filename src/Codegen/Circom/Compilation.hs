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
  ( CompCtx(..)
  , compMainCtx
  , compMainWitCtx
  , nSmtNodes
  , LC
  , QEQ
  , TemplateInvocation
  , AbsTerm(..)
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
  )
where

import           AST.Circom
import qualified Codegen.Circom.Signal         as Sig
import qualified Codegen.Circom.Typing         as Typing

import           Control.Monad
import           Control.Monad.State.Strict
import           Data.Ix
import qualified Data.Bits                     as Bits
import qualified Data.Map.Strict               as Map
import qualified Data.Set                      as Set
import qualified Data.Maybe                    as Maybe
import qualified Data.Array                    as Arr
import qualified Data.List                     as List
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import           Debug.Trace                    ( trace )
import qualified IR.TySmt                      as Smt
import           GHC.TypeLits.KnownNat
import           GHC.TypeNats
import           Data.Proxy                     ( Proxy(Proxy) )

intLog2 :: Integral a => a -> a
intLog2 n = if n <= fromInteger 1
  then fromInteger 0
  else fromInteger 1 + intLog2 (n `div` fromInteger 2)

-- XXX(HACK): Log2 0 is actually undefined, but who wants to deal with that?
--            we treat it as 0, even though the type systems rejects it.
instance KnownNat x => KnownNat1 $(nameToSymbol ''Log2) x where
  natSing1 = SNatKn (intLog2 (natVal (Proxy @x)))
  {-# INLINE natSing1 #-}


type LC s n = (Map.Map s n, n) -- A linear combination of signals and gen-time constants
type QEQ s n = (LC s n, LC s n, LC s n)

data LowDeg n = Scalar n
              | Linear (LC Sig.Signal n)
              | Quadratic (QEQ Sig.Signal n)
              | HighDegree
              deriving (Show,Eq,Ord)

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

class (BaseTerm b k) => BaseCtx c b k | c -> b where
  assert :: b -> c -> c
  emptyCtx :: c
  storeCtx :: SignalKind -> LTerm -> b -> c -> c
  -- Notification that a particular signal was gotten.
  getCtx :: SignalKind -> LTerm -> c -> c
  ignoreCompBlock :: c -> Bool

newtype LowDegCtx k = LowDegCtx { constraints :: [QEQ Sig.Signal k] }

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
  storeCtx _sigKind _signalLoc _term = id
  getCtx _kind _loc = id
  ignoreCompBlock = const True

data AbsTerm b k = Base b
                 | Array (Arr.Array Int (AbsTerm b k))
                 | Component TemplateInvocation
                 | Const k
                 deriving (Show,Eq,Ord)

type LowDegTerm k = AbsTerm (LowDeg k) k

termAsNum :: (Show b, Num n, PrimeField k) => AbsTerm b k -> n
termAsNum t = case t of
  Const n -> fromInteger $ fromP n
  _ -> error $ "term " ++ show t ++ " should be constant integer, but is not"

instance (BaseTerm b k, GaloisField k) => Num (AbsTerm b k) where
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

instance (BaseTerm b k, GaloisField k) => Fractional (AbsTerm b k) where
  fromRational = Const . fromRational
  recip t = case t of
    a@Array{}     -> error $ "Cannot invert array term " ++ show a
    a@Component{} -> error $ "Cannot invert component term " ++ show a
    Base  a       -> Base $ recip a
    Const a       -> Const $ recip a

instance (BaseTerm b (Prime k), KnownNat k) => BaseTerm (AbsTerm b (Prime k)) (Prime k) where
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
                               , alreadyInstantiated :: Set.Set Sig.IndexedIdent
                               -- The order in which signals and components are written to
                               -- lefts are signals, rights are components
                               , assignmentOrder :: [Either LTerm Sig.IndexedIdent]
                               } deriving (Show)

instance KnownNat n => BaseCtx (WitBaseCtx n) (WitBaseTerm n) (Prime n) where
  assert _ = id
  emptyCtx = WitBaseCtx Map.empty Set.empty []
  storeCtx kind sig term ctx = if Map.member sig (signalTerms ctx)
    then error $ "Signal " ++ show sig ++ " has already been assigned to"
    else
      let newCtx = ctx { signalTerms     = Map.insert sig term $ signalTerms ctx
                       , assignmentOrder = Left sig : assignmentOrder ctx
                       }
      in  case sig of
            LTermLocal{} -> newCtx
            LTermForeign cLoc sLoc ->
              if isInput kind && Set.member cLoc (alreadyInstantiated ctx)
                then
                  error
                  $  "Cannot write to input "
                  ++ show sLoc
                  ++ " of component at location "
                  ++ show cLoc
                  ++ " when an output of that component has already been read"
                else newCtx
  getCtx kind sig c = if kind == Out
    then case sig of
      LTermLocal{}        -> c
      LTermForeign cLoc _ -> if Set.member cLoc (alreadyInstantiated c)
        then c
        else c { alreadyInstantiated = Set.insert cLoc $ alreadyInstantiated c
               , assignmentOrder     = Right cLoc : assignmentOrder c
               }
    else c
  ignoreCompBlock = const False

nSmtNodes :: WitBaseCtx n -> Int
nSmtNodes =
  Map.foldr ((+) . (\(WitBaseTerm a) -> Smt.nNodes a)) 0 . signalTerms

type TemplateInvocation = (String, [Integer])

data IdKind = IKVar | IKSig | IKComp deriving (Show,Eq,Ord)


data CompCtx c b n = CompCtx { env :: Map.Map String (AbsTerm b n)
                             , baseCtx :: c
                             , signals :: Map.Map String (SignalKind, [Int])
                             , type_ :: Typing.InstanceType
                             , ids :: Map.Map String IdKind
                             , returning :: Maybe (AbsTerm b n)
                             --                             isFn, frmlArgs, code
                             , callables :: Map.Map String (Bool, [String], Block)
                             , cache :: Map.Map TemplateInvocation (CompCtx c b n)
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
    $ filter (\(k, _) -> k == PublicIn)
    $ Fold.toList
    $ signals c

data LTerm = LTermLocal Sig.IndexedIdent
           | LTermForeign Sig.IndexedIdent Sig.IndexedIdent
           deriving (Show,Eq,Ord)

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
  => IndexedIdent
  -> CompState c b n Sig.IndexedIdent
compIndexedIdent (name, dims) = do
  dimTerms <- compExprs dims
  let dimInts = map termAsNum dimTerms
  return (name, dimInts)

compLoc
  :: (BaseCtx c b (Prime n), KnownNat n) => Location -> CompState c b n LTerm
compLoc l = case l of
  LocalLocation a -> do
    at <- compIndexedIdent a
    return $ LTermLocal at
  ForeignLocation a b -> do
    at <- compIndexedIdent a
    bt <- compIndexedIdent b
    return $ LTermForeign at bt

compExpr
  :: (BaseCtx c b (Prime n), KnownNat n)
  => Expr
  -> CompState c b n (AbsTerm b (Prime n))
compExpr e = case e of
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
    term <- load lval
    let term' = term + Const (toP $ opToOffset op)
    modify (store lval term')
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
      t       -> error $ "Cannot condition on term " ++ show t
  LValue loc -> do
    lt <- compLoc loc
    load lt
  Call name args -> do
    tArgs <- compExprs args
    -- TODO: Allow non-constant arguments
    let intArgs :: [Integer] = map termAsNum tArgs
    let invocation           = (name, intArgs)
    c <- get
    let (isFn, formalArgs, code) = Maybe.fromMaybe
          (error $ "Unknown callable " ++ name)
          (callables c Map.!? name)
    unless (length args == length formalArgs)
      $  return
      $  error
      $  "Wrong number of arguments for "
      ++ show name
    let callState = empty { callables = callables c
                          , cache     = cache c
                          , env       = Map.fromList $ zip formalArgs tArgs
                          , ids       = Map.fromList $ map (, IKVar) formalArgs
                          }
    if isFn
      then do
        let ((), c') = runCompState (compStatements code) callState
        let returnValue = Maybe.fromMaybe
              (error $ "Function " ++ name ++ " did not return")
              (returning c')
        return returnValue
      else do
        unless (Map.member invocation (cache c)) $ do
          let ((), c') = runCompState (compStatements code) callState
          let newCache = cache c'
          let strippedCtx = c'
                { env   = Map.restrictKeys
                            (env c')
                            (Map.keysSet $ Map.filter (== IKComp) (ids c'))
                , cache = Map.empty
                , ids   = Map.empty
                }
          modify
            (\cc -> cc { cache = Map.insert invocation strippedCtx newCache })
        return $ Component invocation

compExprs
  :: (BaseCtx c b (Prime n), KnownNat n)
  => [Expr]
  -> CompState c b n [AbsTerm b (Prime n)]
compExprs = mapM compExpr

compStatements
  :: (BaseCtx c b (Prime n), KnownNat n) => [Statement] -> CompState c b n ()
compStatements = void . mapM compStatement

compStatement
  :: (BaseCtx c b (Prime n), KnownNat n) => Statement -> CompState c b n ()
compStatement s = do
  ctx <- get
  if Maybe.isJust (returning ctx)
    then return ()
    else case s of
      Assign loc expr -> do
        lval <- compLoc loc
        term <- compExpr expr
        modify (store lval term)
      -- TODO Not quite right: evals location twice
      OpAssign op loc expr ->
        compStatement (Assign loc (BinExpr op (LValue loc) expr))
      Constrain l r -> do
        lt <- compExpr l
        rt <- compExpr r
        -- Construct the zero term
        let zt = lt - rt
        case zt of
          Base b -> modify $ \c -> c { baseCtx = assert b (baseCtx c) }
          _      -> error $ "Cannot constain " ++ show zt ++ " to zero"
      -- TODO Not quite right: evals location twice
      AssignConstrain l e ->
        compStatements [Assign l e, Constrain (LValue l) e]
      VarDeclaration name dims ini -> do
        ts <- compExprs dims
        modify (alloc name IKVar $ termMultiDimArray (Const 0) ts)
        case ini of
          Just e  -> compStatement (Assign (LocalLocation (name, [])) e)
          Nothing -> modify id
      SigDeclaration name kind dims -> do
        ts    <- compExprs dims
        fresh <- gets (not . Map.member name . ids)
        unless fresh $ error $ "Signal name " ++ show name ++ " is not fresh"
        modify
          (\c -> c
            { signals = Map.insert name (kind, map termAsNum ts) $ signals c
            , ids     = Map.insert name IKSig $ ids ctx
            }
          )
      SubDeclaration name dims ini -> do
        ts <- compExprs dims
        modify (alloc name IKComp (termMultiDimArray (Const 0) ts))
        case ini of
          Just e  -> compStatement (Assign (LocalLocation (name, [])) e)
          Nothing -> return ()
      If cond true false -> do
        tcond <- compExpr cond
        case tcond of
          Const 0 -> maybe (return ()) compStatements false
          Const _ -> compStatements true
          _ ->
            error
              $  "Invalid conditional term "
              ++ show tcond
              ++ " in if condition "
              ++ show cond
      While cond block -> do
        tcond <- compExpr cond
        case tcond of
          Const 0 -> return ()
          Const _ -> do
            compStatements block
            compStatement (While cond block)
          _ ->
            error
              $  "Invalid conditional term "
              ++ show tcond
              ++ " in while condition "
              ++ show cond
      For ini cond step block ->
        compStatements [ini, While cond (block ++ [step])]
      DoWhile block expr -> compStatements (block ++ [While expr block])
      Compute b          -> do
        ignore <- gets (ignoreCompBlock . baseCtx)
        if ignore then return () else compStatements b
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
  => LTerm
  -> CompState c b k (AbsTerm b (Prime k))
load loc = do
  ctx <- get
  case loc of
    LTermLocal (name, idxs) -> case ids ctx Map.!? name of
      Just IKVar  -> return $ extract idxs (env ctx Map.! name)
      Just IKComp -> return $ extract idxs (env ctx Map.! name)
      Just IKSig  -> do
        modify (\c -> c { baseCtx = getCtx kind loc $ baseCtx c })
        return $ Base $ fromSignal $ either
          error
          (const $ Sig.SigLocal (name, idxs))
          (checkDims idxs dims)
        where (kind, dims) = signals ctx Map.! name
      Nothing ->
        error $ "Unknown identifier `" ++ name ++ "` in" ++ show (ids ctx)
    LTermForeign (name, idxs) sigLoc -> case ids ctx Map.!? name of
      Just IKComp -> case extract idxs (env ctx Map.! name) of
        Component invoc ->
          let forCtx = cache ctx Map.! invoc
          in  case signals forCtx Map.!? fst sigLoc of
                Just (k, dims) | isVisible k -> do
                  modify (\c -> c { baseCtx = getCtx k loc $ baseCtx c })
                  return $ Base $ fromSignal $ either
                    error
                    (const $ Sig.SigForeign (name, idxs) sigLoc)
                    (checkDims (snd sigLoc) dims)
                Just (k, _) ->
                  error
                    $  "Cannot load foreign signal "
                    ++ show (fst sigLoc)
                    ++ " of type "
                    ++ show k
                    ++ " at "
                    ++ show loc
                _ -> error $ "Unknown foreign signal " ++ show (fst sigLoc)
        _ -> error "Unreachable: non-component in component id!"
      Just _  -> error $ "Identifier " ++ show name ++ " is not a component"
      Nothing -> error $ "Identifier " ++ show name ++ " is unknown"

subscript :: Show b => Int -> AbsTerm b (Prime k) -> AbsTerm b (Prime k)
subscript i t = case t of
  Array a -> a Arr.! i
  _       -> error $ "Cannot index term " ++ show t

extract :: Show b => [Int] -> AbsTerm b (Prime k) -> AbsTerm b (Prime k)
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
  => String
  -> IdKind
  -> AbsTerm b (Prime k)
  -> CompCtx c b (Prime k)
  -> CompCtx c b (Prime k)
-- Stores a term in a location
alloc name kind term ctx = if Map.member name (ids ctx)
  then error $ "Identifier " ++ show name ++ " already used"
  else ctx { env = Map.insert name term $ env ctx
           , ids = Map.insert name kind $ ids ctx
           }

store
  :: forall c b k
   . (BaseCtx c b (Prime k), KnownNat k)
  => LTerm
  -> AbsTerm b (Prime k)
  -> CompCtx c b (Prime k)
  -> CompCtx c b (Prime k)
store loc term ctx = case loc of
  LTermLocal (name, idxs) -> case ids ctx Map.!? name of
    Nothing    -> error $ "Unknown identifier `" ++ name ++ "`"
    Just IKSig -> case signals ctx Map.!? name of
      Just (k, dims) ->
        either error (const $ storeSig k loc ctx) (checkDims idxs dims)
      Nothing -> error "Unreachable"
    Just _ -> case env ctx Map.!? name of
      Just t ->
        ctx { env = Map.insert name (modifyIn idxs (const term) t) (env ctx) }
      Nothing -> case signals ctx Map.!? name of
        Just _  -> ctx
        Nothing -> error $ "Unknown identifier `" ++ name ++ "`"
   where
    arrayUpdate :: Ix i => i -> (a -> a) -> Arr.Array i a -> Arr.Array i a
    arrayUpdate i f a = a Arr.// [(i, f (a Arr.! i))]

    modifyIn
      :: [Int]
      -> (AbsTerm b (Prime k) -> AbsTerm b (Prime k))
      -> AbsTerm b (Prime k)
      -> AbsTerm b (Prime k)
    modifyIn is f t = case is of
      []      -> f t
      i : is' -> case t of
        Array a -> Array $ arrayUpdate i (modifyIn is' f) a
        _ ->
          error
            $  "Cannot update index "
            ++ show i
            ++ " of non-array "
            ++ show t
  LTermForeign (name, idxs) sigLoc -> case ids ctx Map.!? name of
    Just IKComp -> case extract idxs (env ctx Map.! name) of
      Component invoc ->
        let forCtx = cache ctx Map.! invoc
        in  case signals forCtx Map.!? fst sigLoc of
              Just (k, dims) | isInput k -> either
                error
                (const $ storeSig k loc ctx)
                (checkDims (snd sigLoc) dims)
              Just (k, _) ->
                error
                  $  "Cannot store into foreign signal "
                  ++ show (fst sigLoc)
                  ++ " of type "
                  ++ show k
              _ -> error $ "Unknown foreign signal " ++ show (fst sigLoc)
      _ -> error "Unreachable: non-component in component id!"
    Just _  -> error $ "Identifier " ++ show name ++ " is not a component"
    Nothing -> error $ "Identifier " ++ show name ++ " is unknown"
 where
  storeSig k l c = case term of
    Base  b -> c { baseCtx = storeCtx k l b $ baseCtx c }
    Const b -> c { baseCtx = storeCtx k l (fromConst b) $ baseCtx c }
    _ ->
      error
        $  "Cannot store non-base term "
        ++ show term
        ++ " in signal "
        ++ show l

termMultiDimArray
  :: (Show b, KnownNat k)
  => AbsTerm b (Prime k)
  -> [AbsTerm b (Prime k)]
  -> AbsTerm b (Prime k)
termMultiDimArray = foldr
  (\d acc -> case d of
    Const n -> Array $ Arr.listArray (0, i - 1) (replicate i acc)
      where i = fromIntegral $ fromP n
    _ -> error $ "Illegal dimension " ++ show d
  )

compMain
  :: forall c b k
   . (KnownNat k, BaseCtx c b (Prime k))
  => MainCircuit
  -> CompCtx c b (Prime k)
compMain m =
  snd
    $ runCompState (compStatement (SubDeclaration "main" [] (Just (main m))))
    $ empty
        { callables = Map.union
                        (Map.map (\(p, b) -> (False, p, b)) (templates m))
                        (Map.map (\(p, b) -> (True, p, b)) (functions m))
        }

compMainCtx
  :: KnownNat k
  => MainCircuit
  -> CompCtx (LowDegCtx (Prime k)) (LowDeg (Prime k)) (Prime k)
compMainCtx = compMain

compMainWitCtx
  :: KnownNat k
  => MainCircuit
  -> CompCtx (WitBaseCtx k) (WitBaseTerm k) (Prime k)
compMainWitCtx = compMain

getMainInvocation :: forall k . KnownNat k => Proxy k -> MainCircuit -> TemplateInvocation
getMainInvocation _order m = case main m of
  Call name args ->
    (name, map termAsNum $ fst $ runLowDegCompState @k (compExprs args) empty)
  expr -> error $ "Invalid main expression " ++ show expr



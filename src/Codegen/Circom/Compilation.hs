{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Codegen.Circom.Compilation
  ( CompCtx(..)
  , compMainCtx
  , LC
  , QEQ
  , TemplateInvocation
  , AbsTerm(..)
  , Term
  , CompState(..)
  , LowDegCtx(..)
  , LowDegCompCtx
  , empty
  , runLowDegCompState
  , compExprs
  , termAsNum
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
import qualified Data.Maybe                    as Maybe
import qualified Data.Array                    as Arr
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , GaloisField
                                                , fromP
                                                , toP
                                                )
import           Debug.Trace                    ( trace )
import           GHC.TypeLits

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

data AbsTerm b k = Base b
                 | Array (Arr.Array Int (AbsTerm b k))
                 | Component TemplateInvocation
                 | Const k
                 deriving (Show,Eq,Ord)

type Term k = AbsTerm (LowDeg k) k

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

type TemplateInvocation = (String, [Integer])

data IdKind = IKVar | IKSig | IKComp deriving (Show,Eq,Ord)


data CompCtx c b n = CompCtx { lowDegEnv :: Map.Map String (AbsTerm b n)
                             , baseCtx :: c
                             , signals :: Map.Map String (SignalKind, [Int])
                             , type_ :: Typing.InstanceType
                             , ids :: Map.Map String IdKind
                             , returning :: Maybe (AbsTerm b n)
                             --                             isFn, frmlArgs, code
                             , callables :: Map.Map String (Bool, [String], Block)
                             , cache :: Map.Map TemplateInvocation (CompCtx c b n)
                             } deriving (Show)

type LowDegCompCtx n = CompCtx (LowDegCtx n) (LowDeg n) n

empty :: (BaseCtx c b n) => CompCtx c b n
empty = CompCtx { lowDegEnv = Map.empty
                , baseCtx   = emptyCtx
                , signals   = Map.empty
                , type_     = Typing.emptyType
                , ids       = Map.empty
                , returning = Nothing
                , callables = Map.empty
                , cache     = Map.empty
                }

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

compLoc :: (BaseCtx c b (Prime n), KnownNat n) => Location -> CompState c b n LTerm
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
    term <- gets (load lval)
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
    -- TODO(aozdemir): enforce no ctx change for sanity?
    lt <- compLoc loc
    gets (load lt)
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
                          , lowDegEnv = Map.fromList $ zip formalArgs tArgs
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
                { lowDegEnv = Map.restrictKeys
                                (lowDegEnv c')
                                (Map.keysSet $ Map.filter (== IKComp) (ids c'))
                , cache     = Map.empty
                , ids       = Map.empty
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
      -- TODO Not quite right: evals twice
      OpAssign op loc expr ->
        compStatement (Assign loc (BinExpr op (LValue loc) expr))
      Constrain l r -> do
        lt <- compExpr l
        rt <- compExpr r
        -- Construct the zero term
        let zt = lt - rt
        case zt of
          Base  b -> modify $ \c -> c { baseCtx = assert b (baseCtx c) }
          _ -> error $ "Cannot constain " ++ show zt ++ " to zero"
      -- TODO Not quite right: evals twice
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
      -- TODO: Dont' ignore!
      Compute _          -> return ()
      Ignore  e          -> do
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
   . (BaseTerm b (Prime k), KnownNat k)
  => LTerm
  -> CompCtx c b (Prime k)
  -> AbsTerm b (Prime k)
load loc ctx = case loc of
  LTermLocal (name, idxs) -> case ids ctx Map.!? name of
    Just IKVar  -> extract idxs (lowDegEnv ctx Map.! name)
    Just IKComp -> extract idxs (lowDegEnv ctx Map.! name)
    Just IKSig  -> Base $ fromSignal $ either
      error
      (const $ Sig.SigLocal (name, idxs))
      (checkDims idxs dims)
      where (_kind, dims) = signals ctx Map.! name
    Nothing ->
      error $ "Unknown identifier `" ++ name ++ "` in" ++ show (ids ctx)
  LTermForeign (name, idxs) sigLoc -> case ids ctx Map.!? name of
    Just IKComp -> case extract idxs (lowDegEnv ctx Map.! name) of
        -- TODO: bounds check
      Component invoc ->
        let forCtx = cache ctx Map.! invoc
        in  case signals forCtx Map.!? fst sigLoc of
              Just (k, dims) | isVisible k -> Base $ fromSignal $ either
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
  else ctx { lowDegEnv = Map.insert name term $ lowDegEnv ctx
           , ids       = Map.insert name kind $ ids ctx
           }

store
  :: forall c b k
   . (Show b, KnownNat k)
  => LTerm
  -> AbsTerm b (Prime k)
  -> CompCtx c b (Prime k)
  -> CompCtx c b (Prime k)
store loc term ctx = case loc of
  LTermLocal (name, idxs) -> case lowDegEnv ctx Map.!? name of
    Just t -> ctx
      { lowDegEnv = Map.insert name
                               (modifyIn idxs (const term) t)
                               (lowDegEnv ctx)
      }
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
  -- TODO: Different update?
  LTermForeign (name, idxs) sigLoc -> case ids ctx Map.!? name of
    Just IKComp -> case extract idxs (lowDegEnv ctx Map.! name) of
        -- TODO: bounds check
      Component invoc ->
        let forCtx = cache ctx Map.! invoc
        in  case signals forCtx Map.!? fst sigLoc of
              Just (PublicIn, dims) ->
                either error (const ctx) (checkDims (snd sigLoc) dims)
              Just (PrivateIn, dims) ->
                either error (const ctx) (checkDims (snd sigLoc) dims)
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

compMainCtx :: KnownNat k => MainCircuit -> CompCtx (LowDegCtx (Prime k)) (LowDeg (Prime k)) (Prime k)
compMainCtx m =
  snd
    $ runCompState (compStatement (SubDeclaration "main" [] (Just (main m))))
    $ empty
        { callables = Map.union
                        (Map.map (\(p, b) -> (False, p, b)) (templates m))
                        (Map.map (\(p, b) -> (True, p, b)) (functions m))
        }

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Codegen.Circom.Compilation
  ( CompCtx(..)
  , compMainCtx
  , LC
  , QEQ
  , TemplateInvocation
  , Term(..)
  , CompState(..)
  , empty
  , runCompState
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

lowDegAsNum :: (Num n, PrimeField k) => LowDeg k -> n
lowDegAsNum t = case t of
  Scalar n -> fromInteger $ fromP n
  _ ->
    error $ "low-degree term " ++ show t ++ " should be constant, but is not"

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


data Term k = Base (LowDeg k)
            | Array (Arr.Array Int (Term k))
            | Component TemplateInvocation
            deriving (Show,Eq,Ord)

termAsNum :: (Num n, PrimeField k) => Term k -> n
termAsNum t = case t of
  Base n -> lowDegAsNum n
  _ -> error $ "term " ++ show t ++ " should be constant integer, but is not"

instance GaloisField k => Num (Term k) where
  s + t = case (s, t) of
    (a@Array{}, _) ->
      error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Component{}, _) ->
      error $ "Cannot add component term " ++ show a ++ " to anything"
    (Base a, Base b) -> Base $ a + b
    (l     , r     ) -> r + l
  s * t = case (s, t) of
    (a@Array{}, _) ->
      error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Component{}, _) ->
      error $ "Cannot multiply component term " ++ show a ++ " with anything"
    (Base a, Base b) -> Base $ a * b
    (l     , r     ) -> r * l
  fromInteger n = Base $ fromInteger n
  signum s = case s of
    Array{}     -> error $ "Cannot get sign of array term " ++ show s
    Component{} -> error $ "Cannot get sign of component term " ++ show s
    Base a      -> Base $ signum a
  abs s = case s of
    Array a     -> Base $ fromIntegral $ length a
    Component{} -> error $ "Cannot get size of component term " ++ show s
    Base a      -> Base $ abs a
  negate s = fromInteger (-1) * s

instance GaloisField k => Fractional (Term k) where
  fromRational = Base . fromRational
  recip t = case t of
    a@Array{}     -> error $ "Cannot invert array term " ++ show a
    a@Component{} -> error $ "Cannot invert component term " ++ show a
    Base a        -> Base $ recip a

type TemplateInvocation = (String, [Integer])

data IdKind = IKVar | IKSig | IKComp deriving (Show,Eq,Ord)


data CompCtx n = CompCtx { constraints :: [QEQ Sig.Signal n]
                         , lowDegEnv :: Map.Map String (Term n)
                         , signals :: Map.Map String (SignalKind, [Int])
                         , type_ :: Typing.InstanceType
                         , ids :: Map.Map String IdKind
                         , returning :: Maybe (Term n)
                         --                             isFn, frmlArgs, code
                         , callables :: Map.Map String (Bool, [String], Block)
                         , cache :: Map.Map TemplateInvocation (CompCtx n)
                         } deriving (Show)

empty :: CompCtx n
empty = CompCtx { constraints = []
                , lowDegEnv   = Map.empty
                , signals     = Map.empty
                , type_       = Typing.emptyType
                , ids         = Map.empty
                , returning   = Nothing
                , callables   = Map.empty
                , cache       = Map.empty
                }

data LTerm = LTermLocal Sig.IndexedIdent
           | LTermForeign Sig.IndexedIdent Sig.IndexedIdent
           deriving (Show,Eq,Ord)

newtype CompState n a = CompState (State (CompCtx (Prime n)) a)
    deriving (Functor, Applicative, Monad, MonadState (CompCtx (Prime n)))

runCompState
  :: KnownNat n => CompState n a -> CompCtx (Prime n) -> (a, CompCtx (Prime n))
runCompState (CompState s) = runState s

compIndexedIdent :: KnownNat n => IndexedIdent -> CompState n Sig.IndexedIdent
compIndexedIdent (name, dims) = do
  dimTerms <- compExprs dims
  let dimInts = map termAsNum dimTerms
  return (name, dimInts)

compLoc :: KnownNat n => Location -> CompState n LTerm
compLoc l = case l of
  LocalLocation a -> do
    at <- compIndexedIdent a
    return $ LTermLocal at
  ForeignLocation a b -> do
    at <- compIndexedIdent a
    bt <- compIndexedIdent b
    return $ LTermForeign at bt

compExpr :: KnownNat n => Expr -> CompState n (Term (Prime n))
compExpr e = case e of
  NumLit   i  -> return $ Base $ fromInteger $ fromIntegral i
  ArrayLit es -> do
    ts <- compExprs es
    return $ Array $ Arr.listArray (0, length ts - 1) ts
  BinExpr op l r -> do
    l' <- compExpr l
    r' <- compExpr r
    return $ f l' r'
   where
    f = case op of
      Add    -> (+)
      Sub    -> (-)
      Mul    -> (*)
      Div    -> (/)
      IntDiv -> liftLowDegToTerm "//" $ liftIntToLowDeg div
      Mod    -> liftLowDegToTerm "%" $ liftIntToLowDeg mod
      Lt     -> liftLowDegToTerm "<" $ liftIntPredToLowDeg (<)
      Gt     -> liftLowDegToTerm ">" $ liftIntPredToLowDeg (>)
      Le     -> liftLowDegToTerm "<=" $ liftIntPredToLowDeg (<=)
      Ge     -> liftLowDegToTerm "<=" $ liftIntPredToLowDeg (>=)
      Eq     -> liftLowDegToTerm "==" $ liftIntPredToLowDeg (==)
      Ne     -> liftLowDegToTerm "!=" $ liftIntPredToLowDeg (/=)
      And    -> liftLowDegToTerm "&&" $ liftBoolToLowDeg (&&)
      Or     -> liftLowDegToTerm "||" $ liftBoolToLowDeg (||)
      BitAnd -> liftLowDegToTerm "&" $ liftIntToLowDeg (Bits..&.)
      BitOr  -> liftLowDegToTerm "|" $ liftIntToLowDeg (Bits..|.)
      BitXor -> liftLowDegToTerm "^" $ liftIntToLowDeg Bits.xor
      Pow    -> liftLowDegToTerm "**" $ liftIntToLowDeg (^)
      Shl ->
        liftLowDegToTerm "<<" $ liftIntToLowDeg (liftShiftToInt Bits.shiftL)
      Shr ->
        liftLowDegToTerm ">>" $ liftIntToLowDeg (liftShiftToInt Bits.shiftR)
    liftShiftToInt
      :: (Integer -> Int -> Integer) -> Integer -> Integer -> Integer
    liftShiftToInt a b c = a b (fromIntegral c)
  UnExpr op e' -> do
    t <- compExpr e'
    return $ case op of
      UnNeg -> -t
      Not   -> liftUnLowDegToTerm
        "!"
        (liftUnIntToLowDeg (\c -> if c /= 0 then 0 else 1))
        t
      UnPos -> case t of
        Array ts -> Base $ fromInteger $ fromIntegral $ length ts
        _        -> t
      BitNot -> error "Bitwise negation has unclear semantics" -- The sematics of this are unclear.
  UnMutExpr op loc -> compUnMutExpr op loc
  Ite c l r        -> do
    condT <- compExpr c
    caseT <- compExpr l
    caseF <- compExpr r
    return $ case condT of
      Base (Scalar 0) -> caseF
      Base (Scalar _) -> caseT
      Base _          -> Base HighDegree
      t               -> error $ "Cannot condition on term " ++ show t
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
                          , lowDegEnv = Map.fromList (zip formalArgs tArgs)
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

compUnMutExpr
  :: KnownNat k => UnMutOp -> Location -> CompState k (Term (Prime k))
compUnMutExpr op loc = do
  lval <- compLoc loc
  term <- gets (load lval)
  let offset = opToOffset op
  let term'  = term + Base (Scalar $ toP offset)
  modify (store lval term')
  case unMutOpTime op of
    Post -> return term
    Pre  -> return term'
 where
  opToOffset o = case unMutOpOp o of
    Inc -> 1
    Dec -> -1

compExprs :: KnownNat n => [Expr] -> CompState n [Term (Prime n)]
compExprs = mapM compExpr

compStatements :: KnownNat n => [Statement] -> CompState n ()
compStatements = void . mapM compStatement

compStatement :: KnownNat n => Statement -> CompState n ()
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
          Base (Scalar 0 ) -> modify id
          Base (Linear lc) -> modify
            $ \c -> c { constraints = (lcZero, lcZero, lc) : constraints c }
          Base (Quadratic q) ->
            modify $ \c -> c { constraints = q : constraints c }
          _ -> error $ "Cannot constain " ++ show zt ++ " to zero"
      -- TODO Not quite right: evals twice
      AssignConstrain l e ->
        compStatements [Assign l e, Constrain (LValue l) e]
      VarDeclaration name dims ini -> do
        ts <- compExprs dims
        modify (alloc name IKVar $ termMultiDimArray (Base (Scalar 0)) ts)
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
        modify (alloc name IKComp (termMultiDimArray (Base (Scalar 0)) ts))
        case ini of
          Just e  -> compStatement (Assign (LocalLocation (name, [])) e)
          Nothing -> return ()
      If cond true false -> do
        tcond <- compExpr cond
        case tcond of
          Base (Scalar 0) -> maybe (return ()) compStatements false
          Base (Scalar _) -> compStatements true
          _ ->
            error
              $  "Invalid conditional term "
              ++ show tcond
              ++ " in "
              ++ show cond
      While cond block -> do
        tcond <- compExpr cond
        case tcond of
          Base (Scalar 0) -> return ()
          Base (Scalar _) -> do
            compStatements block
            compStatement (While cond block)
          _ ->
            error
              $  "Invalid conditional term "
              ++ show tcond
              ++ " in "
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
load :: forall k . KnownNat k => LTerm -> CompCtx (Prime k) -> Term (Prime k)
load loc ctx = case loc of
  LTermLocal (name, idxs) -> case ids ctx Map.!? name of
    Just IKVar  -> extract idxs (lowDegEnv ctx Map.! name)
    Just IKComp -> extract idxs (lowDegEnv ctx Map.! name)
    Just IKSig  -> Base $ Linear $ lcSig $ either
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
              Just (k, dims) | isVisible k -> Base $ Linear $ lcSig $ either
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

subscript :: Int -> Term (Prime k) -> Term (Prime k)
subscript i t = case t of
  Array a -> a Arr.! i
  _       -> error $ "Cannot index term " ++ show t

extract :: [Int] -> Term (Prime k) -> Term (Prime k)
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
  :: forall k
   . KnownNat k
  => String
  -> IdKind
  -> Term (Prime k)
  -> CompCtx (Prime k)
  -> CompCtx (Prime k)
-- Stores a term in a location
alloc name kind term ctx = if Map.member name (ids ctx)
  then error $ "Identifier " ++ show name ++ " already used"
  else ctx { lowDegEnv = Map.insert name term $ lowDegEnv ctx
           , ids       = Map.insert name kind $ ids ctx
           }

store
  :: forall k
   . KnownNat k
  => LTerm
  -> Term (Prime k)
  -> CompCtx (Prime k)
  -> CompCtx (Prime k)
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
      -> (Term (Prime k) -> Term (Prime k))
      -> Term (Prime k)
      -> Term (Prime k)
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
  :: KnownNat k => Term (Prime k) -> [Term (Prime k)] -> Term (Prime k)
termMultiDimArray = foldr
  (\d acc -> case d of
    Base (Scalar n) -> Array $ Arr.listArray (0, i - 1) (replicate i acc)
      where i = fromIntegral $ fromP n
    _ -> error $ "Illegal dimension " ++ show d
  )

-- lifting integer/boolean functions to scalar functions
liftIntToLowDeg
  :: KnownNat n
  => (Integer -> Integer -> Integer)
  -> LowDeg (Prime n)
  -> LowDeg (Prime n)
  -> LowDeg (Prime n)
liftIntToLowDeg f a b = case (a, b) of
  (Scalar c1, Scalar c2) -> Scalar $ toP $ f (fromP c1) (fromP c2)
  _                      -> HighDegree

liftLowDegToTerm
  :: KnownNat n
  => String
  -> (LowDeg (Prime n) -> LowDeg (Prime n) -> LowDeg (Prime n))
  -> Term (Prime n)
  -> Term (Prime n)
  -> Term (Prime n)
liftLowDegToTerm name f s t = case (s, t) of
  (Base a, Base b) -> Base $ f a b
  _ ->
    error
      $  "Cannot perform operation \""
      ++ name
      ++ "\" on terms "
      ++ show s
      ++ " and "
      ++ show t

liftIntPredToLowDeg
  :: KnownNat n
  => (Integer -> Integer -> Bool)
  -> LowDeg (Prime n)
  -> LowDeg (Prime n)
  -> LowDeg (Prime n)
liftIntPredToLowDeg f = liftIntToLowDeg (\a b -> if f a b then 1 else 0)

liftBoolToLowDeg
  :: KnownNat n
  => (Bool -> Bool -> Bool)
  -> LowDeg (Prime n)
  -> LowDeg (Prime n)
  -> LowDeg (Prime n)
liftBoolToLowDeg f =
  liftIntToLowDeg (\a b -> if f (a /= 0) (b /= 0) then 1 else 0)

liftUnLowDegToTerm
  :: KnownNat n
  => String
  -> (LowDeg (Prime n) -> LowDeg (Prime n))
  -> Term (Prime n)
  -> Term (Prime n)
liftUnLowDegToTerm name f t = case t of
  Base a -> Base $ f a
  _ -> error $ "Cannot perform operation \"" ++ name ++ "\" on term " ++ show t

-- Lifts a fun: Integer -> Integer to one that operates over gen-time constant terms
liftUnIntToLowDeg
  :: KnownNat k => (Integer -> Integer) -> LowDeg (Prime k) -> LowDeg (Prime k)
liftUnIntToLowDeg f s = case s of
  Scalar c -> (Scalar . toP . f . fromP) c
  _        -> HighDegree

compMainCtx :: KnownNat k => MainCircuit -> CompCtx (Prime k)
compMainCtx m =
  snd
    $ runCompState (compStatement (SubDeclaration "main" [] (Just (main m))))
    $ empty
        { callables = Map.union
                        (Map.map (\(p, b) -> (False, p, b)) (templates m))
                        (Map.map (\(p, b) -> (True, p, b)) (functions m))
        }

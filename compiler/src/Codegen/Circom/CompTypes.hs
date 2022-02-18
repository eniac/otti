{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Codegen.Circom.CompTypes
  ( BaseTerm(..)
  , BaseCtx(..)
  , LTerm(..)
  , Term(..)
  , termAsNum
  , termAsConst
  , CompCtx(..)
  , IdKind(..)
  , TemplateInvocation
  , ctxOrderedSignals
  , primeUnOp
  , primeBinOp
  , primeIte
  , CompState(..)
  , load
  , store
  , alloc
  , empty
  , nPublicInputs
  , runCompState
  , ltermToSig
  , sigToLterm
  )
where



import           AST.Circom                     ( BinOp(..)
                                                , UnOp(..)
                                                , Span
                                                , SignalKind
                                                , SBlock
                                                , SString
                                                , ann
                                                , ast
                                                , isPublic
                                                , isVisible
                                                , isInput
                                                )
import qualified Codegen.Circom.Signal         as Sig
import           Codegen.Circom.Utils           ( spanE
                                                , mapGetE
                                                )
import qualified Codegen.Circom.Typing         as Typing

import           Control.Monad.State.Strict
import qualified Data.Array                    as Arr
import qualified Data.Bits                     as Bits
import           Data.Field.Galois              ( Prime
                                                , PrimeField
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import           Data.Ix
import qualified Data.List                     as List
import qualified Data.Map.Strict               as Map
import           Util.Log
import           GHC.TypeNats

data LTerm = LTermLocal !Sig.IndexedIdent
           | LTermForeign !Sig.IndexedIdent !Sig.IndexedIdent
           deriving (Show,Eq,Ord,Read)

ltermToSig :: LTerm -> Sig.Signal
ltermToSig l = case l of
  LTermLocal a     -> Sig.SigLocal a
  LTermForeign a b -> Sig.SigForeign a b

sigToLterm :: Sig.Signal -> LTerm
sigToLterm l = case l of
  Sig.SigLocal a     -> LTermLocal a
  Sig.SigForeign a b -> LTermForeign a b


-- A base term type `b` over constant type `k`
class Show b => BaseTerm b k | b -> k where
  fromConst :: k -> b
  fromSignal :: Sig.Signal -> b
  binOp :: BinOp -> b -> b -> b
  unOp :: UnOp -> b -> b
  ite :: b -> b -> b -> b

class (Show c, BaseTerm b k) => BaseCtx c b k | c -> b where
  assert :: b -> c -> c
  emptyCtx :: c
  storeCtx :: Span -> SignalKind -> LTerm -> b -> c -> c
  -- Notification that a particular signal was gotten.
  getCtx :: SignalKind -> LTerm -> c -> c
  ignoreCompBlock :: c -> Bool
  -- Called after function exit.
  finalize :: c -> c

data Term b n = Base !b
              | Array !(Arr.Array Int (Term b n))
              | Component !(TemplateInvocation n)
              | Const !n
              deriving (Show,Eq,Ord)

termAsBase :: (BaseTerm b k) => Term b k -> Maybe b
termAsBase t = case t of
  Base  b -> Just b
  Const n -> Just $ fromConst n
  _       -> Nothing

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

instance (BaseTerm b (Prime k), KnownNat k) => BaseTerm (Term b (Prime k)) (Prime k) where
  fromConst  = Const
  fromSignal = Base . fromSignal
  unOp o t = case (o, t) of
    (_    , Base b ) -> Base $ unOp o b
    (_    , Const f) -> Const $ primeUnOp o f
    (UnPos, Array a) -> Const $ fromInteger $ fromIntegral $ length a
    _                -> error $ "Cannot perform " ++ show o ++ " on " ++ show t

  binOp o s t = case (s, t) of
    (Const a, Const b) -> Const $ primeBinOp o a b
    _                  -> case (termAsBase s, termAsBase t) of
      (Just a, Just b) -> Base $ binOp o a b
      _ ->
        error
          $  "Cannot perform "
          ++ show o
          ++ " on "
          ++ show s
          ++ " and "
          ++ show t
  ite c t f = case c of
    Const c' -> if c' /= 0 then t else f
    Base  c' -> case (termAsBase t, termAsBase f) of
      (Just t', Just f') -> Base $ ite c' t' f'
      -- TODO: base-conditioned, array-branched ITEs
      _ ->
        error
          $  "Cannot evaluate ITE("
          ++ show c
          ++ ", "
          ++ show t
          ++ ", "
          ++ show f
          ++ ")"
    _ -> error $ "Cannot condition on " ++ show c

type TemplateInvocation n = (String, [Term (Void n) n])

data IdKind = IKVar | IKSig | IKComp deriving (Show,Eq,Ord)


data CompCtx c b n = CompCtx { env :: !(Map.Map String (Term b n))
                             , baseCtx :: !c
                             , signals :: !(Map.Map String (SignalKind, [Int]))
                             , type_ :: !Typing.InstanceType
                             , ids :: !(Map.Map String IdKind)
                             , returning :: !(Maybe (Term b n))
                             --                             isFn, frmlArgs, code
                             , callables :: !(Map.Map String (Bool, [String], SBlock))
                             , cache :: !(Map.Map (TemplateInvocation n) (CompCtx c b n))
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

data Void k deriving (Show,Eq,Ord)

instance BaseTerm (Void k) k where
  fromConst _ = error "Void"
  fromSignal _ = error "Void"
  binOp _ = error "Void"
  unOp _ = error "Void"
  ite _ = error "Void"

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

primeIte :: (KnownNat k) => Prime k -> Prime k -> Prime k -> Prime k
primeIte a b c = if a /= 0 then b else c

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

newtype CompState c b n a = CompState (StateT (CompCtx c b (Prime n)) Log a)
    deriving (Functor, Applicative, Monad, MonadState (CompCtx c b (Prime n)), MonadLog, MonadIO)

runCompState
  :: KnownNat n
  => CompState c b n a
  -> CompCtx c b (Prime n)
  -> Log (a, CompCtx c b (Prime n))
runCompState (CompState s) = runStateT s

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

subscript :: (KnownNat k, Show b) => Int -> Term b (Prime k) -> Term b (Prime k)
subscript i t = case t of
  Array a -> a Arr.! i
  _       -> error $ "Cannot index term " ++ show t

extract :: (KnownNat k, Show b) => [Int] -> Term b (Prime k) -> Term b (Prime k)
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

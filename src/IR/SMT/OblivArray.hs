{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : OblivArray
Description : Elimination of array terms which are accessed only at constant indices.

= Overview

This module attempts to identify *oblivious* arrays: those that are only
accessed at constant indices[1]. These arrays can be replaced with normal terms.

It operates in three passes: (1) determine which arrays are oblivious (2)
compute the size of the array back all array terms and (3) replace oblivious
arrays with (haskell) lists of terms.

== Pass 1: Identifying oblivious arrays

We maintain a set of non-oblivious arrays, initially empty. We traverse the
whole SMT constraint system, performing the following inferences:

* If @a[i]@ for non-constant @i@, then @a@ is not oblivious
* If @a[i\v]@ for non-constant @i@, then neither @a[i\v]@ nor @a@ are oblivious
* If @a[i\v]@, then @a[i\v]@ and @a@ are equi-oblivious
* If @ite(c,a,b)@, then @ite(c,a,b)@, @a@, and @b@ are equi-oblivious
* If @a=b@, then @a@ and @b@ are equi-oblivious

This procedure is iterated to fixpoint.

== Pass 2: Computing array sizes

This is done similar to the above, propagating array sizes instead of
non-obliviousness. If a conflict is found, throw an error.

== Pass 3: Replacing oblivious arrays with term lists.

In this pass, the goal is to

* map array terms to haskell lists of value terms
* map array selections to specific value terms

The pass maintains:

* a map from array terms to lists of values

It then does a bottom-up formula traversal, performing the following
transformations:

* oblivious array variables are mapped to a list of (derivative)
  variables
* oblivious constant arrays are mapped to a list that replicates the
  constant
* accesses to oblivious arrays are mapped to the appropriate term from the
  value list of the array
* stores to oblivious arrays are mapped to updated value lists
* equalities between oblivious arrays are mapped to conjunctions of equalities

[1]: Our use of "oblivious" is inspired by *oblivious turing
machines* <https://en.wikipedia.org/wiki/Turing_machine_equivalents#Oblivious_Turing_machines>
which access their tape in a data-indepedant way.

-}

module IR.SMT.OblivArray
  ( elimOblivArrays
  )
where

import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           IR.SMT.TySmt
import           IR.SMT.MemReplacePass
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Control                   ( whenM )
import           Util.Cfg                       ( MonadCfg(..) )
import           Util.Log
import qualified Util.Progress                 as P
import           Util.Progress                  ( Progress )

type MemSort = ArraySort DynBvSort DynBvSort
type TermMem = Term MemSort
type ArraySet = ShowMap TermMem ()

-- |
--
-- Given a list of assertions, build a set of array terms which are *not*
-- oblivious.
findNonObliviousArrays :: [TermBool] -> Log ArraySet
findNonObliviousArrays ts = P.runToFixPoint pass SMap.empty
 where
  -- |
  --
  -- Mark this array non-oblivious, and if it wasn't already so marked set the
  -- progress flag.
  markNotOblivious :: TermMem -> Progress ArraySet ()
  markNotOblivious a = do
    alreadyMarked <- gets (SMap.member a . fst)
    unless alreadyMarked $ do
      logIf "array::elim::mark" $ "Marking: " ++ show a
      P.modify $ SMap.insert a ()
      P.setProgress

  isBvLiteral :: TermDynBv -> Bool
  isBvLiteral t = case t of
    DynBvLit{} -> True
    _          -> False

  -- | Assert that the non-obliviousness of a implies that of b.
  implicate :: TermMem -> TermMem -> Progress ArraySet ()
  implicate a b = whenM (gets (SMap.member a . fst)) $ markNotOblivious b

  -- | Assert equality of obliviousness
  biImplicate a b = implicate a b >> implicate b a

  onePass = defaultMemReplacePass
    { visitSelect = \_ _ a i ->
      unless (isBvLiteral i) (markNotOblivious a) >> return Nothing
    , visitStore  = \a i v _ _ _ -> if isBvLiteral i
                      then biImplicate a (Store a i v)
                      else markNotOblivious a >> markNotOblivious (Store a i v)
    , visitEq     = \a b _ _ -> biImplicate a b >> return Nothing
    , visitIte    = \c t f _ _ _ -> do
                      biImplicate f (Ite c t f)
                      biImplicate t (Ite c t f)
                      biImplicate t f
    }

  pass :: Progress ArraySet ()
  pass = do
    logIf "array::elim::mark" "Start mark pass"
    void $ runMemReplacePass onePass ts

-- | Propagate static array size through a formula.
propSizes :: ArraySizes -> [TermBool] -> Log ArraySizes
propSizes initSizes ts = P.runToFixPoint pass initSizes
 where
  equateSizes :: TermMem -> TermMem -> Progress ArraySizes ()
  equateSizes a b = do
    logIf "array::elim::sizes"
      $ unlines ["Equating sizes:", "  " ++ show a, "  " ++ show b]
    mAS <- P.gets $ SMap.lookup a
    mBS <- P.gets $ SMap.lookup b
    case (mAS, mBS) of
      (Just aS, Just bS) -> unless (aS == bS) $ error $ unlines
        [ "Unequal array sizes:"
        , "size " ++ show aS ++ ": " ++ show a
        , "size " ++ show bS ++ ": " ++ show b
        ]
      (Just aS, Nothing) -> P.setProgress >> P.modify (SMap.insert b aS)
      (Nothing, Just bS) -> P.setProgress >> P.modify (SMap.insert a bS)
      (Nothing, Nothing) -> return ()

  onePass = defaultMemReplacePass
    { visitStore = \a i v _ _ _ -> equateSizes (Store a i v) a
    , visitIte   = \c t f _ _ _ -> do
                     equateSizes (Ite c t f) t
                     equateSizes t           f
                     equateSizes (Ite c t f) f
    , visitEq    = \a b _ _ -> equateSizes a b >> return Nothing
    }

  pass :: Progress ArraySizes ()
  pass = do
    logIf "array::elim::sizes" "Start sizes pass"
    -- We traverse forwards and then backwards, since assertions fall in
    -- assignment order, and natural propagation can go either way.
    void $ runMemReplacePass onePass (ts ++ reverse ts)

type TermListMap = ShowMap TermMem [TermDynBv]
type ArraySizes = ShowMap TermMem Int

newtype ArrayElim a = ArrayElim (StateT TermListMap Log a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState TermListMap)

modList :: Int -> a -> [a] -> [a]
modList _ _ []      = error "oob modList"
modList 0 v (_ : t) = v : t
modList i v (h : t) = h : modList (i - 1) v t


replaceObliviousArrays :: ArraySizes -> ArraySet -> [TermBool] -> Log [TermBool]
replaceObliviousArrays arraySizes nonOblivious ts = evalStateT pass SMap.empty
 where
  asConstInt :: TermDynBv -> Maybe Int
  asConstInt t = case t of
    DynBvLit bv -> Just $ fromInteger $ Bv.nat bv
    _           -> Nothing

  isOblivious :: TermMem -> Bool
  isOblivious t = not $ SMap.member t nonOblivious

  size :: TermMem -> Int
  size t =
    fromMaybe (error $ "No size for " ++ show t) $ SMap.lookup t arraySizes

  getValues :: TermMem -> ArrayElim [TermDynBv]
  getValues array = gets
    (fromMaybe (error $ "No value list for " ++ show array) . SMap.lookup array)

  atIndex :: Int -> [a] -> a
  atIndex i =
    fromMaybe (error $ "No value at index " ++ show i) . listToMaybe . drop i

  valueSort :: Sort -> Sort
  valueSort s = case s of
    SortArray _ v -> v
    _             -> error $ "Sort " ++ show s ++ " is not an array sort"

  store :: TermMem -> [TermDynBv] -> ArrayElim ()
  store t l = do
    logIf "array::elim::replace" $ "Replace: " ++ show t ++ " -> " ++ show l
    modify $ SMap.insert t l

  visitors = (defaultMemReplacePass :: MemReplacePass ArrayElim)
    { visitConstArray = \v sort v' ->
                          let c = ConstArray sort v
                          in  when (isOblivious c) $ store c $ replicate
                                (size c)
                                v'
    , visitStore      = \a i v a' i' v' -> case asConstInt i' of
                          Just ci | isOblivious (Store a i v) ->
                            (modList ci v' <$> getValues a')
                              >>= store (Store a' i' v')
                          _ -> return ()
    , visitIte        = \c t f c' t' f' ->
                          when (isOblivious (Ite c t f))
                            $ liftM2 (zipWith (Ite c')) (getValues t') (getValues f')
                            >>= store (Ite c' t' f')
    , visitVar        = \name sort ->
                          let var = Var name sort
                          in  when (isOblivious var) $ store var $ map
                                (\i -> Var (name ++ "_" ++ show i) (valueSort sort))
                                [0 .. (size var - 1)]
    , visitSelect     = \a _ a' i' -> case asConstInt i' of
                          Just ci | isOblivious a ->
                            Just . atIndex ci <$> getValues a'
                          _ -> return Nothing
    , visitEq         = \a _ a' b' -> if isOblivious a
                          then Just . BoolNaryExpr And <$> liftM2 (zipWith mkEq)
                                                                  (getValues a')
                                                                  (getValues b')
                          else return Nothing
    }

  ArrayElim pass = runMemReplacePass visitors ts


elimOblivArrays :: ArraySizes -> [TermBool] -> Log [TermBool]
elimOblivArrays sizes ts = do
  nonOblivious <- findNonObliviousArrays ts
  sizes'       <- propSizes sizes ts
  logIf "array::elim" $ "Array sizes: " ++ show sizes'
  replaceObliviousArrays sizes' nonOblivious ts

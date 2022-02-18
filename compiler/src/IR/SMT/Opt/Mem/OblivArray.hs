{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

module IR.SMT.Opt.Mem.OblivArray
  ( elimOblivArrays
  )
where

import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           IR.SMT.TySmt
import qualified IR.SMT.Opt.Assert             as OA
import           IR.SMT.Opt.Assert              ( Assert )
import           IR.SMT.Opt.Mem.Util
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Control                   ( whenM )
import           Util.Cfg                       ( MonadCfg(..) )
import           Util.Log
import qualified Util.Progress                 as P
import           Util.Progress                  ( Progress )
import           Util.Show

type ArraySet = ShowMap TMem ()

-- | Given a list of assertions, build a set of array terms which are *not*
-- oblivious.
findNonObliviousArrays :: [TermBool] -> Log ArraySet
findNonObliviousArrays ts = P.runToFixPoint pass SMap.empty
 where
  -- |
  --
  -- Mark this array non-oblivious, and if it wasn't already so marked set the
  -- progress flag.
  markNotOblivious :: TMem -> Progress ArraySet ()
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
  implicate :: TMem -> TMem -> Progress ArraySet ()
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
    void $ mapM (runMemReplacePass onePass) ts


type TermListMap = ShowMap TMem [TermDynBv]

newtype Obliv a = Obliv (StateT TermListMap Assert a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState TermListMap, OA.MonadAssert)

modList :: Int -> a -> [a] -> [a]
modList _ _ []      = error "oob modList"
modList 0 v (_ : t) = v : t
modList i v (h : t) = h : modList (i - 1) v t


replaceObliviousArrays :: ArraySizes -> ArraySet -> Assert ()
replaceObliviousArrays arraySizes nonOblivious = evalStateT pass SMap.empty
 where
  asConstInt :: TermDynBv -> Maybe Int
  asConstInt t = case t of
    DynBvLit bv -> Just $ fromInteger $ Bv.nat bv
    _           -> Nothing

  isOblivious :: TMem -> Bool
  isOblivious t = not $ SMap.member t nonOblivious

  size :: TMem -> Int
  size t =
    fromMaybe (error $ "No size for " ++ show t) $ SMap.lookup t arraySizes

  getTerms :: TMem -> Obliv [TermDynBv]
  getTerms array = gets
    (fromMaybe (error $ "No value list for " ++ show array) . SMap.lookup array)

  atIndex :: Int -> [a] -> a
  atIndex i =
    fromMaybe (error $ "No value at index " ++ show i) . listToMaybe . drop i

  valueSort :: Sort -> Sort
  valueSort s = case s of
    SortArray _ v -> v
    _             -> error $ "Sort " ++ show s ++ " is not an array sort"

  idxWidth :: Sort -> Int
  idxWidth s = case s of
    SortArray (SortBv i) _ -> i
    _ -> error $ "Sort " ++ show s ++ " is not an array(Bv,_) sort"

  store :: TMem -> [TermDynBv] -> Obliv ()
  store t l = do
    -- We truncate the list, because ConstArrays have an infinite list
    logIf "array::elim::replace" $ "Replace: " ++ show t ++ " -> " ++ show
      (take 10 l)
    modify $ SMap.insert t l

  visitors = (defaultMemReplacePass :: MemReplacePass Obliv)
    { visitConstArray = \v sort v' ->
                          let c = ConstArray sort v
                          in  when (isOblivious c) $ store c $ repeat v'
    , visitStore      = \a i v a' i' v' -> case asConstInt i' of
                          Just ci | isOblivious (Store a i v) -> do
                            logIf "array::elim::store" $ "Store: " ++ show
                              (Store a i v)
                            l' <- modList ci v' <$> getTerms a'
                            logIf "array::elim::store" $ " => " ++ show l'
                            store (Store a' i' v') l'
                          _ -> return ()
    , visitIte        = \c t f c' t' f' ->
                          when (isOblivious (Ite c t f))
                            $ liftM2 (zipWith (Ite c')) (getTerms t') (getTerms f')
                            >>= store (Ite c' t' f')
    , visitVar        = \name sort -> do
      let var = Var name sort
      when (isOblivious var) $ do
        let varName i = name ++ "_" ++ show i
        let w = idxWidth sort
        ts :: [TermDynBv] <- forM [0 .. (size var - 1)] $ \i -> do
          let s = mkSelect (Var name sort) $ DynBvLit $ Bv.bitVec w i
          OA.newVar (varName i) (valueSort sort) s
        store var ts
    , visitSelect     = \a _ a' i' -> case asConstInt i' of
                          Just ci | isOblivious a ->
                            Just . atIndex ci <$> getTerms a'
                          _ -> return Nothing
    , visitEq         = \a _ a' b' -> if isOblivious a
                          then Just . BoolNaryExpr And <$> liftM2 (zipWith mkEq)
                                                                  (getTerms a')
                                                                  (getTerms b')
                          else return Nothing
    }

  Obliv pass = OA.modifyAssertionsWith $ runMemReplacePass visitors


elimOblivArrays :: Assert ()
elimOblivArrays = do
  -- TODO: push Asssert deeper?
  sizes        <- gets (OA._sizes)
  as           <- gets OA.listAssertions
  nonOblivious <- liftLog $ findNonObliviousArrays as
  logIf "array::elim::sizes" $ "Initial sizes: " ++ pShow sizes
  sizes' <- liftLog $ propSizes sizes as
  replaceObliviousArrays sizes' nonOblivious

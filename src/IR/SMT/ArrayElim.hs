{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : ArrayElim
Description : Elimination of array terms which are accessed only at constant indices.

= Overview

This module attempts to identify *oblivious* arrays: those that are only
accessed at constant indices[1]. These arrays can be replaced with normal terms.

It operates in two passes: (1) determine which arrays are oblivious and (2)
replace oblivious arrays with (haskell) lists of terms.

== Pass 1: Identifying oblivious arrays

We maintain a set of non-oblivious arrays, initially empty. We traverse the
whole SMT constraint system, performing the following inferences:

* If @-Ob(a[i])@ and @i@ is not constant, then @-Ob(a)@.
* If @-Ob(a[i\v])@ and @i@ is not constant, then @-Ob(a)@.
* If @-Ob(a[i\v])@ and @-Ob(a[i\v])@, then @-Ob(a)@.
* If @a = a'@ and @-Ob(a')@, then @-Ob(a)@ (the same symmetrically).

where Ob is the obliviousness predicate. This procedure is iterated to fixpoint.

== Pass 2: Replacing oblivious arrays with term lists.

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

module IR.SMT.ArrayElim
  ( elimArrays
  )
where

import           Control.Monad.State.Strict
import           Data.Typeable                  ( cast )
import           IR.SMT.TySmt
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Log
import           Util.Cfg                       ( MonadCfg(..) )

type MemSort = ArraySort DynBvSort DynBvSort
type TermMem = Term MemSort
type ArraySet = ShowMap TermMem ()

-- | Monad for the identification of oblivious arrays
-- First element is the set of non-oblivious arrays. Second is a progress mark
-- for this pass, used to detect fixed-point.
newtype IdOb a = IdOb (StateT (ArraySet, Bool) Log a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState (ArraySet, Bool))


markNotOblivious :: TermMem -> IdOb ()
markNotOblivious a = do
  alreadyMarked <- gets (SMap.member a . fst)
  unless alreadyMarked $ do
    liftLog $ logIf "array::elim::mark" $ "Marking: " ++ show a
    modify $ \(set, _) -> (SMap.insert a () set, True)

findNonObliviousArrays :: [TermBool] -> Log ArraySet
findNonObliviousArrays ts = go SMap.empty
 where
  isBvLiteral :: TermDynBv -> Bool
  isBvLiteral t = case t of
    DynBvLit{} -> True
    _          -> False

  visit :: forall s . SortClass s => Term s -> IdOb (Maybe (Term s))
  visit t = do
    case t of
      Select array idx -> forM_ (cast (array, idx)) $ \(array', idx') ->
        unless (isBvLiteral idx') $ markNotOblivious array'
      Store array idx _value ->
        forM_ (cast (t, array, idx)) $ \(t', array', idx') ->
          if isBvLiteral idx'
            then do
              nonObliviousOuter <- gets (SMap.member t' . fst)
              when nonObliviousOuter $ markNotOblivious array'
            else markNotOblivious array'
      Eq a0 a1 -> forM_ (cast (a0, a1)) $ \(a0', a1') -> do
        a0nonOblivious <- gets (SMap.member a0' . fst)
        if a0nonOblivious
          then markNotOblivious a1'
          else do
            a1nonOblivious <- gets (SMap.member a1' . fst)
            when a1nonOblivious $ markNotOblivious a0'
      _ -> return ()
    return Nothing

  go :: ArraySet -> Log ArraySet
  go s = do
    liftLog $ logIf "array::elim::mark" "Start mark pass"
    let (IdOb pass) = forM_ ts $ mapTermM visit
    (s', progress) <- execStateT pass (s, False)
    if progress then go s' else return s


elimArrays :: [TermBool] -> Log [TermBool]
elimArrays ts = do
  _ <- findNonObliviousArrays ts
  return ts

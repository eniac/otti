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
import           Data.Typeable                  ( cast
                                                , Typeable
                                                , eqT
                                                , (:~:)(..)
                                                )
import qualified Data.BitVector                as Bv
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           IR.SMT.TySmt
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Log
import           Util.Cfg                       ( MonadCfg(..) )

type MemSort = ArraySort DynBvSort DynBvSort
type TermMem = Term MemSort
type ArraySet = ShowMap TermMem ()

-- |
--
-- Monad for the identification of oblivious arrays.
--
-- First element is the set of non-oblivious arrays. Second is a progress mark
-- for this pass, used to detect fixed-point.
newtype IdOb a = IdOb (StateT (ArraySet, Bool) Log a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState (ArraySet, Bool))

-- |
--
-- Mark this array non-oblivious, and if it wasn't already so marked set the
-- progress flag.
markNotOblivious :: TermMem -> IdOb ()
markNotOblivious a = do
  alreadyMarked <- gets (SMap.member a . fst)
  unless alreadyMarked $ do
    liftLog $ logIf "array::elim::mark" $ "Marking: " ++ show a
    modify $ \(set, _) -> (SMap.insert a () set, True)


-- |
--
-- Given a list of assertions, build a set of array terms which are *not*
-- oblivious.
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

type TermListMap = ShowMap TermMem [TermDynBv]
type ArraySizes = ShowMap TermMem Int

newtype ArrayElim a = ArrayElim (StateT TermListMap Log a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState TermListMap)

modList :: Int -> a -> [a] -> [a]
modList _ _ []      = error "oob modList"
modList 0 v (h : t) = v : t
modList i v (h : t) = h : modList (i - 1) v t


replaceObliviousArrays :: ArraySizes -> ArraySet -> [TermBool] -> Log [TermBool]
replaceObliviousArrays arraySizes nonOblivious ts = evalStateT elimInAll
                                                               SMap.empty
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

  fCast :: (Typeable a, Typeable b) => a -> b
  fCast = fromMaybe (error "Bad cast") . cast

  valueSort :: Sort -> Sort
  valueSort s = case s of
    SortArray _ v -> v
    _             -> error $ "Sort " ++ show s ++ " is not an array sort"

  visit :: forall s . SortClass s => Term s -> ArrayElim (Maybe (Term s))
  visit t = case eqT @(Term s) @TermMem of
    Just Refl -> case t of
      ConstArray sort value -> do
        value' <- rec value
        let t' = ConstArray sort value'
        when (isOblivious t) $ do
          l <- replicate (size t) <$> rec value
          liftLog
            $  logIf "array::elim::replace"
            $  "Replace: "
            ++ show t
            ++ " -> "
            ++ show l
          modify $ SMap.insert t' l
        return $ Just t'
      Store array idx value -> do
        array' <- rec array
        idx'   <- rec idx
        value' <- rec value
        let t' = Store array' idx' value'
        case asConstInt idx' of
          Just constIdx | isOblivious array' -> do
            l <- modList constIdx value' <$> getValues array'
            liftLog
              $  logIf "array::elim::replace"
              $  "Replace: "
              ++ show t
              ++ " -> "
              ++ show l
            modify $ SMap.insert t' l
          _ -> pure ()
        return $ Just t'
      Ite{}         -> error ""
      Var name sort -> do
        let l = map (\i -> Var (name ++ "_" ++ show i) (valueSort sort))
                    [(0 :: Int) .. (error "need to get array var sizes")]
        liftLog
          $  logIf "array::elim::replace"
          $  "Replace: "
          ++ show t
          ++ " -> "
          ++ show l
        modify $ SMap.insert t l
        return $ Just t
      Exists{}   -> error "nyi"
      ArrayLit{} -> error "nyi"
      Let{}      -> error "nyi"
      NewArray{} -> error "nyi"
      Select{}   -> error "nyi"
    Nothing -> case t of
      Select array idx -> do
        array'' <- rec array
        idx''   <- rec idx
        Just <$> case cast (array'', idx'') of
          Just (array', idx') -> fCast <$> case asConstInt idx' of
            Just constIdx | isOblivious array' -> do
              v <- atIndex constIdx <$> getValues array'
              liftLog
                $  logIf "array::elim::replace"
                $  "Replace: "
                ++ show t
                ++ " -> "
                ++ show v
              return v
            _ -> return $ Select array' idx'
          Nothing -> return $ Select array'' idx''
      Eq a0 a1 -> do
        a0' <- rec a0
        a1' <- rec a1
        Just <$> case cast (a0', a1') of
          Just (a0'', a1'') | isOblivious a0'' && isOblivious a1'' -> do
            v <- BoolNaryExpr And
              <$> liftM2 (zipWith Eq) (getValues a0'') (getValues a1'')
            liftLog
              $  logIf "array::elim::replace"
              $  "Replace: "
              ++ show t
              ++ " -> "
              ++ show v
            return v
          _ -> return $ Eq a0' a1'
      _ -> return Nothing
   where
    rec :: SortClass s2 => Term s2 -> ArrayElim (Term s2)
    rec = mapTermM visit

  (ArrayElim elimInAll) = mapM (mapTermM visit) ts


elimArrays :: ArraySizes -> [TermBool] -> Log [TermBool]
elimArrays sizes ts = do
  nonOblivious <- findNonObliviousArrays ts
  logIf "array::elim" $ "Array sizes: " ++ show sizes
  ts'          <- replaceObliviousArrays sizes nonOblivious ts
  return ts'

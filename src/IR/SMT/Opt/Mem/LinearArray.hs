{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-|
Module      : LinearArray
Description : Replacing array terms with sequences of values that are
              linearly traversed during accesses

= Overview

This module replaces the indicated arrays with lists of terms that are
linearly scanned during access.


-}

module IR.SMT.Opt.Mem.LinearArray
  ( linearizeArrays
  )
where

import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import qualified Data.Set                      as Set
import           Data.Set                       ( Set )
import           Data.Maybe                     ( fromMaybe )
import           Data.List                      ( foldl' )
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

-- |
--
-- Given a base set of array terms, and a list of assertions, close the set of
-- array terms under STORE and EQ
findArrays :: ArraySet -> [TermBool] -> Log ArraySet
findArrays intialSet ts = P.runToFixPoint pass intialSet
 where
  -- |
  --
  -- Add this array to the set, and if it wasn't already so marked set the
  -- progress flag.
  addToSet :: TMem -> Progress ArraySet ()
  addToSet a = do
    present <- gets (SMap.member a . fst)
    unless present $ do
      logIf "array::lin::mark" $ "Marking: " ++ show a
      P.modify $ SMap.insert a ()
      P.setProgress

  -- | Assert that the presence of a implies that of b.
  implicate :: TMem -> TMem -> Progress ArraySet ()
  implicate a b = whenM (gets (SMap.member a . fst)) $ addToSet b

  -- | Assert equality of presence
  biImplicate a b = implicate a b >> implicate b a

  onePass = defaultMemReplacePass
    { visitStore = \a i v _ _ _ -> biImplicate a (Store a i v)
    , visitEq    = \a b _ _ -> biImplicate a b >> return Nothing
    , visitIte   = \c t f _ _ _ -> do
                     biImplicate f (Ite c t f)
                     biImplicate t (Ite c t f)
                     biImplicate t f
    }

  pass :: Progress ArraySet ()
  pass = do
    logIf "array::lin::mark" "Start mark pass"
    void $ mapM (runMemReplacePass onePass) ts

type TermListMap = ShowMap TMem [TBv]

newtype Lin a = Lin (StateT TermListMap Assert a)
 deriving (MonadLog, MonadCfg, Functor, Applicative, Monad, MonadState TermListMap, OA.MonadAssert)

-- | Referencing @arraySizes@, replaces all arrays in @toLin@ with term
-- sequences that are linearly scanned.
replaceLinearArrays :: ArraySizes -> ArraySet -> Assert ()
replaceLinearArrays arraySizes toLin = evalStateT pass SMap.empty
 where
  replace :: TMem -> Bool
  replace t = SMap.member t toLin

  size :: TMem -> Int
  size t =
    fromMaybe (error $ "No size for " ++ show t) $ SMap.lookup t arraySizes

  getTerms :: TMem -> Lin [TBv]
  getTerms array = gets
    (fromMaybe (error $ "No value list for " ++ show array) . SMap.lookup array)

  mkLinSelect :: [TBv] -> TBv -> TBv
  mkLinSelect l addr = if null l
    then error "Cannot mkLinSelect from null array"
    else foldl' ite (head l) (zip [(1 :: Int) ..] (tail l))
   where
    ite acc (i, v) = mkIte (Eq addr (DynBvLit $ Bv.bitVec len i)) v acc
    len = dynBvWidth addr

  mkLinStore :: [TBv] -> TBv -> TBv -> [TBv]
  mkLinStore array addr value = zipWith ite [(0 :: Int) ..] array
   where
    ite i oldValue =
      mkIte (Eq addr (DynBvLit $ Bv.bitVec len i)) value oldValue
    len = dynBvWidth addr


  valueSort :: Sort -> Sort
  valueSort s = case s of
    SortArray _ v -> v
    _             -> error $ "Sort " ++ show s ++ " is not an array sort"

  idxWidth :: Sort -> Int
  idxWidth s = case s of
    SortArray (SortBv i) _ -> i
    _ -> error $ "Sort " ++ show s ++ " is not an array(Bv,_) sort"

  store :: TMem -> [TBv] -> Lin ()
  store t l = do
    -- We truncate the list, because ConstArrays have an infinite list
    logIf "array::lin::replace" $ "Replace: " ++ show t ++ " -> " ++ show
      (take 10 l)
    modify $ SMap.insert t l

  visitors = (defaultMemReplacePass :: MemReplacePass Lin)
    { visitConstArray = \v sort v' ->
                          let c = ConstArray sort v
                          in  when (replace c) $ store c $ repeat v'
    , visitStore      = \a i v a' i' v' -> when (replace (Store a i v)) $ do
                          logIf "array::lin::store" $ "Store: " ++ show (Store a i v)
                          let s = size a
                          l <- take s <$> getTerms a'
                          let l' = mkLinStore l i' v'
                          logIf "array::lin::store" $ " => " ++ show l'
                          store (Store a' i' v') l'
    , visitIte        = \c t f c' t' f' ->
                          when (replace (Ite c t f))
                            $ liftM2 (zipWith (Ite c')) (getTerms t') (getTerms f')
                            >>= store (Ite c' t' f')
    , visitVar        = \name sort -> do
      let var = Var name sort
      when (replace var) $ do
        let varName i = name ++ "_" ++ show i
        let w = idxWidth sort
        ts :: [TBv] <- forM [0 .. (size var - 1)] $ \i -> do
          let s = mkSelect (Var name sort) $ DynBvLit $ Bv.bitVec w i
          OA.newVar (varName i) (valueSort sort) s
        store var ts
    , visitSelect     = \a _ a' i' -> if (replace a)
                          then do
                            l <- getTerms a'
                            when (length l > 10000)
                              $ error "Array in select too big"
                            return $ Just $ mkLinSelect l i'
                          else return Nothing
    , visitEq         = \a _ a' b' -> if replace a
                          then Just . BoolNaryExpr And <$> liftM2 (zipWith mkEq)
                                                                  (getTerms a')
                                                                  (getTerms b')
                          else return Nothing
    }

  Lin pass = OA.modifyAssertionsWith $ runMemReplacePass visitors

getTermsForVars :: Set String -> [TermBool] -> Log ArraySet
getTermsForVars names as =
  let v = (defaultMemReplacePass :: MemReplacePass (StateT ArraySet Log))
        { visitVar = \name sort ->
                       when (Set.member name names) $ modify $ SMap.insert
                         (Var name sort)
                         ()
        }
      f = mapM (runMemReplacePass v) as
  in  execStateT f SMap.empty

linearizeArrays :: Set String -> Assert ()
linearizeArrays arrayNames = do
  sizes          <- gets (OA._sizes)
  as             <- gets OA.listAssertions
  initArrayTerms <- liftLog $ getTermsForVars arrayNames as
  toLin          <- liftLog $ findArrays initArrayTerms as
  logIf "array::lin::sizes" $ "Initial sizes: " ++ pShow sizes
  sizes' <- liftLog $ propSizes sizes as
  replaceLinearArrays sizes' toLin

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : Util
Description : General SMT Mem traversals

= MemReplace Pass

For visiting and optionally replacing memory interactions.

That is, interactions with terms which are sorted as arrays from dynamic
bitvectors to dynamic bitvectors.
-}
module IR.SMT.Opt.Mem.Util
  ( ArraySizes
  , propSizes
  , TBv
  , TMem
  , MemReplacePass(..)
  , runMemReplacePass
  , defaultMemReplacePass
  )
where
import           Control.Monad.State.Strict     ( void
                                                , unless
                                                )
import           IR.SMT.TySmt
import           IR.SMT.TySmt.Alg               ( mapTermM )
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )
import           Util.Log                       ( logIf
                                                , Log
                                                , MonadLog
                                                )
import qualified Util.Progress                 as P
import           Util.Progress                  ( Progress )
import           Data.Typeable                  ( cast
                                                , Typeable
                                                , eqT
                                                , (:~:)(..)
                                                )
import           Data.Maybe                     ( fromMaybe )


type TBv = Term DynBvSort
type MemSort = ArraySort DynBvSort DynBvSort
type TMem = Term MemSort
type ArraySizes = ShowMap TMem Int

isConstArray :: TMem -> Bool
isConstArray (ConstArray{}) = True
isConstArray _              = False

-- | Propagate static array size through a formula.
propSizes :: ArraySizes -> [TermBool] -> Log ArraySizes
propSizes initSizes ts = P.runToFixPoint pass initSizes
 where
  equateSizes :: TMem -> TMem -> Progress ArraySizes ()
  equateSizes a b = unless (isConstArray a || isConstArray b) $ do
    mAS <- P.gets $ SMap.lookup a
    mBS <- P.gets $ SMap.lookup b
    case (mAS, mBS) of
      (Just aS, Just bS) -> unless (aS == bS) $ error $ unlines
        [ "Unequal array sizes:"
        , "size " ++ show aS ++ ": " ++ show a
        , "size " ++ show bS ++ ": " ++ show b
        ]
      (Just aS, Nothing) -> setSize b aS
      (Nothing, Just bS) -> setSize a bS
      (Nothing, Nothing) -> return ()

  setSize :: TMem -> Int -> Progress ArraySizes ()
  setSize t s = do
    P.setProgress
    P.modify (SMap.insert t s)
    logIf "array::elim::sizes" $ unwords ["Size", show s, show t]

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
    void $ mapM (runMemReplacePass onePass) (ts ++ reverse ts)



data MemReplacePass m = MemReplacePass
  { -- Each function gets the original and new version of each child.
    -- Original versions first. They may be useful for interacting with a
    -- preprocessing analysis.

    -- No previous sort, since it is unchanged
    visitConstArray :: TBv -> Sort -> TBv -> m ()
  , visitEq         :: TMem -> TMem -> TMem -> TMem -> m (Maybe TermBool)
  , visitIte :: TermBool -> TMem -> TMem -> TermBool -> TMem -> TMem -> m ()
  , visitStore      :: TMem -> TBv -> TBv -> TMem -> TBv -> TBv -> m ()
  , visitSelect     :: TMem -> TBv -> TMem -> TBv -> m (Maybe TBv)
    -- No previous children, as they do not change.
  , visitVar        :: String -> Sort -> m ()
  }

defaultMemReplacePass :: Monad m => MemReplacePass m
defaultMemReplacePass = MemReplacePass
  { visitConstArray = \_ _ _ -> return ()
  , visitEq         = \_ _ _ _ -> return Nothing
  , visitIte        = \_ _ _ _ _ _ -> return ()
  , visitVar        = \_ _ -> return ()
  , visitStore      = \_ _ _ _ _ _ -> return ()
  , visitSelect     = \_ _ _ _ -> return Nothing
  }

runMemReplacePass
  :: forall m . MonadLog m => MemReplacePass m -> TermBool -> m TermBool
runMemReplacePass pass = mapTermM visit
 where
  -- Force cast
  fCast :: (Typeable a, Typeable b) => a -> b
  fCast = fromMaybe (error "Bad cast") . cast

  visit :: forall s . SortClass s => Term s -> m (Maybe (Term s))
  visit t = case eqT @(Term s) @TMem of
    Just Refl -> case t of
      ConstArray sort value -> do
        value' <- rec value
        logIf "mem::replace::pass" $ "visitConstArray: " ++ show t
        visitConstArray pass value sort value'
        return $ Just $ ConstArray sort value'
      Store array idx value -> do
        array' <- rec array
        idx'   <- rec idx
        value' <- rec value
        logIf "mem::replace::pass" $ "visitStore: " ++ show t
        visitStore pass array idx value array' idx' value'
        return $ Just $ Store array' idx' value'
      Ite c a b -> do
        c' <- rec c
        a' <- rec a
        b' <- rec b
        logIf "mem::replace::pass" $ "visitIte: " ++ show t
        visitIte pass c a b c' a' b'
        return $ Just $ Ite c' a' b'
      Var name sort -> do
        logIf "mem::replace::pass" $ "visitVar: " ++ show t
        visitVar pass name sort
        return $ Just t
      Exists{} -> error "nyi: existential memories"
      Let{}    -> error "nyi: let bindings for memories"
      Select{} -> error "nyi: selecting a member from an array"
    Nothing -> case t of
      Select array idx -> do
        array'' <- rec array
        idx''   <- rec idx
        Just <$> case cast (array'', idx'') of
          Just (array', idx') -> do
            logIf "mem::replace::pass" $ "visitSelect: " ++ show t
            mReplaced <- visitSelect pass (fCast array) (fCast idx) array' idx'
            return $ fCast $ fromMaybe (Select array' idx') mReplaced
          Nothing -> return $ Select array'' idx''
      Eq a0 a1 -> do
        a0' <- rec a0
        a1' <- rec a1
        Just <$> case cast (a0', a1') of
          Just (a0'', a1'') -> do
            logIf "mem::replace::pass" $ "visitEq: " ++ show t
            mReplaced <- visitEq pass (fCast a0) (fCast a1) a0'' a1''
            return $ fromMaybe (mkEq a0'' a1'') mReplaced
          _ -> return $ Eq a0' a1'
      _ -> return Nothing
   where
    rec :: SortClass s2 => Term s2 -> m (Term s2)
    rec = mapTermM visit

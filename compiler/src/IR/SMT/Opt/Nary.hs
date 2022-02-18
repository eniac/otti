{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-|

= N-ary operator flattening

This module searches for nested n-ary bit-vector and boolean operators,
flattening them.

Enables more efficient translation to R1cs later in the pipeline.
-}
module IR.SMT.Opt.Nary
  ( flattenNary
  )
where
import           IR.SMT.TySmt
import           IR.SMT.TySmt.Alg               ( mapTermM )
import qualified IR.SMT.Opt.Assert             as OA
import           IR.SMT.Opt.Assert              ( Assert )
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict           as HMap
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , view
                                                )
import           Util.Log

data FState = FState
  { _bvs :: !(HMap.HashMap TermDynBv (BvNaryOp, [TermDynBv]))
  , _bools :: !(HMap.HashMap TermBool (BoolNaryOp, [TermBool]))
  }
  deriving Show

$(makeLenses ''FState)

newtype F a = F (StateT FState Assert a)
    deriving (Functor, Applicative, Monad, MonadState FState, MonadLog, OA.MonadAssert)

flattenNary :: Assert ()
flattenNary =
  let F a = pass in evalStateT a (FState HMap.empty HMap.empty)


pass :: F ()
pass = do
  idxs <- OA.liftAssert OA.listAssertionIdxs
  forM_ (IntSet.toList idxs) $ \idx -> do
    a  <- OA.liftAssert $ OA.getAssertion idx
    a' <- process a
    OA.liftAssert $ modify $ over OA.assertions $ IntMap.insert idx a'
 where
  process :: SortClass s => Term s -> F (Term s)
  process = mapTermM visit
  visit :: SortClass s => Term s -> F (Maybe (Term s))
  visit t = case t of
    DynBvNaryExpr o _w as -> do
      as'  <- mapM process as
      as'' <- forM as' $ \a -> do
        entry <- gets (HMap.lookup a . view bvs)
        case entry of
          Just (o', elems) | o == o' -> return elems
          _                          -> return [a]
      let as''' = concat as''
      let t'    = mkDynBvNaryExpr o as'''
      modify $ over bvs $ HMap.insert t' (o, as''')
      return $ Just t'
    BoolNaryExpr o as -> do
      as'  <- mapM process as
      as'' <- forM as' $ \a -> do
        entry <- gets (HMap.lookup a . view bools)
        case entry of
          Just (o', elems) | o == o' -> return elems
          _                          -> return [a]
      let as''' = concat as''
      let t'    = BoolNaryExpr o as'''
      modify $ over bools $ HMap.insert t' (o, as''')
      return $ Just t'
    _ -> return Nothing

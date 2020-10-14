{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
module IR.SMT.Opt.EqElim
  ( eqElim
  , eqElimFn
  , eqElimGen
  , EqElimFns(..)
  )
where
import           IR.SMT.TySmt
import qualified IR.SMT.Opt.Assert             as OA
import           IR.SMT.Opt.Assert              ( Assert )
import           IR.SMT.Opt.ConstFoldEqElim     ( constantFold )
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                , fromDyn
                                                )
import           Data.List                      ( foldl' )
import           Data.Hashable                  ( Hashable )
import qualified Data.HashMap.Strict           as HMap
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Set                      as Set
import qualified Data.Sequence                 as Seq
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , set
                                                , view
                                                )
import           Util.Cfg                       ( MonadCfg(..)
                                                , _smtOptCfg
                                                , _allowSubBlowup
                                                , _cFoldInSub
                                                )
import           Util.Control                   ( unlessM )
import           Util.Log

data EqElimFns = EqElimFns
  { asSub    :: TermBool -> Maybe (String, Dynamic)
  , preCheck :: TermBool -> TermBool
  }

data ConstFoldEqState = ConstFoldEqState
  { _terms  :: !(IntMap.IntMap TermBool)
  , _uses   :: !(HMap.HashMap String IntSet.IntSet)
  , _queue  :: Seq.Seq Int
  , _queued :: IntSet.IntSet
  }
  deriving Show

$(makeLenses ''ConstFoldEqState)

newtype ConstFoldEq a = ConstFoldEq (StateT ConstFoldEqState Log a)
    deriving (Functor, Applicative, Monad, MonadState ConstFoldEqState, MonadIO, MonadLog)

toIntSetMap :: (Eq a, Hashable a) => [(a, Int)] -> HMap.HashMap a IntSet.IntSet
toIntSetMap = foldl'
  (\m (k, v) ->
    HMap.alter (Just . maybe (IntSet.singleton v) (IntSet.insert v)) k m
  )
  HMap.empty

eqElimGen :: EqElimFns -> [TermBool] -> Log [TermBool]
eqElimGen fns ts =
  filter (/= BoolLit True)
    .   map snd
    .   IntMap.toAscList
    .   _terms
    <$> execStateT action initState
 where
  ConstFoldEq action = stepToFixPoint

  ids                = [0 .. (length ts - 1)]
  ts'                = zip ids $ map (preCheck fns) ts
  uses' = toIntSetMap [ (v, i) | (i, t) <- ts', v <- Set.toList $ vars t ]


  initState          = ConstFoldEqState { _terms  = IntMap.fromList $ ts'
                                        , _uses   = uses'
                                        , _queue  = Seq.fromList ids
                                        , _queued = IntSet.fromList ids
                                        }

  dequeue :: ConstFoldEq (Maybe Int)
  dequeue = do
    q <- gets (view queue)
    case q of
      Seq.Empty   -> return Nothing
      h Seq.:<| t -> do
        modify (set queue t)
        modify (over queued $ IntSet.delete h)
        return (Just h)

  enqueue :: Int -> ConstFoldEq ()
  enqueue id' = do
    there <- gets (IntSet.member id' . view queued)
    unless there $ do
      modify $ over queued $ IntSet.insert id'
      modify $ over queue (Seq.|> id')

  stepToFixPoint :: ConstFoldEq ()
  stepToFixPoint = step >> unlessM (gets (Seq.null . _queue)) stepToFixPoint

  step :: ConstFoldEq ()
  step = do
    mI <- dequeue
    forM_ mI $ \i -> do
      modify $ over terms $ IntMap.adjust (preCheck fns) i
      a <- gets ((IntMap.! i) . view terms)
      logIf "smt::opt::cfee::debug" $ "Check " ++ show i ++ " : " ++ show a
      forM_ (asSub fns a) $ \(var, val) -> do
        vUses <- gets (fromMaybe IntSet.empty . HMap.lookup var . view uses)
        logIf "smt::opt::cfee::debug" $ "Sub in " ++ show vUses
        modify $ over terms $ IntMap.delete i
        forM_ (IntSet.toList vUses) $ \useI -> do
          when (useI /= i) $ do
            mT <- gets (IntMap.lookup useI . view terms)
            forM_ mT $ \t -> do
              let t'    = sub var val t
              let oVars = vars t
              let nVars = vars t'
              forM_ (Set.toList $ nVars Set.\\ oVars) $ \v ->
                modify $ over uses $ HMap.adjust (IntSet.insert useI) v
              forM_ (Set.toList $ oVars Set.\\ nVars) $ \v ->
                modify $ over uses $ HMap.adjust (IntSet.delete useI) v
              modify $ over terms $ IntMap.insert useI t'
              enqueue useI

  sub :: SortClass s => String -> Dynamic -> Term s -> Term s
  sub name_ value = mapTerm visit
   where
    visit :: forall t . SortClass t => Term t -> Maybe (Term t)
    visit term = case term of
      Var name' _ -> if name' == name_
        then Just $ fromDyn @(Term t) value (error "wrong sort")
        else Nothing
      _ -> Nothing

inTerm :: SortClass s => String -> Term s -> Bool
inTerm name_ = reduceTerm visit False (||)
 where
  visit :: SortClass t => Term t -> Maybe Bool
  visit term = case term of
    Var name' _ -> Just $ name' == name_
    _           -> Nothing

eqElim :: Assert ()
eqElim = do
  noElim <- gets (OA._public)
  OA.modifyAssertions (eqElimFn noElim)
  OA.refresh

eqElimFn :: Set.Set String -> [TermBool] -> Log [TermBool]
eqElimFn noElim ts = do
  allowBlowup <- liftCfg $ asks (_allowSubBlowup . _smtOptCfg)
  cFold       <- liftCfg $ asks (_cFoldInSub . _smtOptCfg)
  let preCheck' = if cFold then constantFold else id

      isArray :: Sort -> Bool
      isArray (SortArray{}) = True
      isArray _             = False

      subbable :: SortClass s => String -> Sort -> Term s -> Bool
      subbable v s t =
        (allowBlowup || nNodes t == 1)
          &&              v
          `Set.notMember` noElim
          &&              not (v `inTerm` t)
          &&              not (isArray s)

      asSub' :: TermBool -> Maybe (String, Dynamic)
      asSub' a = do
        case a of
          -- Combining these cases is tough b/c the sorts may differ
          Eq (Var v s) t | subbable v s t -> Just (v, toDyn t)
          Eq t (Var v s) | subbable v s t -> Just (v, toDyn t)
          _                               -> Nothing
  eqElimGen (EqElimFns { asSub = asSub', preCheck = preCheck' }) ts

{-# LANGUAGE BangPatterns #-}
module Util.ShowMap
  ( ShowMap(..)
  , empty
  , insert
  , insertWith
  , lookup
  , (!?)
  , adjust
  , member
  , toList
  )
where

-- A map defined over types that are Eq and Show.
-- Places elements into show-equivalent buckets.
-- Within that, uses an associative list

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( fromMaybe )

import           Prelude                 hiding ( lookup )
import qualified Prelude
import           Util.Show                      ( pShow )

newtype ShowMap k v = ShowMap (Map String [(k, v)])

empty :: ShowMap k v
empty = ShowMap Map.empty

inner :: ShowMap k v -> Map String [(k, v)]
inner !(ShowMap m) = m

entryInsertWith
  :: (Show k, Eq k) => (v -> v -> v) -> k -> v -> [(k, v)] -> [(k, v)]
entryInsertWith !f !k !newV !e = case Prelude.lookup k e of
  Just oldV -> modify k (f newV oldV) e
  Nothing   -> (k, newV) : e
 where
  modify !k !v !(h : t) = if fst h == k then (k, v) : t else h : modify k v t
  modify !k _  []       = error $ "Cannot " ++ show k ++ " to modify"

entryAdjust :: (Show k, Eq k) => (v -> v) -> k -> [(k, v)] -> [(k, v)]
entryAdjust !f !k !e = case Prelude.lookup k e of
  Just{}  -> modify e
  Nothing -> error $ "missing " ++ show k ++ " in " ++ pShow (map fst e)
 where
  modify !(h : t) = if fst h == k then (k, f $ snd h) : t else h : modify t
  modify []       = error $ "Cannot " ++ show k ++ " to modify"

maybeEntryInsertWith
  :: (Show k, Eq k) => (v -> v -> v) -> k -> v -> Maybe [(k, v)] -> [(k, v)]
maybeEntryInsertWith !f !k !v = maybe [(k, v)] (entryInsertWith f k v)

insertWith
  :: (Show k, Eq k) => (v -> v -> v) -> k -> v -> ShowMap k v -> ShowMap k v
insertWith !f !k !v =
  ShowMap . Map.alter (Just . maybeEntryInsertWith f k v) (show k) . inner

insert :: (Show k, Eq k) => k -> v -> ShowMap k v -> ShowMap k v
insert = insertWith const

lookup :: (Show k, Eq k) => k -> ShowMap k v -> Maybe v
lookup !k !(ShowMap m) = Map.lookup (show k) m >>= Prelude.lookup k

(!?) :: (Show k, Eq k) => ShowMap k v -> k -> Maybe v
(!?) = flip lookup

adjust :: (Show k, Eq k) => (v -> v) -> k -> ShowMap k v -> ShowMap k v
adjust !f !k = ShowMap . Map.adjust (entryAdjust f k) (show k) . inner

member :: (Show k, Eq k) => k -> ShowMap k v -> Bool
member !k !m = fromMaybe False $ do
  e <- Map.lookup (show k) $ inner m
  return $ elem k $ map fst e

toList :: ShowMap k v -> [(k, v)]
toList !(ShowMap a) = concat $ Map.elems a

instance (Show k, Eq k, Show v) => Show (ShowMap k v) where
  show a =
    let baseMappings = map (\(a, b) -> (show a, show b)) $ toList a
    in  unlines $ map (\(a, b) -> a ++ "\n  -> " ++ b) baseMappings

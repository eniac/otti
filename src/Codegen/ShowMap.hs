module Codegen.ShowMap
  ( ShowMap(..)
  , empty
  , insert
  , lookup
  , (!?)
  )
where

-- A map defined over types that are Eq and Show.
-- Places elements into show-equivalent buckets.
-- Within that, uses an associative list

import qualified Data.Map                      as Map
import           Data.Map                       ( Map )
import           Data.Maybe                     ( isJust )

import           Prelude                 hiding ( lookup )
import qualified Prelude

newtype ShowMap k v = ShowMap (Map String [(k, v)])

empty :: ShowMap k v
empty = ShowMap Map.empty

inner :: ShowMap k v -> Map String [(k, v)]
inner (ShowMap m) = m

entryInsert :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
entryInsert k v e = if isJust (Prelude.lookup k e)
  then modify k v e
  else (k, v) : e
 where
  modify k v (h : t) = if fst h == k then (k, v) : t else h : modify k v t

maybeEntryInsert :: Eq k => k -> v -> Maybe [(k, v)] -> [(k, v)]
maybeEntryInsert k v = maybe [(k, v)] (entryInsert k v)

insert :: (Show k, Eq k) => k -> v -> ShowMap k v -> ShowMap k v
insert k v = ShowMap . Map.alter (Just . maybeEntryInsert k v) (show k) . inner

lookup :: (Show k, Eq k) => k -> ShowMap k v -> Maybe v
lookup k (ShowMap m) = Map.lookup (show k) m >>= Prelude.lookup k

(!?) :: (Show k, Eq k) => ShowMap k v -> k -> Maybe v
(!?) = flip lookup

{-| A Map with an optional defaul
 -
 -}
{-# LANGUAGE BangPatterns #-}
module IR.SMT.TySmt.DefaultMap
  ( insert
  , lookup
  , (!)
  , (!?)
  , empty
  , emptyWithDefault
  , DefaultMap
  , fromMap
  )
where

import           Prelude                 hiding ( lookup )

import           Control.Applicative     hiding ( empty )

import           Data.Hashable                  ( Hashable(..) )
import qualified Data.Map.Strict               as Map
import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )

data DefaultMap k v = DefaultMap !(Map k v) !(Maybe v) deriving (Show, Eq, Ord)

type M = DefaultMap

instance (Hashable k, Hashable v) => Hashable (DefaultMap k v) where
  hashWithSalt s (DefaultMap m d) =
    s `hashWithSalt` (Map.toList m) `hashWithSalt` d

insert :: (Ord k) => k -> v -> M k v -> M k v
insert !k !v !(DefaultMap m d) = DefaultMap (Map.insert k v m) d

lookup :: (Ord k) => k -> M k v -> Maybe v
lookup !k (DefaultMap m d) = Map.lookup k m <|> d

(!?) :: (Ord k) => M k v -> k -> Maybe v
(!?) = flip lookup

(!) :: (Ord k, Show k) => M k v -> k -> v
(!) m k = fromMaybe (error $ "Missing key: " ++ show k) $ lookup k m

empty :: M k v
empty = DefaultMap Map.empty Nothing

emptyWithDefault :: v -> M k v
emptyWithDefault = DefaultMap Map.empty . Just

fromMap :: Map k v -> M k v
fromMap = flip DefaultMap Nothing

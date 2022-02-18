{-# LANGUAGE BangPatterns #-}
module Util.AliasMap
  ( AliasMap
  , lookup
  , alias
  , insert
  , insertWith
  , adjust
  , empty
  , dealias
  , member
  , memberOrAlias
  )
where

import           Prelude                 hiding ( lookup )
import qualified Util.ShowMap                  as SMap
import           Util.ShowMap                   ( ShowMap )


-- Layer on top of ShowMap which allows keys to be aliased to one another
data AliasMap k v = AliasMap { base :: !(ShowMap k v)
                             , aliases :: !(ShowMap k k)
                             }

-- Lift an access of the base map
getBase :: (Show k, Eq k) => (k -> ShowMap k v -> a) -> k -> AliasMap k v -> a
getBase !f !k !m = f (dealias k m) (base m)

-- Lift a modification of the base map
modBase
  :: (Show k, Eq k)
  => (k -> ShowMap k v -> ShowMap k v)
  -> k
  -> AliasMap k v
  -> AliasMap k v
modBase !f !k !m = m { base = getBase f k m }

-- Follow aliases
dealias :: (Show k, Eq k) => k -> AliasMap k v -> k
dealias !k !m = case SMap.lookup k (aliases m) of
  Just k' -> dealias k' m
  Nothing -> k

-- Get value
lookup :: (Show k, Eq k) => k -> AliasMap k v -> Maybe v
lookup = getBase SMap.lookup

-- Create alias
alias :: (Show k, Eq k) => k -> k -> AliasMap k v -> AliasMap k v
alias !k !k' !m = case SMap.lookup k (aliases m) of
  Just k'' -> error $ unwords
    [ "Cannot alias"
    , show k
    , "to"
    , show k'
    , "since the former is already aliaed to"
    , show k''
    ]
  Nothing -> case lookup k m of
    Just{} -> error $ unwords
      [ "Cannot alias"
      , show k
      , "to"
      , show k'
      , "since the former is already mapped"
      ]
    Nothing -> m { aliases = SMap.insert k k' $ aliases m }

-- Insert value
insert :: (Show k, Eq k) => k -> v -> AliasMap k v -> AliasMap k v
insert !k !v = modBase (flip SMap.insert v) k

-- Empty map
empty :: AliasMap k v
empty = AliasMap SMap.empty SMap.empty

-- Adjsut an existing entry
adjust :: (Show k, Eq k) => (v -> v) -> k -> AliasMap k v -> AliasMap k v
adjust !f = modBase (SMap.adjust f)

-- Is an entry mapped to a value?
member :: (Show k, Eq k) => k -> AliasMap k v -> Bool
member = getBase SMap.member

-- Is an entry mapped to a value or aliaed?
memberOrAlias :: (Show k, Eq k) => k -> AliasMap k v -> Bool
memberOrAlias !k !m = SMap.member k (aliases m) || member k m

insertWith
  :: (Show k, Eq k) => (v -> v -> v) -> k -> v -> AliasMap k v -> AliasMap k v
insertWith !f !k !v = modBase (\k -> SMap.insertWith f k v) k

instance (Show k, Eq k, Show v) => Show (AliasMap k v) where
  show a =
    let baseMappings = map (\(a, b) -> (show a, show b)) $ SMap.toList (base a)
        aliasMappings =
            map (\(x, _) -> (show x, show $ dealias x a)) $ SMap.toList $ aliases
              a
        aliasLines = map (\(a, b) -> a ++ "\n  -> " ++ b) $ aliasMappings
        baseLines  = map (\(a, b) -> a ++ "\n  -> " ++ show b) $ baseMappings
    in  unlines $ ["Aliases:"] ++ aliasLines ++ ["Base mappings:"] ++ baseLines

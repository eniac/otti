{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Codegen.Circom.Signal
  ( IndexedIdent
  , Signal(..)
  , GlobalSignal(..)
  )
where

import           Data.List                      ( intercalate )
import           GHC.Generics                   ( Generic )
import           Control.DeepSeq                ( NFData )

type IndexedIdent = (String, [Int])

data Signal = SigLocal !IndexedIdent
            | SigForeign !IndexedIdent !IndexedIdent
            deriving (Show,Read,Eq,Ord)

--showIndexedIdent :: IndexedIdent -> String
--showIndexedIdent (n, is) =
--  n ++ intercalate "" [ "[" ++ show i ++ "]" | i <- is ]
--
-- instance Show Signal where
--   show (SigLocal ii     ) = showIndexedIdent ii
--   show (SigForeign ii jj) = showIndexedIdent ii ++ "." ++ showIndexedIdent jj
-- 
-- Not how this should work
--instance Read Signal where
--  read s = case Split.splitOneOf "." of
--    [a] -> SigLocal (readIi a)
--    [a, b] -> SigForeign (readIi a) (readIi b)
--    _ -> error "Too many dots for " ++ show s ++ " to be a signal"
--   where
--    readIi s = let toks = Split.splitOneOf "[]"
--               in  (head toks, map read $ tail toks)

newtype GlobalSignal = GlobalSignal [IndexedIdent]
                       deriving (Ord, Eq, Generic, NFData)

instance Show GlobalSignal where
  show (GlobalSignal ids) =
    intercalate "."
      $ map (\(s, is) -> s ++ concatMap (\i -> "[" ++ show i ++ "]") is)
      $ reverse ids

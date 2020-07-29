{-# OPTIONS_GHC -Wall #-}
module Codegen.Circom.Signal
  ( IndexedIdent
  , Signal(..)
  , GlobalSignal(..)
  )
where

import           Data.List                      ( intercalate )

type IndexedIdent = (String, [Int])

data Signal = SigLocal !IndexedIdent
            | SigForeign !IndexedIdent !IndexedIdent
            deriving (Show,Eq,Ord,Read)

newtype GlobalSignal = GlobalSignal [IndexedIdent]
                       deriving (Ord, Eq)

instance Show GlobalSignal where
  show (GlobalSignal ids) =
    intercalate "."
      $ map (\(s, is) -> s ++ concatMap (\i -> "[" ++ show i ++ "]") is)
      $ reverse ids

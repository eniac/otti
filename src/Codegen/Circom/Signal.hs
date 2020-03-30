{-# OPTIONS_GHC -Wall #-}
module Codegen.Circom.Signal ( IndexedIdent
                             , Signal(..)
                             , GlobalSignal
                             ) where

type IndexedIdent = (String, [Int])

data Signal = SigLocal IndexedIdent
            | SigForeign IndexedIdent IndexedIdent
            deriving (Show,Eq,Ord)

type GlobalSignal = [IndexedIdent]

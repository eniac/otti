module Codegen.Circom.Signal ( IndexedIdent
                             , Signal(..)
                             , GlobalSignal
                             ) where

import qualified AST.Circom as AST

type IndexedIdent = (String, [Int])

data Signal = SigLocal IndexedIdent
            | SigForeign IndexedIdent IndexedIdent
            deriving (Show,Eq,Ord)

type GlobalSignal = [IndexedIdent]

module Codegen.Circom.Utils
  ( spanE
  , mapGetE
  )
where

import           AST.Circom                     ( Span(..) )
import qualified Data.Map.Strict               as Map

spanE :: Span -> String -> a
spanE (Span path s e) m =
  error
    $  m
    ++ "\nLocation:\n\t"
    ++ path
    ++ "\n\tbetween "
    ++ show s
    ++ "\n\t    and "
    ++ show e

mapGetE :: Ord k => String -> k -> Map.Map k v -> v
mapGetE = Map.findWithDefault . error

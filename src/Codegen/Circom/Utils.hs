module Codegen.Circom.Utils
  ( spanE
  )
where

import           AST.Circom                     ( Span(..) )

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

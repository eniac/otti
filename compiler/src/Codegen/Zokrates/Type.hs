module Codegen.Zokrates.Type
  ( Type(..)
  )
where

import qualified Data.Map.Strict               as Map

data Type = Uint Int
          | Bool
          | Field
          | Struct String (Map.Map String Type)
          | Array Int Type
          deriving (Show,Eq)

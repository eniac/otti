module Codegen.Circom.Typing
  ( InstanceType
  , emptyType
  )
where

import qualified Data.Map.Strict               as Map

data InstanceType = InstanceType { inputs :: Map.Map String [Int]
                                 , outputs :: Map.Map String [Int] }
                                 deriving (Show,Ord,Eq)

emptyType :: InstanceType
emptyType = InstanceType Map.empty Map.empty

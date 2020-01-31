module Codegen.Circom.Constraints where

import qualified Data.Set            as Set
import qualified Data.Map.Strict    as Map

data Signal = SigLocal String [Int]
            -- Subcomponent name, subcomponent indices, signal name, signal indices
            | SigForeign String [Int] Signal
            deriving (Show,Ord,Eq)
type LC k = (Map.Map Signal k, k) -- A linear combination of signals and gen-time constants

type Constraint k = (LC k, LC k, LC k)

data Constraints k = Constraints { equalities :: [Constraint k]
                                 , public     :: Set.Set Signal
                                 , private    :: Set.Set Signal
                                 }
                                 deriving (Show, Eq, Ord)

addPublic :: Signal -> Constraints k -> Constraints k
addPublic s cs = cs { public = Set.insert s $ public cs }

addPrivate :: Signal -> Constraints k -> Constraints k
addPrivate s cs = cs { private = Set.insert s $ private cs }

addEquality :: Constraint k -> Constraints k -> Constraints k
addEquality e cs = cs { equalities = e : equalities cs }

union :: Constraints k -> Constraints k -> Constraints k
union a b = Constraints { equalities = equalities a ++ equalities b
                        , public = Set.union (public a) (public b)
                        , private = Set.union (private a) (private b)
                        }

mapSignals :: (Signal -> Signal) -> Constraints k -> Constraints k
mapSignals f cs = Constraints { equalities = map (mapSignalsInConstraint f) (equalities cs)
                              , public = Set.map f (public cs)
                              , private = Set.map f (private cs)
                              }

mapSignalsInConstraint :: (Signal -> Signal) -> Constraint k -> Constraint k
mapSignalsInConstraint f (a, b, c) = (m f a , m f b , m f c)
    where m = mapSignalsInLc

mapSignalsInLc :: (Signal -> Signal) -> LC k -> LC k
mapSignalsInLc f (m, c) = (Map.mapKeys f m, c)

empty :: Constraints k
empty = Constraints { equalities = [], public = Set.empty, private = Set.empty }

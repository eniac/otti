module IR.SlimSMT where

import AST.Typed        (Typed)
import Math.NumberTheory.Primes.Testing (millerRabinV)

-- List of base, power pairs
-- Use `makeFieldOrder`.
newtype FieldOrder = FieldOrder [(Int, Int)]

fieldSize :: FieldOrder -> Int
fieldSize (FieldOrder fieldOrder) = foldl (\a p -> uncurry (^) p * a) 1 fieldOrder

fieldBitCount :: FieldOrder -> Int
fieldBitCount f = ceiling $ logBase 2 (fromIntegral (fieldSize f))

fieldBitCapacity :: FieldOrder -> Int
fieldBitCapacity f = floor $ logBase 2 (fromIntegral (fieldSize f))

makeFieldOrder :: [(Int, Int)] -> FieldOrder
makeFieldOrder pairs = if all (\p -> millerRabinV (fst p) 10) pairs
                       then FieldOrder pairs
                       else error $ "The field order " ++ show pairs ++ " contains non-primes"


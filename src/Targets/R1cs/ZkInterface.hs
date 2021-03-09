{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
module Targets.R1cs.ZkInterface
  ( zkifWitnessEncode
  , zkifR1csEncode
  , zkifCircuitHeaderEncode
  )
where

import qualified Data.Map.Strict               as M
import           FlatBuffers
import           Data.ByteArray                 ( Bytes )
import qualified Data.ByteArray                as B
import           FlatBuffers                    ( )
import           FlatBuffers.Vector             ( WriteVector
                                                , WriteVectorElement
                                                )
import qualified FlatBuffers.Vector            as V
import           Data.Field.Galois              ( Prime )
import qualified Data.Foldable                 as Fold
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Crypto.Number.Serialize        ( i2osp
                                                , os2ip
                                                )
import qualified Data.IntMap.Strict            as IM
import           Targets.R1cs.Main
import           Data.ByteString.Lazy           ( ByteString )
import           Data.Proxy                     ( Proxy(..) )

$(mkFlatBuffers "schema/zkinterface.fbs" defaultOptions)

class Serialize a where
    serialize :: a -> Bytes
    deserialize :: Bytes -> a

instance Serialize Integer where
  serialize   = B.reverse . i2osp
  deserialize = os2ip . B.reverse

-- i2osp gives big-endian, reverse to lil-endian
instance (forall. KnownNat n => Serialize (Prime n)) where
  serialize   = B.reverse . i2osp . toInteger
  deserialize = fromInteger . os2ip . B.reverse

flatVector :: WriteVectorElement a => [a] -> Maybe (WriteVector a)
flatVector l = Just . V.fromList (fromIntegral $ length l) $ l

zkifVariables :: KnownNat n => [(Int, Prime n)] -> WriteTable Variables
zkifVariables vs =
  let (keys, bins) = unzip vs
  in  let bin_values =
              concatMap
                  (pad_with_zeros (max_length bins 0) . B.unpack . serialize)
                $ bins
      in  let in variables (flatVector $ map (fromIntegral . total_sub) keys)
                           (flatVector bin_values)
                           Nothing
 where
  total_sub c | c == 0    = 0
              | otherwise = c - 1
  max_length (h : ts) m =
    let len = B.length . serialize $ h
    in  if len > m then max_length ts len else max_length ts m
  max_length [] m = m
  pad_with_zeros n ba = ba ++ replicate (n - length ba) 0

zkifBilinearConstraint
  :: KnownNat n => QEQ Int (Prime n) -> WriteTable BilinearConstraint
zkifBilinearConstraint ((ma, ca), (mb, cb), (mc, cc)) = bilinearConstraint
  (prependOne ma ca)
  (prependOne mb cb)
  (prependOne mc cc)
 where
  prependOne m c | c == 0    = Just $ zkifVariables . M.toAscList $ m
                 | otherwise = Just $ zkifVariables $ (0, c) : (M.toAscList m)

zkifConstraintSystem
  :: (Show s, KnownNat n) => R1CS s n -> WriteTable ConstraintSystem
zkifConstraintSystem r1cs = constraintSystem
  (flatVector . map zkifBilinearConstraint . Fold.toList . constraints $ r1cs)
  Nothing

mapZip :: (a -> b) -> [a] -> [(a, b)]
mapZip f = map (\x -> (x, f x))

zkifCircuitHeader
  :: forall s n . (Show s, KnownNat n) => R1CS s n -> WriteTable CircuitHeader
zkifCircuitHeader r1cs =
  let lookupSignalVal = r1csNumValue r1cs
      free_var_id     = 1 + nPublicInputs r1cs
      ivs = zkifVariables . mapZip lookupSignalVal $ [2 .. free_var_id]
      field_max       = B.unpack . serialize $ order - 1
  in  circuitHeader (Just ivs)
                    (Just . fromIntegral $ free_var_id)
                    (flatVector field_max)
                    Nothing
  where order = natVal (Proxy @n)

zkifWitness :: (Show s, KnownNat n) => R1CS s n -> WriteTable Witness
zkifWitness r1cs =
  let lookupSignalVal = r1csNumValue r1cs
      free_var_id     = 1 + nPublicInputs r1cs
  in  witness
        . Just
        . zkifVariables
        . mapZip lookupSignalVal
        $ [1 + free_var_id .. (nextSigNum r1cs - 1)]

zkifCircuitHeaderEncode :: (Show s, Ord s, KnownNat n) => R1CS s n -> ByteString
zkifCircuitHeaderEncode =
  encodeWithFileIdentifier . root . messageCircuitHeader . zkifCircuitHeader

zkifWitnessEncode :: (Show s, Ord s, KnownNat n) => R1CS s n -> ByteString
zkifWitnessEncode =
  encodeWithFileIdentifier . root . messageWitness . zkifWitness

zkifR1csEncode :: (Show s, Ord s, KnownNat n) => R1CS s n -> ByteString
zkifR1csEncode =
  encodeWithFileIdentifier
    . root
    . messageConstraintSystem
    . zkifConstraintSystem

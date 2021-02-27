{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Targets.R1cs.ZkInterface where

import qualified Data.Map.Strict               as M
import           FlatBuffers
import           Data.ByteArray                 ( Bytes )
import qualified Data.ByteArray                as B
import           FlatBuffers
import           FlatBuffers.Vector             ( WriteVector
                                                , WriteVectorElement
                                                )
import qualified FlatBuffers.Vector            as V
import           Data.Field.Galois              ( Prime )
import qualified Data.Foldable                 as Fold
import           GHC.TypeLits                   ( KnownNat )
import           Crypto.Number.Serialize        ( i2osp
                                                , os2ip
                                                )
import qualified Data.IntMap.Strict            as IM
import           Targets.R1cs.Main              ( R1CS
                                                , QEQ
                                                , constraints
                                                , values
                                                )

$(mkFlatBuffers "schema/zkinterface.fbs" defaultOptions)

class Serialize a where
    serialize :: a -> Bytes
    deserialize :: Bytes -> a

-- i2osp gives big-endian, reverse to lil-endian
instance (forall. KnownNat n => Serialize (Prime n)) where
  serialize   = B.reverse . i2osp . toInteger
  deserialize = fromInteger . os2ip . B.reverse

flatVector :: WriteVectorElement a => [a] -> Maybe (WriteVector a)
flatVector l = Just . V.fromList (fromIntegral $ length l) $ l

mkVariables :: KnownNat n => [(Int, Prime n)] -> WriteTable Variables
mkVariables vs =
  let (keys, bins) = unzip vs
  in  let bin_values =
              concatMap
                  (pad_with_zeros (max_length bins 0) . B.unpack . serialize)
                $ bins
      in  let in variables (flatVector $ map fromIntegral keys)
                           (flatVector bin_values)
                           Nothing
 where
  max_length (h : ts) m =
    let len = B.length . serialize $ h
    in  if len > m then max_length ts len else max_length ts m
  max_length [] m = m
  pad_with_zeros n ba = ba ++ replicate n 0

mkBilinearConstraint
  :: KnownNat n => QEQ Int (Prime n) -> WriteTable BilinearConstraint
mkBilinearConstraint ((ma, ca), (mb, cb), (mc, cc)) = bilinearConstraint
  (prependOne ma ca)
  (prependOne mb cb)
  (prependOne mc cc)
  where prependOne m c = Just $ mkVariables $ (0, c) : (M.toAscList m) -- Variable 0 -> 1*c

mkConstraintSystem :: KnownNat n => R1CS Int n -> WriteTable ConstraintSystem
mkConstraintSystem r1cs = constraintSystem
  (flatVector . map mkBilinearConstraint . Fold.toList . constraints $ r1cs)
  Nothing

mkWitness :: KnownNat n => R1CS Int n -> WriteTable Witness
mkWitness = witness . fmap mkVariables . fmap IM.toAscList . values

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}
module Targets.R1cs.Output
  ( r1csWriteAssignments
  , writeToR1csFile
  , qeqToJsonString
  , lcToJsonString
  , emitAssignment
  , r1csAsLines
  , r1csStats
  , r1csCheck
  , r1csShow
  , qeqShow
  , primeShow
  )
where

import           Control.Monad
import           Control.Monad.State.Strict
import           Control.Monad.Reader
import qualified Data.Text                     as T
import           Data.Aeson
import qualified Data.ByteString.Lazy          as ByteString
import qualified Data.ByteString.Lazy.Char8    as Char8
import           Data.Field.Galois              ( Prime
                                                , fromP
                                                , toP
                                                )
import qualified Data.Foldable                 as Fold
import qualified Data.IntMap.Strict            as IntMap
import qualified Data.IntSet                   as IntSet
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.Sequence                 as Seq
import qualified Data.Text                     as Text
import qualified Data.List                     as List
import           GHC.TypeLits                   ( KnownNat
                                                , natVal
                                                )
import           Data.Proxy                     ( Proxy(..) )
import           Targets.R1cs.ZkInterface
import           Targets.R1cs.Main
import           System.IO                      ( hPutStr
                                                , openFile
                                                , IOMode(WriteMode)
                                                , hClose
                                                )
import           Util.Cfg                       ( liftCfg
                                                , Cfg
                                                , CfgState(..)
                                                )

instance {-# OVERLAPS #-} forall s n. (Show s, KnownNat n) => ToJSON (LC s (Prime n)) where
  toJSON (m, c) =
    let back =
            map (\(k, v) -> Text.pack (show k) .= show (fromP v)) (Map.toList m)
    in
      object
        [ "type" .= Text.pack "LINEARCOMBINATION"
        , "values"
          .= object (if c == 0 then back else ("one" .= show (fromP c)) : back)
        ]

instance {-# OVERLAPS #-} forall s n. (Show s, KnownNat n) => ToJSON (QEQ s (Prime n)) where
  toJSON (a, b, c) = object
    [ "type" .= Text.pack "QEQ"
    , "a" .= toJSON a
    , "b" .= toJSON b
    , "c" .= toJSON c
    ]

qeqToJsonString :: (Show s, KnownNat n) => QEQ s (Prime n) -> String
qeqToJsonString = Char8.unpack . encode

lcToJsonString :: (Show s, KnownNat n) => LC s (Prime n) -> String
lcToJsonString = Char8.unpack . encode

instance forall s n. (Show s, KnownNat n) => ToJSON (R1CS s n) where
  toJSON r1cs = object
    [ "constraints" .= map qeqToJson (Fold.toList $ constraints r1cs)
    , "signals" .= (sigOne : map sigToJson (IntMap.toList $ numSigs r1cs))
    ]
   where
    sigOne :: Value
    sigOne = object ["names" .= ["one" :: String]]
    sigToJson :: (Int, [s]) -> Value
    sigToJson (i, ss) = object ["idx" .= (i - 1), "names" .= map show ss]
    qeqToJson (a, b, c) = map lcToJson [a, b, c]
    lcToJson (m, c) =
      let back = map (\(k, v) -> Text.pack (show (k - 1)) .= (primeShow v))
                     (Map.toList m)
      in  object $ if c == 0 then back else ("0" .= show (fromP c)) : back

instance forall s n. (Show s, KnownNat n) => Show (R1CS s n) where
  show = Char8.unpack . encode

r1csShow :: (KnownNat n, Show s, Ord s) => R1CS s n -> String
r1csShow r1cs =
  List.intercalate "" $ map (\qeq -> "  " ++ qeqShow qeq ++ "\n") $ r1csQeqs
    r1cs

primeToSignedInt :: forall n . KnownNat n => Prime n -> Integer
primeToSignedInt p =
  let v  = fromP p
      o  = natVal $ Proxy @n
      ho = o `div` 2
  in  if v < ho then v else v - o

primeShow :: KnownNat n => Prime n -> String
primeShow = show . primeToSignedInt

qeqShow :: (KnownNat n, Show s) => QEQ s (Prime n) -> String
qeqShow (a, b, c) =
  unwords ["(" ++ lcShow a ++ ")", "*", "(" ++ lcShow b ++ ")", "=", lcShow c]

lcShow :: (KnownNat n, Show s) => LC s (Prime n) -> String
lcShow (m, c) =
  let list =
          map (\(x, v) -> primeShow v ++ " " ++ show x) (Map.toList m)
            ++ [ primeShow c | c /= toP 0 ]
  in  List.intercalate " + " (if null list then ["0"] else list)


r1csStats :: R1CS s n -> String
r1csStats r = unlines
  [ "Signals: " ++ show (IntMap.size $ numSigs r)
  , "Constraints: " ++ show (length $ constraints r)
  ]

r1csAsLines :: KnownNat n => R1CS s n -> [[Integer]]
r1csAsLines r1cs =
  let nPubIns         = fromIntegral $ IntSet.size $ publicInputs r1cs
      nWit            = fromIntegral (Map.size $ sigNums r1cs) - nPubIns
      nConstraints    = fromIntegral $ Seq.length $ constraints r1cs
      constraintLines = concatMap qeqToR1csLines $ constraints r1cs
  in  [nPubIns, nWit, nConstraints] : constraintLines

-- Todo: implement this using a better IO system.
writeToR1csFile :: (Show s, Ord s, KnownNat n) => R1CS s n -> FilePath -> Cfg ()
writeToR1csFile r1cs path = do
  outputFiles <- liftCfg $ asks _outputs
  let jsonFiles = filter_ext ".json" outputFiles
  let zkifFiles = filter_ext ".zkif" outputFiles
  -- Output libsnark output file always
  liftIO
    . writeFile path
    . unlines
    . map (unwords . map show)
    . r1csAsLines
    $ r1cs
  -- Optionally output json, zkif
  liftIO $ forM_ jsonFiles (\path -> ByteString.writeFile path $ encode r1cs)
  liftIO $ forM_ zkifFiles
                 (\path -> ByteString.writeFile path . zkifR1csEncode $ r1cs)
  where filter_ext ext = filter (T.isSuffixOf ext . T.pack)

r1csWriteAssignments
  :: forall s n
   . (Ord s, Show s, KnownNat n)
  => R1CS s n
  -> FilePath
  -> FilePath
  -> Cfg ()
r1csWriteAssignments r1cs inputPath witPath = do
  let lookupSignalVal = r1csNumValue r1cs
  liftIO $ emitAssignment
    (map lookupSignalVal [2 .. (1 + nPublicInputs r1cs)])
    inputPath
  liftIO $ emitAssignment
    (map lookupSignalVal [(2 + nPublicInputs r1cs) .. (nextSigNum r1cs - 1)])
    witPath

emitAssignment :: KnownNat n => [Prime n] -> FilePath -> IO ()
emitAssignment xs path = do
  handle <- openFile path WriteMode
  hPutStr handle $ concatMap (\i -> show (fromP i :: Integer) ++ "\n")
                             (toP (toInteger $ length xs) : xs)
  hClose handle

r1csCheck
  :: forall s n . (Show s, Ord s, KnownNat n) => R1CS s n -> Either String ()
r1csCheck r1cs = if (null $ values r1cs)
  then Right ()
  else forM_ (constraints r1cs) $ \c ->
    let v = qeqEval r1cs c
    in  if 0 == fromP v
          then Right ()
          else Left $ unwords
            [ "The constraint"
            , qeqShow $ r1csExternQeq r1cs c
            , "evaluated to"
            , primeShow v
            , "not 0"
            ]
 where

qeqEval
  :: forall s n
   . (Show s, Ord s, KnownNat n)
  => R1CS s n
  -> QEQ Int (Prime n)
  -> Prime n
qeqEval r1cs (a, b, c) = lcEval a * lcEval b - lcEval c
 where
  lcEval :: LC Int (Prime n) -> Prime n
  lcEval (m, c) =
    c + sum (map (\(k, v) -> v * r1csNumValue r1cs k) $ Map.toList m)

r1csNumValue :: (KnownNat n, Show s) => R1CS s n -> Int -> Prime n
r1csNumValue r1cs i =
  fromMaybe
      (error $ "Could not find r1cs var: " ++ show i ++ ": " ++ show
        (numSigs r1cs IntMap.!? i)
      )
    $         fromMaybe (error "No r1cs values!") (values r1cs)
    IntMap.!? i

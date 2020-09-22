{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Parser.Circom.Inputs
  ( parseSignalsFromFile
  , parseIntsFromFile
  )
where

import           Data.Field.Galois              ( Prime
                                                , toP
                                                )
import           GHC.TypeLits                   ( KnownNat )
import           Data.Bifunctor                 ( bimap )
import qualified Data.List.Split               as Split
import qualified Data.Map.Strict               as Map
import           Data.Proxy                     ( Proxy )
import           System.IO                      ( Handle
                                                , hGetContents
                                                )

type IndexedIdent = (String, [Int])

parseIndexedIdent :: String -> IndexedIdent
parseIndexedIdent s =
  let parts = filter (not . null) $ Split.splitOneOf "[]" s
  in  (head parts, map (read @Int) $ tail parts)

parseIntsFromFile :: Handle -> IO (Map.Map String Integer)
parseIntsFromFile path = do
  contents <- hGetContents path
  return
    $ Map.fromList
    $ map
        (\l -> case words l of
          [a, b] -> (a, read b)
          _      -> error $ "Invalid input line: " ++ show l
        )
    $ lines contents

parseSignalsFromFile
  :: forall n
   . KnownNat n
  => Proxy n
  -> Handle
  -> IO (Map.Map IndexedIdent (Prime n))
parseSignalsFromFile _order path =
  Map.fromList
    .   map (bimap parseIndexedIdent toP)
    .   Map.toAscList
    <$> parseIntsFromFile path


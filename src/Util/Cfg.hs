module Util.Cfg
  ( readCfgDefault
  , cfgStrList
  )
where

import           Data.Typeable                  ( Typeable
                                                , typeOf
                                                )
import           System.Environment             ( lookupEnv )
import           Text.Read                      ( readMaybe )
import           Data.List.Split                ( splitOn )

-- Gets the value of an environment-configurable option name "name" with
-- default value "default_"
readCfgDefault :: (Read a, Typeable a) => String -> a -> IO a
readCfgDefault name default_ = do
  mS <- lookupEnv $ "C_" ++ name
  case mS of
    Just s -> case readMaybe s of
      Just v -> return v
      Nothing ->
        error
          $  "The string '"
          ++ s
          ++ "' for C_"
          ++ name
          ++ " is not a "
          ++ show (typeOf default_)
    Nothing -> return default_

cfgStrList :: String -> IO [String]
cfgStrList name = do
  mS <- lookupEnv $ "C_" ++ name
  return $ case mS of
    Just s  -> filter (not . null) $ splitOn "," s
    Nothing -> []

{-|
 - Module for language-level input values
 -}

module Codegen.LangVal
  ( InMap
  , parseToMap
  , modelMapToExtMap
  , setInputFromMap
  )
where

import qualified Data.Map.Strict               as Map
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import qualified Targets.SMT.Z3         as ToZ3
import           Control.Applicative            ( (<|>) )
import           Data.Maybe                     ( fromMaybe )
import           Text.Read                      ( readMaybe )
import           Util.Control

type InMap = Map.Map String Integer

parseToMap :: String -> Map.Map String Integer
parseToMap s = Map.fromList $ map pL $ filter (not . null) $ lines s
 where
  pL l = case words l of
    [l, r] -> (l, fromMaybe (error "No int on right") $ readMaybe r)
    _      -> error $ "Line " ++ show l ++ "does not have 2 tokens"

-- Convert a model map (from Z3) to an ext map
-- TODO: handle structures
modelMapToExtMap :: Map.Map String ToZ3.Val -> InMap
modelMapToExtMap m = Map.fromList $ map f $ Map.toList m
 where
  f (k, v) =
    let i = case v of
          ToZ3.BVal b -> toInteger $ fromEnum b
          ToZ3.IVal i -> toInteger i
          _           -> error $ "Unhandled model entry value: " ++ show v
    in  (k, i)

setInputFromMap
  :: Ty.SortClass s
  => Maybe InMap -- ^ An optional map of inputs
  -> (Integer -> Ty.Value s) -- ^ A function from integers to values
  -> Ty.Value s -- ^ A default value
  -> String -- ^ An smtName to lookup in the inputs
  -> Maybe String -- ^ An optional user name to lookup in the inputs
  -> Assert.Assert ()
setInputFromMap inMap f d s mS = whenJust inMap $ \iM ->
  let v   = f <$> ((iM Map.!?) =<< mS)
      v'  = f <$> (iM Map.!? s)
      v'' = d
  in  Assert.setValue s $ fromMaybe v'' (v <|> v')

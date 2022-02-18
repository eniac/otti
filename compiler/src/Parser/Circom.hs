{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Parser.Circom
  ( parseFile
  , loadFilesRecursively
  , loadMain
  )
where

import           AST.Circom                     ( Annotated(..)
                                                , MainCircuit(..)
                                                , PosnPair(..)
                                                , SFile
                                                , SStatement
                                                , Span(..)
                                                , collectFunctions
                                                , collectIncludes
                                                , collectMains
                                                , collectTemplates
                                                , mapAnns
                                                )
import qualified Data.Map.Strict               as Map
import           Parser.Circom.Lexer            ( tokenize )
import           Parser.Circom.Parser           ( parseCircomFile )
import           Parser.Includer


parseFile :: FilePath -> IO SFile
parseFile p = do
  f <- readFile p
  return (map (mapAnns toSpan) $ parseCircomFile $ tokenize f)
  where toSpan (PosnPair x y) = Span p x y

newtype LoadableFile = LoadableFile SFile
instance Includer LoadableFile where
  parse p = LoadableFile <$> parseFile p

  includes p (LoadableFile f) =
    return $ map ((p `relativize`) . ast) $ collectIncludes f

-- Given a file, gets the first main expression
getFirstMain :: SFile -> SStatement
getFirstMain file | l == 0    = error "Missing main"
                  | l == 1    = head mains
                  | otherwise = error "Multiple mains"
 where
  mains = collectMains file
  l     = length mains

loadFilesRecursively :: FilePath -> IO (Map.Map FilePath SFile)
loadFilesRecursively path = Map.map (\(LoadableFile f) -> f) <$> loadGraph path

loadMain path = do
  loaded <- loadFilesRecursively path
  let mainItems =
        Map.findWithDefault (error $ "missing main file " ++ path) path loaded
  let allItems  = concat (Map.elems loaded)
  let functions = collectFunctions allItems
  let templates = collectTemplates allItems
  return $ MainCircuit
    (getFirstMain mainItems)
    (Map.fromList
      (Prelude.map (\t -> let (n, a, b) = t in (ast n, (map ast a, b)))
                   functions
      )
    )
    (Map.fromList
      (Prelude.map (\t -> let (n, a, b) = t in (ast n, (map ast a, b)))
                   templates
      )
    )

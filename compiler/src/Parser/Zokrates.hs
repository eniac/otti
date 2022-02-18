module Parser.Zokrates
  ( parseFile
  , loadFilesRecursively
  , absolutePath
  )
where

import           AST.Zokrates
import           AST.Util
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.List                      ( isInfixOf
                                                , isSuffixOf
                                                )
import           Control.Monad
import qualified Data.Map.Strict               as Map
import           Parser.Zokrates.Lexer          ( tokenize )
import           Parser.Zokrates.Parser         ( parseZokratesFile )
import           Parser.Includer                ( Includer(..)
                                                , relativize
                                                , loadGraph
                                                )
import           System.Directory               ( doesFileExist )
import           System.FilePath                ( (</>) )

-- Parse the given file.
parseFile :: FilePath -> IO [SItem]
parseFile p = do
  f <- readFile p
  return
    (map (mapAnns toSpan) $ parseZokratesFile $ either error id $ tokenize f)
  where toSpan (PosnPair x y) = Span p x y

newtype LoadableFile = LoadableFile [SItem]
instance Includer LoadableFile where
  parse p = LoadableFile <$> parseFile p

  includes p (LoadableFile f) =
    mapM (absolutePath p)
      $ filter (not . isInfixOf "EMBED")
      $ map (\(rp, _, _) -> ast rp)
      $ collectImports
      $ map ast f

stdlibLoc :: String
stdlibLoc = "test/Code/Zokrates/stdlib"

absolutePath :: FilePath -> FilePath -> IO FilePath
absolutePath current relative = do
  let ensureZok p = if isSuffixOf ".zok" p then p else p ++ ".zok"
  let paths =
        map ensureZok [current `relativize` relative, stdlibLoc </> relative]
  files <- filterM doesFileExist paths
  return
    $ fromMaybe
        (error $ "Cannot find " ++ show relative ++ " checked " ++ show paths)
    $ listToMaybe files

loadFilesRecursively :: FilePath -> IO SFiles
loadFilesRecursively path = Map.map (\(LoadableFile f) -> f) <$> loadGraph path

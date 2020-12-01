module Parser.Includer
  ( loadGraph
  , Includer(..)
  , relativize
  , Files
  )
where

import qualified Data.Map.Strict               as Map
import           System.FilePath                ( splitFileName
                                                , (</>)
                                                )

class Includer i where
  -- | Map a path to a parsed file
  parse :: FilePath -> IO i
  -- | Map a path and its file to their included (absolute) paths.
  -- The [relativize] function may be helpful.
  includes :: FilePath -> i -> IO [FilePath]

type Files i = Map.Map FilePath i

-- Implementation of DFS file loading
extend :: Includer i => Files i -> [FilePath] -> IO (Files i)
extend fileMap []             = pure fileMap
extend fileMap (path : paths) = if Map.member path fileMap
  then extend fileMap paths
  else do
    file <- parse path
    let fileMap' = Map.insert path file fileMap
    newPaths <- filter (not . flip Map.member fileMap') <$> includes path file
    extend fileMap' (newPaths ++ paths)


-- Given a valid path to A and a path from A -> B, returns a valid path to B
relativize :: FilePath -> FilePath -> FilePath
relativize baseFile relativeFile =
  fst (splitFileName baseFile) </> relativeFile

-- Given a path, loads the file at this path, and all transitive inclusions
loadGraph :: Includer i => FilePath -> IO (Files i)
loadGraph path = extend Map.empty [path]

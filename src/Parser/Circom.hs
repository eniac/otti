module Parser.Circom
  ( parseFile
  , loadFilesRecursively
  , loadMain
  , Files
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
import           System.FilePath

-- Parse the given file.
parseFile :: FilePath -> IO SFile
parseFile p = do
  f <- readFile p
  return (map (mapAnns toSpan) $ parseCircomFile $ tokenize f)
  where toSpan (PosnPair x y) = Span p x y

type Files = Map.Map FilePath SFile

-- Implementation of DFS file loading
extendInclude :: Files -> [FilePath] -> IO Files
extendInclude currentProgram [] = pure currentProgram
extendInclude currentProgram (filePath : toInclude) =
  if Map.member filePath currentProgram
    then extendInclude currentProgram toInclude
    else do
      newFile <- parseFile filePath
      let newProgram = Map.insert filePath newFile currentProgram
      let includedPaths =
            map ((filePath `relativize`) . ast) $ collectIncludes newFile
      let additionalIncludes =
            Prelude.filter (\p -> not $ Map.member p newProgram) includedPaths
      extendInclude newProgram (additionalIncludes ++ toInclude)


-- Given a valid path to A and a path from A -> B, returns a valid path to B
relativize :: FilePath -> FilePath -> FilePath
relativize baseFile relativeFile =
  fst (splitFileName baseFile) </> relativeFile

-- Given a file, gets the first main expression
getFirstMain :: SFile -> SStatement
getFirstMain file | l == 0    = error "Missing main"
                  | l == 1    = head mains
                  | otherwise = error "Multiple mains"
 where
  mains = collectMains file
  l     = length mains

-- Given a path, loads the file at this path, and all transitive inclusions
loadFilesRecursively :: FilePath -> IO Files
loadFilesRecursively path = extendInclude Map.empty [path]


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

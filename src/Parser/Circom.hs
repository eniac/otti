module Parser.Circom(parseFile,loadFilesRecursively,loadMain,Files) where

import System.FilePath
import AST.Circom           (File, Item(..), Expr, Block, MainCircuit(..), collectIncludes, collectFunctions, collectMains, collectTemplates)
import Parser.Circom.Parser (parseCircomFile)
import Parser.Circom.Lexer  (tokenize)
import Data.Map.Strict as Map
import Data.Maybe

-- Parse the given file.
parseFile :: FilePath -> IO File
parseFile p = do
    f <- readFile p
    return (parseCircomFile $ tokenize f)

type Files = Map FilePath File

-- Implementation of DFS file loading
extendInclude :: Files -> [FilePath] -> IO Files
extendInclude currentProgram [] = pure currentProgram
extendInclude currentProgram (filePath:toInclude) =
    if member filePath currentProgram
        then extendInclude currentProgram toInclude
        else
            do
                newFile <- parseFile filePath
                let newProgram = insert filePath newFile currentProgram
                let includedPaths = Prelude.map (filePath `relativize`) $ collectIncludes newFile
                let additionalIncludes = Prelude.filter (\p -> not $ member p newProgram) includedPaths
                extendInclude newProgram (additionalIncludes ++ toInclude)


-- Given a valid path to A and a path from A -> B, returns a valid path to B
relativize :: FilePath -> FilePath -> FilePath
relativize baseFile relativeFile = fst (splitFileName baseFile) </> relativeFile

-- Given a file, gets the first main expression
getFirstMain :: File -> Expr
getFirstMain file
    | l == 0 = error "Missing main"
    | l == 1 = head mains
    | otherwise = error "Multiple mains"
    where
        mains = collectMains file
        l = length mains

-- Given a path, loads the file at this path, and all transitive inclusions
loadFilesRecursively :: FilePath -> IO Files
loadFilesRecursively path = extendInclude Map.empty [path]


loadMain path = do
    loaded <- loadFilesRecursively path
    let mainItems = findWithDefault (error $ "missing main file " ++ path) path loaded
    let allItems = concat (elems loaded)
    let functions = collectFunctions allItems
    let templates = collectTemplates allItems
    return $ MainCircuit
        (getFirstMain mainItems)
        (Map.fromList (Prelude.map (\t -> let (n, a, b) = t in (n, (a, b))) functions))
        (Map.fromList (Prelude.map (\t -> let (n, a, b) = t in (n, (a, b))) templates))

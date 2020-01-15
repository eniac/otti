module Parser.Circom(parseFile,loadFile,Program) where

import System.FilePath
import AST.Circom           (File, Item(Include))
import Parser.Circom.Parser (parseCircomFile)
import Parser.Circom.Lexer  (tokenize)
import Data.Map.Strict as Map

-- Parse the given file.
parseFile :: FilePath -> IO File
parseFile p = do
    f <- readFile p
    return (parseCircomFile $ tokenize f)

type Program = Map FilePath File

-- Implementation of DFS file loading
extendInclude :: Program -> [FilePath] -> IO Program
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

-- Given a file, collects all included (relative) paths
collectIncludes :: File -> [FilePath]
collectIncludes [] = []
collectIncludes ((Include p):r) = p : collectIncludes r
collectIncludes (_:r) = collectIncludes r

-- Given a path, loads the file at this path, and all transitive inclusions
loadFile :: FilePath -> IO Program
loadFile path = extendInclude Map.empty [path]

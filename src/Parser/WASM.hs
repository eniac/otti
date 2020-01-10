module Parser.WASM where
import           Data.ByteString.Char8   as C8 (pack)
import           Data.ByteString.Lazy    as BS (fromStrict)
import           Language.Wasm.Lexer
import           Language.Wasm.Parser
import           Language.Wasm.Structure

-- | Given a filename, parse the wasm in that file
parseWasm :: String -> IO (Either String Module)
parseWasm file = do
  fileContents <- readFile file
  let lexed = scanner $ BS.fromStrict $ C8.pack fileContents
  return $ case lexed of
    Right lexemes -> parseModule lexemes
    Left lexError -> Left $ "Error in lexing:" ++ lexError

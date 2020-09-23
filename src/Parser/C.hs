module Parser.C where
import           Language.C
import           Language.C.System.GCC

-- | Given a filename, parse the C in that file
parseCE :: String -> IO (Either ParseError CTranslUnit)
parseCE file = parseCFile (newGCC "gcc") Nothing [] file

parseC :: String -> IO CTranslUnit
parseC file =
  either (\e -> error $ "parse error: " ++ show e) id <$> parseCE file

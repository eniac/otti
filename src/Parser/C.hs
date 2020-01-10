module Parser.C where
import           Language.C
import           Language.C.System.GCC

-- | Given a filename, parse the C in that file
parseC :: String -> IO (Either ParseError CTranslUnit)
parseC file = parseCFile (newGCC "gcc") Nothing [] file





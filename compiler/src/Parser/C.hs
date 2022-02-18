module Parser.C where
import           Language.C
import           Language.C.System.GCC
import           Util.Cfg                       ( Cfg )
import qualified Util.Cfg                      as Cfg
import           Control.Monad.Reader
import qualified System.FilePath               as F

-- | Given a filename, parse the C in that file
parseCE :: String -> Cfg (Either ParseError CTranslUnit)
parseCE file = do
  gccArgs <- asks Cfg._gccOptions
  liftIO $ parseCFile (newGCC "gcc")
                      Nothing
                      (gccArgs ++ ["-I" ++ F.takeDirectory file])
                      file

parseC :: String -> Cfg CTranslUnit
parseC file =
  either (\e -> error $ "parse error: " ++ show e) id <$> parseCE file

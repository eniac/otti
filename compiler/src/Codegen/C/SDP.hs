{-# LANGUAGE LambdaCase                 #-}
module Codegen.C.SDP where
import qualified Data.Text                     as T
import qualified Data.List                     as L
import qualified System.Process                as Proc
import           Util.Log
import           Control.Monad.IO.Class


sdp_solve :: String -> Log [Double]
sdp_solve dats_file = do
  poutput <- do
    logIf "gadgets::user::verification"
      $ "Starting external SDP solver..."
    liftIO
      $ Proc.readProcess "python3" ["sdp.py", dats_file] []

  return $ parseDoubles poutput
 where
  parseDoubles out =
    fmap (\x -> read x :: Double)
      . fmap T.unpack
      . T.splitOn (T.pack ",")
      . T.init
      . T.tail
      . T.filter (\c -> c /= ' ' && c /= '\n' && c /= '\t')
      . T.pack
      $ out


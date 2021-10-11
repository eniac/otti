{-# LANGUAGE LambdaCase                 #-}
module Codegen.C.SGD where
import qualified Data.Text                     as T
import qualified Data.List                     as L
import qualified System.Process                as Proc
import           Util.Log
import           Control.Monad.IO.Class

sgd_train :: Integer -> Integer -> [String] -> Log [Integer]
sgd_train d n dataset = do
  poutput <- do
    logIf "gadgets::user::verification" $ "Starting external SGD solver ..."
    liftIO
      $  Proc.readProcess "python3" ["sgd.py", show d, show n]
      $  L.intercalate "," dataset
      ++ "\n"
  return $ parseInts poutput
 where
  parseInts out =
    fmap (\x -> read x :: Integer)
      . fmap T.unpack
      . T.splitOn (T.pack ",")
      . T.init
      . T.tail
      . T.filter (\c -> c /= ' ' && c /= '\n' && c /= '\t')
      . T.pack
      $ out


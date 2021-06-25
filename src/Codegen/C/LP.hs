{-# LANGUAGE LambdaCase                 #-}
module Codegen.C.LP where
import qualified Data.Text                     as T
import qualified System.Process                as Proc
import           Util.Log
import           Control.Monad.IO.Class

data LpMode = Primal | Dual

lp_solve :: String -> LpMode -> Log [Double]
lp_solve mps_file mode = do
  poutput <- case mode of
    (Primal) -> do
      logIf "gadgets::user::verification"
        $ "Starting external LPsolver (primal) ..."
      liftIO
        $ Proc.readProcess "python2" ["lpsolver.py", "--primal", mps_file] []
    (Dual) -> do
      logIf "gadgets::user::verification"
        $ "Starting external LPsolver (dual) ..."
      liftIO $ Proc.readProcess "python2" ["lpsolver.py", "--dual", mps_file] []
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


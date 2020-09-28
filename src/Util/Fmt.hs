module Util.Fmt
  ( intToText
  )
where
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as T
                                                ( toStrict )
import qualified Data.Text.Lazy.Builder        as B
import qualified Data.Text.Lazy.Builder.Int    as B

intToText :: Integral a => a -> T.Text
intToText = T.toStrict . B.toLazyText . B.decimal

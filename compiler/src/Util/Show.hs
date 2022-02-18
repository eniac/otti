module Util.Show
  ( pShow
  )
where

import qualified Text.Pretty.Simple            as Simple
import qualified Data.Text.Lazy                as Lazy

pShow :: Show a => a -> String
pShow = Lazy.unpack . Simple.pShow

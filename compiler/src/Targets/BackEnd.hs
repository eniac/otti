module Targets.BackEnd
  ( BackEnd(..)
  )
where

import qualified IR.SMT.Assert                 as Assert
import           Util.Log


class BackEnd o where
  target :: Assert.AssertState -> Log o

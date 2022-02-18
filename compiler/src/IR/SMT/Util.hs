module IR.SMT.Util
  ( smtDiv
  , smtRem
  )
where

import Data.BitVector

-- | Divide as in the SMT bitvector language. All ones if dividing by zero
smtDiv :: BV -> BV -> BV
smtDiv y x = if x == 0 then ones (size y) else div y x

-- | Remainder as in the SMT bitvector language. Left operand if right is zero.
smtRem :: BV -> BV -> BV
smtRem y x = if x == 0 then y else rem y x

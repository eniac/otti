module IR.SMT where
import           IR.IR
import           Targets.SMT.Z3Wrapper

data Type = Type

data CInfo = CInfo { ctype  :: Type
                   , cundef :: Node
                   }

type SMTNode = IRNode CInfo

module IR.IR where
import           Targets.SMT.Z3Wrapper

-- | IR node.
-- An IR node consists of an SMT node and some optional other fields
data IRNode a = IRNode { smtNode    :: Node
                       , extraState :: a
                       }








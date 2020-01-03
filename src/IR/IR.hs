module IR.IR where
import           Targets.SMT

-- | IR node.
-- An IR node consists of an SMT node and some optional other fields
data IRNode a = IRNode { smtNode    :: Node
                       , extraState :: a
                       } deriving (Eq, Ord, Show)








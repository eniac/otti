module IR.SMT (
              ) where
import           AST.Simple                 (Type (..))
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           IR.IR
import           Targets.SMT.Z3Wrapper
import           Z3.Monad                   as Z

{-|

SMT IR functions. These give the codegen phase a low-level language to translate
AST nodes into

-}

-- | IR information important for the C/C++ language
data CInfo = CInfo { ctype  :: Type
                   , cundef :: Node
                   }

-- | An IR node for converting C/C++ into raw SMT
type SMTNode = IRNode CInfo

newVar :: Type
       -> String
       -> SMTNode
newVar ty name = error ""




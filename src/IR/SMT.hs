module IR.SMT (
              ) where
import           AST.Simple                 (Type (..), isDouble)
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           IR.IR
import           Targets.SMT                as SMT
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

mkNode :: Node -> Type -> Node -> SMTNode
mkNode smtNode cty cundef = IRNode smtNode $ CInfo cty cundef

t :: SMTNode -> Type
t = ctype . extraState

u :: SMTNode -> Node
u = cundef . extraState

n :: SMTNode -> Node
n = smtNode

newVar :: Type
       -> String
       -> SMTNode
newVar ty name = error ""

---
--- IR operations for translating C++ to SMT
---

cppNeg :: SMTNode -> SMT SMTNode
cppNeg node = do
  let op = if isDouble $ t node then SMT.fpNeg else SMT.neg
  result <- op (n node)
  return $ mkNode result (t node) (u node)


---
--- IR operations for translating JS to SMT
---

---
--- IR operations for translating verification statements to SMT
---






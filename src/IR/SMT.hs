{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

---
--- Monad defintions
---

-- | State for keeping track of SMT-layer information
data SMTState = SMTState { vars         :: M.Map String Node
                         , solverResult :: SMTResult
                         }

newtype SMT a = SMT (StateT SMTState Z.Z3 a)
    deriving (Functor, Applicative, Monad, MonadState SMTState, MonadIO)

instance Z.MonadZ3 SMT where
    getSolver = SMT $ lift $ Z.getSolver
    getContext = SMT $ lift $ Z.getContext

data SMTResult = SolverSat { example :: (M.Map String Double) }
               | SolverUnsat
               | SolverFailed
               deriving (Eq, Ord, Show)

---
--- IR functions
---

newVar :: Type
       -> String
       -> SMTNode
newVar ty name = error ""




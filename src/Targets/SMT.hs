module Targets.SMT ( Sort
                   , Node
                     -- * SMT monad and related operations
                   , SMT
                   , runSMT
                   , evalSMT
                   , execSMT
                   , emptySMTState
                   , SMTResult(..)
                   , getVars
                   , runSolver
                     -- * Solver directives
                   , push
                   , pop
                   , assert
                     -- * Operations
                   , eq
                   , add
                   , sub
                   , mul
                   , sdiv
                   , udiv
                   , mod
                   , srem
                   , urem
                   , and
                   , or
                   , xor
                   , not
                   , neg
                   , sll
                   , sra
                     -- * Comparisons
                   , ugt
                   , sgt
                   , ugte
                   , sgte
                   , ult
                   , slt
                   , ulte
                   , slte
                     -- * Other floating and non-floating constants and operations
                   , cond
                   , sext
                   , uext
                   , slice
                   , double
                   , inf
                   , fpzero
                   , nan
                     -- * Rounding modes
                   , rna
                   , rne
                   , rtn
                   , rtp
                   , rtz
                   , rtntte
                     -- * Floating-point queries
                   , isInf
                   , isNan
                   , isNeg
                   , isPos
                   , isZero
                     -- * Floating-point operations
                   , fpAbs
                   , fpAdd
                   , fpSub
                   , fpDiv
                   , fpMul
                   , fpRem
                   , fpNeg
                   , fpEq
                   , fpGte
                   , fpGt
                   , fpLte
                   , fpLt
                   , fpMin
                   , fpMax
                   , fpFloor
                   , fpCeil
                   , castSBv
                   , castUBv
                   , castFp
                     -- * Overflow and underflow detection operations
                   , addOverflows
                   , addUnderflows
                   , addUndef
                   , mulOverflows
                   , mulUnderflows
                   , mulUndef
                   , subOverflows
                   , subUnderflows
                   , subUndef
                   ) where
import           Prelude               hiding (and, mod, not, or)
import           Targets.SMT.SMTMonad
import           Targets.SMT.Z3Wrapper

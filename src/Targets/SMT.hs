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
                   , assign
                     -- * Make variables and numbers
                   , newVar
                   , bvNum
                   , doubleNum
                   , bvSort
                   , doubSort
                   , arraySort
                   , ones
                     -- * Accessing bits and arrays
                   , getBitsFrom
                   , setBitsTo
                   , load
                   , store
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
                   , srl
                   , umin
                   , umax
                   , smin
                   , smax
                     -- * Comparisons
                   , ugt
                   , sgt
                   , ugte
                   , sgte
                   , ult
                   , slt
                   , ulte
                   , slte
                     -- * Other ops
                   , cond
                   , sext
                   , uext
                   , slice
                   , concat
                   , concatMany
                     -- * Floating point
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
import           Prelude               hiding (and, concat, mod, not, or)
import           Targets.SMT.SMTMonad
import           Targets.SMT.Z3Wrapper

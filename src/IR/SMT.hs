module IR.SMT (
              ) where
import           AST.Simple                 (Type (..), isDouble, isSignedInt,
                                             isUnsignedInt)
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

-- Unary operations

-- | C++ unary negation---meaning 5 becomes -5. This is not a bitwise negation
cppNeg :: SMTNode -> SMT SMTNode
cppNeg node = do
  let op = if isDouble $ t node then SMT.fpNeg else SMT.neg
  result <- op $ n node
  return $ mkNode result (t node) (u node)

-- | C++ bitwise negation
cppBitwiseNeg :: SMTNode -> SMT SMTNode
cppBitwiseNeg node = do
  when (isDouble $ t node) $ error "Cannot bitwise negate double"
  result <- SMT.not $ n node
  return $ mkNode result (t node) (u node)

-- Comparisons

-- | Wrapper for the C++ comparison operations.
-- To: (1) make them polymorphic across types
--     (2) choose the right kind of comparison depending on the input types
--     (3) set the undef bit
cppCompareWrapper :: SMTNode -- ^ Left operand
                  -> SMTNode -- ^ Right operand
                  -> (Node -> Node -> SMT Node) -- ^ Unsigned comparison op
                  -> (Node -> Node -> SMT Node) -- ^ Signed comparison op
                  -> (Node -> Node -> SMT Node) -- ^ Floating point comparison op
                  -> SMT SMTNode -- ^ Result
cppCompareWrapper left right uCompare sCompare fCompare
 | isDouble (t left) || isDouble (t right) = do
     unless (t left == t right) $ error "Expected two doubles as argumnets to comparison"
     compare <- fCompare (n left) (n right)
     maybeDefinedNode left right compare Bool
 | isUnsignedInt (t left) || isUnsignedInt (t right) = do
     compare <- uCompare (n left) (n right)
     maybeDefinedNode left right compare Bool
 | otherwise = do
     compare <- sCompare (n left) (n right)
     maybeDefinedNode left right compare Bool

cppEq, cppGt, cppGte, cppLt, cppLte :: SMTNode -> SMTNode -> SMT SMTNode
cppEq left right = cppCompareWrapper left right SMT.eq SMT.eq SMT.fpEq
cppGt left right = cppCompareWrapper left right SMT.ugt SMT.sgt SMT.fpGt
cppGte left right = cppCompareWrapper left right SMT.ugte SMT.sgte SMT.fpGte
cppLt left right = cppCompareWrapper left right SMT.ult SMT.slt SMT.fpLt
cppLte left right = cppCompareWrapper left right SMT.ulte SMT.slte SMT.fpLte

-- Binary operators

binOpWrapper :: SMTNode -- ^ Left operand
             -> SMTNode -- ^ Right operand
             -> (Node -> Node -> SMT Node) -- ^ Operation
             -> Maybe (Bool -> Node -> Node -> SMT Node) -- ^ Overflow detection operation
             -> String -- ^ Operation name
             -> SMT SMTNode -- ^ Result
binOpWrapper left right op overflowOp opName = do
  unless (numBits (t left) == numBits (t right)) $
    error $ unwords ["Mismatched types to operation"
                    , opName
                    , ":"
                    , show $ numBits $ t left
                    , "and"
                    , show $ numBits $ t right
                    ]
  when (isDouble $ t left) $ unless (isDouble $ t right) $
    error $ unwords ["Expected two doubles to", opName]
  parentsUndef <- SMT.or (u left) (u right)
  canOverflow <- case overflowOp of
                   -- No overflow-checking op provided: there isn't the opertunity
                   -- to overflow/underflow and cause undefined behavior
                   Nothing  -> return parentsUndef
                   -- There is an overflow op, so if there's overflow or the parents are
                   -- undef, the child node is also undef
                   Just oop -> do
                     flow <- oop (isSignedInt $ t left) (n left) (n right)
                     SMT.or parentsUndef flow
  result <- op (n left) (n right)
  let ty = if t left == t right
           then t left
           else if isSignedInt $ t left then t left else t right
  return $ mkNode result ty canOverflow

cppOr, cppAnd, cppSub, cppMul, cppAdd, cppMin, cppMax :: SMTNode -> SMTNode -> SMT SMTNode

cppOr left right
  | (isDouble $ t left) || (isDouble $ t right) = error "No bitwise or for doubles"
  | otherwise = binOpWrapper left right SMT.or Nothing "or"

cppAnd left right
  | (isDouble $ t left) || (isDouble $ t right) = error "No bitwise and for doubles"
  | otherwise = binOpWrapper left right SMT.and Nothing "and"

cppSub left right
  | isDouble (t left) || isDouble (t right) = binOpWrapper left right SMT.fpSub Nothing "sub"
  | otherwise = binOpWrapper left right SMT.sub (Just SMT.subUndef) "sub"

cppMul left right
  | isDouble (t left) || isDouble (t right) = binOpWrapper left right SMT.fpMul Nothing "mul"
  | otherwise = binOpWrapper left right SMT.mul (Just SMT.mulUndef) "mul"

cppAdd left right
  | isDouble (t left) || isDouble (t right) = binOpWrapper left right SMT.fpAdd Nothing "add"
  | otherwise = binOpWrapper left right SMT.add (Just SMT.addUndef) "add"

cppMin right left
  | isDouble (t right) || isDouble (t left) = binOpWrapper left right SMT.fpMin Nothing "min"
  -- | isUnsigned (t right) && isUnsigned (t left) =
  --     binOpWrapper left right SMT.umin Nothing "min"
--  | isSigned (t right) && isSigned (t left) = binOpWrapper left right SMT.smin Nothing "min"
  | otherwise = error "Compiler error: Can't use std:min on a signed and unsigned"

cppMax right left
  | isDouble (t right) || isDouble (t left) = binOpWrapper left right SMT.fpMax Nothing "max"
  -- | isUnsigned (vtype right) && isUnsigned (vtype left) =
  --     noopWrapper left right D.umax Nothing "max"
  -- | isSigned (vtype right) && isSigned (vtype left) =
  --     noopWrapper left right D.smax Nothing "max"
  | otherwise = error "Compiler error: Can't use std:max on a signed and unsigned"

-- Extra helpers

numBits :: Type -> Int
numBits U8     = 8
numBits S8     = 8
numBits U16    = 16
numBits S16    = 16
numBits U32    = 32
numBits S32    = 32
numBits U64    = 64
numBits S64    = 64
numBits Double = 64

-- | Make a new node that is defined if its parents are defined
maybeDefinedNode :: SMTNode -- ^ Parent 1
                 -> SMTNode -- ^ Parent 2
                 -> Node -- ^ Inner SMT node
                 -> Type -- ^ C++ type
                 -> SMT SMTNode -- ^ Resulting IR node
maybeDefinedNode parent1 parent2 node ty = do
  childUndef <- SMT.or (u parent1) (u parent2)
  return $ mkNode node ty childUndef

---
--- IR operations for translating JS to SMT
---

---
--- IR operations for translating verification statements to SMT
---






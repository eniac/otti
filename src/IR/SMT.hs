module IR.SMT where
import           AST.Simple                 (Type (..), int16, int32, int64,
                                             int8, isDouble, isSignedInt,
                                             isUnsignedInt)
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           IR.IR
import           Targets.SMT                (Node, SMT)
import qualified Targets.SMT                as SMT
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

newSMTVar :: Type
          -> String
          -> SMT SMTNode
newSMTVar ty name = do
  sort <- case ty of
            Double -> SMT.doubSort
            _      -> SMT.bvSort $ numBits ty
  var <- SMT.newVar name sort
  undefSort <- SMT.bvSort 1
  undefVar <- SMT.newVar (name ++ "_undef") undefSort
  return $ mkNode var ty undefVar

newInt :: Type
       -> Integer
       -> SMT SMTNode
newInt ty val = do
  int <- case ty of
           Bool | val <= 1 -> SMT.bvNum 1 val
           Bool -> error $ unwords $ [show val, "is past the range of a boolean"]
           U8 | val <= 255 -> SMT.bvNum 8 val
           U8 -> error $ unwords $ [show val, "is past the range of an i8"]
           S8 | val <= 127 -> SMT.bvNum 8 val
           S8 -> error $ unwords $ [show val, "is past the range of a signed i8"]
           U16 | val <= 65535 -> SMT.bvNum 16 val
           U16 -> error $ unwords $ [show val, "is past the range of an i16"]
           S16 | val <= 32767 -> SMT.bvNum 16 val
           S16 -> error $ unwords $ [show val, "is past the range of a signed i16"]
           U32 | val <= 4294967295 -> SMT.bvNum 32 val
           U32 -> error $ unwords $ [show val, "is past the range of an i32"]
           S32 | val <= 2147483647 -> SMT.bvNum 32 val
           S32 -> error $ unwords $ [show val, "is past the range of a signed i32"]
           U64 | val <= 18446744073709551615 -> SMT.bvNum 64 val
           U64 -> error $ unwords $ [show val, "is past the range of an i64"]
           S64 | val <= 9223372036854775807 -> SMT.bvNum 64 val
           S64 -> error $ unwords $ [show val, "is past the range of a signed i64"]
           _ -> error "Cannot make non-int types with newInt"
  undef <- SMT.bvNum 1 0
  return $ mkNode int ty undef

newDouble :: Type
          -> Double
          -> SMT SMTNode
newDouble ty val = do
  double <- case ty of
              Double -> SMT.doubleNum val
              _      -> error "Cannot make non-double type with newDouble"
  undef <- SMT.bvNum 1 0
  return $ mkNode double ty undef

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

cppOr, cppXor, cppAnd, cppSub, cppMul, cppAdd, cppMin, cppMax :: SMTNode -> SMTNode -> SMT SMTNode

cppOr left right
  | (isDouble $ t left) || (isDouble $ t right) = error "No bitwise or for doubles"
  | otherwise = binOpWrapper left right SMT.or Nothing "or"

cppXor left right
  | (isDouble $ t left) || (isDouble $ t right) = error "No bitwise xor for doubles"
  | otherwise = binOpWrapper left right SMT.xor Nothing "xor"

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
  | isUnsignedInt (t right) && isUnsignedInt (t left) =
      binOpWrapper left right SMT.umin Nothing "min"
  | isSignedInt (t right) && isSignedInt (t left) =
      binOpWrapper left right SMT.smin Nothing "min"
  | otherwise = error "Compiler error: Can't use std:min on a signed and unsigned"

cppMax right left
  | isDouble (t right) || isDouble (t left) = binOpWrapper left right SMT.fpMax Nothing "max"
  | isUnsignedInt (t right) && isUnsignedInt (t left) =
      binOpWrapper left right SMT.umax Nothing "max"
  | isSignedInt (t right) && isSignedInt (t left) =
      binOpWrapper left right SMT.smax Nothing "max"
  | otherwise = error "Compiler error: Can't use std:max on a signed and unsigned"

-- | Make this more general: only supports 32-bit right now, bad!
cppShiftLeft left right
  | not (int32 $ t left) || not (int32 $ t right) =
      error "Only support 32 bit SHL"
  | isUnsignedInt $ t left = do
      parentsUndef <- SMT.or (u left) (u right)
        -- If the right is signed and negative, undefined behavior
      undef <- if isSignedInt $ t right
               then do
                 zero <- SMT.bvNum 32 0
                 opUndef <- SMT.slt (n right) zero
                 SMT.or parentsUndef opUndef
               else return parentsUndef

      result <- SMT.sll (n left) (n right)
      return $ mkNode result (t left) undef
  | otherwise = do

      -- Do the operation in 64-bits and see if any of the left bits are set.
      -- If so, the operation has undefined behavior because some bit was
      -- shifted off the end of the 32-bit variable
      left64 <- SMT.uext (n left) 32
      right64 <- SMT.uext (n right) 32
      result64 <- SMT.sll left64 right64
      top32 <- SMT.slice result64 63 32
      zero <- SMT.bvNum 32 0

      -- If the right is greater than 32, it is definitely undef
      -- This will also catch trying to shift by a negative
      thirtyTwo <- SMT.bvNum 32 32

      -- Is it undef?
      opUndef1 <- SMT.eq top32 zero >>= SMT.not
      opUndef2 <- SMT.ugt (n right) thirtyTwo
      opUndef <- SMT.or opUndef1 opUndef2
      parentsUndef <- SMT.or (n left) (n right)
      undef <- SMT.or opUndef parentsUndef

      result <- SMT.sll (n left) (n right)
      return $ mkNode result (t left) result

-- | Also make this more general (only supports 32)
cppShiftRight left right
  | not (int32 $ t left) || not (int32 $ t right) =
      error "Only support 32 bit SHR"
  | isUnsignedInt (t left) = do
      undef <- makeUndef
      result <- SMT.srl (n left) (n right)
      return $ mkNode result U32 undef
  | otherwise = do
      undef <- makeUndef
      result <- SMT.sra (n left) (n right)
      return $ mkNode result S32 undef
  where
    makeUndef = do
      zero <- SMT.bvNum 32 0
      opUndef <- SMT.slt (n right) zero
      parentsUndef <- SMT.or (u left) (u right)
      SMT.or opUndef parentsUndef

cppCond cond trueBr falseBr = do
  unless (t cond == Bool) $ error "Conditional must be a boolean"
  unless (t trueBr == t falseBr) $ error "Both branches of cond must have same type"
  result <- SMT.cond (n cond) (n trueBr) (n falseBr)
  undef <- SMT.or (u cond) (u trueBr) >>= SMT.or (u falseBr)
  return $ mkNode result (t trueBr) undef

cppCast node toTy
  -- | isDouble fromTy = case toTy of
  --                       Signed     -> do
  --                         result <- D.castFp (vnode node) 32
  --                         return $ VNode (vundef node) result Signed
  --                       Signed64   -> do
  --                         result <- D.castFp (vnode node) 64
  --                         return $ VNode (vundef node) result Signed64
  --                         _          -> error "We only suppor Double to int32 casts rn"
    | int8 fromTy = case toTy of
                      _ | int8 toTy -> return $ mkNode (n node) toTy (u node)
                      _ | int16 toTy -> do
                        result <- extend (n node) 8
                        return $ mkNode result toTy (u node)
                      _ | int32 toTy -> do
                        result <- extend (n node) 24
                        return $ mkNode result toTy (u node)
                      _ | int64 toTy -> do
                        result <- extend (n node) 56
                        return $ mkNode result toTy (u node)
                      _          -> error "Illegal cast types"
    | int16 fromTy = case toTy of
                       _ | int8 toTy -> do
                         result <- SMT.slice (n node) 7 0
                         return $ mkNode result toTy (u node)
                       _ | int16 toTy -> return $ mkNode (n node) toTy (u node)
                       _ | int32 toTy -> do
                         result <- extend (n node) 16
                         return $ mkNode result toTy (u node)
                       _ | int64 toTy -> do
                         result <- extend (n node) 48
                         return $ mkNode result toTy (u node)
                       _          -> error "Illegal cast types"
    | int32 fromTy = case toTy of
                       _ | int8 toTy -> do
                         result <- SMT.slice (n node) 23 0
                         return $ mkNode result toTy (u node)
                       _ | int16 toTy -> do
                         result <- SMT.slice (n node) 15 0
                         return $ mkNode result toTy (u node)
                       _ | int32 toTy -> return $ mkNode (n node) toTy (u node)
                       _ | int64 toTy -> do
                         result <- extend (n node) 32
                         return $ mkNode result toTy (u node)
                       _          -> error "Illegal cast types"
    | int64 fromTy = case toTy of
                       _ | int8 toTy -> do
                         result <- SMT.slice (n node) 55 0
                         return $ mkNode result toTy (u node)
                       _ | int16 toTy -> do
                         result <- SMT.slice (n node) 47 0
                         return $ mkNode result toTy (u node)
                       _ | int32 toTy -> do
                         result <- SMT.slice (n node) 31 0
                         return $ mkNode result toTy (u node)
                       _ | int64 toTy -> return $ mkNode (n node) toTy (u node)
                       _          -> error "Illegal cast types"
    | otherwise = error "Illegal cast types"
    where fromTy = t node
          extend = if isSignedInt toTy then SMT.sext else SMT.uext

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

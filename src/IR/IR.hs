{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module IR.IR where
import           AST.Typed
import           Control.Monad (unless)
import           IR.SMTIRMonad
import           Targets.SMT   (Node, SMTResult)
import qualified Targets.SMT   as SMT

class (Typed b, Eq b) => IRNode a b | a -> b where
  n :: a -> Node
  t :: a -> b

--
-- SMT assertions and assignments
--

smtResult :: IR SMTResult
smtResult = liftSMT SMT.runSolver

smtPush :: IR ()
smtPush = liftSMT SMT.push

smtPop :: IR ()
smtPop = liftSMT $ SMT.pop

smtAssert :: (IRNode a b) => a -> IR ()
smtAssert = liftSMT . SMT.assert . n

smtAssign :: (IRNode a b) => a -> a -> IR ()
smtAssign n1 n2 = do
--  unless (t n1 == t n2)  $ error "Tried to assign nodes of different types"
  SMT.assign (n n1) (n n2)

smtImplies :: (IRNode a b)
           => a
           -> a
           -> IR ()
smtImplies a b = do
  notA <- SMT.not $ n a
  SMT.or notA (n b) >>= SMT.assert

--
--
--

-- | Make a new SMT integer from the input type
irInt :: (Typed a)
      => a -- ^ Type
      -> Integer -- ^ Value
      -> IR Node
irInt ty val = liftSMT $ do
  let width = numBits ty
      signed = isSignedInt ty
--  unless (isSignedInt ty || isUnsignedInt ty ) $ error $ show ty ADD BOOL
  case width of
    1  | val <= 1 -> SMT.bvNum 1 val
    1  -> error $ unwords $ [show val, "is past the range of a boolean"]
    8  | not signed && val <= 255 -> SMT.bvNum 8 val
    8  | not signed -> error $ unwords $ [show val, "is past the range of an i8"]
    8  | signed && val <= 127 -> SMT.bvNum 8 val
    8  | signed -> error $ unwords $ [show val, "is past the range of a signed i8"]
    16 | not signed && val <= 65535 -> SMT.bvNum 16 val
    16 | not signed -> error $ unwords $ [show val, "is past the range of an i16"]
    16 | signed && val <= 32767 -> SMT.bvNum 16 val
    16 | signed -> error $ unwords $ [show val, "is past the range of a signed i16"]
    32 | not signed && val <= 4294967295 -> SMT.bvNum 32 val
    32 | not signed -> error $ unwords $ [show val, "is past the range of an i32"]
    32 | signed && val <= 2147483647 -> SMT.bvNum 32 val
    32 | signed -> error $ unwords $ [show val, "is past the range of a signed i32"]
    64 | not signed && val <= 18446744073709551615 -> SMT.bvNum 64 val
    64 | not signed -> error $ unwords $ [show val, "is past the range of an i64"]
    64 | signed && val <= 9223372036854775807 -> SMT.bvNum 64 val
    64 | signed -> error $ unwords $ [show val, "is past the range of a signed i64"]
    _ -> SMT.bvNum width val

irFloat = undefined

irGetIdx :: (IRNode a b)
         => a
         -> a
         -> IR Node
irGetIdx node idx = do
  unless (isArray $ t node) $ error "Must call irGetIdx on array"
  let baseType = arrayBaseType $ t node
      elemSize = numBits baseType
      idxSize = numBits $ t idx
  idxBits <- SMT.bvNum idxSize (fromIntegral elemSize) >>= SMT.mul (n idx)
  SMT.getBitsFromBE (n node) elemSize idxBits

irSetIdx :: (IRNode a b)
         => a
         -> a
         -> a
         -> IR Node
irSetIdx arr idx elem = do
  let arrTy = t arr
      idxTy = t idx
      elemTy = t elem
  unless (isArray arrTy) $ error "Cannot call getIdx on non-array"
  let arrBaseType = arrayBaseType arrTy
      elemSize = numBits arrBaseType
  unless (elemTy == arrBaseType) $ error "Wrong element type to setIdx"
  idxBits <- SMT.bvNum (numBits idxTy) (fromIntegral elemSize) >>= SMT.mul (n idx)
  liftSMT $ SMT.setBitsTo (n elem) (n arr) idxBits

-- | Get a field from a struct
-- We don't use getBitsFrom because that allows symbolic indecies and is therefore
-- likely slower than a simple array slice
irGetField :: (IRNode a b)
         => a -- ^ Struct
         -> Int -- ^ Index
         -> IR Node -- ^ Element
irGetField struct idx' = do
  let structType = t struct
      fieldTypes = structFieldTypes structType
      -- Reverse index not from [0..n] but from [n..0] to make SMT.slice happy
      -- I guess its a little endian slice and we have big endian structs
      -- because they're easier to think about
      idx = length fieldTypes - idx' - 1
  unless (idx' < length fieldTypes) $ error "Out of bounds index for getField"
  -- [ elems ] [ target elem] [ elems]
  --          ^ start        ^ end
  let startIdx = numBits $ newStructType $ take idx fieldTypes
      endIdx = (numBits $ newStructType $ take (idx + 1) fieldTypes) - 1
  -- High index to low index to make SMT.slice happy
  SMT.slice (n struct) endIdx startIdx

-- | Set a field in a struct.
-- This does not use setBits from SMT because it is likely slower than array slices
irSetField :: (IRNode a b)
           => a -- ^ Struct
           -> Int -- ^ Index
           -> a -- ^ Element
           -> IR Node -- ^ Result struct
irSetField struct idx elem = do
  let structType = t struct
      fieldTypes = structFieldTypes structType
  unless (idx < length fieldTypes) $ error "Out of bounds index for getField"
  unless (fieldTypes !! idx == t elem) $ error "Mismatch between element type and index"
  -- Too much of a pain to do the slicing thing here
  idxBits <- SMT.bvNum 64 (fromIntegral $ numBits $ newStructType $ take idx fieldTypes)
  liftSMT $ SMT.setBitsTo (n elem) (n struct) idxBits

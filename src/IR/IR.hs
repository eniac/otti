{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module IR.IR where
import           AST.Typed
import           Control.Monad (unless)
import           IR.SMTIRMonad
import           Targets.SMT   (Node, SMTResult)
import qualified Targets.SMT   as SMT

class IRNode a b | a -> b where
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

irGetIdx :: (IRNode a b, Typed b)
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

irSetIdx :: (IRNode a b, Typed b, Eq b)
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



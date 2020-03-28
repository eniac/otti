{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module IR.IR where
import           AST.Typed
import           Control.Monad (forM, forM_, unless, when)
import           Data.Maybe    (fromJust, isNothing)
import           IR.SMTIRMonad
import           Targets.SMT   (Node, SMTResult)
import qualified Targets.SMT   as SMT

class (Typed b, Eq b, Show b) => IRNode a b | a -> b where
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
-- Making numbers
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

---
--- Memory and structures
---

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

irLoad :: (IRNode a b)
       => a
       -> IR Node
irLoad addr = do
  (unless $ isPointer $ t addr) $ error $ unwords [ "Must load from pointer, not"
                                                  , show $ t addr
                                                  ]
  memStrat <- getMemoryStrategy
  case memStrat of
    Flat blockSize -> do
      mem <- currentMem
      -- Figure out how many blocks to read
      -- The following comment explains the three different cases we may face.
      -- We don't special case now on whether the addr is concrete or symbolic, but we could

      -- Consider a load of 32 bits off 0 and blockSize of 32
      -- In this case, the load size is 32
      -- The underestimate of the blocks to read is 1, which in this case is correct
      -- The overestimate of the blocks to read is 2, which is one more than necessary

      -- Now consider a load of 32 bits starting from 16 with the same block size
      -- The underestimate of the blocks to read is 1, but this is too few! The read spans 2
      -- The overestimate of the blocks to read is 2, which captures the span

      -- Finally, consider a load of 16 bits starting from 0 with 32 size blocks
      -- The underestimate of the blocks to read is 16/32, which is zero!
      -- The estimate of the block size is now one, which sounds right.
      -- Finally, the overestimate is again 2

      let pointeeTy = pointeeType $ t addr
          readSize = numBits pointeeTy
          underEstimateBlocks = readSize `div` blockSize
          estimateBlocks = if underEstimateBlocks == 0 then 1 else underEstimateBlocks
          overEstimateBlocks = estimateBlocks + 1 -- if > 8
          blocksToRead = overEstimateBlocks

      when (blocksToRead > 2000) $ error "Load is too large"

      -- Read all the blocks and then smoosh them together into one bv
      let pointerSize = numBits $ t addr
      reads <- forM [0..blocksToRead - 1] $ \offset -> do
        nextPointer <- SMT.bvNum pointerSize (fromIntegral offset) >>= SMT.add (n addr)
        SMT.load mem nextPointer
      wholeRead <- SMT.concatMany reads

      -- Get the start location of the read in wholeRead
      widthSMT <- SMT.bvNum pointerSize $ fromIntegral blockSize
      readStart <- SMT.urem (n addr) widthSMT

      -- Perform the read!
      SMT.getBitsFromBE wholeRead readSize readStart

irStore :: (IRNode a b, Show b)
        => a -- ^ Address
        -> a -- ^ Value
        -> Maybe a -- ^ Guard
        -> IR ()
irStore addr val mGuard = do
  (unless $ isPointer $ t addr) $ error "Must store to pointer"
  let addrSMT = n addr
      valSMT = n val
  memStrat <- getMemoryStrategy

  case memStrat of
    Flat blockSize -> do
      -- Figure out how many blocks we need
      let pointerSize = numBits $ t addr
          writeSize = numBits $ t val

      unless (pointerSize == 32) $ error "Only support 32-bit pointers"
      when (writeSize `mod` 8 /= 0) $ error $ unwords ["Unaligned type size:"
                                                      , show $ t val
                                                      , show writeSize
                                                      ]

      width <- SMT.bvNum pointerSize $ fromIntegral blockSize
      addressSym <- SMT.udiv addrSMT width
      -- We may be reading across block bounaries if the cell size is more than 8
      -- See giant comment in smtLoad
      let underEstimateBlocks = writeSize `div` blockSize
          estimateBlocks = if underEstimateBlocks == 0 then 1 else underEstimateBlocks
          blocksToWrite = estimateBlocks + 1

      when (blocksToWrite > 2000) $ error "Write too large"

      -- Read the contents of memory at all the blocks
      oldMem <- currentMem
      reads <- forM [0..blocksToWrite - 1] $ \offset -> do
        nextPointer <- SMT.bvNum pointerSize (fromIntegral offset) >>= SMT.add addrSMT
        SMT.load oldMem nextPointer
      currentContents <- SMT.concatMany reads

      -- Write the relevant bits in the read
      writeStart <- SMT.urem addrSMT width
      maybeWrite <- liftSMT $ SMT.setBitsTo valSMT currentContents writeStart
      write <- if isNothing mGuard
               then return maybeWrite
               else SMT.cond (n $ fromJust mGuard) maybeWrite currentContents

      -- Write the updated bits back to memory
      -- Write from high bits to low bits as the pointer values increase:
      -- ptr:   1        2        3      4
      -- val: [high][high mid][low mid][last]

      forM_ [0..blocksToWrite - 1] $ \offset -> do
        nextPointer <- SMT.bvNum pointerSize (fromIntegral offset) >>= SMT.add addrSMT
        let sliceStart = blocksToWrite * blockSize - (offset * blockSize) - 1
            sliceEnd = sliceStart - blockSize + 1
        writeSlice <- SMT.slice write sliceStart sliceEnd
        oldMem <- currentMem
        newMem <- nextMem
        SMT.store oldMem nextPointer writeSlice >>= SMT.assign newMem

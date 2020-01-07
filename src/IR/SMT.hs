module IR.SMT ( SMTNode
              , SMTResult(..)
              , IR
              , runIR
              , evalIR
              , execIR
              , initMem
                -- * Interacting with the SMT solver
              , smtAssert
              , smtAssign
              , smtImplies
              , smtResult
              , smtTrue
              , smtFalse
              , smtLoad
              , smtStore
              , smtPush
              , smtPop
                -- * Variables
              , newVar
              , newInt
              , newIntStruct
              , newStruct
              , newIntArray
              , newArray
              , newPtr
              , newDouble
                -- * Struct
              , getIdx
              , setIdx
              , getField
              , setField
                -- * C++ IR
              , cppNeg
              , cppBitwiseNeg
              , cppEq
              , cppGt
              , cppGte
              , cppLt
              , cppLte
              , cppOr
              , cppXor
              , cppAnd
              , cppSub
              , cppMul
              , cppAdd
              , cppMin
              , cppMax
              , cppShiftLeft
              , cppShiftRight
              , cppCond
              , cppCast
              ) where
import           AST.Simple                 (Type (..), arrayBaseType,
                                             arrayNumElems, int16, int32, int64,
                                             int8, isArray, isDouble, isPointer,
                                             isSignedInt, isStruct,
                                             isUnsignedInt, numBits,
                                             pointeeType, structFieldTypes)
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           IR.IR
import           IR.SMTIRMonad
import           Targets.SMT                (Node, SMT, SMTResult)
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
           deriving (Eq, Ord, Show)

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

---
--- Variables and numbers
---

newVar :: Type
       -> String
       -> IR SMTNode
newVar ty name = liftSMT $ do
  sort <- case ty of
            Double -> SMT.doubSort
            _      -> SMT.bvSort $ numBits ty
  var <- SMT.newVar name sort
  undefSort <- SMT.bvSort 1
  undefVar <- SMT.newVar (name ++ "_undef") undefSort
  return $ mkNode var ty undefVar

newIntStruct :: Type
          -> [Integer]
          -> IR SMTNode
newIntStruct ty vals = do
  resultElems <- case ty of
    Struct tys -> do
      unless (length tys == length vals) $ error "Wrong number of element args to struct"
      forM (zip tys vals) $ uncurry newInt
    _ -> error "Wrong type to newIntStruct"
  result <- SMT.concatMany $ map n $ resultElems
  undef <- SMT.bvNum 1 0
  return $ mkNode result ty undef

newStruct :: Type
          -> [SMTNode]
          -> IR SMTNode
newStruct ty nodes = do
  unless (isStruct ty) $ error "Must have struct type to make new struct"
  unless (length nodes == length (structFieldTypes ty)) $
    error "Wrong number of element args to struct"
  result <- SMT.concatMany $ map n nodes
  undef <- foldM SMT.or (u $ head nodes) (map u $ tail nodes)
  return $ mkNode result ty undef

newIntArray :: Type
            -> [Integer]
            -> IR SMTNode
newIntArray ty vals = do
  resultElems <- case ty of
    Array num ty -> do
      unless (length vals == num) $ error "Wrong number of element args to array"
      forM vals $ \v -> newInt ty v
    _ -> error "Wrong type to newIntArray"
  result <- SMT.concatMany $ map n resultElems
  undef <- SMT.bvNum 1 0
  return $ mkNode result ty undef

newArray :: Type
         -> [SMTNode]
         -> IR SMTNode
newArray ty nodes = do
  unless (isArray ty) $ error "Must have array type to make new struct"
  unless (length nodes == arrayNumElems ty) $ error "Wrong number of element args to array"
  result <- SMT.concatMany $ map n nodes
  undef <- foldM SMT.or (u $ head nodes) (map u $ tail nodes)
  return $ mkNode result ty undef

newPtr :: Type
       -> Integer
       -> IR SMTNode
newPtr ty val = liftSMT $ do
  result <- case ty of
    Ptr32{} -> SMT.bvNum 32 val
    Ptr64{} -> SMT.bvNum 64 val
    _       -> error "Cannot make non-pointer with newPtr"
  undef <- SMT.bvNum 1 0
  return $ mkNode result ty undef

newInt :: Type
       -> Integer
       -> IR SMTNode
newInt ty val = liftSMT $ do
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

newDouble :: Type -- ^ One day we will support different floating point type arguments
          -> Double
          -> IR SMTNode
newDouble ty val = liftSMT $ do
  double <- case ty of
              Double -> SMT.doubleNum val
              _      -> error "Cannot make non-double type with newDouble"
  undef <- SMT.bvNum 1 0
  return $ mkNode double ty undef

-- Asserting and assigning

smtAssert :: SMTNode -> IR ()
smtAssert = liftSMT . SMT.assert . n

smtAssign :: SMTNode -> SMTNode -> IR ()
smtAssign n1 n2 = do
--  unless (t n1 == t n2)  $ error "Tried to assign nodes of different types"
  SMT.assign (n n1) (n n2)
  SMT.assign (u n1) (u n2)

smtTrue :: IR SMTNode
smtTrue = newInt Bool 1

smtFalse :: IR SMTNode
smtFalse = newInt Bool 0

smtImplies :: SMTNode
           -> SMTNode
           -> IR ()
smtImplies a b = do
  notA <- cppBitwiseNeg a
  cppOr notA b >>= smtAssert

smtResult :: IR SMTResult
smtResult = liftSMT SMT.runSolver

smtPush :: IR ()
smtPush = liftSMT push

smtPop :: IR ()
smtPop = liftSMT $ pop 1

-- Struct and array access

getIdx :: SMTNode -- ^ Array
       -> SMTNode -- ^ Index
       -> IR SMTNode -- ^ Element
getIdx arr idx = do
  let arrType = t arr
      arrBaseType = arrayBaseType arrType
  unless (isArray arrType) $ error "Cannot call getIdx on non-array"
  let elemSize = numBits arrBaseType
  idxBits <- SMT.bvNum (numBits $ t idx) (fromIntegral elemSize) >>= SMT.mul (n idx)
  result <- SMT.getBitsFromBE (n arr) elemSize idxBits
  undef <- SMT.or (u arr) (u idx)
  return $ mkNode result arrBaseType undef

setIdx :: SMTNode -- ^ Array
       -> SMTNode -- ^ Index
       -> SMTNode -- ^ Element
       -> IR SMTNode -- ^ Result array
setIdx arr idx elem = do
  let arrType = t arr
      arrBaseType = arrayBaseType arrType
      elemSize = numBits arrBaseType
  unless (isArray arrType) $ error "Cannot call getIdx on non-array"
  unless (t elem == arrBaseType) $ error "Wrong element type to setIdx"
  idxBits <- SMT.bvNum (numBits $ t idx) (fromIntegral elemSize) >>= SMT.mul (n idx)
  result <- liftSMT $ SMT.setBitsTo (n elem) (n arr) idxBits
  undef <- SMT.or (u arr) (u idx) >>= SMT.or (u elem)
  return $ mkNode result arrType undef

-- | Get a field from a struct
-- We don't use getBitsFrom because that allows symbolic indecies and is therefore
-- likely slower than a simple array slice
getField :: SMTNode -- ^ Struct
         -> Int -- ^ Index
         -> IR SMTNode -- ^ Element
getField struct idx' = do
  let structType = t struct
      fieldTypes = structFieldTypes structType
      -- Reverse index not from [0..n] but from [n..0] to make SMT.slice happy
      -- I guess its a little endian slice and we have big endian structs
      -- because they're easier to think about
      idx = length fieldTypes - idx' - 1
  unless (idx' < length fieldTypes) $ error "Out of bounds index for getField"
  -- [ elems ] [ target elem] [ elems]
  --          ^ start        ^ end
  let startIdx = numBits $ Struct $ take idx fieldTypes
      endIdx = (numBits $ Struct $ take (idx + 1) fieldTypes) - 1
  -- High index to low index to make SMT.slice happy
  result <- SMT.slice (n struct) endIdx startIdx
  -- Confusingly, we use idx' here because we're back to our own representation
  -- (don't have to care about slice, do have to care about ordering of field list)
  return $ mkNode result (fieldTypes !! idx') (u struct)

-- | Set a field in a struct.
-- This does not use setBits from SMT because it is likely slower than array slices
setField :: SMTNode -- ^ Struct
         -> Int -- ^ Index
         -> SMTNode -- ^ Element
         -> IR SMTNode -- ^ Result struct
setField struct idx elem = do
  let structType = t struct
      fieldTypes = structFieldTypes structType
  unless (idx < length fieldTypes) $ error "Out of bounds index for getField"
  unless (fieldTypes !! idx == t elem) $ error "Mismatch between element type and index"
  -- Too much of a pain to do the slicing thing here
  idxBits <- SMT.bvNum 64 (fromIntegral $ numBits $ Struct $ take idx fieldTypes)
  result <- liftSMT $ SMT.setBitsTo (n elem) (n struct) idxBits
  undef <- SMT.or (u struct) (u elem)
  return $ mkNode result structType undef

-- memory

smtLoad :: SMTNode
        -> IR SMTNode
smtLoad addr = do
  (unless $ isPointer $ t addr) $ error "Must load from pointer"
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
      result <- SMT.getBitsFromBE wholeRead readSize readStart
      let undef = u addr
      return $ mkNode result pointeeTy undef

smtStore :: SMTNode -> SMTNode -> IR ()
smtStore addr val = do
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
      write <- liftSMT $ SMT.setBitsTo valSMT currentContents writeStart

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

-- Unary operations

-- | C++ unary negation---meaning 5 becomes -5. This is not a bitwise negation
cppNeg :: SMTNode -> IR SMTNode
cppNeg node = liftSMT $ do
  let op = if isDouble $ t node then SMT.fpNeg else SMT.neg
  result <- op $ n node
  return $ mkNode result (t node) (u node)

-- | C++ bitwise negation
cppBitwiseNeg :: SMTNode -> IR SMTNode
cppBitwiseNeg node = liftSMT $ do
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
                  -> IR SMTNode -- ^ Result
cppCompareWrapper left right uCompare sCompare fCompare
 | isDouble (t left) || isDouble (t right) = liftSMT $ do
     unless (t left == t right) $ error "Expected two doubles as argumnets to comparison"
     compare <- fCompare (n left) (n right)
     maybeDefinedNode left right compare Bool
 | isUnsignedInt (t left) || isUnsignedInt (t right) = liftSMT $ do
     compare <- uCompare (n left) (n right)
     maybeDefinedNode left right compare Bool
 | otherwise = liftSMT $ do
     compare <- sCompare (n left) (n right)
     maybeDefinedNode left right compare Bool

cppEq, cppGt, cppGte, cppLt, cppLte :: SMTNode -> SMTNode -> IR SMTNode
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
             -> IR SMTNode -- ^ Result
binOpWrapper left right op overflowOp opName = liftSMT $ do
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

cppOr, cppXor, cppAnd, cppSub, cppMul, cppAdd, cppMin, cppMax :: SMTNode -> SMTNode -> IR SMTNode

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
cppShiftLeft :: SMTNode -> SMTNode -> IR SMTNode
cppShiftLeft left right
  | not (int32 $ t left) || not (int32 $ t right) =
      error "Only support 32 bit SHL"
  | isUnsignedInt $ t left = liftSMT $ do
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
cppShiftRight :: SMTNode -> SMTNode -> IR SMTNode
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

cppCond :: SMTNode
        -> SMTNode
        -> SMTNode
        -> IR SMTNode
cppCond cond trueBr falseBr = liftSMT $ do
  unless (t cond == Bool) $ error "Conditional must be a boolean"
  unless (numBits (t trueBr) == numBits (t falseBr)) $
    error "Both branches of cond must have same width"
  result <- SMT.cond (n cond) (n trueBr) (n falseBr)
  undef <- SMT.or (u cond) (u trueBr) >>= SMT.or (u falseBr)
  return $ mkNode result (t trueBr) undef

cppCast :: SMTNode -> Type -> IR SMTNode
cppCast node toTy
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

-- | Make a new node that is defined if its parents are defined
maybeDefinedNode :: SMTNode -- ^ Parent 1
                 -> SMTNode -- ^ Parent 2
                 -> Node -- ^ Inner SMT node
                 -> Type -- ^ C++ type
                 -> SMT SMTNode -- ^ Resulting IR node
maybeDefinedNode parent1 parent2 node ty = do
  childUndef <- SMT.or (u parent1) (u parent2)
  return $ mkNode node ty childUndef

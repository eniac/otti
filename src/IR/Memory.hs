{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
module IR.Memory where
import           Control.Monad.State.Strict
import           Data.Maybe                    ( fromJust )
import qualified IR.TySmt                      as Ty
import           Targets.SMT.Assert            as Assert

bvNum :: Bool -> Int -> Integer -> Ty.TermDynBv
bvNum signed width val
  | signed && val >= 2 ^ (width - 1) = error
  $ unwords [show val, "does not fit in", show width, "signed bits"]
  | not signed && val >= 2 ^ width = error
  $ unwords [show val, "does not fit in", show width, "unsigned bits"]
  | otherwise = Ty.IntToDynBv width (Ty.IntLit val)

ones :: Int -> Ty.TermDynBv
ones width = bvNum False width ((2 :: Integer) ^ width - 1)

---
--- Loading, storing, getting, setting
---

loadBlock
  :: Ty.Term (Ty.ArraySort Ty.DynBvSort Ty.DynBvSort) -- ^ Array
  -> Ty.TermDynBv -- ^ Index
  -> Ty.TermDynBv
loadBlock = Ty.Select

storeBlock
  :: Ty.Term (Ty.ArraySort Ty.DynBvSort Ty.DynBvSort) -- ^ Array
  -> Ty.TermDynBv -- ^ Index
  -> Ty.TermDynBv -- ^ Value
  -> Ty.Term (Ty.ArraySort Ty.DynBvSort Ty.DynBvSort)
storeBlock = Ty.Store

-- | Cast a node to a new width
-- If the new width is larger, use unsigned extension
-- If the new width is smaller, slice
castToWidth
  :: Ty.TermDynBv -- ^ Node to be cast
  -> Int -- ^ New width
  -> Ty.TermDynBv -- ^ Result
castToWidth t newW =
  let oldW = Ty.dynBvWidth t
  in  case compare oldW newW of
        LT -> Ty.mkDynBvUext newW t
        GT -> Ty.mkDynBvExtract 0 newW t
        EQ -> t
-- Wat.
-- Wat.-- | Get a given number of bits from a structure starting from a given symbolic index. Little end
-- Wat.getBitsFromLE
-- Wat.  :: Ty.TermDynBv -- ^ In this structure
-- Wat.  -> Int -- ^ How large of a read
-- Wat.  -> Ty.TermDynBv -- ^ Starting from this index [0..n]
-- Wat.  -> Ty.TermDynBv
-- Wat.getBitsFromLE structure width index =
-- Wat.  let castIndex         = castToWidth index 64
-- Wat.      structureWidth    = Ty.dynBvWidth structure
-- Wat.      -- Easier to think about slicing indeices [n..0] so convert the index
-- Wat.      structureWidthSym = bvNum False 64 $ fromIntegral structureWidth
-- Wat.      subIndex          = Ty.mkDynBvBinExpr Ty.BvSub structureWidthSym castIndex
-- Wat.      finalIndex        = Ty.mkDynBvBinExpr Ty.BvSub subIndex (bvNum False 64 1)
-- Wat.      -- Shift structure so the start of the element is the high bit
-- Wat.      elemAtHigh        = Ty.mkDynBvBinExpr Ty.BvShl structure finalIndex
-- Wat.  in  Ty.mkDynBvExtract (structureWidth - width) width elemAtHigh

-- | Get a given number of bits from a structure starting from a given symbolic index. Big endian
getBits
  :: Ty.TermDynBv -- ^ In this structure
  -> Int -- ^ How many bits to read
  -> Ty.TermDynBv -- ^ Starting from this index [0..n]
  -> Ty.TermDynBv
getBits structure width index =
  let castIndex      = castToWidth index (Ty.dynBvWidth structure)
      -- Shift structure so LSB of read is at 0
      shiftedStruct  = Ty.mkDynBvBinExpr Ty.BvLshr structure castIndex
  in  Ty.mkDynBvExtract 0 width shiftedStruct

-- | Set a given number of bits in a structure starting from a given symbolic index
setBits
  :: Ty.TermDynBv -- ^ Set to this
  -> Ty.TermDynBv -- ^ In this structure
  -> Ty.TermDynBv -- ^ Starting from this index [0..n]
  -> Ty.TermDynBv -- ^ result
setBits element structure index =
  let castIndex       = castToWidth index 64
      -- Information we will need later
      structureWidth  = Ty.dynBvWidth structure
      elementWidth    = Ty.dynBvWidth element
      widthDifference = structureWidth - elementWidth
  in  if widthDifference == 0
      -- Setting every bit is just the same as returning the element
        then element
      -- Otherwise we have to change some bits while preserving others
        else
          let
     -- struct: 1001..01011...1011
     -- mask:   1111..00000...1111
     ----------------------------- AND
     -- res:    1001..00000...1011
     -- elem:   0000..10110...0000
     ----------------------------- OR
     -- final:  1001..10110...1011

       -- Consturct *mask*:
       -- (0) Make [1 repeat width(element)][0 repeat width(structure - element0]
              ones'        = ones elementWidth
              zeros        = bvNum False widthDifference 0
              preShiftMask = Ty.mkDynBvConcat ones' zeros
              -- (1) Right shift to start at the correct index
              preNegMask   = Ty.mkDynBvBinExpr Ty.BvLshr preShiftMask castIndex
              -- (2) Bitwise negate the whole thing
              mask         = Ty.mkDynBvUnExpr Ty.BvNot preNegMask

              -- And the struct with the mask
              res          = Ty.mkDynBvBinExpr Ty.BvAnd mask structure

              -- Construct the *padded elemnt*:
              -- (0) Make [element][0 repeat width(structure - element)]
              preShiftElem = Ty.mkDynBvConcat element zeros
              -- (2) Right shift to start at the correct index
              finalElem    = Ty.mkDynBvBinExpr Ty.BvLshr preShiftElem castIndex

       -- Or the two together!
          in  Ty.mkDynBvBinExpr Ty.BvOr finalElem res

data MemoryStrategy = Flat { blockSize :: Int } deriving (Eq,Ord,Show)

type MemSort = Ty.ArraySort Ty.DynBvSort Ty.DynBvSort
type TermMem = Ty.Term MemSort

-- | State for keeping track of Mem-layer information
data MemState = MemState { pointerSize    :: Int
                         , memoryStrategy :: MemoryStrategy
                         , memorySort     :: Maybe Ty.Sort -- ^ Redundant, but saves having to re-calc
                         , memories       :: [TermMem]
                         }

newtype Mem a = Mem (StateT MemState Assert.Assert a)
    deriving (Functor, Applicative, Monad, MonadState MemState, MonadIO)

emptyMem :: MemState
emptyMem = MemState 32 (Flat 32) Nothing []

runMem :: Mem a -> Assert.Assert (a, MemState)
runMem (Mem act) = runStateT act emptyMem

evalMem :: Mem a -> Assert.Assert a
evalMem act = fst <$> runMem act

execMem :: Mem a -> Assert.Assert MemState
execMem act = snd <$> runMem act

liftAssert :: Assert.Assert a -> Mem a
liftAssert = Mem . lift


---
--- Getters and setters
---

setPointerSize :: Int -> Mem ()
setPointerSize size = modify $ \s -> s { pointerSize = size }

setMemoryStrategy :: MemoryStrategy -> Mem ()
setMemoryStrategy strategy = modify $ \s -> s { memoryStrategy = strategy }

initMem :: Mem ()
initMem = do
  strat <- gets memoryStrategy
  psize <- gets pointerSize
  s0 <- get
  case strat of
    Flat blockSize' -> do
      let dSort = Ty.SortBv psize
          rSort = Ty.SortBv blockSize'
          memSort = Ty.SortArray dSort rSort
      firstMem <- liftAssert $ newVar @MemSort "global__mem" memSort
      put $ s0 { memorySort = Just memSort
               , memories = [firstMem]
               }

currentMem :: Mem TermMem
currentMem = gets (head . memories)

nextMem :: Mem TermMem
nextMem = do
  s0 <- get
  let curMems = memories s0
      memSort = fromJust $ memorySort s0
  newMem <- liftAssert $ newVar @MemSort ("global__mem_" ++ show (length curMems)) memSort
  put $ s0 { memories = newMem:curMems }
  return newMem

intCeil :: Int -> Int -> Int
intCeil x y = 1 + ((x - 1) `div` y)

memLoad :: Ty.TermDynBv -- ^ low idx
        -> Int          -- ^ # of bits
        -> Mem Ty.TermDynBv
memLoad addr nbits = do
  pointerSize <- gets pointerSize
  unless (pointerSize == Ty.dynBvWidth addr) $ error "Pointer size mismatch"
  memStrat <- gets memoryStrategy
  case memStrat of
    Flat blockSize -> do
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

      -- We must read at least ceiling( nbits / blockSize ) bits
      -- We may read one more, because of alignment
      let blocksToRead = intCeil nbits blockSize + 1
      -- TODO: If an addresss refers to at most B bits, we could shrink our ra

      when ((blocksToRead :: Int) > 2000) $ error "Load is too large"

      -- Read all the blocks and then smoosh them together into one bv
      mem <- currentMem
      let chunk = foldl1 Ty.mkDynBvConcat $ map
            (\blockIdx -> loadBlock mem $ Ty.mkDynBvBinExpr Ty.BvAdd (bvNum False pointerSize $ fromIntegral blockIdx) addr)
            [0..blocksToRead - 1]
          startInChunk = Ty.mkDynBvBinExpr Ty.BvUrem addr $ bvNum False pointerSize $ fromIntegral blockSize
      return $ getBits chunk nbits startInChunk

memStore :: Ty.TermDynBv -- ^ addr
         -> Ty.TermDynBv -- ^ value
         -> Maybe Ty.TermBool -- ^ guard
         -> Mem ()
memStore addr val mGuard = do
  pointerSize <- gets pointerSize
  unless (pointerSize == Ty.dynBvWidth addr) $ error "Pointer size mismatch"
  memStrat <- gets memoryStrategy
  case memStrat of
    Flat blockSize -> do
      -- Figure out how many blocks to read (see above)
      let nbits = Ty.dynBvWidth val
          blocksToRead = intCeil nbits blockSize + 1
      -- TODO: If an addresss refers to at most B bits, we could shrink our ra

      when (blocksToRead > 2000) $ error "Load is too large"

      -- Read all the blocks and then smoosh them together into one bv
      mem <- currentMem
      let chunk = foldl1 Ty.mkDynBvConcat $ map
            (\blockIdx -> loadBlock mem $ Ty.mkDynBvBinExpr Ty.BvAdd (bvNum False pointerSize $ fromIntegral blockIdx) addr)
            [0..blocksToRead - 1]
          blockSizeSym = bvNum False pointerSize $ fromIntegral blockSize
          startInChunk = Ty.mkDynBvBinExpr Ty.BvUrem addr blockSizeSym
          newChunk = setBits val chunk startInChunk
          guardedNewChunk = case mGuard of
            Nothing -> newChunk
            Just g  -> Ty.Ite g newChunk chunk
          writeBlock :: TermMem -> Int -> TermMem
          writeBlock m blockIdxInChunk = 
            let blockIdxInChunkSym = bvNum False pointerSize $ fromIntegral blockIdxInChunk
                blockVal = Ty.mkDynBvExtract (blockIdxInChunk * blockSize) blockSize guardedNewChunk
                blockIdx = Ty.mkDynBvBinExpr Ty.BvAdd addr blockIdxInChunkSym
            in storeBlock m blockIdx blockVal
      next <- nextMem
      liftAssert $ Assert.assign next $ foldl writeBlock mem [0..blocksToRead - 1]

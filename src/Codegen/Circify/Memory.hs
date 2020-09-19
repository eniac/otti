{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Codegen.Circify.Memory where
import           Control.Monad.State.Strict
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe )
import qualified Data.BitVector                as Bv
import qualified IR.SMT.TySmt                  as Ty
import           IR.SMT.Assert                 as Assert
import           Util.Log

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

-- | Get a given number of bits from a structure starting from a given symbolic index. Big endian
getBits
  :: Ty.TermDynBv -- ^ In this structure
  -> Int -- ^ How many bits to read
  -> Ty.TermDynBv -- ^ Starting from this index [0..n]
  -> Ty.TermDynBv
getBits structure width index =
  let castIndex     = castToWidth index (Ty.dynBvWidth structure)
      -- Shift structure so LSB of read is at 0
      shiftedStruct = Ty.mkDynBvBinExpr Ty.BvLshr structure castIndex
  in  if width <= Ty.dynBvWidth structure
        then Ty.mkDynBvExtract 0 width shiftedStruct
        else
          error
          $  "In getBits, the source has width "
          ++ show (Ty.dynBvWidth structure)
          ++ " but the load has width "
          ++ show width

-- | Set a given number of bits in a structure starting from a given symbolic index
setBits
  :: Ty.TermDynBv -- ^ Set to this
  -> Ty.TermDynBv -- ^ In this structure
  -> Ty.TermDynBv -- ^ Starting from this index [0..n]
  -> Ty.TermDynBv -- ^ result
setBits element structure index =
  let castIndex       = castToWidth index (Ty.dynBvWidth structure)
      -- Information we will need later
      structureWidth  = Ty.dynBvWidth structure
      elementWidth    = Ty.dynBvWidth element
      widthDifference = structureWidth - elementWidth
  in  case widthDifference of
      -- Setting every bit is just the same as returning the element
        0 -> element
      -- Otherwise we have to change some bits while preserving others
        _ | widthDifference > 0 ->
          let
     -- struct: 1001..01011...1011
     -- mask:   1111..00000...1111
     ----------------------------- AND
     -- res:    1001..00000...1011
     -- elem:   0000..10110...0000
     ----------------------------- OR
     -- final:  1001..10110...1011

       -- Consturct *mask*:
              -- (0) Make [0 repeat width(structure - element)][1 repeat width(element)]
              ones'        = ones elementWidth
              zeros        = bvNum False widthDifference 0
              preShiftMask = Ty.mkDynBvConcat zeros ones'
              -- (1) Left shift to start at the correct index
              elemMask     = Ty.mkDynBvBinExpr Ty.BvShl preShiftMask castIndex
              -- (2) Bitwise negate the whole thing
              structMask   = Ty.mkDynBvUnExpr Ty.BvNot elemMask

              -- And the struct with the mask
              res          = Ty.mkDynBvBinExpr Ty.BvAnd structMask structure

              -- Construct the *padded elemnt*:
              -- (0) Make [element][0 repeat width(structure - element)]
              preShiftElem = Ty.mkDynBvConcat zeros element
              -- (2) Right shift to start at the correct index
              shiftedElem  = Ty.mkDynBvBinExpr Ty.BvShl preShiftElem castIndex

       -- Or the two together!
          in  Ty.mkDynBvBinExpr Ty.BvOr shiftedElem res
        _ ->
          error
            $  "In setBits, the destination has width "
            ++ show structureWidth
            ++ " but the set value has width "
            ++ show elementWidth

type MemSort = Ty.ArraySort Ty.DynBvSort Ty.DynBvSort
type TermMem = Ty.Term MemSort

data StackAlloc = StackAlloc { idxWidth :: Int
                             , valWidth :: Int
                             , array    :: TermMem
                             , size     :: Int
                             } deriving (Show)
type StackAllocId = Int

-- Return the index and value widths
termMemWidths :: TermMem -> (Int, Int)
termMemWidths t = case Ty.sort t of
  Ty.SortArray (Ty.SortBv i) (Ty.SortBv v) -> (i, v)
  s -> error $ "Bad memory term sort " ++ show s

-- | State for keeping track of Mem-layer information
data MemState = MemState { stackAllocations :: Map.Map StackAllocId StackAlloc
                         , nextStackId :: StackAllocId
                         }

instance Show MemState where
  show s =
    unlines
      $  [ "MemState:"
         , unwords ["  Next stack allocation id:", show $ nextStackId s]
         , "  Stack allocations:"
         ]
      ++ if Map.null (stackAllocations s)
           then ["  (empty)"]
           else map (\(k, v) -> "  - " ++ show k ++ " -> " ++ show v)
                    (Map.toAscList $ stackAllocations s)

newtype Mem a = Mem (StateT MemState Assert.Assert a)
    deriving (Functor, Applicative, Monad, MonadState MemState, MonadIO, MonadLog, Assert.MonadAssert)

class Monad m => MonadMem m where
  liftMem :: Mem a -> m a
instance MonadMem Mem where
  liftMem = id
instance (MonadMem m) => MonadMem (StateT s m) where
  liftMem = lift . liftMem

emptyMem :: MemState
emptyMem = MemState Map.empty 0

runMem :: Mem a -> Assert.Assert (a, MemState)
runMem (Mem act) = runStateT act emptyMem

evalMem :: Mem a -> Assert.Assert a
evalMem act = fst <$> runMem act

execMem :: Mem a -> Assert.Assert MemState
execMem act = snd <$> runMem act

---
--- Getters and setters
---

takeNextStackId :: Mem StackAllocId
takeNextStackId = do
  i <- gets nextStackId
  modify $ \s -> s { nextStackId = i + 1 }
  return i

intCeil :: Int -> Int -> Int
intCeil x y = 1 + ((x - 1) `div` y)

stackAlloc :: TermMem -> Int -> Int -> Int -> Mem StackAllocId
stackAlloc array' size' idxWidth' valWidth' = do
  i <- takeNextStackId
  let a = StackAlloc { idxWidth = idxWidth'
                     , valWidth = valWidth'
                     , array    = array'
                     , size     = size'
                     }
  modify $ \s -> s { stackAllocations = Map.insert i a $ stackAllocations s }
  return i

stackAllocCons :: Int -> [Ty.TermDynBv] -> Mem StackAllocId
stackAllocCons idxWidth' elems =
  let s         = length elems
      valWidth' = Ty.dynBvWidth $ head elems
      m =
          foldl
              (\a (i, e) -> if Ty.dynBvWidth e == valWidth'
                then Ty.Store a (Ty.DynBvLit $ Bv.bitVec idxWidth' i) e
                else error $ "Bad size: " ++ show e
              )
              (Ty.ConstArray (Ty.SortBv idxWidth') (bvNum False valWidth' 0))
            $ zip [(0 :: Integer) ..] elems
  in  stackAlloc m s idxWidth' valWidth'

stackNewAlloc
  :: Int -- ^ size
  -> Int -- ^ idx bits
  -> Int -- ^ value bits
  -> Mem StackAllocId -- ^ id of allocation
stackNewAlloc size' idxWidth' valWidth' = stackAlloc
  (Ty.ConstArray (Ty.SortBv idxWidth') (bvNum False valWidth' 0))
  size'
  idxWidth'
  valWidth'

stackGetAlloc :: StackAllocId -> Mem StackAlloc
stackGetAlloc id = do
  mAlloc <- Map.lookup id <$> gets stackAllocations
  return $ fromMaybe (error $ "No stack allocation id: " ++ show id) mAlloc

stackLoad
  :: StackAllocId -- ^ Allocation to load from
  -> Ty.TermDynBv -- ^ offset
  -> Mem Ty.TermDynBv
stackLoad id offset = do
  alloc <- stackGetAlloc id
  -- TODO: Enforce bound?
  unless (idxWidth alloc == Ty.dynBvWidth offset)
    $  error
    $  "Bad index size: "
    ++ show offset
  return $ Ty.Select (array alloc) offset

stackStore
  :: StackAllocId -- ^ Allocation to load from
  -> Ty.TermDynBv -- ^ offset
  -> Ty.TermDynBv -- ^ value
  -> Ty.TermBool  -- ^ guard
  -> Mem ()
stackStore id offset value guard = do
  alloc <- stackGetAlloc id
  -- TODO: Enforce bound?
  unless (idxWidth alloc == Ty.dynBvWidth offset)
    $  error
    $  "Bad index size: "
    ++ show offset
  unless (valWidth alloc == Ty.dynBvWidth value)
    $  error
    $  "Bad value size: "
    ++ show value
  let a      = array alloc
      alloc' = alloc { array = Ty.Ite guard (Ty.Store a offset value) a }
  modify
    $ \s -> s { stackAllocations = Map.insert id alloc' $ stackAllocations s }

stackIsLoadable :: StackAllocId -> Ty.TermDynBv -> Mem Ty.TermBool
stackIsLoadable id offset = do
  alloc <- stackGetAlloc id
  let s = Bv.bitVec (Ty.dynBvWidth offset) (size alloc)
  return $ Ty.mkDynBvBinPred Ty.BvUlt offset (Ty.DynBvLit s)

stackIdUnknown :: StackAllocId
stackIdUnknown = maxBound
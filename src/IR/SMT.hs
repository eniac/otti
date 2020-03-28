{-# LANGUAGE MultiParamTypeClasses #-}
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
              , cppType
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
              , cppDiv
              , cppRem
              , cppShiftLeft
              , cppShiftRight
              , cppCond
              , cppCast
              ) where
import           AST.Simple
import           Control.Monad
import           Control.Monad.State.Strict
import qualified Data.Map                   as M
import           IR.IR                      hiding (smtAssign)
import           IR.SMTIRMonad
import           Targets.SMT                (Node, SMT, SMTResult)
import qualified Targets.SMT                as SMT
import           Z3.Monad                   as Z

{-|

SMT IR functions. These give the codegen phase a low-level language to translate
AST nodes into

-}

data SMTNode = SMTNode { cNode  :: Node
                       , cType  :: Type
                       , cUndef :: Node
                       }
             deriving (Eq, Ord, Show)

instance IRNode SMTNode Type where
  n = cNode
  t = cType

u :: SMTNode -> Node
u = cUndef

mkNode :: Node -> Type -> Node -> SMTNode
mkNode = SMTNode

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
newInt ty val = do
  int <- irInt ty val
  undef <- liftSMT $ SMT.bvNum 1 0
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

smtAssign :: SMTNode -> SMTNode -> IR ()
smtAssign n1 n2 = do
  -- handle implicit casts
  let bits1 = numBits $ t n1
      bits2 = numBits $ t n2
  n1' <- if bits1 >= bits2 then return n1 else cppImplicitCast n1 $ t n2
  n2' <- if bits2 >= bits1 then return n2 else cppImplicitCast n2 $ t n1
  SMT.assign (n n1') (n n2')
  SMT.assign (u n1) (u n2)

smtTrue :: IR SMTNode
smtTrue = newInt Bool 1

smtFalse :: IR SMTNode
smtFalse = newInt Bool 0

--
-- Struct and array access
--

getIdx :: SMTNode -- ^ Array
       -> SMTNode -- ^ Index
       -> IR SMTNode -- ^ Element
getIdx arr idx = do
  result <- irGetIdx arr idx
  undef <- SMT.or (u arr) (u idx)
  return $ mkNode result (arrayBaseType $ t arr) undef

setIdx :: SMTNode -- ^ Array
       -> SMTNode -- ^ Index
       -> SMTNode -- ^ Element
       -> IR SMTNode -- ^ Result array
setIdx arr idx elem = do
  result <- irSetIdx arr idx elem
  undef <- SMT.or (u arr) (u idx) >>= SMT.or (u elem)
  return $ mkNode result (t arr) undef

getField :: SMTNode -- ^ Struct
         -> Int -- ^ Index
         -> IR SMTNode -- ^ Element
getField struct idx = do
  result <- irGetField struct idx
  let fieldTypes = structFieldTypes $ t struct
  return $ mkNode result (fieldTypes !! idx) (u struct)

-- | Set a field in a struct.
-- This does not use setBits from SMT because it is likely slower than array slices
setField :: SMTNode -- ^ Struct
         -> Int -- ^ Index
         -> SMTNode -- ^ Element
         -> IR SMTNode -- ^ Result struct
setField struct idx elem = do
  result <- irSetField struct idx elem
  undef <- SMT.or (u struct) (u elem)
  return $ mkNode result (t struct) undef

-- memory

smtLoad :: SMTNode
        -> IR SMTNode
smtLoad addr = do
  result <- irLoad addr
  let pointeeTy = pointeeType $ t addr
  return $ mkNode result pointeeTy (u addr)

smtStore :: SMTNode -> SMTNode -> SMTNode -> IR ()
smtStore addr val guard = irStore addr val $ Just guard


-- Types

cppType :: SMTNode -> Type
cppType = t

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
binOpWrapper left right op overflowOp opName = do
  -- no fp implicit casting for now
  when (isDouble $ t left) $ unless (isDouble $ t right) $
    error $ unwords ["Expected two doubles to", opName]
  -- handle implicit casts
  let leftBits  = numBits $ t left
      rightBits = numBits $ t right
  left'  <- if leftBits >= rightBits then return left else cppImplicitCast left $ t right
  right' <- if rightBits >= leftBits then return right else cppImplicitCast right $ t left
  parentsUndef <- liftSMT $ SMT.or (u left') (u right')
  canOverflow <- case overflowOp of
                   -- No overflow-checking op provided: there isn't the opertunity
                   -- to overflow/underflow and cause undefined behavior
                   Nothing  -> return parentsUndef
                   -- There is an overflow op, so if there's overflow or the parents are
                   -- undef, the child node is also undef
                   Just oop -> do
                     flow <- liftSMT $ oop (isSignedInt $ t left') (n left') (n right')
                     liftSMT $ SMT.or parentsUndef flow
  result <- liftSMT $ op (n left') (n right')
  let ty = if t left' == t right'
           then t left'
           else if isSignedInt $ t left' then t left' else t right'
  return $ mkNode result ty canOverflow

cppOr, cppXor, cppAnd, cppSub, cppMul, cppAdd, cppMin, cppMax, cppDiv, cppRem :: SMTNode -> SMTNode -> IR SMTNode

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

cppDiv right left
  | isDouble (t right) || isDouble (t left) = error "UNAVAILABLE"
  | isUnsignedInt (t right) && isUnsignedInt (t left) =
      binOpWrapper left right SMT.udiv Nothing "div"
  | isSignedInt (t right) || isSignedInt (t left) =
      binOpWrapper left right SMT.sdiv Nothing "div"

cppRem right left
  | isDouble (t right) || isDouble (t left) = error "UNAVAILABLE"
  | isUnsignedInt (t right) && isUnsignedInt (t left) =
      binOpWrapper left right SMT.urem Nothing "rem"
  | isSignedInt (t right) || isSignedInt (t left) =
      binOpWrapper left right SMT.srem Nothing "rem"

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
                       _          -> error $ unwords [ "Illegal cast types:"
                                                     , show toTy
                                                     , show fromTy
                                                     ]
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

cppImplicitCast :: SMTNode -> Type -> IR SMTNode
cppImplicitCast node toty = do
  let oldWidth = numBits $ t node
      newWidth = numBits toty
      extend   = if isSignedInt $ t node then SMT.sext else SMT.uext
  unless (isIntegerType $ t node) $ error "Don't support implicit casts from non-ints"
  when (oldWidth >= newWidth) $ error $ unwords ["Bad implicit cast:"
                                                , show oldWidth
                                                , show newWidth
                                                ]
  result <- extend (n node) (newWidth - oldWidth)
  let fixedType = makeType newWidth $ isSignedInt $ t node
  return $ mkNode result fixedType (u node)



module Targets.SMT.Z3Wrapper where
import           Control.Monad.State.Strict (liftIO, unless)
import qualified Data.Map.Strict            as M
import           Prelude                    hiding (not, or)
import           Z3.Monad                   (MonadZ3)
import qualified Z3.Monad                   as Z

type Sort = Z.Sort
type Node = Z.AST
type AST = Z.AST

solverToString :: MonadZ3 z3 => z3 String
solverToString = do
  Z.setASTPrintMode Z.Z3_PRINT_SMTLIB2_COMPLIANT
  Z.solverToString

push :: MonadZ3 z3 => z3 ()
push = Z.push

pop :: MonadZ3 z3 => z3 ()
pop = Z.pop 1

-- | Assert a statement. The statement must be a boolean type (for us, either
-- a bool sort or a bitvector sort of size one, because of funkiness within z3 bindings)
assert :: MonadZ3 z3 => AST -> z3 ()
assert stmt = do
  sort <- Z.getSort stmt
  kind <- Z.getSortKind sort
  stmt' <- case kind of
    Z.Z3_BOOL_SORT -> return stmt
    Z.Z3_BV_SORT -> do
      size <- Z.getBvSortSize sort
      unless (size == 1) $ error "Cannot assert non-bool statement"
      bvTrue <- Z.mkBvNum 1 (1 :: Integer)
      Z.mkEq stmt bvTrue
    s              -> error $ unwords ["Can't assert sort", show s]
  Z.assert stmt'

assign :: MonadZ3 z3 => AST -> AST -> z3 ()
assign n1 n2 = do
  sort1 <- Z.getSort n1
  sort2 <- Z.getSort n2
  unless (sort1 == sort2) $ error "Can't assign nodes of different sorts"
  size1 <- Z.getBvSortSize sort1
  size2 <- Z.getBvSortSize sort2
  unless (size1 == size2) $ error "Can't assign nodes of different widths"
  Z.mkEq n1 n2 >>= Z.assert

---
--- Numbers. Variables are in SMTMonad, since they change underlying state---but nums don't
---

bvNum :: MonadZ3 z3 => Int -> Integer -> z3 AST
bvNum width val = Z.mkBitvector width val

doubleNum :: MonadZ3 z3 => Double -> z3 AST
doubleNum doub = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpFromDouble doub doubSort

---
--- Sorts
---

bvSort :: MonadZ3 z3 => Int -> z3 Z.Sort
bvSort = Z.mkBvSort

doubSort :: MonadZ3 z3 => z3 Z.Sort
doubSort = Z.mkDoubleSort

-- | Get the bitvector sort of ast. If ast does not have bitvector sort, error
getBVSort :: MonadZ3 z3 => String -> AST -> z3 Z.Sort
getBVSort op ast = do
  sort <- Z.getSort ast
  kind <- Z.getSortKind sort
  case kind of
    Z.Z3_BV_SORT -> return sort
    s            -> error $ unwords ["Expected BV sort, not", show s, "in", op]

getBVSortSize op ast = do
  sort <- Z.getSort ast
  kind <- Z.getSortKind sort
  case kind of
    Z.Z3_BV_SORT -> Z.getBvSortSize sort
    s            -> error $ unwords ["Expected BV sort, not", show s, "in", op]

-- | Perform the given binary operation op in a type safe way
typeSafeBinary :: MonadZ3 z3 => String -> AST -> AST -> z3 ()
typeSafeBinary op ast1 ast2 = do
  s1 <- getBVSort op ast1
  s2 <- getBVSort op ast2
  size1 <- Z.getBvSortSize s1
  size2 <- Z.getBvSortSize s2
  unless (size1 == size2) $ error $ unwords [op, ": bit-widths must match"]

--mkTypeSafeBinary :: MonadZ3 z3 => String -> AST -> AST -> z3 ()
mkTypeSafeBinary op opName a b = do
  typeSafeBinary opName a b
  op a b

---
--- Operations
---

eq :: MonadZ3 z3 => AST -> AST -> z3 AST
eq = mkTypeSafeBinary Z.mkEq "eq"

add :: MonadZ3 z3 => AST -> AST -> z3 AST
add = mkTypeSafeBinary Z.mkBvadd "add"

sub :: MonadZ3 z3 => AST -> AST -> z3 AST
sub = mkTypeSafeBinary Z.mkBvsub "sub"

mul :: MonadZ3 z3 => AST -> AST -> z3 AST
mul = mkTypeSafeBinary Z.mkBvmul "mul"

sdiv :: MonadZ3 z3 => AST -> AST -> z3 AST
sdiv = mkTypeSafeBinary Z.mkBvsdiv "sdiv"

udiv :: MonadZ3 z3 => AST -> AST -> z3 AST
udiv = mkTypeSafeBinary Z.mkBvudiv "udiv"

mod :: MonadZ3 z3 => AST -> AST -> z3 AST
mod = mkTypeSafeBinary Z.mkBvsmod "mod"

srem :: MonadZ3 z3 => AST -> AST -> z3 AST
srem = mkTypeSafeBinary Z.mkBvsrem "srem"

urem :: MonadZ3 z3 => AST -> AST -> z3 AST
urem = mkTypeSafeBinary Z.mkBvurem "urem"

and :: MonadZ3 z3 => AST -> AST -> z3 AST
and = mkTypeSafeBinary Z.mkBvand "and"

or :: MonadZ3 z3 => AST -> AST -> z3 AST
or = mkTypeSafeBinary Z.mkBvor "or"

xor :: MonadZ3 z3 => AST -> AST -> z3 AST
xor = mkTypeSafeBinary Z.mkBvxor "xor"

not :: MonadZ3 z3 => AST -> z3 AST
not = Z.mkBvnot

neg :: MonadZ3 z3 => AST -> z3 AST
neg = Z.mkBvneg

sll :: MonadZ3 z3 => AST -> AST -> z3 AST
sll = mkTypeSafeBinary Z.mkBvshl "sll"

srl :: MonadZ3 z3 => AST -> AST -> z3 AST
srl = mkTypeSafeBinary Z.mkBvlshr "srl"

sra :: MonadZ3 z3 => AST -> AST -> z3 AST
sra = mkTypeSafeBinary Z.mkBvashr "sra"

smin :: MonadZ3 z3 => AST -> AST -> z3 AST
smin x y = do
  isLess <- slte x y
  cond isLess x y

smax :: MonadZ3 z3 => AST -> AST -> z3 AST
smax x y = do
  isMore <- sgte x y
  cond isMore x y

umin :: MonadZ3 z3 => AST -> AST -> z3 AST
umin x y = do
  isLess <- ulte x y
  cond isLess x y

umax :: MonadZ3 z3 => AST -> AST -> z3 AST
umax x y = do
  isMore <- ugte x y
  cond isMore x y

---
--- Comparisons
---

-- | Wrap a comparison operation.
-- Comparisons return booleans, but all of our operations are on bitvectors.
-- Therefore, we turn the boolean result into a one-bit bitvector result
cmpWrapper :: MonadZ3 z3 => AST -> z3 AST
cmpWrapper a = do
  true <- Z.mkBvNum 1 (1 :: Integer)
  false <- Z.mkBvNum 1 (0 :: Integer)
  Z.mkIte a true false

mkTypeSafeCmp op opName a b = do
  typeSafeBinary opName a b
  op a b >>= cmpWrapper

ugt :: MonadZ3 z3 => AST -> AST -> z3 AST
ugt = mkTypeSafeCmp Z.mkBvugt "ugt"

sgt :: MonadZ3 z3 => AST -> AST -> z3 AST
sgt = mkTypeSafeCmp Z.mkBvsgt "sgt"

ugte :: MonadZ3 z3 => AST -> AST -> z3 AST
ugte = mkTypeSafeCmp Z.mkBvuge "ugte"

sgte :: MonadZ3 z3 => AST -> AST -> z3 AST
sgte = mkTypeSafeCmp Z.mkBvsge "sgte"

ult :: MonadZ3 z3 => AST -> AST -> z3 AST
ult = mkTypeSafeCmp Z.mkBvult "ult"

slt :: MonadZ3 z3 => AST -> AST -> z3 AST
slt = mkTypeSafeCmp Z.mkBvslt "slt"

ulte :: MonadZ3 z3 => AST -> AST -> z3 AST
ulte = mkTypeSafeCmp Z.mkBvule "ulte"

slte :: MonadZ3 z3 => AST -> AST -> z3 AST
slte = mkTypeSafeCmp Z.mkBvsle "slte"

iseq :: MonadZ3 z3 => AST -> AST -> z3 AST
iseq = mkTypeSafeCmp Z.mkEq "iseq"

---
--- Other operations
---

cond :: MonadZ3 z3 => AST -> AST -> AST -> z3 AST
cond c a b = do
  -- Type check c
  size <- getBVSortSize "conditional" c
  unless (size == 1) $ error $ unwords ["Cannot condition on non-boolean variable"]
  -- Perform the computation
  bvTrue <- Z.mkBvNum 1 (1 :: Integer)
  isTrue <- Z.mkEq c bvTrue
  Z.mkIte isTrue a b

sext :: MonadZ3 z3 => AST -> Int -> z3 AST
sext a i = Z.mkSignExt i a

uext :: MonadZ3 z3 => AST -> Int -> z3 AST
uext a i = Z.mkZeroExt i a

slice :: MonadZ3 z3 => AST -> Int -> Int -> z3 AST
slice a i1 i2 = Z.mkExtract i1 i2 a

---
--- Shifts: Do we need these anymore or not? Get rid of if not
---

-- | Wrapper for boolector shift operations
shiftWrapper :: (MonadZ3 m)
             => (AST -> AST -> m AST) -- ^ Shift op
             -> AST -- ^ Thing to shift
             -> AST -- ^ Thing to shift by
             -> m AST -- ^ Result
shiftWrapper shiftOp toShift shiftVal = do
  toShiftSort <- Z.getSort toShift
  castVal <- Z.getBvSortSize toShiftSort >>= castToWidth shiftVal
  shiftOp toShift castVal

-- | Shift operations
safeSll, safeSrl, safeSra :: MonadZ3 m => AST -> AST -> m AST
safeSll = shiftWrapper sll
safeSrl = shiftWrapper srl
safeSra = shiftWrapper sra

---
--- Casting
---

-- | Cast a node to a new width
-- If the new width is larger, use unsigned extension
-- If the new width is smaller, slice
castToWidth :: (MonadZ3 m)
            => Node -- ^ Node to be cast
            -> Int -- ^ New width
            -> m Node -- ^ Result
castToWidth varToCast newWidth = do
  sort <- Z.getSort varToCast
  sortKind <- Z.getSortKind sort
  let isBv = sortKind == Z.Z3_BV_SORT
  unless isBv $ error $ "Should never cast non-bitvector sort (" ++ show sort ++ ")"
  oldWidth' <- Z.getBvSortSize sort
  let oldWidth = fromIntegral oldWidth'
  case compare oldWidth newWidth of
    LT -> uext varToCast (newWidth - oldWidth)
    GT -> slice varToCast (newWidth - 1) 0
    _  -> return varToCast

-- Floating point wrappers

-- double :: MonadZ3 z3 => Double -> z3 AST
-- double doub = do
--   doubSort <- Z.mkDoubleSort
--   Z.mkFpFromDouble doub doubSort

inf :: MonadZ3 z3 => Bool -> z3 AST
inf positive = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpInf doubSort positive

fpzero ::MonadZ3 z3 => Bool -> z3 AST
fpzero negative = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpZero doubSort negative

nan :: MonadZ3 z3 => z3 AST
nan = do
  doubSort <- Z.mkDoubleSort
  Z.mkFpNan doubSort

rna :: MonadZ3 z3 => z3 AST
rna = Z.mkFpRna

rne :: MonadZ3 z3 => z3 AST
rne = Z.mkFpRne

rtn :: MonadZ3 z3 => z3 AST
rtn = Z.mkFpRtn

rtp :: MonadZ3 z3 => z3 AST
rtp = Z.mkFpRtp

rtz :: MonadZ3 z3 => z3 AST
rtz = Z.mkFpRtz

rtntte :: MonadZ3 z3 => z3 AST
rtntte = Z.mkFpRoundToNearestTiesToEven

isInf :: MonadZ3 z3 => AST -> z3 AST
isInf a = Z.mkFpIsInf a >>= cmpWrapper

isNan :: MonadZ3 z3 => AST -> z3 AST
isNan a = Z.mkFpIsNan a >>= cmpWrapper

isNeg :: MonadZ3 z3 => AST -> z3 AST
isNeg a = Z.mkFpIsNeg a >>= cmpWrapper

isPos :: MonadZ3 z3 => AST -> z3 AST
isPos a = Z.mkFpIsPos a >>= cmpWrapper

isZero :: MonadZ3 z3 => AST -> z3 AST
isZero a = Z.mkFpIsZero a >>= cmpWrapper

rmWrapper :: MonadZ3 z3
          => (AST -> AST -> AST -> z3 AST)
          -> AST
          -> AST
          -> z3 AST
rmWrapper op a b = do
  -- In the future we will get the current rounding mode from the monad
  rm <- rtntte
  op rm a b

fpAbs :: MonadZ3 z3 => AST -> z3 AST
fpAbs = Z.mkFpAbs

fpAdd :: MonadZ3 z3 => AST -> AST -> z3 AST
fpAdd = rmWrapper Z.mkFpAdd

fpSub :: MonadZ3 z3 => AST -> AST -> z3 AST
fpSub = rmWrapper Z.mkFpSub

fpDiv :: MonadZ3 z3 => AST -> AST -> z3 AST
fpDiv = rmWrapper Z.mkFpDiv

fpMul :: MonadZ3 z3 => AST -> AST -> z3 AST
fpMul = rmWrapper Z.mkFpMul

fpRem :: MonadZ3 z3 => AST -> AST -> z3 AST
fpRem = Z.mkFpRem

fpNeg :: MonadZ3 z3 => AST -> z3 AST
fpNeg = Z.mkFpNeg

fpEq :: MonadZ3 z3 => AST -> AST -> z3 AST
fpEq a b = Z.mkFpEq a b >>= cmpWrapper

fpGte :: MonadZ3 z3 => AST -> AST -> z3 AST
fpGte a b = Z.mkFpGeq a b >>= cmpWrapper

fpGt :: MonadZ3 z3 => AST -> AST -> z3 AST
fpGt a b = Z.mkFpGt a b >>= cmpWrapper

fpLte :: MonadZ3 z3 => AST -> AST -> z3 AST
fpLte a b = Z.mkFpLeq a b >>= cmpWrapper

fpLt :: MonadZ3 z3 => AST -> AST -> z3 AST
fpLt a b = Z.mkFpLt a b >>= cmpWrapper

fpMin :: MonadZ3 z3 => AST -> AST -> z3 AST
fpMin = Z.mkFpMin

fpMax :: MonadZ3 z3 => AST -> AST -> z3 AST
fpMax = Z.mkFpMax

fpFloor :: MonadZ3 z3 => AST -> z3 AST
fpFloor toRound = do
  rm <- rtn
  Z.mkFpRound rm toRound

fpCeil :: MonadZ3 z3 => AST -> z3 AST
fpCeil toRound = do
  rm <- rtp
  Z.mkFpRound rm toRound

castSBv :: MonadZ3 z3 => AST -> z3 AST
castSBv bv = do
  rm <- rtntte
  doubSort <- Z.mkDoubleSort
  Z.mkSBvToFp rm bv doubSort

castUBv :: MonadZ3 z3 => AST -> z3 AST
castUBv bv = do
  rm <- rtntte
  doubSort <- Z.mkDoubleSort
  Z.mkUBvToFp rm bv doubSort

castFp :: MonadZ3 z3 => AST -> Int -> z3 AST
castFp fp sz = do
  rm <- rtntte
  Z.mkFpToBv rm fp $ fromIntegral sz

bvSize :: MonadZ3 z3 => AST -> z3 Int
bvSize bv = do
  sort <- Z.getSort bv
  Z.getBvSortSize sort

ieeeBv :: MonadZ3 z3 => AST -> z3 AST
ieeeBv = Z.mkFpIEEEBv

addOverflows :: MonadZ3 z3 => AST -> AST -> Bool -> z3 AST
addOverflows a b s = Z.mkBvaddNoOverflow a b s >>= cmpWrapper >>= not

addUnderflows :: MonadZ3 z3 => AST -> AST -> z3 AST
addUnderflows a b = Z.mkBvaddNoUnderflow a b >>= cmpWrapper >>= not

addUndef :: MonadZ3 z3 => Bool -> AST -> AST -> z3 AST
addUndef signed a1 a2 = do
  overflows <- addOverflows a1 a2 signed
  underflows <- addUnderflows a1 a2
  or overflows underflows

mulOverflows :: MonadZ3 z3 => AST -> AST -> Bool -> z3 AST
mulOverflows a b s = Z.mkBvmulNoOverflow a b s >>= cmpWrapper >>= not

mulUnderflows :: MonadZ3 z3 => AST -> AST -> z3 AST
mulUnderflows a b = Z.mkBvmulNoUnderflow a b >>= cmpWrapper >>= not

mulUndef :: MonadZ3 z3 => Bool -> AST -> AST -> z3 AST
mulUndef signed a1 a2 = do
  overflows <- mulOverflows a1 a2 signed
  underflows <- mulUnderflows a1 a2
  or overflows underflows

subOverflows :: MonadZ3 z3 => AST -> AST -> z3 AST
subOverflows a b = Z.mkBvsubNoOverflow a b >>= cmpWrapper >>= not

subUnderflows :: MonadZ3 z3 => AST -> AST -> z3 AST
subUnderflows a b = Z.mkBvsubNoUnderflow a b >>= cmpWrapper >>= not

subUndef :: MonadZ3 z3 => Bool -> AST -> AST -> z3 AST
subUndef _ a1 a2 = do
  overflows <- subOverflows a1 a2
  underflows <- subUnderflows a1 a2
  or overflows underflows

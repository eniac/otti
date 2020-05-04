{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module IR.CUtils
  ( CTerm(..)
  , CTermData(..)
  , Bitable(..)
  , newVar
  , cppLoad
  , cppStore
  )
where

import qualified IR.TySmt                      as Ty
import qualified AST.Simple                    as AST
import qualified Targets.SMT.Assert            as Assert
import           Targets.SMT.Assert             ( Assert )
import qualified IR.Memory                     as Mem
import           IR.Memory                      ( Mem )
import           Data.BitVector                as BitVector
import           Data.Foldable                 as Fold

-- data CInt = CInt { _cintSigned :: Bool
--                  , _cintBits :: Int
--                  , _cintSmt :: Ty.TermDynBv
--                  }

type Bv = Ty.TermDynBv

class Bitable s where
  nbits :: s -> Int
  serialize :: s -> Bv
  deserialize :: AST.Type -> Bv -> s


data CTermData = CInt Bool Int Bv
               | CBool Ty.TermBool
               | CDouble Ty.TermDouble
               | CPtr AST.Type Bv
               deriving (Show)

asDouble :: CTermData -> Ty.TermDouble
asDouble (CDouble d) = d
asDouble t           = error $ unwords [show t, "is not a double"]

asInt :: CTermData -> (Bool, Int, Bv)
asInt (CInt s w i) = (s, w, i)
asInt t            = error $ unwords [show t, "is not an integer"]

data CTerm = CTerm { term :: CTermData
                   , udef :: Ty.TermBool
                   }
                   deriving (Show)

instance Bitable CTermData where

assign :: CTerm -> CTerm -> Assert ()
assign a b = undefined


nyi :: String -> a
nyi msg = error $ "Not yet implemented: " ++ msg

udefName :: String -> String
udefName s = s ++ "_undef"

newVar :: AST.Type -> String -> Assert CTerm
newVar ty name = do
  u <- Assert.newVar (udefName name) Ty.SortBool
  t <- case ty of
    AST.Bool -> CBool <$> Assert.newVar name Ty.SortBool
    _ | AST.isIntegerType ty ->
      CInt (AST.isSignedInt ty) (AST.numBits ty)
        <$> Assert.newVar name (Ty.SortBv $ AST.numBits ty)
    AST.Double    -> CDouble <$> Assert.newVar name Ty.sortDouble
    AST.Ptr32 iTy -> CPtr ty <$> Assert.newVar name (Ty.SortBv 32)
    _             -> nyi $ "newVar for type " ++ show ty
  return $ CTerm t u

-- TODO: shadow memory
cppLoad :: AST.Type -> Bv -> Ty.TermBool -> Mem CTerm
cppLoad ty addr udef' =
  flip CTerm udef' . deserialize ty <$> Mem.memLoad addr (AST.numBits ty)

-- TODO: shadow memory
cppStore :: Bv -> CTerm -> Ty.TermBool -> Mem ()
cppStore addr val guard = Mem.memStore addr (serialize $ term val) (Just guard)

-- intOp :: (Bv -> Bv -> Bv) -> 

boolToBv :: Ty.TermBool -> Int -> Bv
boolToBv b w = Ty.Ite b (Mem.bvNum False w 1) (Mem.bvNum False w 0)

intResize :: Bool -> Int -> Bv -> Bv
intResize fromSign toWidth from =
  let fromWidth = Ty.dynBvWidth from
  in  case compare fromWidth fromWidth of
        LT ->
          (if fromSign then Ty.mkDynBvSext else Ty.mkDynBvUext) toWidth from
        EQ -> from
        GT -> Ty.mkDynBvExtract 0 toWidth from

-- | C++ unary negation---meaning 5 becomes -5. This is not a bitwise negation
cppNeg :: CTerm -> CTerm
cppNeg node =
  let t' = case term node of
        CDouble d  -> CDouble (Ty.FpUnExpr Ty.FpNeg d)
        -- bools cast to 32b under arith.
        CBool   _  -> term $ cppNeg $ cppCast node AST.S32
        CInt s w t -> CInt s w (Ty.mkDynBvUnExpr Ty.BvNeg t)
        CPtr ty t  -> CPtr ty (Ty.mkDynBvUnExpr Ty.BvNeg t)
  in  CTerm t' (udef node)

-- Returns the address
cppPtrPlusInt :: AST.Type -> Bv -> Bool -> Bv -> Bv
cppPtrPlusInt pTy ptr signed int =
  Ty.mkDynBvBinExpr Ty.BvAdd ptr
    $ intResize signed (AST.numBits pTy)
    $ Ty.mkDynBvBinExpr
        Ty.BvMul
        (Mem.bvNum False
                   (Ty.dynBvWidth int)
                   (fromIntegral $ AST.numBits $ AST.pointeeType pTy)
        )
        int


-- This is not quite right, but it's a reasonable approximation of C++ arithmetic.
-- For an expression l (+) r, 
-- 1. Both l and r undergo **integral promotion** (bool -> int)
-- 2. If either are floating, the other is cast to that floating type
--    * Ptrs not castable.
-- 3. If pointers are allowed:
--    * If there is a pointer, scale the int and do the op
--    * ow error
-- 4. Scale the int, do the op.
cppWrapBinArith
  :: String
  -> (Bv -> Bv -> Bv)
  -> (Ty.TermDouble -> Ty.TermDouble -> Ty.TermDouble)
  -- Undef function, takes sign and Bv term for each argument
  -> Maybe (Bool -> Bv -> Bv -> Maybe Ty.TermBool)
  -> Bool
  -> Bool
  -> Bool
  -> CTerm
  -> CTerm
  -> CTerm
cppWrapBinArith name bvF doubleF ubF allowPleft allowPright allowDouble a b = convert
  (integralPromotion a)
  (integralPromotion b)
 where
  convert a b =
    let
      cannot with = error $ unwords ["Cannot do", name, "with", with]
      (t, u) = case (term a, term b) of
        (CDouble d, _) ->
          if allowDouble
          then (CDouble $ doubleF d $ asDouble $ term $ cppCast b AST.Double, Nothing)
          else cannot "a double"
        (_, CDouble d) ->
          if allowDouble
          then (CDouble $ doubleF d $ asDouble $ term $ cppCast b AST.Double, Nothing)
          else cannot "a double"
        (CPtr ty addr, CInt s _ i) -> if allowPleft
          then (CPtr ty $ cppPtrPlusInt ty addr s i, Nothing)
          else cannot "a pointer on the left"
        (CInt s _ i, CPtr ty addr) -> if allowPright
          then (CPtr ty $ cppPtrPlusInt ty addr s i, Nothing)
          else cannot "a pointer on the right"
        (CInt s w i, CInt s' w' i') ->
          let width = max w w'
              sign  = max s s'
          in  (CInt sign width
                $ bvF (intResize s width i) (intResize s' width i')
                , ubF >>= (\f -> f (s && s') i i'))
        (_, _) -> cannot $ unwords [show a, "and", show b]
      pUdef = Ty.BoolNaryExpr Ty.Or (udef a : udef b : Fold.toList u)
    in  CTerm t pUdef

cppBitOr, cppBitXor, cppBitAnd, cppSub, cppMul, cppAdd, cppMin, cppMax, cppDiv, cppRem :: CTerm -> CTerm -> CTerm
cppAdd = cppWrapBinArith "+" (Ty.mkDynBvBinExpr Ty.BvAdd) (Ty.FpBinExpr Ty.FpAdd) Nothing True True True
cppSub = cppWrapBinArith "-" (Ty.mkDynBvBinExpr Ty.BvSub) (Ty.FpBinExpr Ty.FpSub) Nothing True False True
cppMul = cppWrapBinArith "*" (Ty.mkDynBvBinExpr Ty.BvMul) (Ty.FpBinExpr Ty.FpMul) Nothing False False True
cppDiv = cppWrapBinArith "/" (Ty.mkDynBvBinExpr Ty.BvUdiv) (Ty.FpBinExpr Ty.FpDiv) Nothing False False True
-- CPP reference says that % requires integral arguments
cppRem = cppWrapBinArith "%" (Ty.mkDynBvBinExpr Ty.BvUrem) (Ty.FpBinExpr Ty.FpRem) Nothing False False False
cppMin = undefined
cppMax = undefined
cppBitOr = cppWrapBinArith "|" (Ty.mkDynBvBinExpr Ty.BvOr) (const $ const $ error "no fp |") Nothing False False False
cppBitAnd = cppWrapBinArith "&" (Ty.mkDynBvBinExpr Ty.BvAnd) (const $ const $ error "no fp &") Nothing False False False
cppBitXor = cppWrapBinArith "^" (Ty.mkDynBvBinExpr Ty.BvXor) (const $ const $ error "no fp ^") Nothing False False False

-- Promote integral types
-- Do not mess with pointers
integralPromotion :: CTerm -> CTerm
integralPromotion n = case term n of
  CBool{} -> cppCast n AST.S32
  _       -> n

cppCast :: CTerm -> AST.Type -> CTerm
cppCast node toTy = case term node of
  CBool t -> if AST.isIntegerType toTy
    then
      let width = AST.numBits toTy
          cint  = CInt (AST.isSignedInt toTy) width (boolToBv t width)
      in  CTerm cint (udef node)
    else if AST.isPointer toTy
      then
        let width = AST.numBits toTy
            cptr  = CPtr (AST.pointeeType toTy) (boolToBv t width)
        in  CTerm cptr (udef node)
      else if AST.isDouble toTy
        then CTerm (CDouble $ (Ty.DynUbvToFp (boolToBv t 1))) (udef node)
        else if toTy == AST.Bool
          then node
          else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CInt fromS fromW t -> if AST.isIntegerType toTy
    then
      let toW = AST.numBits toTy
          toS = AST.isSignedInt toTy
          t'  = intResize fromS toW t
      in  CTerm (CInt toS toW t') (udef node)
    else if AST.isPointer toTy
      then CTerm (CPtr (AST.pointeeType toTy) (intResize fromS 32 t))
                 (udef node)
      else if AST.isDouble toTy
        then CTerm
          (CDouble $ (if fromS then Ty.DynSbvToFp else Ty.DynUbvToFp) t)
          (udef node)
        else if toTy == AST.Bool
          then CTerm (CBool $ Ty.Not $ Ty.Eq (Mem.bvNum False fromW 0) t)
                     (udef node)
          else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CDouble t -> if AST.isIntegerType toTy
    then CTerm
      ( CInt (AST.isSignedInt toTy) (AST.numBits toTy)
      $ Ty.RoundFpToDynBv (AST.numBits toTy) True t
      )
      (udef node)
    else if toTy == AST.Bool
      then CTerm (CBool $ Ty.FpBinPred Ty.FpEq (Ty.Fp64Lit 0.0) t) (udef node)
      else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CPtr _ t -> if AST.isIntegerType toTy
    then cppCast (CTerm (CInt False (Ty.dynBvWidth t) t) (udef node)) toTy
    else if AST.isPointer toTy
      then node
      else error $ unwords ["Bad cast from", show t, "to", show toTy]

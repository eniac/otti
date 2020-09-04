{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE DataKinds #-}

module Codegen.C.CUtils
  ( CTerm(..)
  , CTermData(..)
  , Bitable(..)
  , cppDeclVar
  , cppDeclInitVar
  -- Memory Operations
  , cppLoad
  , cppStore
  -- Arith Operations
  , cppBitOr
  , cppBitXor
  , cppBitNot
  , cppBitAnd
  , cppSub
  , cppMul
  , cppAdd
  , cppMin
  , cppMax
  , cppDiv
  , cppRem
  , cppPos
  , cppNeg
  , cppShl
  , cppShr
  -- Logical Operations
  , cppAnd
  , cppOr
  , cppNot
  -- Comparisons
  , cppLt
  , cppLe
  , cppGt
  , cppGe
  , cppEq
  , cppNe
  -- Ites
  , cppCond
  , cppIte
  -- Other
  , cppCast
  , cppBool
  -- Consts
  , cppTrue
  , cppFalse
  -- Literals
  , cppIntLit
  , cppFloatLit
  , cppDoubleLit
  , cppArrayLit
  , cppStructLit
  -- Structs
  , cppStructGet
  , cppStructSet
  -- Pointers & Arrays
  , cppIndex
  -- Reflection
  , cppType
  -- Utilities
  , asBool
  , asInt
  , asStackPtr
  , asArray
  , asDouble
  , asVar
  -- evaluation
  , ctermEval
  , ctermInit
  )
where

import qualified IR.SMT.TySmt                  as Ty
import qualified AST.Simple                    as AST
import qualified IR.SMT.Assert                 as Assert
import qualified Codegen.C.Memory              as Mem
import           Codegen.C.Memory               ( Mem )
import           Control.Monad                  ( unless
                                                , when
                                                , forM
                                                )
import qualified Data.BitVector                as Bv
import           Data.Foldable                 as Fold
import qualified Data.Map                      as Map
import           Data.Maybe                     ( fromMaybe )
import           Data.Dynamic                   ( Dynamic
                                                , toDyn
                                                )


type Bv = Ty.TermDynBv

class Bitable s where
  nbits :: s -> Int
  serialize :: s -> Bv
  deserialize :: AST.Type -> Bv -> s

data CTermData = CInt Bool Int Bv
               | CBool Ty.TermBool
               | CDouble Ty.TermDouble
               | CFloat Ty.TermFloat
               -- The array type, the offset, the underlying array.
               | CArray AST.Type Mem.StackAllocId
               -- The type is the pointer (not pointee) type!
               -- Also the offset and the underlying allocation.
               | CStackPtr AST.Type Bv Mem.StackAllocId
               | CStruct AST.Type [(String, CTerm)]
               deriving (Show, Eq)

ctermDataTy :: CTermData -> AST.Type
ctermDataTy t = case t of
  CInt True  8  _  -> AST.S8
  CInt False 8  _  -> AST.U8
  CInt True  16 _  -> AST.S16
  CInt False 16 _  -> AST.U16
  CInt True  32 _  -> AST.S32
  CInt False 32 _  -> AST.U32
  CInt True  64 _  -> AST.S64
  CInt False 64 _  -> AST.U64
  CInt _     w  _  -> error $ unwords ["Invalid int width:", show w]
  CBool{}          -> AST.Bool
  CDouble{}        -> AST.Double
  CFloat{}         -> AST.Float
  CArray  ty _     -> ty
  CStruct ty _     -> ty
  CStackPtr ty _ _ -> ty

ctermEval :: Map.Map String Dynamic -> CTerm -> Mem Dynamic
ctermEval env t = case term t of
  CInt _ _ t'      -> return $ toDyn $ Ty.eval env t'
  CBool   t'       -> return $ toDyn $ Ty.eval env t'
  CDouble t'       -> return $ toDyn $ Ty.eval env t'
  CFloat  t'       -> return $ toDyn $ Ty.eval env t'
  CStackPtr _ t' _ -> return $ toDyn $ Ty.eval env t'
  CStruct _ _t'    -> error "nyi"
  CArray  _ i      -> do
    array <- Mem.stackGetAlloc i
    return $ toDyn $ Ty.eval env array

ctermInit :: AST.Type -> Integer -> CTerm
ctermInit ty value = mkCTerm
  (case ty of
    AST.Bool -> CBool (Ty.BoolLit (value /= 0))
    _ | AST.isIntegerType ty ->
      let n = AST.numBits ty
      in  CInt False n (Ty.IntToDynBv n (Ty.IntLit (value `rem` toInteger n)))
    _ -> error "Cannot init"
  )
  (Ty.BoolLit False)

asDouble :: CTermData -> Ty.TermDouble
asDouble (CDouble d) = d
asDouble t           = error $ unwords [show t, "is not a double"]

asFloat :: CTermData -> Ty.TermFloat
asFloat (CFloat d) = d
asFloat t          = error $ unwords [show t, "is not a float"]

asInt :: CTermData -> (Bool, Int, Bv)
asInt (CInt s w i) = (s, w, i)
asInt t            = error $ unwords [show t, "is not an integer"]

asBool :: CTermData -> Ty.TermBool
asBool (CBool b) = b
asBool t         = error $ unwords [show t, "is not a boolean"]

asStackPtr :: CTermData -> (AST.Type, Bv, Mem.StackAllocId)
asStackPtr (CStackPtr ty bv alloc) = (ty, bv, alloc)
asStackPtr t = error $ unwords [show t, "is not a pointer"]

asArray :: CTermData -> (AST.Type, Mem.StackAllocId)
asArray (CArray ty alloc) = (ty, alloc)
asArray t                 = error $ unwords [show t, "is not an array"]

asVar :: CTerm -> Maybe String
asVar t = case term t of
  CInt _ _ t' -> Ty.asVarName t'
  CBool   t'  -> Ty.asVarName t'
  CDouble t'  -> Ty.asVarName t'
  CFloat  t'  -> Ty.asVarName t'
  _           -> error $ "Var name unsupported for " ++ show t

data CTerm = CTerm { term :: CTermData
                   , udef :: Ty.TermBool
                   }
                   deriving (Show, Eq)

-- Checks widths
mkCTerm :: CTermData -> Ty.TermBool -> CTerm
mkCTerm d b = case d of
  CInt _ w bv -> if Ty.dynBvWidth bv == w
    then CTerm d b
    else error $ unwords ["Bad width in CTerm", show d]
  CStackPtr ty off _alloc -> if Ty.dynBvWidth off == AST.numBits ty
    then CTerm d b
    else error $ unwords ["Bad width in CTerm", show d]
  _ -> CTerm d b

instance Bitable CTermData where
  nbits c = case c of
    CBool{}    -> 1
    CInt _ w _ -> w
    CDouble{}  -> 64
    CFloat{}   -> 32
    --CStackPtr ty _ _ -> AST.numBits ty
    _          -> error $ "Cannot serialize: " ++ show c
  serialize c = case c of
    CBool b      -> Ty.Ite b (Mem.bvNum False 1 1) (Mem.bvNum False 1 0)
    CInt _ _ bv  -> bv
    CDouble d    -> Ty.mkDynamizeBv $ Ty.FpToBv d
    CFloat  d    -> Ty.mkDynamizeBv $ Ty.FpToBv d
    -- Need to reverse because concant fives the first element the highest order.
    CStruct _ fs -> foldl1 Ty.mkDynBvConcat $ reverse $ map (serialize . term . snd) fs
    -- Pointer & Array serialization is hard, b/c you forget which allocation
    -- you refer to.
    _            -> error $ "Cannot serialize: " ++ show c
  deserialize ty bv = case ty of
    t | AST.isIntegerType t -> CInt (AST.isSignedInt t) (AST.numBits t) bv
    AST.Double              -> CDouble $ Ty.BvToFp $ Ty.mkStatifyBv @64 bv
    AST.Float               -> CFloat $ Ty.BvToFp $ Ty.mkStatifyBv @32 bv
    AST.Bool                -> CBool $ Ty.mkDynBvEq bv (Mem.bvNum False 1 1)
    AST.Struct fs ->
      let
        sizes  = map (AST.numBits . snd) fs
        starts = 0 : scanl1 (+) sizes
        chunks =
          zipWith (\start size -> Ty.mkDynBvExtract start size bv) starts sizes
          -- TODO: Don't make up undef here!
      in
        CStruct ty $ zipWith
          (\(f, fTy) chunk ->
            (f, mkCTerm (deserialize fTy chunk) (Ty.BoolLit False))
          )
          fs
          chunks
    _ -> error $ unwords ["Cannot deserialize", show ty]



cppType :: CTerm -> AST.Type
cppType = ctermDataTy . term

cppBool :: CTerm -> Ty.TermBool
cppBool = asBool . term . cppCast AST.Bool

nyi :: String -> a
nyi msg = error $ "Not yet implemented: " ++ msg

udefName :: String -> String
udefName s = s ++ "_undef"


-- Makes `name` an alias for `t`.
-- That is, creates new SMT variable corresponding to the terms in `t`,
-- constructs an equiavlent term with these new variables, and returns that new
-- term.
-- Useful for surfacing abtract terms under an easily identifiable name: `name`.
alias :: Bool -> String -> CTerm -> Mem CTerm
alias trackUndef name t = do
  u <- Mem.liftAssert $ Assert.newVar (udefName name) Ty.SortBool
  when trackUndef $ Mem.liftAssert $ Assert.assign (udef t) u
  d <- case term t of
    CBool b -> Mem.liftAssert $ do
      let sort = Ty.SortBool
      v <- Assert.newVar name sort
      Assert.assign v b
      return $ CBool b
    CInt isNeg width val -> Mem.liftAssert $ do
      let sort = Ty.SortBv width
      v <- Assert.newVar name sort
      Assert.assign v val
      return $ CInt isNeg width v
    CFloat val -> Mem.liftAssert $ do
      let sort = Ty.sortFloat
      v <- Assert.newVar name sort
      Assert.assign v val
      return $ CFloat v
    CDouble val -> Mem.liftAssert $ do
      let sort = Ty.sortDouble
      v <- Assert.newVar name sort
      Assert.assign v val
      return $ CDouble v
    CStackPtr ty off id -> Mem.liftAssert $ do
      let sort = Ty.SortBv $ AST.numBits ty
      v <- Assert.newVar name sort
      Assert.assign v off
      return $ CStackPtr ty v id
    -- Arrays have to term-specific SMT variables, so there is nothign to alias.
    CArray  ty id     -> return $ CArray ty id
    CStruct ty fields -> CStruct ty <$> forM
      fields
      (\(f, t) -> (f, ) <$> alias trackUndef (structVarName name f) t)
  return $ mkCTerm d u

structVarName :: String -> String -> String
structVarName baseName fieldName = baseName ++ "." ++ fieldName

-- Declare a new variable, initialize it to a value.
cppDeclInitVar :: Bool -> AST.Type -> String -> CTerm -> Mem CTerm
cppDeclInitVar trackUndef ty name init =
  alias trackUndef name $ cppCast ty init

-- Declare a new variable, do not initialize it.
cppDeclVar :: AST.Type -> String -> Mem CTerm
cppDeclVar ty name = do
  u <- Mem.liftAssert $ Assert.newVar (udefName name) Ty.SortBool
  -- Mem.liftAssert $ Assert.assign u (Ty.BoolLit True)
  t <- case ty of
    AST.Bool -> Mem.liftAssert $ CBool <$> Assert.newVar name Ty.SortBool
    _ | AST.isIntegerType ty ->
      Mem.liftAssert
        $   CInt (AST.isSignedInt ty) (AST.numBits ty)
        <$> Assert.newVar name (Ty.SortBv $ AST.numBits ty)
    AST.Double -> Mem.liftAssert $ CDouble <$> Assert.newVar name Ty.sortDouble
    AST.Float  -> Mem.liftAssert $ CFloat <$> Assert.newVar name Ty.sortFloat
    AST.Array (Just size) innerTy ->
      CArray ty <$> Mem.stackNewAlloc (size * AST.numBits innerTy)
    AST.Array Nothing _ -> return $ CArray ty Mem.stackIdUnknown
    AST.Ptr32 _         -> do
      bv :: Bv <- Mem.liftAssert $ Assert.newVar name (Ty.SortBv 32)
      return $ CStackPtr ty bv Mem.stackIdUnknown
    AST.Struct fields -> CStruct ty <$> forM
      fields
      (\(f, fTy) -> (f, ) <$> cppDeclVar fTy (structVarName name f))
    _ -> nyi $ "cppDeclVar for type " ++ show ty
  return $ mkCTerm t u

cppIntLit :: AST.Type -> Integer -> CTerm
cppIntLit t v =
  let s = AST.isSignedInt t
      w = AST.numBits t
  in  mkCTerm (CInt s w (Ty.IntToDynBv w (Ty.IntLit v))) (Ty.BoolLit False)

cppDoubleLit :: Double -> CTerm
cppDoubleLit v = mkCTerm (CDouble $ Ty.Fp64Lit v) (Ty.BoolLit False)

cppFloatLit :: Float -> CTerm
cppFloatLit v = mkCTerm (CFloat $ Ty.Fp32Lit v) (Ty.BoolLit False)

cppArrayLit :: AST.Type -> [CTerm] -> Mem CTerm
cppArrayLit ty vals = do
  forM_ vals $ \v -> unless (cppType v == ty) $ error $ unwords
    ["Type mismatch in cppArrayLit:", show ty, "vs", show $ cppType v]
  id <- Mem.stackAlloc
    (foldl1 Ty.mkDynBvConcat $ reverse $ map (serialize . term) vals)
  return $ mkCTerm (CArray (AST.Array (Just $ length vals) ty) id)
                   (Ty.BoolLit False)

cppStructLit :: AST.Type -> [CTerm] -> Mem CTerm
cppStructLit ty vals = do
  let fieldTys = AST.structFieldTypes ty
  forM_ (zip fieldTys vals) $ \(fTy, v) ->
    unless (cppType v == fTy) $ error $ unwords
      ["Type mismatch in cppStructLit:", show ty, "vs", show $ cppType v]
  return $ mkCTerm (CStruct ty (zip (map fst $ AST.structFieldList ty) vals))
                   (Ty.BoolLit False)

-- Do a load, returning whether is was UB
cppLoad :: CTerm -> Mem (Ty.TermBool, CTerm)
cppLoad ptr = case term ptr of
  CStackPtr ty offset id -> do
    bits <- Mem.stackLoad id offset (AST.numBits $ AST.pointeeType ty)
    oob  <- Ty.Not <$> Mem.stackIsLoadable id offset
    let value = deserialize (AST.pointeeType ty) bits
        -- TODO: Check bounds
        undef = Ty.BoolNaryExpr Ty.Or [udef ptr, oob]
    return (oob, mkCTerm value undef)
  _ -> error $ unwords ["The value", show ptr, "cannot be cppLoad'ed"]

-- Do a store, returning whether is was UB
cppStore :: CTerm -> CTerm -> Ty.TermBool -> Mem Ty.TermBool
cppStore ptr val guard = case term ptr of
  --TODO: serialize the udef bit too.
  CStackPtr ty offset id ->
    let bits = serialize (term $ cppCast (AST.pointeeType ty) val)
    in  if AST.numBits (AST.pointeeType ty) == Ty.dynBvWidth bits
          then do
            Mem.stackStore id offset bits guard
            Ty.Not <$> Mem.stackIsLoadable id offset
          else error $ unwords
            ["CTerm", show val, "is not", show (AST.numBits ty), "bits wide"]
  _ -> error $ unwords ["The value", show ptr, "cannot be cppStore'd"]

arrayToPointer :: CTerm -> CTerm
arrayToPointer arr =
  let (ty, id) = asArray $ term arr
  in  mkCTerm
        (CStackPtr (AST.Ptr32 $ AST.arrayBaseType ty) (Mem.bvNum False 32 0) id)
        (udef arr)

cppStructGet :: CTerm -> String -> CTerm
cppStructGet struct field = case term struct of
  CStruct _ fields ->
    fromMaybe (error $ "There is no '" ++ field ++ "' field in " ++ show struct)
      $ lookup field fields
  _ -> error $ "Cannot do a struct-get against " ++ show struct

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update k v l = case l of
  []           -> error "Missing key in list"
  (k', v') : r -> if k' == k then (k, v) : r else (k', v') : update k v r

cppStructSet :: String -> CTerm -> CTerm -> CTerm
cppStructSet field value struct = case term struct of
  CStruct ty fields -> mkCTerm
    (CStruct ty $ update field value fields)
    (Ty.BoolNaryExpr Ty.Or [udef struct, udef value])
  _ -> error $ "Cannot do a struct-set against " ++ show struct

cppIndex
  :: CTerm -- ^ Base
  -> CTerm -- ^ Index
  -> Mem CTerm -- Pointer
cppIndex base idx = case term base of
  CStackPtr{} -> return $ cppAdd base idx
  CArray{}    -> return $ cppAdd (arrayToPointer base) idx
  _           -> error $ unwords ["The value", show base, "cannot be indexed"]


intResize :: Bool -> Int -> Bv -> Bv
intResize fromSign toWidth from =
  let fromWidth = Ty.dynBvWidth from
  in  case compare fromWidth toWidth of
        LT ->
          (if fromSign then Ty.mkDynBvSext else Ty.mkDynBvUext) toWidth from
        EQ -> from
        GT -> Ty.mkDynBvExtract 0 toWidth from


-- TODO: clean this the fuck up.
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
  -> Ty.BvBinOp
  -> (  forall f
      . Ty.ComputableFp f
     => Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     )
  -- Undef function, takes sign and Bv term for each argument
  -> Maybe (Bool -> Bv -> Bool -> Bv -> Maybe Ty.TermBool)
  -> Bool -- ^ allow double
  -> Bool -- ^ make width the max of the two (alternative: the left)
  -> CTerm
  -> CTerm
  -> CTerm
cppWrapBinArith name bvOp doubleF ubF allowDouble mergeWidths a b = convert
  (integralPromotion a)
  (integralPromotion b)
 where
  convert a b =
    let
      cannot with = error $ unwords ["Cannot do", name, "with", with]

      -- TODO: check bounds!
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

      (t, u) = case (term a, term b) of
        (CDouble d, _) -> if allowDouble
          then
            ( CDouble $ doubleF d $ asDouble $ term $ cppCast AST.Double b
            , Nothing
            )
          else cannot "a double"
        (_, CDouble d) -> if allowDouble
          then
            ( CDouble $ doubleF d $ asDouble $ term $ cppCast AST.Double a
            , Nothing
            )
          else cannot "a double"
        (CFloat d, _) -> if allowDouble
          then
            (CFloat $ doubleF d $ asFloat $ term $ cppCast AST.Float b, Nothing)
          else cannot "a double"
        (_, CFloat d) -> if allowDouble
          then
            (CFloat $ doubleF d $ asFloat $ term $ cppCast AST.Float a, Nothing)
          else cannot "a double"
        (CStackPtr ty off id, CInt s _ i) ->
          if bvOp == Ty.BvAdd || bvOp == Ty.BvSub
            then (CStackPtr ty (cppPtrPlusInt ty off s i) id, Nothing)
            else cannot "a pointer on the left"
        (CStackPtr ty off id, CStackPtr ty' off' id') ->
          if bvOp == Ty.BvSub && ty == ty' && id == id'
            then -- TODO: ptrdiff_t?
              ( CInt True (AST.numBits ty) (Ty.mkDynBvBinExpr bvOp off off')
              , ubF >>= (\f -> f True off True off')
              )
            else
              cannot
                "two pointers, or two pointers of different types, or pointers to different allocations"
        (CInt s _ i, CStackPtr ty addr id) -> if bvOp == Ty.BvAdd
          then (CStackPtr ty (cppPtrPlusInt ty addr s i) id, Nothing)
          else cannot "a pointer on the right"
        -- Ptr diff
        (CInt s w i, CInt s' w' i') ->
          let width = if mergeWidths then max w w' else w
              sign  = max s s'
              l     = intResize s width i
              r     = intResize s' width i'
          in  ( CInt sign width $ Ty.mkDynBvBinExpr bvOp l r
              , ubF >>= (\f -> f s l s' r)
              )
        (_, _) -> cannot $ unwords [show a, "and", show b]
      pUdef = Ty.BoolNaryExpr Ty.Or (udef a : udef b : Fold.toList u)
    in
      mkCTerm t pUdef

cppBitOr, cppBitXor, cppBitAnd, cppSub, cppMul, cppAdd, cppMin, cppMax, cppDiv, cppRem, cppShl, cppShr
  :: CTerm -> CTerm -> CTerm
cppAdd = cppWrapBinArith "+"
                         Ty.BvAdd
                         (Ty.FpBinExpr Ty.FpAdd)
                         (Just overflow)
                         True
                         True
 where
  overflow s i s' i' =
    if s && s' then Just $ Ty.mkDynBvBinPred Ty.BvSaddo i i' else Nothing
cppSub = cppWrapBinArith "-"
                         Ty.BvSub
                         (Ty.FpBinExpr Ty.FpSub)
                         (Just overflow)
                         True
                         True
 where
  overflow s i s' i' =
    if s && s' then Just $ Ty.mkDynBvBinPred Ty.BvSsubo i i' else Nothing
cppMul = cppWrapBinArith "*"
                         Ty.BvMul
                         (Ty.FpBinExpr Ty.FpMul)
                         (Just overflow)
                         True
                         True
 where
  overflow s i s' i' =
    if s && s' then Just $ Ty.mkDynBvBinPred Ty.BvSmulo i i' else Nothing
-- TODO: div overflow
cppDiv = cppWrapBinArith "/"
                         Ty.BvUdiv
                         (Ty.FpBinExpr Ty.FpDiv)
                         (Just isDivZero)
                         True
                         True
isDivZero :: Bool -> Bv -> Bool -> Bv -> Maybe Ty.TermBool
isDivZero _s _i _s' i' =
  Just $ Ty.mkDynBvEq i' (Ty.DynBvLit (Bv.zeros (Ty.dynBvWidth i')))

-- TODO: CPP reference says that % requires integral arguments
cppRem = cppWrapBinArith "%"
                         Ty.BvUrem
                         (Ty.FpBinExpr Ty.FpRem)
                         (Just isDivZero)
                         False
                         True
cppMin = undefined
cppMax = undefined
noFpError
  :: forall f
   . Ty.ComputableFp f
  => Ty.Term (Ty.FpSort f)
  -> Ty.Term (Ty.FpSort f)
  -> Ty.Term (Ty.FpSort f)
noFpError = const $ const $ error "Invalid FP op"
cppBitOr = cppWrapBinArith "|" Ty.BvOr noFpError Nothing False True
cppBitAnd = cppWrapBinArith "&" Ty.BvAnd noFpError Nothing False True
cppBitXor = cppWrapBinArith "^" Ty.BvXor noFpError Nothing False True
-- Not quite right, since we're gonna force these to be equal in size
cppShl = cppWrapBinArith "<<" Ty.BvShl noFpError (Just overflow) False True
 where
  overflow s i s' i' =
    let baseNonNeg =
            [ Ty.mkDynBvBinPred Ty.BvSlt (Mem.bvNum True (Ty.dynBvWidth i) 0) i
            | s
            ]
        shftNonNeg =
            [ Ty.mkDynBvBinPred Ty.BvSlt (Mem.bvNum True (Ty.dynBvWidth i') 0) i'
            | s'
            ]
        shftSmall =
            [ Ty.mkDynBvBinPred
                Ty.BvSge
                (Mem.bvNum True
                           (Ty.dynBvWidth i')
                           (fromIntegral $ Ty.dynBvWidth i)
                )
                i'
            ]
    in  Just $ Ty.BoolNaryExpr Ty.Or $ baseNonNeg ++ shftNonNeg ++ shftSmall
-- Not quite right, since we're gonna force these to be equal in size
cppShr = cppWrapBinArith ">>" Ty.BvAshr noFpError (Just overflow) False True
 where
  overflow _s i s' i' =
    let shftNonNeg =
            [ Ty.mkDynBvBinPred Ty.BvSlt (Mem.bvNum True (Ty.dynBvWidth i') 0) i'
            | s'
            ]
        shftSmall =
            [ Ty.mkDynBvBinPred
                Ty.BvSge
                (Mem.bvNum True
                           (Ty.dynBvWidth i')
                           (fromIntegral $ Ty.dynBvWidth i)
                )
                i'
            ]
    in  Just $ Ty.BoolNaryExpr Ty.Or $ shftNonNeg ++ shftSmall

cppWrapUnArith
  :: String
  -> (Bv -> Bv)
  -> (  forall f
      . Ty.ComputableFp f
     => Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     )
  -> CTerm
  -> CTerm
cppWrapUnArith name bvF doubleF a =
  let t = case term $ integralPromotion a of
        CDouble d  -> CDouble $ doubleF d
        CInt _ w i -> CInt True w $ bvF i
        _          -> error $ unwords ["Cannot do", name, "on", show a]
  in  mkCTerm t (udef a)

cppPos, cppNeg, cppBitNot :: CTerm -> CTerm
cppPos = cppWrapUnArith "unary +" id id
cppNeg =
  cppWrapUnArith "unary -" (Ty.mkDynBvUnExpr Ty.BvNeg) (Ty.FpUnExpr Ty.FpNeg)
cppBitNot =
  cppWrapUnArith "~" (Ty.mkDynBvUnExpr Ty.BvNot) (Ty.FpUnExpr Ty.FpNeg)

cppWrapBinLogical
  :: String
  -> (Ty.TermBool -> Ty.TermBool -> Ty.TermBool)
  -> CTerm
  -> CTerm
  -> CTerm
cppWrapBinLogical name f a b =
  case (term $ cppCast AST.Bool a, term $ cppCast AST.Bool b) of
    (CBool a', CBool b') ->
      mkCTerm (CBool $ f a' b') (Ty.BoolNaryExpr Ty.Or [udef a, udef b])
    _ -> error $ unwords ["Cannot do", name, "on", show a, "and", show b]
cppOr, cppAnd :: CTerm -> CTerm -> CTerm
cppOr = cppWrapBinLogical "||" (((.) . (.)) (Ty.BoolNaryExpr Ty.Or) doubleton)
cppAnd =
  cppWrapBinLogical "&&" (((.) . (.)) (Ty.BoolNaryExpr Ty.And) doubleton)

cppWrapUnLogical :: String -> (Ty.TermBool -> Ty.TermBool) -> CTerm -> CTerm
cppWrapUnLogical name f a = case term $ cppCast AST.Bool a of
  CBool a' -> mkCTerm (CBool $ f a') (udef a)
  _        -> error $ unwords ["Cannot do", name, "on", show a]

cppNot :: CTerm -> CTerm
cppNot = cppWrapUnLogical "!" Ty.Not

-- This is not quite right, but it's a reasonable approximation of C++ arithmetic.
-- For an expression l (><) r, 
-- 1. Both l and r undergo **integral promotion** (bool -> int)
-- 2. If either are floating, the other is cast to that floating type
--    * Ptrs not castable.
-- 3. Scale the int, do the op.
cppWrapCmp
  :: String
  -> (Bool -> Bv -> Bv -> Ty.TermBool) -- ^f(signed, bv_a, bv_b)
  -> (  forall f
      . Ty.ComputableFp f
     => Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     -> Ty.TermBool
     )
  -> CTerm
  -> CTerm
  -> CTerm
cppWrapCmp name bvF doubleF a b = convert (integralPromotion a)
                                          (integralPromotion b)
 where
  convert a b =
    let
      cannot with = error $ unwords ["Cannot do", name, "with", with]
      t = case (term a, term b) of
        (CDouble d, _) -> doubleF d (asDouble $ term $ cppCast AST.Double b)
        (_, CDouble d) -> doubleF (asDouble $ term $ cppCast AST.Double a) d
        (CFloat d, _) -> doubleF d (asFloat $ term $ cppCast AST.Float b)
        (_, CFloat d) -> doubleF (asFloat $ term $ cppCast AST.Float a) d
        (CStackPtr ty addr id, CStackPtr ty' addr' id') ->
          if ty == ty' && id == id'
            then bvF False addr addr'
            else
              cannot
                "two pointers, or two pointers of different types, or pointers to different allocations"
        (CInt s w i, CInt s' w' i') ->
          let width = max w w'
              sign  = max s s'
          in  bvF sign (intResize s width i) (intResize s' width i')
        (_, _) -> cannot $ unwords [show a, "and", show b]
      pUdef = Ty.BoolNaryExpr Ty.Or [udef a, udef b]
    in
      mkCTerm (CBool t) pUdef

cppEq, cppNe, cppLt, cppGt, cppLe, cppGe :: CTerm -> CTerm -> CTerm
cppEq = cppWrapCmp "==" (const Ty.mkDynBvEq) Ty.Eq
cppNe = ((.) . (.)) cppNot cppEq
cppLt = cppWrapCmp
  "<"
  (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSlt else Ty.BvUlt))
  (Ty.FpBinPred Ty.FpLt)
cppGt = cppWrapCmp
  ">"
  (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSgt else Ty.BvUgt))
  (Ty.FpBinPred Ty.FpGt)
cppLe = cppWrapCmp
  "<="
  (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSle else Ty.BvUle))
  (Ty.FpBinPred Ty.FpLe)
cppGe = cppWrapCmp
  ">="
  (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSge else Ty.BvUge))
  (Ty.FpBinPred Ty.FpGe)

-- Promote integral types
-- Do not mess with pointers
integralPromotion :: CTerm -> CTerm
integralPromotion n = case term n of
  CBool{} -> cppCast AST.S32 n
  _       -> n

cppCast :: AST.Type -> CTerm -> CTerm
cppCast toTy node = case term node of
  CBool t -> case toTy of
    _ | AST.isIntegerType toTy ->
      let width = AST.numBits toTy
          cint  = CInt (AST.isSignedInt toTy) width (boolToBv t width)
      in  mkCTerm cint (udef node)
    AST.Double -> mkCTerm (CDouble $ Ty.DynUbvToFp $ boolToBv t 1) (udef node)
    AST.Float  -> mkCTerm (CFloat $ Ty.DynUbvToFp $ boolToBv t 1) (udef node)
    AST.Bool   -> node
    _          -> error $ unwords ["Bad cast from", show t, "to", show toTy]
  CInt fromS fromW t -> case toTy of
    _ | AST.isIntegerType toTy ->
      let toW = AST.numBits toTy
          toS = AST.isSignedInt toTy
          t'  = intResize fromS toW t
      in  mkCTerm (CInt toS toW t') (udef node)
    AST.Double -> mkCTerm
      (CDouble $ (if fromS then Ty.DynSbvToFp else Ty.DynUbvToFp) t)
      (udef node)
    AST.Float -> mkCTerm
      (CFloat $ (if fromS then Ty.DynSbvToFp else Ty.DynUbvToFp) t)
      (udef node)
    AST.Bool ->
      mkCTerm (CBool $ Ty.Not $ Ty.Eq (Mem.bvNum False fromW 0) t) (udef node)
    _ -> error $ unwords ["Bad cast from", show t, "to", show toTy]
  CDouble t -> case toTy of
    _ | AST.isIntegerType toTy ->
      -- TODO: Do this with rounding modes
      let half    = Ty.Fp64Lit 0.5
          negHalf = Ty.Fp64Lit (-0.5)
      in  mkCTerm
            (CInt (AST.isSignedInt toTy) (AST.numBits toTy) $ Ty.RoundFpToDynBv
              (AST.numBits toTy)
              True
              (Ty.FpBinExpr
                Ty.FpAdd
                t
                (Ty.Ite (Ty.FpUnPred Ty.FpIsPositive t) negHalf half)
              )
            )
            (udef node)
    AST.Bool ->
      mkCTerm (CBool $ Ty.Not $ Ty.FpUnPred Ty.FpIsZero t) (udef node)
    AST.Double -> node
    AST.Float  -> mkCTerm (CFloat $ Ty.FpToFp t) (udef node)
    _          -> error $ unwords ["Bad cast from", show t, "to", show toTy]
  CFloat t -> case toTy of
    _ | AST.isIntegerType toTy ->
      -- TODO: Do this with rounding modes
      let half    = Ty.Fp32Lit 0.5
          negHalf = Ty.Fp32Lit (-0.5)
      in  mkCTerm
            (CInt (AST.isSignedInt toTy) (AST.numBits toTy) $ Ty.RoundFpToDynBv
              (AST.numBits toTy)
              True
              (Ty.FpBinExpr
                Ty.FpAdd
                t
                (Ty.Ite (Ty.FpUnPred Ty.FpIsPositive t) negHalf half)
              )
            )
            (udef node)
    AST.Bool ->
      mkCTerm (CBool $ Ty.Not $ Ty.FpUnPred Ty.FpIsZero t) (udef node)
    AST.Double -> mkCTerm (CDouble $ Ty.FpToFp t) (udef node)
    AST.Float  -> node
    _          -> error $ unwords ["Bad cast from", show t, "to", show toTy]
  CStackPtr ty t _id -> if AST.isIntegerType toTy
    then cppCast toTy $ mkCTerm (CInt False (Ty.dynBvWidth t) t) (udef node)
    else if toTy == AST.Bool
      then mkCTerm
        (CBool $ Ty.Not $ Ty.Eq (Mem.bvNum False (AST.numBits ty) 0) t)
        (udef node)
      else if AST.isPointer toTy
        -- TODO: Not quite right: widths
        then node
        else error $ unwords ["Bad cast from", show t, "to", show toTy]
  CArray ty id -> case toTy of
    AST.Ptr32 iTy | iTy == AST.arrayBaseType ty -> mkCTerm
      (CStackPtr toTy (Mem.bvNum False (AST.numBits toTy) 0) id)
      (udef node)
    AST.Array Nothing toBaseTy | toBaseTy == AST.arrayBaseType ty ->
      mkCTerm (CArray toTy id) (udef node)
    _ | toTy == ty -> node
    _ -> error $ unwords ["Bad cast from", show node, "to", show toTy]
  CStruct ty _ -> case toTy of
    _ | toTy == ty -> node
    _ -> error $ unwords ["Bad cast from", show node, "to", show toTy]
 where
  boolToBv :: Ty.TermBool -> Int -> Bv
  boolToBv b w = Ty.Ite b (Mem.bvNum False w 1) (Mem.bvNum False w 0)

-- A ITE based on a CTerm condition.
cppCond :: CTerm -> CTerm -> CTerm -> CTerm
cppCond cond t f =
  let condB  = asBool $ term $ cppCast AST.Bool cond
      r      = cppIte condB t f
  in  mkCTerm (term r) (Ty.BoolNaryExpr Ty.Or [udef cond, udef r])

-- A ITE based on an SMT boolean condition.
cppIte :: Ty.TermBool -> CTerm -> CTerm -> CTerm
cppIte condB t f =
  let
    result = case (term t, term f) of
      (CBool   tB, CBool fB  ) -> CBool $ Ty.Ite condB tB fB
      (CDouble tB, CDouble fB) -> CDouble $ Ty.Ite condB tB fB
      (CFloat  tB, CFloat fB ) -> CFloat $ Ty.Ite condB tB fB
      (CStackPtr tTy tB tId, CStackPtr fTy fB fId) | tTy == fTy && tId == fId ->
        CStackPtr tTy (Ty.Ite condB tB fB) tId
      (CInt s w i, CInt s' w' i') ->
        let sign  = s && s' -- Not really sure is this is correct b/c of ranks.
            width = max w w'
        in  CInt sign width
              $ Ty.Ite condB (intResize s width i) (intResize s' width i')
      (CStruct aTy a, CStruct bTy b) | aTy == bTy ->
        CStruct aTy $ zipWith (\(f, aV) (_, bV) -> (f, cppIte condB aV bV)) a b
      _ ->
        error $ unwords
          ["Cannot construct conditional with", show t, "and", show f]
  in
    mkCTerm result (Ty.Ite condB (udef t) (udef f))


doubleton :: a -> a -> [a]
doubleton x y = [x, y]

cppTrue :: CTerm
cppTrue = mkCTerm (CBool $ Ty.BoolLit True) (Ty.BoolLit False)

cppFalse :: CTerm
cppFalse = mkCTerm (CBool $ Ty.BoolLit False) (Ty.BoolLit False)

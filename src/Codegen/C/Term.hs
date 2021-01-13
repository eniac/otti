{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-| This module define CTerm: an SMT/Mem embedding of C values and terms.
 - It does not handle control flow.
 -}

module Codegen.C.Term
  ( CTerm(..)
  , CTermData(..)
  , Bitable(..)
  , cDeclVar
  , ctermGetVars
  , ctermIsUndef
  -- Memory Operations
  , cLoad
  , cStore
  -- Arith Operations
  , cBitOr
  , cBitXor
  , cBitNot
  , cBitAnd
  , cSub
  , cMul
  , cAdd
  , cMin
  , cMax
  , cDiv
  , cRem
  , cPos
  , cNeg
  , cShl
  , cShr
  -- Logical Operations
  , cAnd
  , cOr
  , cNot
  -- Comparisons
  , cLt
  , cLe
  , cGt
  , cGe
  , cEq
  , cNe
  -- Ites
  , cCond
  -- Other
  , cCast
  , cBool
  -- Literals
  , cBoolLit
  , cIntLit
  , cFloatLit
  , cDoubleLit
  , cArrayLit
  , cStructLit
  -- Structs
  , cStructGet
  , cStructSet
  -- Pointers & Arrays
  , cIndex
  -- Reflection
  , cType
  -- Utilities
  , asBool
  , asInt
  , asStackPtr
  , asArray
  , asVar
  , CCirc
  )
where

import           Codegen.LangVal                ( InMap
                                                , setInputFromMap
                                                )
import qualified Codegen.C.Type                as Type
import           Codegen.Circify.Memory         ( Mem )
import           Codegen.Circify                ( Circify
                                                , Embeddable(..)
                                                )
import qualified Codegen.Circify.Memory        as Mem
import           Control.Monad                  ( forM
                                                , unless
                                                , when
                                                )
import           Control.Monad.State.Strict
import qualified Data.BitVector                as Bv
import           Data.Foldable                 as Fold
import qualified Data.Set                      as Set
import           Data.Maybe                     ( fromMaybe
                                                , isJust
                                                )
import           IR.SMT.Assert                  ( liftAssert )
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as Ty
import qualified IR.SMT.TySmt.Alg              as TyAlg
import           Util.Log


type Bv = Ty.TermDynBv

class Bitable s where
  nbits :: s -> Int
  serialize :: s -> Bv
  deserialize :: Type.Type -> Bv -> s

data CTermData = CInt Bool Int Bv
               | CBool Ty.TermBool
               | CDouble Ty.TermDouble
               | CFloat Ty.TermFloat
               -- The array type, the offset, the underlying array.
               | CArray Type.Type Mem.StackAllocId
               -- The type is the pointer (not pointee) type!
               -- Also the offset and the underlying allocation.
               | CStackPtr Type.Type Bv Mem.StackAllocId
               | CStruct Type.Type [(String, CTerm)]
               deriving (Show, Eq)

ctermDataTy :: CTermData -> Type.Type
ctermDataTy t = case t of
  CInt True  8  _  -> Type.S8
  CInt False 8  _  -> Type.U8
  CInt True  16 _  -> Type.S16
  CInt False 16 _  -> Type.U16
  CInt True  32 _  -> Type.S32
  CInt False 32 _  -> Type.U32
  CInt True  64 _  -> Type.S64
  CInt False 64 _  -> Type.U64
  CInt _     w  _  -> error $ unwords ["Invalid int width:", show w]
  CBool{}          -> Type.Bool
  CDouble{}        -> Type.Double
  CFloat{}         -> Type.Float
  CArray  ty _     -> ty
  CStruct ty _     -> ty
  CStackPtr ty _ _ -> ty



asInt :: CTermData -> (Bool, Int, Bv)
asInt (CInt s w i) = (s, w, i)
asInt t            = error $ unwords [show t, "is not an integer"]

asBool :: CTermData -> Ty.TermBool
asBool (CBool b) = b
asBool t         = error $ unwords [show t, "is not a boolean"]

asStackPtr :: CTermData -> (Type.Type, Bv, Mem.StackAllocId)
asStackPtr (CStackPtr ty bv alloc) = (ty, bv, alloc)
asStackPtr t = error $ unwords [show t, "is not a pointer"]

asArray :: CTermData -> (Type.Type, Mem.StackAllocId)
asArray (CArray ty alloc) = (ty, alloc)
asArray t                 = error $ unwords [show t, "is not an array"]

asVar :: CTerm -> Maybe String
asVar t = case term t of
  CInt _ _ t' -> Ty.asVarName t'
  CBool   t'  -> Ty.asVarName t'
  CDouble t'  -> Ty.asVarName t'
  CFloat  t'  -> Ty.asVarName t'
  _           -> error $ "Var name unsupported for " ++ show t

data CTerm = CTerm
  { term :: CTermData
  , udef :: Ty.TermBool
  }
  deriving (Show, Eq)

-- Checks widths
mkCTerm :: CTermData -> Ty.TermBool -> CTerm
mkCTerm d b = case d of
  CInt _ w bv -> if Ty.dynBvWidth bv == w
    then CTerm d b
    else error $ unwords ["Bad width in CTerm", show d]
  CStackPtr ty off _alloc -> if Ty.dynBvWidth off == Type.numBits ty
    then CTerm d b
    else error $ unwords ["Bad width in CTerm", show d]
  _ -> CTerm d b

instance Bitable CTermData where
  nbits c = case c of
    CBool{}    -> 1
    CInt _ w _ -> w
    CDouble{}  -> 64
    CFloat{}   -> 32
    --CStackPtr ty _ _ -> Type.numBits ty
    _          -> error $ "Cannot serialize: " ++ show c
  serialize c = case c of
    CBool b     -> Ty.BoolToDynBv b
    CInt _ _ bv -> bv
    CDouble d   -> Ty.mkDynamizeBv $ Ty.FpToBv d
    CFloat  d   -> Ty.mkDynamizeBv $ Ty.FpToBv d
    -- Need to reverse because concant fives the first element the highest order.
    CStruct _ fs ->
      foldl1 Ty.mkDynBvConcat $ reverse $ map (serialize . term . snd) fs
    -- Pointer & Array serialization is hard, b/c you forget which allocation
    -- you refer to.
    _ -> error $ "Cannot serialize: " ++ show c
  deserialize ty bv = case ty of
    t | Type.isIntegerType t -> CInt (Type.isSignedInt t) (Type.numBits t) bv
    Type.Double              -> CDouble $ Ty.BvToFp $ Ty.mkStatifyBv @64 bv
    Type.Float               -> CFloat $ Ty.BvToFp $ Ty.mkStatifyBv @32 bv
    Type.Bool                -> CBool $ Ty.mkDynBvExtractBit 0 bv
    Type.Struct fs ->
      let
        sizes  = map (Type.numBits . snd) fs
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



cType :: CTerm -> Type.Type
cType = ctermDataTy . term

cBool :: CTerm -> Ty.TermBool
cBool = asBool . term . cCast Type.Bool

nyi :: String -> a
nyi msg = error $ "Not yet implemented: " ++ msg

udefName :: String -> String
udefName s = s ++ "_undef"

cSetValues :: Bool -> String -> CTerm -> Assert.Assert ()
cSetValues trackUndef name t = do
  logIf "values" $ "Setting " ++ show name ++ " to " ++ show t
  case term t of
    CBool b    -> Assert.evalAndSetValue name b
    CInt _ _ i -> Assert.evalAndSetValue name i
    CFloat  f  -> Assert.evalAndSetValue name f
    CDouble d  -> Assert.evalAndSetValue name d
    CStruct _ty fs ->
      forM_ fs $ \(f, t) -> cSetValues trackUndef (structVarName name f) t
    CArray{} -> return ()
    _        -> error $ "Cannot set value for a term: " ++ show t
  when trackUndef $ Assert.evalAndSetValue (udefName name) (udef t)

cEvaluate :: Bool -> CTerm -> Assert.Assert (Maybe CTerm)
cEvaluate trackUndef t = do
  logIf "values" $ "Evaluating " ++ show t
  t' <- case term t of
    CBool   b     -> fmap CBool <$> Assert.evalToTerm b
    CFloat  b     -> fmap CFloat <$> Assert.evalToTerm b
    CDouble b     -> fmap CDouble <$> Assert.evalToTerm b
    CInt s w b    -> fmap (CInt s w) <$> Assert.evalToTerm b
    CStruct ty fs -> fmap (CStruct ty) . sequence <$> forM
      fs
      (\(f, t) -> fmap (f, ) <$> cEvaluate trackUndef t)
    CArray{}    -> error "Cannot evaluate array term: not-yet implemented"
    CStackPtr{} -> error "Cannot evaluate stack pointer: not-yet implemented"
  if trackUndef
    then do
      u' <- fmap TyAlg.valueToTerm <$> Assert.eval (udef t)
      return $ mkCTerm <$> t' <*> u'
    else return $ mkCTerm <$> t' <*> pure (Ty.BoolLit False)


-- Makes `name` an alias for `t`.
-- That is, creates new SMT variable corresponding to the terms in `t`,
-- constructs an equiavlent term with these new variables, and returns that new
-- term.
-- Useful for surfacing abtract terms under an easily identifiable name: `name`.
alias :: Bool -> String -> CTerm -> Assert.Assert CTerm
alias trackUndef name t = do
  u <- Assert.newVar (udefName name) Ty.SortBool
  when trackUndef $ Assert.assign u (udef t)
  d <- case term t of
    CBool b -> do
      let sort = Ty.SortBool
      v <- Assert.newVar name sort
      Assert.assign v b
      return $ CBool v
    CInt isNeg width val -> do
      let sort = Ty.SortBv width
      v <- Assert.newVar name sort
      Assert.assign v val
      return $ CInt isNeg width v
    CFloat val -> do
      let sort = Ty.sortFloat
      v <- Assert.newVar name sort
      Assert.assign v val
      return $ CFloat v
    CDouble val -> do
      let sort = Ty.sortDouble
      v <- Assert.newVar name sort
      Assert.assign v val
      return $ CDouble v
    -- TODO set value?
    CStackPtr ty off id -> do
      let sort = Ty.SortBv $ Type.numBits ty
      v <- Assert.newVar name sort
      Assert.assign v off
      return $ CStackPtr ty v id
    -- Arrays have to term-specific SMT variables, so there is nothign to alias.
    -- TODO set value?
    CArray  ty id     -> return $ CArray ty id
    CStruct ty fields -> CStruct ty <$> forM
      fields
      (\(f, t) -> (f, ) <$> alias trackUndef (structVarName name f) t)
  return $ mkCTerm d u

structVarName :: String -> String -> String
structVarName baseName fieldName = baseName ++ "." ++ fieldName

arrayNameMod :: String -> Int -> String
arrayNameMod baseName idx = baseName ++ "." ++ show idx

-- Declare a new variable, initialize it to a value.
-- Returns the new term, and the old one, casted to be of the right type.
cDeclInitVar :: Bool -> Type.Type -> String -> CTerm -> Mem (CTerm, CTerm)
cDeclInitVar trackUndef ty name init =
  let init' = cCast ty init
  in  liftAssert $ (, init') <$> alias trackUndef name init'

-- Declare a new variable, initializing it to a value.
-- Returns the term corresponding to the new variable and
-- whatever it was ultimately assigned to.
cAssign :: Bool -> Type.Type -> String -> CTerm -> Mem (CTerm, CTerm)
cAssign trackUndef ty name value =
  cDeclInitVar trackUndef ty name (cCast ty value)

-- Declare a new variable, do not initialize it.
cDeclVar
  :: Maybe InMap -> Bool -> Type.Type -> String -> Maybe String -> Mem CTerm
cDeclVar inMap trackUndef ty smtName mUserName = do
  logIf "cDeclVar" $ show trackUndef ++ "cDeclVar: " ++ smtName
  u <- liftAssert $ if trackUndef
    then do
      t <- Assert.newVar (udefName smtName) Ty.SortBool
      getBaseInput (Ty.ValBool . (/= 0))
                   (Ty.ValBool False)
                   (udefName smtName)
                   (udefName <$> mUserName)
      return t
    else return (Ty.BoolLit False)
  t <- case ty of
    Type.Bool -> liftAssert $ do
      t <- CBool <$> Assert.newVar smtName Ty.SortBool
      getBaseInput (Ty.ValBool . (/= 0)) (Ty.ValBool False) smtName mUserName
      return t
    _ | Type.isIntegerType ty -> liftAssert $ do
      let n = Type.numBits ty
      t <- liftAssert $ CInt (Type.isSignedInt ty) n <$> Assert.newVar
        smtName
        (Ty.SortBv n)
      getBaseInput (Ty.ValDynBv . Bv.bitVec n)
                   (Ty.ValDynBv $ Bv.bitVec n (0 :: Int))
                   smtName
                   mUserName
      return t
    Type.Double -> liftAssert $ do
      t <- CDouble <$> Assert.newVar smtName Ty.sortDouble
      getBaseInput (error "nyi: double input")
                   (Ty.ValDouble 0)
                   smtName
                   mUserName
      return t
    Type.Float -> liftAssert $ do
      t <- CFloat <$> Assert.newVar smtName Ty.sortFloat
      getBaseInput (error "nyi: float input") (Ty.ValFloat 0) smtName mUserName
      return t
    Type.Array (Just size) innerTy -> do
      id <- Mem.stackNewAlloc size 32 (Type.numBits innerTy)
      forM_ mUserName $ \userName -> do
        unless (Type.isSimple innerTy)
          $  error
          $  "We only support user visible arrays of primitives: "
          ++ show ty
        forM_ [0 .. (size - 1)] $ \i -> do
          let smtName'  = arrayNameMod smtName i
              userName' = Just $ arrayNameMod userName i
          innerT <- cDeclVar inMap trackUndef innerTy smtName' userName'
          let off  = Mem.bvNum False 32 $ toInteger i
              ptr  = CStackPtr (Type.Ptr32 innerTy) off id
              ptrT = mkCTerm ptr (Ty.BoolLit False)
          -- We know this is not UB, so ignore
          void $ cStore ptrT innerT (Ty.BoolLit True)
      return $ CArray ty id
    Type.Array Nothing _ -> return $ CArray ty Mem.stackIdUnknown
    Type.Ptr32 _         -> do
      bv :: Bv <- liftAssert $ Assert.newVar smtName (Ty.SortBv 32)
      return $ CStackPtr ty bv Mem.stackIdUnknown
    Type.Struct fields -> CStruct ty <$> forM
      fields
      (\(f, fTy) -> (f, ) <$> cDeclVar inMap
                                       trackUndef
                                       fTy
                                       (structVarName smtName f)
                                       (flip structVarName f <$> mUserName)
      )
    _ -> nyi $ "cDeclVar for type " ++ show ty
  when (isJust mUserName && (Type.Bool == ty || Type.isIntegerType ty))
    $ liftAssert
    $ Assert.publicize smtName
  return $ mkCTerm t u
 where
  getBaseInput
    :: Ty.SortClass s
    => (Integer -> Ty.Value s)
    -> Ty.Value s
    -> String
    -> Maybe String
    -> Assert.Assert ()
  getBaseInput = setInputFromMap inMap

cIntLit :: Type.Type -> Integer -> CTerm
cIntLit t v =
  let s = Type.isSignedInt t
      w = Type.numBits t
  in  mkCTerm (CInt s w (Ty.DynBvLit $ Bv.bitVec w v)) (Ty.BoolLit False)

cDoubleLit :: Double -> CTerm
cDoubleLit v = mkCTerm (CDouble $ Ty.Fp64Lit v) (Ty.BoolLit False)

cFloatLit :: Float -> CTerm
cFloatLit v = mkCTerm (CFloat $ Ty.Fp32Lit v) (Ty.BoolLit False)

cArrayLit :: Type.Type -> [CTerm] -> Mem CTerm
cArrayLit ty vals = do
  forM_ vals $ \v -> unless (cType v == ty) $ error $ unwords
    ["Type mismatch in cArrayLit:", show ty, "vs", show $ cType v]
  let bvs = map (serialize . term) vals
  id <- Mem.stackAllocCons 32 bvs
  return $ mkCTerm (CArray (Type.Array (Just $ length vals) ty) id)
                   (Ty.BoolLit False)

cStructLit :: Type.Type -> [CTerm] -> Mem CTerm
cStructLit ty vals = do
  let fieldTys = Type.structFieldTypes ty
  forM_ (zip fieldTys vals) $ \(fTy, v) ->
    unless (cType v == fTy) $ error $ unwords
      ["Type mismatch in cStructLit:", show ty, "vs", show $ cType v]
  return $ mkCTerm (CStruct ty (zip (map fst $ Type.structFieldList ty) vals))
                   (Ty.BoolLit False)

-- Do a load, returning whether is was UB
cLoad :: CTerm -> Mem (Ty.TermBool, CTerm)
cLoad ptr = case term ptr of
  CStackPtr ty offset id -> do
    bits <- Mem.stackLoad id offset
    oob  <- Ty.Not <$> Mem.stackIsLoadable id offset
    let value = deserialize (Type.pointeeType ty) bits
        -- TODO: Check bounds
        undef = Ty.BoolNaryExpr Ty.Or [udef ptr, oob]
    return (oob, mkCTerm value undef)
  _ -> error $ unwords ["The value", show ptr, "cannot be cLoad'ed"]

-- Do a store, returning whether is was UB
cStore :: CTerm -> CTerm -> Ty.TermBool -> Mem Ty.TermBool
cStore ptr val guard = case term ptr of
  --TODO: serialize the udef bit too.
  CStackPtr ty offset id ->
    let bits = serialize (term $ cCast (Type.pointeeType ty) val)
    in  if Type.numBits (Type.pointeeType ty) == Ty.dynBvWidth bits
          then do
            Mem.stackStore id offset bits guard
            Ty.Not <$> Mem.stackIsLoadable id offset
          else error $ unwords
            ["CTerm", show val, "is not", show (Type.numBits ty), "bits wide"]
  _ -> error $ unwords ["The value", show ptr, "cannot be cStore'd"]

arrayToPointer :: CTerm -> CTerm
arrayToPointer arr =
  let (ty, id) = asArray $ term arr
  in  mkCTerm
        (CStackPtr (Type.Ptr32 $ Type.arrayBaseType ty)
                   (Mem.bvNum False 32 0)
                   id
        )
        (udef arr)

cStructGet :: CTerm -> String -> CTerm
cStructGet struct field = case term struct of
  CStruct _ fields ->
    fromMaybe (error $ "There is no '" ++ field ++ "' field in " ++ show struct)
      $ lookup field fields
  _ -> error $ "Cannot do a struct-get against " ++ show struct

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update k v l = case l of
  []           -> error "Missing key in list"
  (k', v') : r -> if k' == k then (k, v) : r else (k', v') : update k v r

cStructSet :: String -> CTerm -> CTerm -> CTerm
cStructSet field value struct = case term struct of
  CStruct ty fields -> mkCTerm
    (CStruct ty $ update field value fields)
    (Ty.BoolNaryExpr Ty.Or [udef struct, udef value])
  _ -> error $ "Cannot do a struct-set against " ++ show struct

cIndex
  :: CTerm -- ^ Base
  -> CTerm -- ^ Index
  -> Mem CTerm -- Pointer
cIndex base idx = case term base of
  CStackPtr{} -> return $ cAdd base idx
  CArray{}    -> return $ cAdd (arrayToPointer base) idx
  _           -> error $ unwords ["The value", show base, "cannot be indexed"]


intResize :: Bool -> Int -> Bv -> Bv
intResize fromSign toWidth from =
  let fromWidth = Ty.dynBvWidth from
  in  case compare fromWidth toWidth of
        LT ->
          (if fromSign then Ty.mkDynBvSext else Ty.mkDynBvUext) toWidth from
        EQ -> from
        GT -> Ty.mkDynBvExtract 0 toWidth from

-- | Implementation of integer promotion (C11, 6.3.1.1.3)
-- Does not handle bitfields, enumerations
intPromotion :: CTerm -> CTerm
intPromotion t =
  let ty = cType t
  in  if (Type.isIntegerType ty || Type.Bool == ty)
           && Type.intConversionRank ty
           <  Type.intConversionRank Type.S32
        then case term t of
          CInt sign width val ->
            -- "If an int can represent all values ... converted to an int ...
            -- otherwise an unsigned int"
            let maxVal = (2 :: Integer) ^ (width - if sign then 1 else 0) - 1
                term'  = if maxVal < maxIntVal
                  then mkInt $ Ty.mkDynBvSext 32 val
                  else CInt False 32 $ Ty.mkDynBvUext 32 val
            in  mkCTerm term' (udef t)
          CBool b ->
            mkCTerm (mkInt $ Ty.mkDynBvUext 32 $ Ty.BoolToDynBv b) (udef t)
          _ -> t
        else t
 where
  maxIntVal = (2 :: Integer) ^ (31 :: Integer) - 1
  mkInt     = CInt True 32

-- | Implementation of *usual arithmetic conversions* (C11, 6.3.1.8)
-- No-op if there is a non-arith term.
usualArithConversions :: CTerm -> CTerm -> (CTerm, CTerm)
usualArithConversions x y =
  if Type.isArithType (cType x) && Type.isArithType (cType y)
    then
      let (x', y') = inner x y
      in
        if cType x' == cType y'
          then (x', y')
          else
            error $ unwords
              ["UAC failed:", show (x, y), "to non-equal", show (x', y')]
    else (x, y)
 where
  inner a b
    | Type.isDouble aTy || Type.isDouble bTy
    = (cCast Type.Double a, cCast Type.Double b)
    | Type.isFloat aTy || Type.isFloat bTy
    = (cCast Type.Float a, cCast Type.Float b)
    | aPromTy == bPromTy
    = (aProm, bProm)
    | Type.isSignedInt aPromTy == Type.isSignedInt bPromTy
    = if Type.intConversionRank aPromTy < Type.intConversionRank bPromTy
      then (cCast bPromTy aProm, bProm)
      else (aProm, cCast aPromTy bProm)
    | otherwise
    = let
        signedFirst = Type.isSignedInt aPromTy
        (s, u)      = if signedFirst then (aProm, bProm) else (bProm, aProm)
        sTy         = cType s
        uTy         = cType u
        sR          = Type.intConversionRank sTy
        uR          = Type.intConversionRank uTy
        (s', u')
          | uR >= sR
          = (cCast uTy s, u)
          | Type.numBits sTy > Type.numBits uTy
          = (s, cCast sTy u)
          | otherwise
          = let ty = Type.makeIntTy (Type.numBits sTy) False
            in  (cCast ty s, cCast ty u)
      in
        if signedFirst then (s', u') else (u', s')
   where
    aTy     = cType a
    bTy     = cType b
    aProm   = intPromotion a
    bProm   = intPromotion b
    aPromTy = cType aProm
    bPromTy = cType bProm




cWrapBinArith
  :: String
  -> (Bool -> Either Ty.BvBinOp Ty.BvNaryOp)
  -> (  forall f
      . Ty.ComputableFp f
     => Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     )
  -- Undef function, takes sign and Bv term for each argument
  -> Maybe (Bool -> Bv -> Bv -> Maybe Ty.TermBool)
  -> Bool -- ^ allow double
  -> CTerm
  -> CTerm
  -> CTerm
cWrapBinArith name bvOp doubleF ubF allowDouble a b =
  uncurry convert $ usualArithConversions a b
 where
  bvBinExpr (Left  o) = Ty.mkDynBvBinExpr o
  bvBinExpr (Right o) = \x y -> Ty.mkDynBvNaryExpr o [x, y]
  convert a b =
    let
      cannot with = error $ unwords ["Cannot do", name, "with", with]

      -- TODO: check bounds!
      cPtrPlusInt :: Type.Type -> Bv -> Bool -> Bv -> Bv
      cPtrPlusInt pTy ptr signed int =
        bvBinExpr (Right Ty.BvAdd) ptr $ intResize signed (Type.numBits pTy) int

      (t, u) = case (term a, term b) of
        (CDouble x, CDouble y) -> if allowDouble
          then (CDouble $ doubleF x y, Nothing)
          else cannot "a double"
        (CFloat x, CFloat y) -> if allowDouble
          then (CFloat $ doubleF x y, Nothing)
          else cannot "a float"
        (CStackPtr ty off id, CInt s _ i) ->
          if bvOp s == Right Ty.BvAdd || bvOp s == Left Ty.BvSub
            then (CStackPtr ty (cPtrPlusInt ty off s i) id, Nothing)
            else cannot "a pointer on the left"
        (CStackPtr ty off id, CStackPtr ty' off' id') ->
          if bvOp False == Left Ty.BvSub && ty == ty' && id == id'
            then -- TODO: ptrdiff_t?
              ( CInt True (Type.numBits ty) (bvBinExpr (bvOp False) off off')
              , ubF >>= (\f -> f True off off')
              )
            else
              cannot
                "two pointers, or two pointers of different types, or pointers to different allocations"
        (CInt s _ i, CStackPtr ty addr id) -> if bvOp s == Right Ty.BvAdd
          then (CStackPtr ty (cPtrPlusInt ty addr s i) id, Nothing)
          else cannot "a pointer on the right"
        -- Ptr diff
        (CInt s w i, CInt s' w' i') | s == s' && w == w' ->
          (CInt s w $ bvBinExpr (bvOp s) i i', ubF >>= (\f -> f s i i'))
        (_, _) -> cannot $ unwords [show a, "and", show b]
      pUdef = Ty.BoolNaryExpr Ty.Or (udef a : udef b : Fold.toList u)
    in
      mkCTerm t pUdef

cBitOr, cBitXor, cBitAnd, cSub, cMul, cAdd, cMin, cMax, cDiv, cRem, cShl, cShr
  :: CTerm -> CTerm -> CTerm
cAdd = cWrapBinArith "+"
                     (const $ Right Ty.BvAdd)
                     (Ty.FpBinExpr Ty.FpAdd)
                     (Just overflow)
                     True
 where
  overflow s i i' =
    if s then Just $ Ty.mkDynBvBinPred Ty.BvSaddo i i' else Nothing
cSub = cWrapBinArith "-"
                     (const $ Left Ty.BvSub)
                     (Ty.FpBinExpr Ty.FpSub)
                     (Just overflow)
                     True
 where
  overflow s i i' =
    if s then Just $ Ty.mkDynBvBinPred Ty.BvSsubo i i' else Nothing
cMul = cWrapBinArith "*"
                     (const $ Right Ty.BvMul)
                     (Ty.FpBinExpr Ty.FpMul)
                     (Just overflow)
                     True
 where
  overflow s i i' =
    if s then Just $ Ty.mkDynBvBinPred Ty.BvSmulo i i' else Nothing
cDiv = cWrapBinArith "/"
                     (const $ Left Ty.BvUdiv)
                     (Ty.FpBinExpr Ty.FpDiv)
                     (Just overflow)
                     True
 where
  overflow s i i' =
    let w = Ty.dynBvWidth i'
    in  Just $ Ty.BoolNaryExpr
          Ty.Or
          [ Ty.mkEq i' (Ty.DynBvLit (Bv.zeros w))
          , if s
            then Ty.BoolNaryExpr
              Ty.And
              [ Ty.mkEq i (Ty.DynBvLit (Bv.ones 1 Bv.# Bv.zeros (w - 1)))
              , Ty.mkEq i' (Ty.DynBvLit (Bv.ones w))
              ]
            else Ty.BoolLit False
          ]
isDivZero :: Bool -> Bv -> Bv -> Maybe Ty.TermBool
isDivZero _s _i i' =
  Just $ Ty.mkEq i' (Ty.DynBvLit (Bv.zeros (Ty.dynBvWidth i')))

cRem = cWrapBinArith "%"
                     (const $ Left Ty.BvUrem)
                     (Ty.FpBinExpr Ty.FpRem)
                     (Just isDivZero)
                     False
cMin = undefined
cMax = undefined
noFpError
  :: forall f
   . Ty.ComputableFp f
  => Ty.Term (Ty.FpSort f)
  -> Ty.Term (Ty.FpSort f)
  -> Ty.Term (Ty.FpSort f)
noFpError = const $ const $ error "Invalid FP op"
cBitOr = cWrapBinArith "|" (const $ Right Ty.BvOr) noFpError Nothing False
cBitAnd = cWrapBinArith "&" (const $ Right Ty.BvAnd) noFpError Nothing False
cBitXor = cWrapBinArith "^" (const $ Right Ty.BvXor) noFpError Nothing False

cWrapShift name bvOp ubF a b =
  case (term $ intPromotion a, term $ intPromotion b) of
    (CInt s w i, CInt s' _w' i') -> mkCTerm
      (CInt s w $ Ty.mkDynBvBinExpr (bvOp s) i i')
      (Ty.BoolNaryExpr Ty.Or [udef a, udef b, ubF i s' i'])
    _ -> error $ unwords ["Cannot", name, "on", show a, "and", show b]

overShift i i' = Ty.mkDynBvBinPred Ty.BvUge i'
  $ Mem.bvNum True (Ty.dynBvWidth i') (fromIntegral $ Ty.dynBvWidth i)
cShl = cWrapShift "<<" (const $ Ty.BvShl) overflow
 where
  overflow i s' i'
    | s' = Ty.BoolNaryExpr
      Ty.Or
      [ overShift i i'
      , Ty.mkDynBvBinPred Ty.BvSlt i (Mem.bvNum True (Ty.dynBvWidth i) 0)
      ]
    | otherwise = overShift i i'
cShr = cWrapShift ">>" (\s -> if s then Ty.BvAshr else Ty.BvLshr) overflow
  where overflow i _s' i' = overShift i i'

cWrapUnArith
  :: String
  -> (Bv -> Bv)
  -> (  forall f
      . Ty.ComputableFp f
     => Ty.Term (Ty.FpSort f)
     -> Ty.Term (Ty.FpSort f)
     )
  -> Maybe (Bool -> Bv -> Maybe Ty.TermBool)
  -> CTerm
  -> CTerm
cWrapUnArith name bvF doubleF ubF a =
  let (t, u) = case term $ intPromotion a of
        CDouble d  -> (CDouble $ doubleF d, Nothing)
        CInt s w i -> (CInt True w $ bvF i, join $ ubF <*> pure s <*> pure i)
        _          -> error $ unwords ["Cannot do", name, "on", show a]
  in  mkCTerm t $ maybe (udef a) (binOr $ udef a) u

cPos, cNeg, cBitNot :: CTerm -> CTerm
cPos = cWrapUnArith "unary +" id id Nothing
cNeg = cWrapUnArith "unary -"
                    (Ty.mkDynBvUnExpr Ty.BvNeg)
                    (Ty.FpUnExpr Ty.FpNeg)
                    (Just notMin)
 where
  notMin s i = if s
    then
      let w      = Ty.dynBvWidth i
          intMin = Mem.bvNum True w (negate $ (2 :: Integer) ^ (w - 1))
      in  Just $ Ty.mkEq i intMin
    else Nothing
cBitNot =
  cWrapUnArith "~" (Ty.mkDynBvUnExpr Ty.BvNot) (Ty.FpUnExpr Ty.FpNeg) Nothing

cWrapBinLogical
  :: String
  -> (Ty.TermBool -> Ty.TermBool -> Ty.TermBool)
  -> CTerm
  -> CTerm
  -> CTerm
cWrapBinLogical name f a b =
  case (term $ cCast Type.Bool a, term $ cCast Type.Bool b) of
    (CBool a', CBool b') ->
      mkCTerm (CBool $ f a' b') (Ty.BoolNaryExpr Ty.Or [udef a, udef b])
    _ -> error $ unwords ["Cannot do", name, "on", show a, "and", show b]
cOr, cAnd :: CTerm -> CTerm -> CTerm
cOr = cWrapBinLogical "||" binOr
cAnd = cWrapBinLogical "&&" binAnd

cWrapUnLogical :: String -> (Ty.TermBool -> Ty.TermBool) -> CTerm -> CTerm
cWrapUnLogical name f a = case term $ cCast Type.Bool a of
  CBool a' -> mkCTerm (CBool $ f a') (udef a)
  _        -> error $ unwords ["Cannot do", name, "on", show a]

cNot :: CTerm -> CTerm
cNot = cWrapUnLogical "!" Ty.Not

cWrapCmp
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
cWrapCmp name bvF doubleF a b =
  let
    (a', b') = usualArithConversions a b
    cannot with = error $ unwords ["Cannot do", name, "with", with]
    t = case (term a', term b') of
      (CDouble x, CDouble y) -> doubleF x y
      (CFloat  x, CFloat y ) -> doubleF x y
      (CStackPtr ty addr id, CStackPtr ty' addr' id') ->
        if ty == ty' && id == id'
          then bvF False addr addr'
          else
            cannot
              "two pointers, or two pointers of different types, or pointers to different allocations"
      (CInt s w i, CInt s' w' i') | s == s' && w == w' -> bvF s i i'
      (_, _) -> cannot $ unwords [show a, "and", show b]
  in
    mkCTerm (CBool t) $ Ty.BoolNaryExpr Ty.Or [udef a, udef b]

cEq, cNe, cLt, cGt, cLe, cGe :: CTerm -> CTerm -> CTerm
cEq = cWrapCmp "==" (const Ty.mkEq) Ty.mkEq
cNe = ((.) . (.)) cNot cEq
cLt = cWrapCmp "<"
               (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSlt else Ty.BvUlt))
               (Ty.FpBinPred Ty.FpLt)
cGt = cWrapCmp ">"
               (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSgt else Ty.BvUgt))
               (Ty.FpBinPred Ty.FpGt)
cLe = cWrapCmp "<="
               (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSle else Ty.BvUle))
               (Ty.FpBinPred Ty.FpLe)
cGe = cWrapCmp ">="
               (\s -> Ty.mkDynBvBinPred (if s then Ty.BvSge else Ty.BvUge))
               (Ty.FpBinPred Ty.FpGe)

cCast :: Type.Type -> CTerm -> CTerm
cCast toTy node = case term node of
  CBool t -> case toTy of
    _ | Type.isIntegerType toTy ->
      let width = Type.numBits toTy
          cint  = CInt (Type.isSignedInt toTy) width (boolToBv t width)
      in  mkCTerm cint (udef node)
    Type.Double -> mkCTerm (CDouble $ Ty.DynUbvToFp $ boolToBv t 1) (udef node)
    Type.Float  -> mkCTerm (CFloat $ Ty.DynUbvToFp $ boolToBv t 1) (udef node)
    Type.Bool   -> node
    _           -> badCast t toTy
  CInt fromS fromW t -> case toTy of
    _ | Type.isIntegerType toTy ->
      let toW = Type.numBits toTy
          toS = Type.isSignedInt toTy
          t'  = intResize fromS toW t
          u   = udef node
        -- P 6.3.1.3.3 of the C11 standard says this is "implementation
        -- defined", not "undefined"
        -- u   = if toS && toW < fromW
        --   then binOr (udef node) (Ty.Not $ Ty.mkEq t (intResize toS fromW t'))
        --   else udef node
      in  mkCTerm (CInt toS toW t') u
    Type.Double -> mkCTerm
      (CDouble $ (if fromS then Ty.DynSbvToFp else Ty.DynUbvToFp) t)
      (udef node)
    Type.Float -> mkCTerm
      (CFloat $ (if fromS then Ty.DynSbvToFp else Ty.DynUbvToFp) t)
      (udef node)
    Type.Bool ->
      mkCTerm (CBool $ Ty.Not $ Ty.mkEq (Mem.bvNum False fromW 0) t) (udef node)
    _ -> badCast t toTy
  CDouble t -> case toTy of
    _ | Type.isIntegerType toTy ->
      -- TODO: Do this with rounding modes
      let half    = Ty.Fp64Lit 0.5
          negHalf = Ty.Fp64Lit (-0.5)
      in  mkCTerm
            ( CInt (Type.isSignedInt toTy) (Type.numBits toTy)
            $ Ty.RoundFpToDynBv
                (Type.numBits toTy)
                True
                (Ty.FpBinExpr
                  Ty.FpAdd
                  t
                  (Ty.mkIte (Ty.FpUnPred Ty.FpIsPositive t) negHalf half)
                )
            )
            (udef node)
    Type.Bool ->
      mkCTerm (CBool $ Ty.Not $ Ty.FpUnPred Ty.FpIsZero t) (udef node)
    Type.Double -> node
    Type.Float  -> mkCTerm (CFloat $ Ty.FpToFp t) (udef node)
    _           -> badCast t toTy
  CFloat t -> case toTy of
    _ | Type.isIntegerType toTy ->
      -- TODO: Do this with rounding modes
      let half    = Ty.Fp32Lit 0.5
          negHalf = Ty.Fp32Lit (-0.5)
      in  mkCTerm
            ( CInt (Type.isSignedInt toTy) (Type.numBits toTy)
            $ Ty.RoundFpToDynBv
                (Type.numBits toTy)
                True
                (Ty.FpBinExpr
                  Ty.FpAdd
                  t
                  (Ty.mkIte (Ty.FpUnPred Ty.FpIsPositive t) negHalf half)
                )
            )
            (udef node)
    Type.Bool ->
      mkCTerm (CBool $ Ty.Not $ Ty.FpUnPred Ty.FpIsZero t) (udef node)
    Type.Double -> mkCTerm (CDouble $ Ty.FpToFp t) (udef node)
    Type.Float  -> node
    _           -> badCast t toTy
  CStackPtr ty t _id -> if Type.isIntegerType toTy
    then cCast toTy $ mkCTerm (CInt False (Ty.dynBvWidth t) t) (udef node)
    else if toTy == Type.Bool
      then mkCTerm
        (CBool $ Ty.Not $ Ty.mkEq (Mem.bvNum False (Type.numBits ty) 0) t)
        (udef node)
      else if Type.isPointer toTy
        -- TODO: Not quite right: widths
        then node
        else badCast t toTy
  CArray ty id -> case toTy of
    Type.Ptr32 iTy | iTy == Type.arrayBaseType ty -> mkCTerm
      (CStackPtr toTy (Mem.bvNum False (Type.numBits toTy) 0) id)
      (udef node)
    Type.Array Nothing toBaseTy | toBaseTy == Type.arrayBaseType ty ->
      mkCTerm (CArray toTy id) (udef node)
    _ | toTy == ty -> node
    _              -> badCast node toTy
  CStruct ty _ -> case toTy of
    _ | toTy == ty -> node
    _              -> badCast node toTy
 where
  boolToBv :: Ty.TermBool -> Int -> Bv
  boolToBv b w = Ty.mkIte b (Mem.bvNum False w 1) (Mem.bvNum False w 0)
  badCast from to = error $ unwords ["Bad cast from", show from, "to", show to]

-- A ITE based on a CTerm condition.
cCond :: CTerm -> CTerm -> CTerm -> CTerm
cCond cond t f =
  let condB = asBool $ term $ cCast Type.Bool cond
      r     = cIte condB t f
  in  mkCTerm (term r) (Ty.BoolNaryExpr Ty.Or [udef cond, udef r])

-- A ITE based on an SMT boolean condition.
cIte :: Ty.TermBool -> CTerm -> CTerm -> CTerm
cIte condB t f =
  let
    (t', f') = usualArithConversions t f
    result   = case (term t', term f') of
      (CDouble tB, CDouble fB) -> CDouble $ Ty.mkIte condB tB fB
      (CFloat  tB, CFloat fB ) -> CFloat $ Ty.mkIte condB tB fB
      (CBool   tB, CBool fB  ) -> CBool $ Ty.mkIte condB tB fB
      -- TODO: not quite right.
      -- Wrong in some instances of conditional initialization.
      (CStackPtr tTy tB tId, CStackPtr fTy fB fId)
        | tTy
          == fTy
          && (  tId
             == fId
             || tId
             == Mem.stackIdUnknown
             || fId
             == Mem.stackIdUnknown
             )
        -> CStackPtr tTy (Ty.mkIte condB tB fB) tId
      (CInt s w i, CInt s' w' i') | s == s' && w == w' ->
        CInt s w $ Ty.mkIte condB i i'
      (CStruct aTy a, CStruct bTy b) | aTy == bTy ->
        CStruct aTy $ zipWith (\(f, aV) (_, bV) -> (f, cIte condB aV bV)) a b
      _ -> error $ unwords
        [ "Cannot construct conditional "
        , show condB
        , " with "
        , show t
        , " and "
        , show f
        ]
  in
    mkCTerm result (Ty.mkIte condB (udef t) (udef f))

ctermGetVars :: String -> CTerm -> Mem (Set.Set String)
ctermGetVars name t = do
  logIf "outputs" $ "Getting outputs at " ++ name ++ " : " ++ show t
  case term t of
    CBool b     -> return $ TyAlg.vars b
    CInt _ _ i  -> return $ TyAlg.vars i
    CDouble x   -> return $ TyAlg.vars x
    CFloat  x   -> return $ TyAlg.vars x
    CStruct _ l -> fmap Set.unions $ forM l $ \(fName, fTerm) ->
      ctermGetVars (structVarName name fName) fTerm
    CArray _elemTy id -> do
      size <- Mem.getSize id
      fmap Set.unions $ forM [0 .. (size - 1)] $ \i -> do
        off <-
          fmap snd
          $ cLoad
          $ cAdd (arrayToPointer t)
          $ cIntLit Type.S32
          $ toInteger i
        logIf "outputs::debug" $ unwords ["Idx", show i, ":", show off]
        let elemName = name ++ "." ++ show i
        off' <- liftAssert $ alias False elemName off
        liftAssert $ cSetValues False elemName off
        ctermGetVars (name ++ "." ++ show i) off'
    _ -> error $ "nyi: ctermGetVars: " ++ show t



ctermIsUndef :: CTerm -> Ty.TermBool
ctermIsUndef t = case term t of
  CBool{}     -> udef t
  CInt{}      -> udef t
  CDouble{}   -> udef t
  CFloat{}    -> udef t
  CStruct _ l -> Ty.BoolNaryExpr Ty.Or $ map (ctermIsUndef . snd) l
  _           -> error $ "nyi: ctermGetVars: " ++ show t

binOr :: Ty.TermBool -> Ty.TermBool -> Ty.TermBool
binOr a b = Ty.BoolNaryExpr Ty.Or [a, b]

binAnd :: Ty.TermBool -> Ty.TermBool -> Ty.TermBool
binAnd a b = Ty.BoolNaryExpr Ty.And [a, b]

cBoolLit :: Bool -> CTerm
cBoolLit b = mkCTerm (CBool $ Ty.BoolLit b) (Ty.BoolLit False)

instance Embeddable Type.Type CTerm (Maybe InMap, Bool) where
  declare   = uncurry cDeclVar
  assign    = cAssign . snd
  ite       = const $ ((.) . (.) . (.)) return cIte
  setValues = cSetValues . snd
  evaluate  = cEvaluate . snd

type CCirc a = Circify Type.Type CTerm (Maybe InMap, Bool) a

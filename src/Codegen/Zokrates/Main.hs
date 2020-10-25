{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
module Codegen.Zokrates.Main
  ( ZokratesInputs(..) ) -- FrontEndInputs instance
where

import qualified AST.Zokrates                  as A
import           AST.Util
import           Control.Monad.State.Strict
import           Codegen.Circify
import qualified Codegen.Circify.Memory        as Mem
import           Codegen.FrontEnd
import           Codegen.LangVal
import qualified Codegen.Zokrates.Type         as T
import           Codegen.Zokrates.Term
import qualified Data.BitVector                as Bv
import           Data.Bifunctor                 ( bimap )
import qualified Data.Map.Strict               as Map
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.List                      ( isInfixOf )
import qualified Data.Set                      as Set
import           GHC.TypeNats                   ( KnownNat
                                                , Nat
                                                )
import           Lens.Simple                    ( makeLenses
                                                , over
                                                , view
                                                )
import qualified IR.SMT.Assert                 as Assert
import qualified IR.SMT.TySmt                  as S
import           Parser.Zokrates                ( absolutePath )
import           System.FilePath                ( splitFileName
                                                , splitExtension
                                                )
import qualified Util.Cfg                      as Cfg
import           Util.Control
import           Util.Log

data ZState = ZState
  { _fileStack :: [FilePath] -- ^ Stack of files we're in
  , _funcs     :: Map.Map (FilePath, String) A.SFunc
  , _importMap :: Map.Map (FilePath, String) (FilePath, String)
  }
$(makeLenses ''ZState)

newtype Z n a = Z (StateT ZState (Circify T.Type (Term n) (Maybe InMap)) a)
    deriving (Functor, Applicative, Monad, MonadState ZState, MonadIO, MonadLog, Assert.MonadAssert, Mem.MonadMem, MonadCircify T.Type (Term n) (Maybe InMap), Cfg.MonadCfg, MonadDeepState (((Assert.AssertState, Mem.MemState), CircifyState T.Type (Term n) (Maybe InMap)), ZState))

emptyZState :: ZState
emptyZState =
  ZState { _fileStack = [], _funcs = Map.empty, _importMap = Map.empty }

currentPath :: Z n FilePath
currentPath =
  gets (fromMaybe (error "No current path") . listToMaybe . view fileStack)

registerImport :: FilePath -> Maybe String -> Maybe String -> Z n ()
registerImport srcPath mSrcName mDstName = do
  let srcName = fromMaybe "main" mSrcName
      dstName =
        fromMaybe (fst $ splitExtension $ snd $ splitFileName srcPath) mDstName
  curPath    <- currentPath
  absSrcPath <- if "EMBED" `isInfixOf` srcPath
    then return "EMBED"
    else liftIO $ absolutePath curPath srcPath
  logIf "import" $ show (curPath, dstName) ++ " -> " ++ show
    (absSrcPath, srcName)
  modify $ over importMap $ Map.insert (curPath, dstName) (absSrcPath, srcName)

registerFunc :: A.SFunc -> Z n ()
registerFunc f@(A.Func name _ _ _) = do
  curPath <- currentPath
  modify $ over funcs $ Map.insert (curPath, ast name) f

getFunc :: Span -> FilePath -> String -> Z n A.SFunc
getFunc s p n = fromMaybe (errSpan s $ "Cannot find function: " ++ show n)
  <$> gets (Map.lookup (p, n) . view funcs)

inFile :: FilePath -> Z n a -> Z n a
inFile f a = modify (over fileStack (f :)) *> a <* modify (over fileStack tail)

registerStruct :: KnownNat n => String -> T.Type -> Z n ()
registerStruct name s = do
  curPath <- currentPath
  logIf "struct" $ "Saving " ++ name ++ " in " ++ curPath
  liftCircify $ typedef (unwords [curPath, name]) s

getStruct :: String -> Z n (Maybe T.Type)
getStruct name = do
  cp <- currentPath
  logIf "struct" $ "Looking up " ++ name ++ " in " ++ cp
  (p, n) <- fromMaybe (cp, name)
    <$> gets (Map.lookup (cp, name) . view importMap)
  liftCircify $ untypedef (unwords [p, n])

genLit :: KnownNat n => A.SLiteral -> Term n
genLit l = case ast l of
  A.IntLit i        -> Field $ S.IntToPf $ S.IntLit i
  A.HexLit bits val -> BitInt bits $ S.DynBvLit $ Bv.bitVec bits val
  A.BoolLit b       -> Bool $ S.BoolLit b


genOp :: KnownNat n => A.Op -> Term n -> Term n -> Either String (Term n)
genOp o = case o of
  A.Plus   -> zAdd
  A.Minus  -> zSub
  A.Times  -> zMul
  A.Div    -> zDiv
  A.Pow    -> zPow
  A.Shl    -> zShl
  A.Shr    -> zShr
  A.BitAnd -> zBitAnd
  A.BitOr  -> zBitOr
  A.BitXor -> zBitXor
  A.And    -> zAnd
  A.Or     -> zOr
  A.Eq     -> zEq
  A.Neq    -> zNe
  A.Gt     -> zGt
  A.Ge     -> zGe
  A.Lt     -> zLt
  A.Le     -> zLe

genUnOp :: KnownNat n => A.UnOp -> Term n -> Either String (Term n)
genUnOp o = case o of
  A.Neg -> zNeg
  A.Not -> zNot

errSpan :: Span -> String -> a
errSpan s m = error $ m ++ "\n at: " ++ show s

fromEitherSpan :: Span -> Either String a -> a
fromEitherSpan s = either (errSpan s) id

genElemExpr :: KnownNat n => A.SElemExpr -> Z n [Term n]
genElemExpr e = do
  logIf "elemExpr" $ "ElemExpr" ++ show e
  case ast e of
    A.Spread   a -> ok . zSpread <$> genExpr a
    A.ElemExpr a -> (: []) <$> genExpr a
  where ok = fromEitherSpan (ann e)

genConstInt :: KnownNat n => A.SExpr -> Z n Int
genConstInt e = fromEitherSpan (ann e) . zConstInt <$> genExpr e

genBuiltinCall
  :: KnownNat n => String -> [Term n] -> Z n (Either String (Term n))
genBuiltinCall name actArgs = case name of
  "to_bits"   -> oneArgBuiltin zU32toBits
  "from_bits" -> oneArgBuiltin zU32fromBits
  "unpack"    -> oneArgBuiltin zFieldtoBits
  _           -> return $ Left $ "Unknown builtin fun: " ++ name
 where
  oneArgBuiltin f = case actArgs of
    [a] -> return $ f a
    _ ->
      return $ Left $ "Wrong argument count to " ++ name ++ ": " ++ show actArgs

genExpr :: KnownNat n => A.SExpr -> Z n (Term n)
genExpr e = case ast e of
  A.Ident s -> liftCircify $ ssaValAsTerm "ident" <$> getTerm (SLVar $ ast s)
  A.LitExpr l      -> return $ genLit l
  A.IfElse c t f   -> ok <$> liftM3 zCond (genExpr c) (genExpr t) (genExpr f)
  A.Bin    o a b   -> ok <$> liftM2 (genOp o) (genExpr a) (genExpr b)
  A.Un   o    a    -> ok . genUnOp o <$> genExpr a
  A.Call name args -> do
    args' <- mapM genExpr args
    cp    <- currentPath
    logIf "call" $ "Call: " ++ ast name ++ " at " ++ cp
    (p, n) <- fromMaybe (cp, ast name)
      <$> gets (Map.lookup (cp, ast name) . view importMap)
    if "EMBED" == p
      then ok <$> genBuiltinCall (ast name) args'
      else inFile p $ do
        A.Func _ fArgs retTy body <- getFunc (ann name) p n
        retTy'                    <- genType retTy
        liftCircify $ pushFunction (ast name) (Just retTy')
        forM_ (zip args' fArgs) $ \(aArg, (_, ty, argName)) -> do
          ty' <- genType ty
          liftCircify $ declareInitVar (ast argName) ty' (Base aArg)
        genBlock body
        mRet <- liftCircify popFunction
        return $ fromMaybe (errSpan (ann e) "Missing return") mRet
  A.Array elems -> ok . zArray . concat <$> mapM genElemExpr elems
  A.Repeat elem cnt ->
    ok . zArray <$> liftM2 replicate (genConstInt cnt) (genExpr elem)
  A.Member s f -> ok . zFieldGet (ast f) <$> genExpr s
  A.Idx    a i -> ok <$> liftM2 zArrayGet (genExpr i) (genExpr a)
  A.Slice a bnds ->
    ok <$> liftM2 (uncurry . zSlice) (genExpr a) (genBounds bnds)
  A.Struct name fs -> Struct (ast name) . Map.fromList <$> forM
    fs
    (\(n, f) -> (ast n, ) <$> genExpr f)
  where ok = fromEitherSpan (ann e)

genBounds :: KnownNat n => A.SBounds -> Z n (Maybe Int, Maybe Int)
genBounds b = case ast b of
  A.Bounds s e -> liftM2 (,) (mapM genConstInt s) (mapM genConstInt e)

genStmt :: forall n . KnownNat n => A.SStmt -> Z n ()
genStmt s = case ast s of
  A.For ty v bnds body -> do
    let boundOr = fromMaybe (errSpan (ann bnds) "Must have bounds for loop")
        v'      = ast v
        fpConst = Base . Field . S.IntToPf @n . S.IntLit . toInteger
    (start, end) <- bimap boundOr boundOr <$> genBounds bnds
    ty'          <- genType ty
    unless (ty' == T.Field) $ errSpan (ann s) "Loop idx must be field"
    scoped $ do
      liftCircify $ declareInitVar v' ty' (fpConst start)
      forM_ [start .. (end - 1)] $ \i -> do
        liftCircify $ void $ ssaAssign (SLVar v') (fpConst i)
        scoped $ genBlock body
  A.Assert e ->
    (ok . zBool <$> genExpr e) >>= Assert.liftAssert . Assert.assert
  A.Declare ty v e -> do
    e'  <- Base <$> genExpr e
    ty' <- genType ty
    liftCircify $ declareInitVar (ast v) ty' e'
  A.Assign v e -> join $ liftM2 (setLVal $ ann s) (genLVal v) (genExpr e)
  A.Return e   -> genExpr e >>= liftCircify . doReturn
  where ok = fromEitherSpan (ann s)

genLVal :: KnownNat n => A.SExpr -> Z n (LVal n)
genLVal e = case ast e of
  A.Ident n    -> return $ Var $ SLVar $ ast n
  A.Member s f -> flip Member (ast f) <$> genLVal s
  A.Idx    a i -> liftM2 Idx (genLVal a) (genExpr i)
  t            -> errSpan (ann e) $ "Cannot generate LValue from " ++ show t

setLVal :: KnownNat n => Span -> LVal n -> Term n -> Z n ()
setLVal s lval term = modLVal lval (const term)
 where
  modLVal :: KnownNat n => LVal n -> (Term n -> Term n) -> Z n ()
  modLVal lval f = case lval of
    Var v ->
      liftCircify
        $   (f . ssaValAsTerm "var set" <$> getTerm v)
        >>= (void . ssaAssign v . Base)
    Member lval' field ->
      modLVal lval' $ \t -> ok $ zFieldSet field (f $ ok $ zFieldGet field t) t
    Idx lval' idx ->
      modLVal lval' $ \t -> ok $ zArraySet idx (f $ ok $ zArrayGet idx t) t
  ok = fromEitherSpan s

data LVal n = Var SsaLVal
            | Member (LVal n) String
            | Idx (LVal n) (Term n)

genType :: A.SType -> Z n T.Type
genType ty = case ast ty of
  A.Type ds p ->
    let b = case ast p of
          A.U8    -> T.Uint 8
          A.U16   -> T.Uint 16
          A.U32   -> T.Uint 32
          A.Bool  -> T.Bool
          A.Field -> T.Field
    in  return $ foldr T.Array b $ map (fromIntegral . ast) ds
  A.UStruct n ->
    fromMaybe (errSpan (ann ty) "Unknown struct") <$> getStruct (ast n)

genBlock :: KnownNat n => A.SBlock -> Z n ()
genBlock b = case ast b of
  A.Block b' -> mapM_ genStmt b'

genItem :: KnownNat n => A.SItem -> Z n ()
genItem i = case ast i of
  A.SDef name fields -> do
    tys <- forM fields $ \(ty, fName) -> (ast fName, ) <$> genType ty
    registerStruct (ast name) (T.Struct (ast name) $ Map.fromList tys)
  A.Import path srcN dstN ->
    registerImport (ast path) (ast <$> srcN) (ast <$> dstN)
  A.FuncItem f -> registerFunc f

genEntryFunc :: KnownNat n => A.SFunc -> Z n ()
genEntryFunc (A.Func name args retTy body) = do
  logIf "entry" $ "Entry function: " ++ ast name
  retTy' <- genType retTy
  liftCircify $ pushFunction (ast name) (Just retTy')
  forM_ args $ \(isPrivate, ty, argName) -> do
    ty' <- genType ty
    liftCircify $ declareVar True (ast argName) ty'
    unless isPrivate $ do
      t <- liftCircify $ getTerm (SLVar $ ast argName)
      forM_ (Set.toList $ zTermVars (ast argName) $ ssaValAsTerm "input" t)
        $ Assert.liftAssert
        . Assert.publicize
  genBlock body
  mRet <- liftCircify popFunction
  forM_ mRet $ \rv ->
    forM_ (Set.toList $ zTermVars "return" rv)
      $ Assert.liftAssert
      . Assert.publicize

genFiles :: KnownNat n => A.SFiles -> Z n ()
genFiles fs =
  forM_ (Map.toAscList fs) $ \(path, items) -> inFile path $ mapM_ genItem items

data ZokratesInputs (n :: Nat) = ZokratesInputs String FilePath A.SFiles (Maybe InMap)

instance forall n. KnownNat n => FrontEndInputs (ZokratesInputs n) where
  compile (ZokratesInputs fnName path files inMap) =
    let Z act = inFile path $ do
          genFiles @n files
          f <- getFunc nullSpan path fnName
          genEntryFunc f
    in  do
          whenJust inMap $ const Assert.initValues
          void $ runCircify inMap $ runStateT act emptyZState

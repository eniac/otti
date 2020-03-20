{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Codegen.Circom ( genExpr
                      , genStatement
                      , genStatements
                      , genMain
                      , genMainCtx
                      , lcZero
                      , Ctx(..)
                      , Term(..)
                      , LTerm(..)
                      , Signal(..)
                      , Constraint
                      , LC
                      , ctxStore
                      , ctxGet
                      , ctxAddConstraint
                      , ctxWithEnv
                      ) where

import           AST.Circom                 as AST
import           Codegen.Circom.Constraints (Constraints)
import qualified Codegen.Circom.Constraints as CS
import           Codegen.Circom.Term        as Term
import qualified Data.Bits                  as Bits
import           Data.Field.Galois          (Prime, fromP, toP)
import           Data.List                  (intercalate)
import qualified Data.Map.Strict            as Map
import           Data.Maybe                 as Maybe
import           Data.Proxy                 (Proxy (Proxy))
import qualified Data.Set                   as Set
import qualified Data.Tuple                 as Tuple
import           Debug.Trace                (trace)
import           GHC.TypeLits.KnownNat
import           GHC.TypeNats
import qualified IR.TySmt                   as Smt

intLog2 :: Integral a => a -> a
intLog2 n =
    if n <= fromInteger 1 then
        fromInteger 0
    else
        fromInteger 1 + intLog2 (n `div` fromInteger 2)

-- XXX(HACK): Log2 0 is actually undefined, but who wants to deal with that?
--            we treat it as 0, even though the type systems rejects it.
instance KnownNat x => KnownNat1 $(nameToSymbol ''Log2) x where
  natSing1 = SNatKn (intLog2 (natVal (Proxy @x)))
  {-# INLINE natSing1 #-}



-- Given a value, and dimensions, produces a multi-d array of size given by
-- dimensions, containing copies of the value.
termMultiDimArray :: KnownNat k => Term k -> [Term k] -> Term k
termMultiDimArray = foldr (\d acc -> case d of
        Base (Scalar n, _) -> Array $ replicate (fromIntegral $ fromP n) acc
        _                  -> error $ "Illegal dimension " ++ show d
    )

-- Given a signal name and dimensions, produces a multi-d array containing the
-- consituent signals
--
-- Kind of like "a" [2, 1] to [["a.0.0"], ["a.1.0"]]
termSignalArray :: forall k. KnownNat k => Ctx k -> String -> [Term k] -> (Ctx k, Term k)
termSignalArray ctx name dim = helper ctx name [] (integerizeDims dim)
  where
    integerizeDims :: [Term k] -> [Int]
    integerizeDims [] = []
    integerizeDims (t:ts) = case t of
        Base (Scalar n, _) -> fromIntegral (fromP n) : integerizeDims ts
        _ -> error $ "Illegal dimension " ++ show t

    helper :: Ctx k -> String -> [Int] -> [Int] -> (Ctx k, Term k)
    helper ctx name location dims =
      case dims of
        [] -> (ctx', Base (Sig s, Smt.Var (show i)))
          where
            i = nextSignal ctx
            s = SigLocal name (reverse location)
            ctx' = ctx { nextSignal = nextSignal ctx + 1, numberToSignal = (show i, s) : (numberToSignal ctx) }
        n : rest -> (ctx', Array (reverse ts))
          where
            (ctx', ts) = foldl folder (ctx, []) [0..(n-1)]
            folder :: (Ctx k, [Term k]) -> Int -> (Ctx k, [Term k])
            folder (ctxAcc, tAcc) i =
              let (ctxAcc', t') = helper ctxAcc name (i:location) rest in
                (ctxAcc', t' : tAcc)


wireGenTimeConst :: WireBundle k -> Bool
wireGenTimeConst t = case t of
    Sig {}       -> False
    Quadratic {} -> False
    Scalar {}    -> True
    Linear {}    -> False
    Other        -> False

termGenTimeConst :: Term k -> Bool
termGenTimeConst t = case t of
    Base (b, _) -> wireGenTimeConst b
    Array a     -> all termGenTimeConst a
    Struct c    -> all termGenTimeConst $ env c

genGetUnMutOp :: KnownNat k => UnMutOp -> Term k -> Term k
genGetUnMutOp op = case op of
    PreInc  -> (+ Base (fromInteger 1))
    PostInc -> (+ Base (fromInteger 1))
    PreDec  -> (+ Base (fromInteger (-1)))
    PostDec -> (+ Base (fromInteger (-1)))
  where

genIndexedIdent :: KnownNat k => IndexedIdent -> Ctx k -> (LTerm, Ctx k)
genIndexedIdent (i, dims) ctx =
  foldl (\(l, c) d -> let (c', t) = genExpr c d in (attachDim t l, c'))
        (LTermIdent i, ctx)
        dims
  where
    attachDim :: KnownNat k => Term k -> LTerm -> LTerm
    attachDim dimTerm lTerm  = case dimTerm of
      Base (Scalar d, _) -> LTermIdx lTerm (fromIntegral $ fromP d)
      d -> error $ "Non-scalar " ++ show d ++ " as index in " ++ show dims


genLocation :: KnownNat k => Ctx k -> Location -> (Ctx k, LTerm)
genLocation ctx loc = case loc of
    LocalLocation a -> Tuple.swap $ genIndexedIdent a ctx
    ForeignLocation a b -> (c'', embed at bt)
      where
        (at, c') = genIndexedIdent a ctx
        (bt, c'') = genIndexedIdent b c'
        embed item (LTermIdent s) = LTermPin item s
        embed item (LTermIdx x i) = LTermIdx (embed item x) i
        embed item (LTermPin x p) = LTermPin (embed item x) p

type PfTerm k = Smt.Term (Smt.PfSort k)
type IntTerm = Smt.Term Smt.IntSort
type BoolTerm = Smt.Term Smt.BoolSort

-- Lifts a fun: Integer -> Integer -> Integer to one that operates over gen-time constant
-- terms
liftToBaseTerm :: KnownNat k =>
    (Integer -> Integer -> Integer) ->
    (PfTerm k -> PfTerm k -> PfTerm k) ->
    BaseTerm k ->
    BaseTerm k ->
    BaseTerm k
liftToBaseTerm ft fsmt (sa, sb) (ta, tb) =
    ( case (sa, ta) of
        (Scalar c1 , Scalar c2) -> Scalar $ toP $ ft (fromP c1) (fromP c2)
        _                       -> Other
    , fsmt sb tb
    )

liftToTerm :: KnownNat k =>
    String ->
    (Integer -> Integer -> Integer) ->
    (PfTerm k -> PfTerm k -> PfTerm k) ->
    Term k ->
    Term k ->
    Term k
liftToTerm name f fsmt s t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
    (a@Struct {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on struct term " ++ show a
    (Base a, Base b) -> Base $ liftToBaseTerm f fsmt a b
    (l, r) -> liftToTerm name f fsmt r l

liftBvToTerm :: forall k. KnownNat k =>
    String ->
    (Integer -> Integer -> Integer) ->
    Smt.BvBinOp ->
    Term k ->
    Term k ->
    Term k
liftBvToTerm name f op =
    liftToTerm name
               f
               (\a b -> Smt.IntToPf @k $ Smt.BvToInt $
                        -- Note that `k` is really too big, but it would take
                        -- moew work to convince GHC that Log2 k + 1 is a
                        -- KnownNat.
                        Smt.BvBinExpr op (Smt.IntToBv @(Log2 k + 1) $ Smt.PfToInt a)
                                         (Smt.IntToBv @(Log2 k + 1) $ Smt.PfToInt b))

liftIntFnToTerm :: forall k. KnownNat k =>
    String ->
    (Integer -> Integer -> Integer) ->
    (IntTerm -> IntTerm -> IntTerm) ->
    Term k ->
    Term k ->
    Term k
liftIntFnToTerm name f g =
    liftToTerm name
               f
               (\a b -> Smt.IntToPf $
                        g (Smt.PfToInt a)
                          (Smt.PfToInt b))

liftIntOpToTerm :: KnownNat k => String -> (Integer -> Integer -> Integer) -> Smt.IntBinOp -> Term k -> Term k -> Term k
liftIntOpToTerm name f op = liftIntFnToTerm name f (Smt.IntBinExpr op)

-- Lifts a fun: Integer -> Integer -> Bool to one that operates over gen-time constant
-- terms
liftCmpToTerm :: forall k. KnownNat k =>
    String ->
    (Integer -> Integer -> Bool) ->
    Smt.IntBinPred ->
    Term k ->
    Term k ->
    Term k
liftCmpToTerm name f g =
    liftIntFnToTerm name (\a b -> if f a b then 1 else 0)
                         (\a b -> Smt.BoolToInt $ Smt.IntBinPred g a b)

-- Lifts a fun: Bool -> Bool -> Bool to one that operates over gen-time
-- constant terms
liftBoolFnToTerm :: forall k. KnownNat k =>
    String ->
    (Bool -> Bool -> Bool) ->
    (BoolTerm -> BoolTerm -> BoolTerm) ->
    Term k ->
    Term k ->
    Term k
liftBoolFnToTerm name f g =
    liftToTerm name (\a b -> if f (a /= 0) (b /= 0) then 1 else 0)
                    (\a b -> Smt.IntToPf $ Smt.BoolToInt $ g
                        (Smt.PfBinPred Smt.PfNe z a)
                        (Smt.PfBinPred Smt.PfNe z b))
    where
        z = Smt.IntToPf $ Smt.IntLit 0

-- Lifts a fun: Integer -> Integer to one that operates over gen-time constant terms
liftUnToBaseTerm :: KnownNat k =>
    (Integer -> Integer) ->
    (PfTerm k -> PfTerm k) ->
    BaseTerm k ->
    BaseTerm k
liftUnToBaseTerm f fsmt (sa, sb) =
    ( case sa of
        Scalar c1 -> (Scalar . toP . f . fromP) c1
        _         -> Other
    , fsmt sb
    )


liftUnToTerm :: KnownNat k =>
    String ->
    (Integer -> Integer) ->
    (PfTerm k -> PfTerm k) ->
    Term k ->
    Term k
liftUnToTerm name f fsmt t = case t of
    a@Array {} -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
    a@Struct {} -> error $ "Cannot perform operation \"" ++ name ++ "\" on struct term " ++ show a
    Base a -> Base $ liftUnToBaseTerm f fsmt a

genExpr :: forall k. KnownNat k => Ctx k -> Expr -> (Ctx k, Term k)
genExpr ctx expr = case expr of
    NumLit i -> (ctx, Base $ fromInteger $ fromIntegral i)
    ArrayLit es -> (ctx', Array ts)
      where
        (ctx', ts) = genExprs ctx es
    BinExpr op l r ->
      (ctx'', case op of
        Add    -> l' + r'
        Sub    -> l' - r'
        Mul    -> l' * r'
        Div    -> l' / r'
        IntDiv -> liftIntOpToTerm "//" div  Smt.IntDiv l' r'
        Mod    -> liftIntOpToTerm "%"  mod  Smt.IntMod l' r'
        Lt     -> liftCmpToTerm "<"    (<)  Smt.IntLt  l' r'
        Gt     -> liftCmpToTerm ">"    (>)  Smt.IntGt  l' r'
        Le     -> liftCmpToTerm "<="   (<=) Smt.IntLe  l' r'
        Ge     -> liftCmpToTerm "<="   (>=) Smt.IntGe  l' r'
        Eq     -> liftCmpToTerm "=="   (==) Smt.IntEq  l' r'
        Ne     -> liftCmpToTerm "!="   (/=) Smt.IntNe  l' r'
        And    -> liftBoolFnToTerm "&&" (&&)
            (\a b -> Smt.BoolNaryExpr Smt.And [a, b]) l' r'
        Or     -> liftBoolFnToTerm "||" (||)
            (\a b -> Smt.BoolNaryExpr Smt.Or [a, b]) l' r'
        Shl    -> liftIntOpToTerm "<<" (liftShiftToInt Bits.shiftL) Smt.IntShl l' r'
        Shr    -> liftIntOpToTerm ">>" (liftShiftToInt Bits.shiftR) Smt.IntShr l' r'
        BitAnd -> liftBvToTerm "&" (Bits..&.) Smt.BvAnd l' r'
        BitOr  -> liftBvToTerm "|" (Bits..|.) Smt.BvOr  l' r'
        BitXor -> liftBvToTerm "^" Bits.xor   Smt.BvXor l' r'
        Pow    -> liftIntOpToTerm "**" (^) Smt.IntPow l' r')
      where
        (ctx', l') = genExpr ctx l
        (ctx'', r') = genExpr ctx' r
        liftShiftToInt :: (Integer -> Int -> Integer) -> Integer -> Integer -> Integer
        liftShiftToInt a b c = a b (fromIntegral c)
    UnExpr op e ->
        case op of
            UnNeg -> (ctx', - t)
            Not -> (ctx', liftUnToTerm "!" (\c -> if c /= 0 then 0 else 1)
                (Smt.IntToPf . Smt.BoolToInt . Smt.Not . Smt.PfBinPred Smt.PfNe z) t)
              where
                z = Smt.IntToPf $ Smt.IntLit 0
            UnPos -> (ctx', (case t of
                Array ts     -> Base $ fromInteger $ fromIntegral $ length ts
                Struct c     -> Base $ fromInteger $ fromIntegral $ Map.size $ env c
                _ -> t))
            BitNot -> error "NYI" -- The sematics of this are unclear.
        where
            (ctx', t) = genExpr ctx e
    UnMutExpr op loc -> genUnExpr ctx op loc
    Ite c l r ->
        case condT of
            Base (Scalar 0, _)     -> (ctx''', caseF)
            Base (Scalar _, _)     -> (ctx''', caseT)
            Base (_, v)            ->
                case (caseT, caseF) of
                    (Base t, Base f) -> (ctx''', Base (Other, Smt.Ite (Smt.PfBinPred Smt.PfNe z v) (snd t) (snd f)))
                    (t, f) -> error $ "Cannot evalate a ternary as " ++ show t ++ " or " ++ show f
            t -> error $ "Cannot condition on term " ++ show t
        where
            (ctx', condT) = genExpr ctx c
            (ctx'', caseT) = genExpr ctx' l
            (ctx''', caseF) = genExpr ctx'' r
            z = Smt.IntToPf $ Smt.IntLit 0
    LValue loc ->
            (ctx', ctxGet ctx' lt)
            -- TODO(aozdemir): enforce ctx' == ctx for sanity?
        where (ctx', lt) = genLocation ctx loc
    Call name args -> if all termGenTimeConst actualArgs then
            if isFn then
                (ctx', Maybe.fromMaybe
                    (error $ "The function " ++ name ++ " did not return")
                    (returning postCtx)
                )
            else
                (ctx' { nextSignal = nextSignal postCtx }, ctxToStruct postCtx)
        else
            (ctx', error "NYI")
        where
            postCtx = genStatements newCtx block
            newCtx = ctx' { env = Map.fromList (zip formalArgs actualArgs)
                          , constraints = CS.empty
                          }
            (isFn, formalArgs, block) = ctxGetCallable ctx name
            (ctx', actualArgs) = genExprs ctx args


genUnExpr :: KnownNat k => Ctx k -> UnMutOp -> Location -> (Ctx k, Term k)
genUnExpr ctx op loc = case op of
    PostInc -> (ctx'', term)
    PreInc  -> (ctx'', term')
    PostDec -> (ctx'', term)
    PreDec  -> (ctx'', term')
    where
        -- TODO(aozdemir): enforce ctx' == ctx for sanity?
        (ctx', lval) = genLocation ctx loc
        term = ctxGet ctx' lval
        term' = genGetUnMutOp op term
        ctx'' = ctxStore ctx' lval term'

genExprs :: KnownNat k => Ctx k -> [Expr] -> (Ctx k, [Term k])
genExprs ctx = foldl (\(c, ts) e -> let (c', t) = genExpr c e in (c', ts ++ [t])) (ctx, [])

genStatements :: KnownNat k => Ctx k -> [Statement] -> Ctx k
genStatements = foldl (\c s -> if isJust (returning c) then c else genStatement c s)

genStatement :: forall k. KnownNat k => Ctx k -> Statement -> Ctx k
genStatement ctx statement = case statement of
    -- Note, signals are immutable.
    Assign loc expr -> ctxStore ctx'' lval term
        where
            (ctx', lval) = genLocation ctx loc
            (ctx'', term) = genExpr ctx' expr
    -- TODO Not quite right: evals twice
    OpAssign op loc expr -> genStatement ctx $ Assign loc (BinExpr op (LValue loc) expr)
    Constrain l r ->
        case zeroTerm of
            Base (Scalar 0, _) -> ctx''
            Base (Sig s, _) -> ctxAddConstraint ctx'' (lcZero, lcZero, sigAsLC s)
            Base (Linear lc, _) -> ctxAddConstraint ctx'' (lcZero, lcZero, lc)
            Base (Quadratic a b c, _) -> ctxAddConstraint ctx'' (a, b, c)
            _ -> error $ "Cannot constain " ++ show zeroTerm ++ " to zero"
        where
            (ctx', lt) = genExpr ctx l
            (ctx'', rt) = genExpr ctx' r
            zeroTerm = lt - rt
    -- TODO Not quite right: evals twice
    AssignConstrain l e -> genStatements ctx [Assign l e, Constrain (LValue l) e]
    VarDeclaration name dims ini -> case ini of
            Just e  -> genStatement ctx'' $ Assign (LocalLocation (name, [])) e
            Nothing -> ctx''
        where
            ctx'' = ctxInit ctx' name (termMultiDimArray (Base (Scalar 0, z)) ts)
            (ctx', ts) = genExprs ctx dims
            z = Smt.IntToPf $ Smt.IntLit 0
    SigDeclaration name kind dims -> ctxInit ctx''' name t
        where
            ctx''' = Set.fold sigAdder ctx'' (termSignals t)
            sigAdder = if AST.isPublic kind then Term.ctxAddPublicSig else Term.ctxAddPrivateSig
            (ctx'', t) = termSignalArray ctx' name tdims
            (ctx', tdims) = genExprs ctx dims
    SubDeclaration name dims ini -> case ini of
            Just e  -> genStatement ctx'' $ Assign (LocalLocation (name, [])) e
            Nothing -> ctx''
        where
            ctx'' = ctxInit ctx' name (termMultiDimArray (Base (Scalar 0, z)) ts)
            (ctx', ts) = genExprs ctx dims
            z = Smt.IntToPf $ Smt.IntLit 0
    If cond true false -> case tcond of
            Base (Scalar 0, _) -> genStatements ctx' (concat $ Maybe.maybeToList false)
            Base (Scalar _, _) -> genStatements ctx' true
            _ -> error $ "Invalid conditional term " ++ show tcond ++ " in " ++ show cond
        where
            (ctx', tcond) = genExpr ctx cond
    While cond block -> case tcond of
            Base (Scalar 0, _) -> ctx'
            Base (Scalar _, _) -> genStatement (genStatements ctx block) (While cond block)
            _ -> error $ "Invalid conditional term " ++ show tcond ++ " in " ++ show cond
        where
            (ctx', tcond) = genExpr ctx cond
    For ini cond step block -> genStatements ctx [ini, While cond (block ++ [step])]
    DoWhile block expr -> genStatements ctx (block ++ [While expr block])
    Compute _ -> ctx
    Ignore e -> fst $ genExpr ctx e
    Log e -> trace (show e ++ ": " ++ show t) ctx'
        where (ctx', t) = genExpr ctx e
    Return e -> ctx' { returning = Just t }
        where
            (ctx', t) = genExpr ctx e

genMain :: KnownNat k => MainCircuit -> Integer -> Constraints (Prime k)
genMain m order = constraints $ genMainCtx m order

genMainCtx :: KnownNat k => MainCircuit -> Integer -> Ctx k
genMainCtx m order =
        ctx'
    where
        ctx' = genStatement ctxEmpty (SubDeclaration "main" [] (Just (main m)))
        ctxEmpty = (ctxWithEnv Map.empty order) {
            callables = Map.union
                (Map.map (\(p, b) -> (False, p, b)) (templates m))
                (Map.map (\(p, b) -> (True , p, b)) (functions m))
        }

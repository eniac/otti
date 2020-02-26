{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
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

import qualified IR.TySmt                   as Smt
import           AST.Circom                 as AST
import qualified Codegen.Circom.Constraints as CS
import           Codegen.Circom.Constraints (Constraints)
import           Codegen.Circom.Context     as Context
import           Codegen.Circom.Term        as Term
import qualified Data.Bits                  as Bits
import           Data.Field.Galois          (Prime, PrimeField, fromP, toP)
import           Data.List                  as List
import qualified Data.Map.Strict            as Map
import qualified Data.Set                   as Set
import           Data.Maybe                 as Maybe
import qualified Data.Sequence              as Sequence
import           Debug.Trace                (trace)
import           Data.Proxy                 (Proxy (Proxy))
import           GHC.TypeLits               



-- Given a value, and dimensions, produces a multi-d array of size given by
-- dimensions, containing copies of the value.
termMultiDimArray :: KnownNat k => Term k -> [Term k] -> Term k
termMultiDimArray = foldr (\d acc -> case d of
        Base (Scalar n, _) -> Array $ replicate (fromIntegral $ fromP n) acc
        _        -> error $ "Illegal dimension " ++ show d
    )

data DimArray = DABase String [Int] | DARec [DimArray]

-- Given a signal name and dimensions, produces a multi-d array containing the
-- consituent signals
--
-- Kind of like "a" [2, 1] to [["a.0.0"], ["a.1.0"]]
termSignalArray :: KnownNat k => String -> [Term k] -> Term k
termSignalArray name dim = case dim of
    [] -> sigAsSigTerm (SigLocal name [])
    (Base (Scalar n, _)):rest ->
        Array $ [ mapSignalsInTerm (subscriptSignal (i - 1)) (sSigString (i - 1)) rec | i <- [1..(fromP n)] ]
        where
            subscriptSignal idx (SigLocal name idxs) = SigLocal name (fromIntegral idx:idxs)
            subscriptSignal idx  SigForeign {}       = error "Unreachable"
            sSigString idx s = intercalate "." (t: ("[" ++ show idx ++ "]") : ts)
                where
                  t:ts = split '.' s
            split :: Eq a => a -> [a] -> [[a]]
            split d [] = []
            split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s
            rec = termSignalArray name rest
    (t:ts) -> error $ "Illegal dimension " ++ show t

wireGenTimeConst :: WireBundle k -> Bool
wireGenTimeConst t = case t of
    Sig {}       -> False
    Quadratic {} -> False
    Scalar {}    -> True
    Linear {}    -> False
    Other        -> False

termGenTimeConst :: Term k -> Bool
termGenTimeConst t = case t of
    Base (b, _)  -> wireGenTimeConst b
    Array a      -> all termGenTimeConst a
    Struct map _ -> all termGenTimeConst map

genGetUnMutOp :: KnownNat k => UnMutOp -> Term k -> Term k
genGetUnMutOp op = case op of
    PreInc  -> (+ Base (fromInteger 1))
    PostInc -> (+ Base (fromInteger 1))
    PreDec  -> (+ Base (fromInteger (-1)))
    PostDec -> (+ Base (fromInteger (-1)))
  where

genLocation :: KnownNat k => Ctx k -> Location -> (Ctx k, LTerm)
genLocation ctx loc = case loc of
    Ident s -> (ctx, LTermIdent s)
    Pin loc' pin -> (ctx', LTermPin lt pin)
        where (ctx', lt) = genLocation ctx loc'
    Index loc' ie -> case iterm of
            Base (Scalar i, _) -> (ctx'', LTermIdx lt (fromIntegral $ fromP i))
            i -> error $ "Non-scalar " ++ show i ++ " as index in " ++ show loc
        where
            (ctx', lt) = genLocation ctx loc'
            (ctx'', iterm) = genExpr ctx' ie

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
        _ -> Other
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
                        Smt.BvBinExpr op (Smt.IntToBv @k $ Smt.PfToInt a)
                                         (Smt.IntToBv @k $ Smt.PfToInt b))
    where
        o = natVal (Proxy :: Proxy k)
        bits = (floor . logBase 2 . fromIntegral) o + 1

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
        _ -> Other
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
        IntDiv -> liftIntOpToTerm "//" div Smt.IntDiv l' r'
        Mod    -> liftIntOpToTerm "%"  mod Smt.IntMod l' r'
        Lt     -> liftCmpToTerm "<"   (<)  Smt.IntLt l' r'
        Gt     -> liftCmpToTerm ">"   (>)  Smt.IntGt l' r'
        Le     -> liftCmpToTerm "<="  (<=) Smt.IntLe l' r'
        Ge     -> liftCmpToTerm "<="  (>=) Smt.IntGe l' r'
        Eq     -> liftCmpToTerm "=="  (==) Smt.IntEq l' r'
        Ne     -> liftCmpToTerm "!="  (/=) Smt.IntNe l' r'
        And    -> liftBoolFnToTerm "&&" (&&)
            (\a b -> Smt.BoolNaryExpr Smt.And [a, b]) l' r'
        Or     -> liftBoolFnToTerm "||" (||)
            (\a b -> Smt.BoolNaryExpr Smt.Or [a, b]) l' r'
        Shl    -> liftIntOpToTerm "<<" (liftShiftToInt Bits.shiftL) Smt.IntShl l' r'
        Shr    -> liftIntOpToTerm "<<" (liftShiftToInt Bits.shiftR) Smt.IntShr l' r'
        BitAnd -> liftBvToTerm "&" (Bits..&.) Smt.BvAnd l' r'
        BitOr  -> liftBvToTerm "|" (Bits..|.) Smt.BvOr  l' r'
        BitXor -> liftBvToTerm "^" Bits.xor   Smt.BvXor l' r'
        Pow    -> liftIntOpToTerm "**" (^) Smt.IntPow l' r')
      where
        (ctx', l') = genExpr ctx l
        (ctx'', r') = genExpr ctx' r
        liftShiftToInt :: (Integer -> Int -> Integer) -> Integer -> Integer -> Integer
        liftShiftToInt s l r = s l (fromIntegral r)
    UnExpr op e ->
        case op of
            UnNeg -> (ctx', - t)
            Not -> (ctx', liftUnToTerm "!" (\c -> if c /= 0 then 0 else 1)
                (Smt.IntToPf . Smt.BoolToInt . Smt.Not . Smt.PfBinPred Smt.PfNe z) t)
              where
                z = Smt.IntToPf $ Smt.IntLit 0
            UnPos -> (ctx', (case t of
                Array ts     -> Base $ fromInteger $ fromIntegral $ length ts
                Struct ts _  -> Base $ fromInteger $ fromIntegral $ Map.size ts
                t -> t))
            BitNot -> error "NYI" -- The sematics of this are unclear.
        where
            (ctx', t) = genExpr ctx e
    UnMutExpr op loc -> genUnExpr ctx op loc
    Ite c l r ->
        case condT of
            Base (Scalar 0, _)     -> (ctx''', caseF)
            Base (Scalar _, _)     -> (ctx''', caseT)
            Base (s, v)            ->
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
                (ctx', ctxToStruct postCtx)
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
genExprs c = foldl (\(c, ts) e -> let (c', t) = genExpr c e in (c', ts ++ [t])) (c, [])

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
    VarDeclaration name dims init -> case init of
            Just e  -> genStatement ctx'' $ Assign (Ident name) e
            Nothing -> ctx''
        where
            ctx'' = ctxInit ctx' name (termMultiDimArray (Base (Scalar 0, z)) ts)
            (ctx', ts) = genExprs ctx dims
            z = Smt.IntToPf $ Smt.IntLit 0
    SigDeclaration name kind dims -> ctxInit ctx'' name t
        where
            ctx'' = Set.fold sigAdder ctx' (termSignals t)
            sigAdder = if AST.isPublic kind then Context.ctxAddPublicSig else Context.ctxAddPrivateSig
            t = termSignalArray name tdims
            (ctx', tdims) = genExprs ctx dims
    SubDeclaration name dims init -> case init of
            Just e  -> genStatement ctx'' $ Assign (Ident name) e
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
    For init cond step block -> genStatements ctx [init, While cond (block ++ [step])]
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

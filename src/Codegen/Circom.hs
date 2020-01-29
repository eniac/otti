{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Codegen.Circom ( genExpr
                      , genStatement
                      , genStatements
                      , genMain
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

import           AST.Circom             as AST
import           Codegen.Circom.Context as Context
import           Codegen.Circom.Term    as Term
import qualified Data.Bits              as Bits
import qualified Data.Map.Strict        as Map
import           Data.Maybe             as Maybe
import qualified Data.Sequence          as Sequence
import           Debug.Trace            (trace)
import           Data.Field.Galois      (PrimeField, fromP, toP, Prime)
import           GHC.TypeLits           (KnownNat)



-- Given a value, and dimensions, produces a multi-d array of size given by
-- dimensions, containing copies of the value.
termMultiDimArray :: KnownNat k => Term (Prime k) -> [Term (Prime k)] -> Term (Prime k)
termMultiDimArray = foldr (\d acc -> case d of
        Scalar n -> Array $ replicate (fromIntegral $ fromP n) acc
        _        -> error $ "Illegal dimension " ++ show d
    )

data DimArray = DABase String [Int] | DARec [DimArray]

-- Given a signal name and dimensions, produces a multi-d array containing the
-- consituent signals
--
-- Kind of like "a" [2, 1] to [["a.0.0"], ["a.1.0"]]
termSignalArray :: PrimeField k => String -> [Term k] -> Term k
termSignalArray name dim = case dim of
    [] -> Sig (SigLocal name [])
    (Scalar n):rest ->
        Array $ [ signalTranform (subscriptSignal (i - 1)) rec | i <- [1..(fromP n)] ]
        where
            subscriptSignal idx (SigLocal name idxs) = SigLocal name ((fromIntegral idx):idxs)
            subscriptSignal idx  SigForeign {}       = error "Unreachable"
            rec = termSignalArray name rest
    (t:ts) -> error $ "Illegal dimension " ++ show t

termIsSig :: Term k -> Bool
termIsSig t = case t of
    Sig {} -> True
    _      -> False

termGenTimeConst :: Term k -> Bool
termGenTimeConst t = case t of
    Scalar {}    -> True
    Linear {}    -> False
    Sig {}       -> False
    Quadratic {} -> False
    Other        -> False
    Array a      -> all termGenTimeConst a
    Struct map _ -> all termGenTimeConst map

genGetUnMutOp :: PrimeField k => UnMutOp -> Term k -> Term k
genGetUnMutOp op = case op of
    PreInc  -> (+ Scalar 1)
    PostInc -> (+ Scalar 1)
    PreDec  -> (+ Scalar (-1))
    PostDec -> (+ Scalar (-1))

genLocation :: KnownNat k => Ctx (Prime k) -> Location -> (Ctx (Prime k), LTerm)
genLocation ctx loc = case loc of
    Ident s -> (ctx, LTermIdent s)
    Pin loc' pin -> (ctx', LTermPin lt pin)
        where (ctx', lt) = genLocation ctx loc'
    Index loc' ie -> case iterm of
            Scalar i -> (ctx'', LTermIdx lt (fromIntegral $ fromP i))
            i -> error $ "Non-scalar " ++ show i ++ " as index in " ++ show loc
        where
            (ctx', lt) = genLocation ctx loc'
            (ctx'', iterm) = genExpr ctx' ie

-- Lifts a fun: Integer -> Integer -> Integer to one that operates over gen-time constant
-- terms
genConstantBinLift :: KnownNat k => String -> (Integer -> Integer -> Integer) -> Term (Prime k) -> Term (Prime k) -> Term (Prime k)
genConstantBinLift name f s t = case (s, t) of
    (Scalar c1 , Scalar c2) -> Scalar $ toP $ f (fromP c1) (fromP c2)
    (a@Array {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
    (a@Struct {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on struct term " ++ show a
    (Sig _, _) -> Other
    (Other, _) -> Other
    (Linear {}, _) -> Other
    (Quadratic {}, _) -> Other
    (l, r) -> genConstantBinLift name f r l

-- Lifts a fun: Integer -> Integer -> Bool to one that operates over gen-time constant
-- terms
genConstantCmpLift :: KnownNat k => String -> (Integer -> Integer -> Bool) -> Term (Prime k) -> Term (Prime k) -> Term (Prime k)
genConstantCmpLift name f = genConstantBinLift name (\a b -> if f a b then 1 else 0)

-- Lifts a fun: Bool -> Bool -> Bool to one that operates over gen-time
-- constant terms
genConstantBoolBinLift :: KnownNat k => String -> (Bool -> Bool -> Bool) -> Term (Prime k) -> Term (Prime k) -> Term (Prime k)
genConstantBoolBinLift name f = genConstantBinLift name (\a b -> if f (a /= 0) (b /= 0) then 1 else 0)

-- Lifts a fun: Integer -> Integer to one that operates over gen-time constant terms
genConstantUnLift :: KnownNat k => String -> (Integer -> Integer) -> Term (Prime k) -> Term (Prime k)
genConstantUnLift name f t = case t of
    Scalar c -> Scalar $ toP $ f $ fromP c
    a@Array {} -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
    a@Struct {} -> error $ "Cannot perform operation \"" ++ name ++ "\" on struct term " ++ show a
    Other -> Other
    Linear {} -> Other
    Quadratic {} -> Other
    Sig {} -> Other


genExpr :: KnownNat k => Ctx (Prime k) -> Expr -> (Ctx (Prime k), Term (Prime k))
genExpr ctx expr = case expr of
    NumLit i -> (ctx, Scalar $ toP $ fromIntegral i)
    ArrayLit es -> (ctx', Array ts)
        where
            (ctx', ts) = genExprs ctx es
    BinExpr op l r ->
        (ctx'', case op of
            Add    -> l' + r'
            Sub    -> l' - r'
            Mul    -> l' * r'
            Div    -> l' / r'
            IntDiv -> genConstantBinLift "//" div l' r'
            Mod    -> genConstantBinLift "%" mod l' r'
            Lt     -> genConstantCmpLift "<" (<) l' r'
            Gt     -> genConstantCmpLift ">" (>) l' r'
            Le     -> genConstantCmpLift "<=" (<=) l' r'
            Ge     -> genConstantCmpLift "<=" (>=) l' r'
            Eq     -> genConstantCmpLift "==" (==) l' r'
            Ne     -> genConstantCmpLift "!=" (/=) l' r'
            And    -> genConstantBoolBinLift "&&" (&&) l' r'
            Or     -> genConstantBoolBinLift "||" (||) l' r'
            Shl    -> genConstantBinLift "<<" (liftShiftToInt Bits.shiftL) l' r'
            Shr    -> genConstantBinLift "<<" (liftShiftToInt Bits.shiftR) l' r'
            BitAnd -> genConstantBinLift "&" (Bits..&.) l' r'
            BitOr  -> genConstantBinLift "&" (Bits..|.) l' r'
            BitXor -> genConstantBinLift "&" Bits.xor l' r'
            Pow    -> genConstantBinLift "**" (^) l' r')
        where
            (ctx', l') = genExpr ctx l
            (ctx'', r') = genExpr ctx' r
            liftShiftToInt :: (Integer -> Int -> Integer) -> Integer -> Integer -> Integer
            liftShiftToInt s l r = s l (fromIntegral r)
    UnExpr op e ->
        case op of
            UnNeg -> (ctx', - t)
            Not -> (ctx', genConstantUnLift "!" (\c -> if c /= 0 then 0 else 1) t)
            UnPos -> (ctx', case t of
                Scalar c     -> Scalar c
                Array ts     -> Scalar $ toP $ fromIntegral $ length ts
                Struct ts _  -> Scalar $ toP $ fromIntegral $ Map.size ts
                Other        -> Other
                Sig {}       -> Other
                Linear {}    -> Other
                Quadratic {} -> Other)
            BitNot -> (ctx', genConstantUnLift "~" Bits.complement t)
        where
            (ctx', t) = genExpr ctx e
    UnMutExpr op loc -> genUnExpr ctx op loc
    Ite c l r ->
        case condT of
            Scalar 0 -> genExpr ctx' r
            Scalar _ -> genExpr ctx' l
            t        -> error $ "Cannot condition on term " ++ show t
        where
            (ctx', condT) = genExpr ctx c
    LValue loc ->
            (ctx', ctxGet ctx' lt)
            -- TODO(aozdemir): enforce ctx' == ctx for sanity?
        where (ctx', lt) = genLocation ctx loc
    Call name args -> if all termGenTimeConst actualArgs then
            (ctx', ctxToStruct postCtx)
        else
            error $ "One of the arguments to " ++ name ++ " is not a generation-time constant!"
        where
            postCtx = genStatements newCtx block
            newCtx = ctx' { env = Map.fromList (zip formalArgs actualArgs) , constraints = []}
            (isFn, formalArgs, block) = ctxGetCallable ctx name
            (ctx', actualArgs) = genExprs ctx args


genUnExpr :: KnownNat k => Ctx (Prime k) -> UnMutOp -> Location -> (Ctx (Prime k), Term (Prime k))
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

genExprs :: KnownNat k => Ctx (Prime k) -> [Expr] -> (Ctx (Prime k), [Term (Prime k)])
genExprs c = foldl (\(c, ts) e -> let (c', t) = genExpr c e in (c', t:ts)) (c, [])

genStatements :: KnownNat k => Ctx (Prime k) -> [Statement] -> Ctx (Prime k)
genStatements = foldl (\c s -> if isJust (returning c) then c else genStatement c s)

genStatement :: KnownNat k => Ctx (Prime k) -> Statement -> Ctx (Prime k)
genStatement ctx statement = case statement of
    -- Note, signals are immutable.
    Assign loc expr -> if termIsSig (ctxGet ctx'' lval) then
                ctx''
            else
                ctxStore ctx'' lval term
        where
            (ctx', lval) = genLocation ctx loc
            (ctx'', term) = genExpr ctx' expr
    -- TODO Not quite right: evals twice
    OpAssign op loc expr -> genStatement ctx $ Assign loc (BinExpr op (LValue loc) expr)
    Constrain l r ->
        case zeroTerm of
            Scalar 0 -> ctx''
            Linear lc -> ctxAddConstraint ctx'' (lcZero, lcZero, lc)
            Quadratic a b c -> ctxAddConstraint ctx'' (a, b, c)
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
            ctx'' = ctxInit ctx' name (termMultiDimArray (Scalar 0) ts)
            (ctx', ts) = genExprs ctx dims
    SigDeclaration name kind dims -> ctxInit ctx' name (termSignalArray name tdims)
        where
            (ctx', tdims) = genExprs ctx dims
    SubDeclaration name dims init -> case init of
            Just e  -> genStatement ctx'' $ Assign (Ident name) e
            Nothing -> ctx''
        where
            ctx'' = ctxInit ctx' name (termMultiDimArray (Scalar 0) ts)
            (ctx', ts) = genExprs ctx dims
    If cond true false -> case tcond of
            Scalar 0 -> genStatements ctx' (concat $ Maybe.maybeToList false)
            Scalar _ -> genStatements ctx' true
            _        -> error $ "Invalid conditional term " ++ show tcond
        where
            (ctx', tcond) = genExpr ctx cond
    While cond block -> case tcond of
            Scalar 0 -> ctx'
            Scalar _ -> genStatement (genStatements ctx block) (While cond block)
            _ -> error $ "Invalid conditional term " ++ show tcond
        where
            (ctx', tcond) = genExpr ctx cond
    For init cond step block -> genStatements ctx [init, While cond (block ++ [step])]
    DoWhile block expr -> genStatements ctx (block ++ [While expr block])
    Compute _ -> ctx
    Ignore e -> fst $ genExpr ctx e
    Return e -> ctx' { returning = Just t }
        where
            (ctx', t) = genExpr ctx e

genMain :: KnownNat k => MainCircuit -> Integer -> [Constraint (Prime k)]
genMain m order =
        constraints ctx'
    where
        ctx' = genStatement ctxEmpty (SubDeclaration "main" [] (Just (main m)))
        ctxEmpty = (ctxWithEnv Map.empty order) {
            callables = Map.union
                (Map.map (\(p, b) -> (False, p, b)) (templates m))
                (Map.map (\(p, b) -> (True , p, b)) (functions m))
        }

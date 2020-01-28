{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Codegen.Circom ( cGenExpr
                      , cGenStatement
                      , cGenStatements
                      , cGenMain
                      , lcZero
                      , CGenCtx(..)
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

import AST.Circom
import Data.Maybe       (mapMaybe, fromMaybe, maybeToList)
import qualified Data.Bits as Bits
import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map
import qualified Data.Either as Either
import Debug.Trace      (trace)

data Signal = SigLocal String [Int]
            -- Subcomponent name, subcomponent indices, signal name, signal indices
            | SigForeign String [Int] Signal
            deriving (Show,Ord,Eq)
type LC = (Map.Map Signal Int, Int) -- A linear combination of signals and gen-time constants

data Term = Sig Signal
          | Linear LC                   -- A linear combination
          | Quadratic LC LC LC          -- A * B + C for LC's A, B, C
          | Scalar Int                  -- a gen-time constant
          | Array [Term]                -- An array of terms
          | Struct (Map.Map String Term) [Constraint] -- A structure, environment and constraints
          | Other                       -- A non-gen-time constant that is none of the above.
          deriving (Show,Ord,Eq)

type Constraint = (LC, LC, LC)

-- An evaluated l-value
data LTerm = LTermIdent String
           | LTermPin LTerm String
           | LTermIdx LTerm Int
           deriving (Show,Ord,Eq)

termZeroArray :: [Term] -> Term
termZeroArray = foldr (\d acc -> case d of
        Scalar n -> Array $ replicate n acc
        _ -> error $ "Illegal dimension " ++ show d
    ) (Scalar 0)

data DimArray = DABase String [Int] | DARec [DimArray]

termSignalArray :: String -> [Term] -> Term
termSignalArray ident [] = Sig (SigLocal ident [])
termSignalArray ident ((Scalar n):ts) =
        Array $ [ signalTranform (\t -> case t of
            SigLocal ident idxs -> SigLocal ident (i:idxs)
            SigForeign {} -> error "should be unreachable"
        ) rec | i <- [0..(n-1)]]
    where
        rec = termSignalArray ident ts
termSignalArray ident (t:ts) = error $ "Illegal dimension " ++ show t

signalTranform :: (Signal -> Signal) -> Term -> Term
signalTranform f t = case t of
    Sig s -> Sig $ f s
    Linear (m, c) -> Linear (Map.mapKeys f m, c)
    Quadratic (m1, c1) (m2, c2) (m3, c3) -> Quadratic (Map.mapKeys f m1, c1) (Map.mapKeys f m2, c2) (Map.mapKeys f m3, c3)
    Scalar c -> Scalar c
    Array ts -> Array $ map (signalTranform f) ts
    Struct tmap cs -> Struct (Map.map (signalTranform f) tmap) (map (\(a, b, c) -> (lcXfm a, lcXfm b, lcXfm c)) cs)
        where
            lcXfm (m, c) = (Map.mapKeys f m, c)
    Other -> Other

termIsSig :: Term -> Bool
termIsSig t = case t of
    Sig {} -> True
    _ -> False

termGenTimeConst :: Term -> Bool
termGenTimeConst t = case t of
    Scalar {} -> True
    Linear {} -> False
    Sig {} -> False
    Quadratic {} -> False
    Array a -> all termGenTimeConst a
    Struct map _ -> all termGenTimeConst $ Map.elems map
    Other -> False

linearizeSig :: Signal -> Term
linearizeSig s = Linear (Map.fromList [(s, 1)], 0)

instance Num Term where
  s + t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Struct {}, _) -> error $ "Cannot add struct term " ++ show a ++ " to anything"
    (Other, _) -> Other
    (Sig s, r) -> linearizeSig s + r
    (s, Sig r) -> s + linearizeSig r
    (Linear (m1, c1), Linear (m2, c2)) -> Linear (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Quadratic a b (m2, c2)) -> Quadratic a b (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Scalar c) -> Linear (m1, c1 + c)
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic a b (m, c1), Scalar c2) -> Quadratic a b (m, c1 + c2)
    (Scalar c1, Scalar c2) -> Scalar $ c1 + c2
    (l, r) -> r + l
  s * t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Struct {}, _) -> error $ "Cannot multiply struct term " ++ show a ++ " with anything"
    (Other, _) -> Other
    (Sig s, r) -> linearizeSig s * r
    (s, Sig r) -> s * linearizeSig r
    (Linear l1, Linear l2) -> Quadratic l1 l2 (Map.empty, 0)
    (Linear _, Quadratic {}) -> Other
    (Linear l, Scalar c) -> Linear $ lcScale l c
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic l1 l2 l3, Scalar c) -> Quadratic (lcScale l1 c) (lcScale l2 c) (lcScale l3 c)
    (Scalar c1 , Scalar c2) -> Scalar $ c1 * c2
    (l, r) -> r * l
  fromInteger n = Scalar $ fromInteger n
  signum s = case s of
    Array {} -> error $ "Cannot get sign of array term " ++ show s
    Struct {} -> error $ "Cannot get sign of struct term " ++ show s
    Other -> Scalar 1
    Sig {} -> Scalar 1
    Linear {} -> Scalar 1
    Quadratic {} -> Scalar 1
    Scalar n -> Scalar $ signum n
  abs s = case s of
    Array a -> Scalar $ length a
    Struct s _ -> Scalar $ Map.size s
    Other -> Other
    s@Sig {} -> s
    l@Linear {} -> l
    q@Quadratic {} -> q
    Scalar n -> Scalar $ abs n
  negate s = fromInteger (-1) * s

instance Fractional Term where
  fromRational r = error "NYI"
  recip t = case t of
    a@Array {} -> error $ "Cannot invert array term " ++ show a
    a@Struct {} -> error $ "Cannot invert struct term " ++ show a
    Scalar c1 -> error "NYI"
    Other -> Other
    Linear _ -> Other
    Quadratic {} -> Other
    Sig _ -> Other

lcScale :: LC -> Int -> LC
lcScale (m, c) a = (Map.map (*a) m, a * c)

lcZero :: LC
lcZero = (Map.empty, 0)

cGenGetUnMutOp :: UnMutOp -> Term -> Term
cGenGetUnMutOp op = case op of
    PreInc -> (+ Scalar 1)
    PostInc -> (+ Scalar 1)
    PreDec -> (+ Scalar (-1))
    PostDec -> (+ Scalar (-1))

data CGenCtx = CGenCtx { env :: Map.Map String Term
                       , constraints :: [Constraint]
                       , templates :: Map.Map String ([String], Block)
                       }
                       deriving (Show, Eq)

ctxWithEnv :: Map.Map String Term -> CGenCtx
ctxWithEnv env = CGenCtx { env = env, constraints = [], Codegen.Circom.templates = Map.empty }

cGenLocation :: CGenCtx -> Location -> (CGenCtx, LTerm)
cGenLocation ctx loc = case loc of
    Ident s -> (ctx, LTermIdent s)
    Pin loc' pin -> (ctx', LTermPin lt pin)
        where (ctx', lt) = cGenLocation ctx loc'
    Index loc' ie -> case iterm of
            Scalar i -> (ctx'', LTermIdx lt i)
            i -> error $ "Non-scalar " ++ show i ++ " as index in " ++ show loc
        where
            (ctx', lt) = cGenLocation ctx loc'
            (ctx'', iterm) = cGenExpr ctx' ie

-- Lifts a fun: Int -> Int -> Int to one that operates over gen-time constant
-- terms
cGenConstantBinLift :: String -> (Int -> Int -> Int) -> Term -> Term -> Term
cGenConstantBinLift name f s t = case (s, t) of
    (Scalar c1 , Scalar c2) -> Scalar $ f c1 c2
    (a@Array {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
    (a@Struct {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on struct term " ++ show a
    (Sig _, _) -> Other
    (Other, _) -> Other
    (Linear {}, _) -> Other
    (Quadratic {}, _) -> Other
    (l, r) -> cGenConstantBinLift name f r l

-- Lifts a fun: Int -> Int -> Bool to one that operates over gen-time constant
-- terms
cGenConstantCmpLift :: String -> (Int -> Int -> Bool) -> Term -> Term -> Term
cGenConstantCmpLift name f = cGenConstantBinLift name (\a b -> if f a b then 1 else 0)

-- Lifts a fun: Bool -> Bool -> Bool to one that operates over gen-time
-- constant terms
cGenConstantBoolBinLift :: String -> (Bool -> Bool -> Bool) -> Term -> Term -> Term
cGenConstantBoolBinLift name f = cGenConstantBinLift name (\a b -> if f (a /= 0) (b /= 0) then 1 else 0)

-- Lifts a fun: Int -> Int to one that operates over gen-time constant terms
cGenConstantUnLift :: String -> (Int -> Int) -> Term -> Term
cGenConstantUnLift name f t = case t of
    Scalar c -> Scalar (f c)
    a@Array {} -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
    a@Struct {} -> error $ "Cannot perform operation \"" ++ name ++ "\" on struct term " ++ show a
    Other -> Other
    Linear {} -> Other
    Quadratic {} -> Other
    Sig {} -> Other

updateList :: (a -> a) -> Int -> [a] -> Maybe [a]
updateList f i l = case splitAt i l of
    (h, m:t) -> Just $ h ++ (f m : t)
    _ -> Nothing


-- Modifies a context to store a value in a location
ctxStore :: CGenCtx -> LTerm -> Term -> CGenCtx
ctxStore ctx loc value = case value of
        Struct m c -> if null (Either.lefts ss)
            then
                let
                    m' = Map.map (signalTranform signalXfm) m
                    c' = emmigrateConstraints c
                in
                    ctx { env = Map.update (pure . replacein ss (Struct m' c')) ident (env ctx)
                        , constraints = c' ++ constraints ctx }
            else
                error $ "Cannot assign circuits to non-local location: " ++ show loc
        _ -> ctx { env = Map.update (pure . replacein ss value) ident (env ctx) }
    where
        (ident, ss) = steps loc

        emmigrateConstraints = map (\(a, b, c) -> (lcXfm a, lcXfm b, lcXfm c))
        lcXfm (m, c) = (Map.mapKeys signalXfm m, c)
        signalXfm = SigForeign ident (Either.rights ss)

        -- TODO: nicer way to write this?
        replacein :: [Either String Int] -> Term -> Term -> Term
        replacein [] value _ = value
        replacein (Left pin:t) value (Struct m c) = Struct (Map.update (pure . replacein t value) pin m) c
        replacein (Left pin:_) _ t = error $ "Cannot update pin `" ++ pin ++ "` of non-struct " ++ show t
        replacein (Right idx:t) value (Array m) = Array $ fromMaybe
            (error $ "The index " ++ show idx ++ " is out of bounds for " ++ show m)
            (updateList (replacein t value) idx m)
        replacein (Right idx:_) _ t = error $ "Cannot update index `" ++ show idx ++ "` of non-array " ++ show t

        rsteps (LTermIdent s) = (s, [])
        rsteps (LTermPin lt pin) = let (s, ts) = rsteps lt in (s, Left pin:ts)
        rsteps (LTermIdx lt idx) = let (s, ts) = rsteps lt in (s, Right idx:ts)

        steps l = let (s, ts) = rsteps l in (s, reverse ts)


-- Gets a value from a location in a context
ctxGet :: CGenCtx -> LTerm -> Term
ctxGet ctx loc = case loc of
    LTermIdent s -> Map.findWithDefault (error $ "Unknown identifier `" ++ s ++ "`") s (env ctx)
    LTermPin loc' pin -> case ctxGet ctx loc' of
        Struct pins _ -> pins Map.! pin
        l -> error $ "Non-struct " ++ show l ++ " as location in " ++ show loc
    LTermIdx loc' i -> case ctxGet ctx loc' of
        Array ts -> ts !! i
        l -> error $ "Non-array " ++ show l ++ " as location in " ++ show loc

ctxGetTemplate :: CGenCtx -> String -> ([String], Block)
ctxGetTemplate ctx name = fromMaybe (error $ "No template named " ++ name ++ " found") $ Map.lookup name (Codegen.Circom.templates ctx)

ctxAddConstraint :: CGenCtx -> (LC, LC, LC) -> CGenCtx
ctxAddConstraint ctx c = ctx { constraints = c : constraints ctx }

ctxInitIdent :: CGenCtx -> String -> Term -> CGenCtx
ctxInitIdent ctx name value = ctx { env = Map.insert name value (env ctx) }

ctxToStruct ctx = Struct (env ctx) (constraints ctx)


cGenExpr :: CGenCtx -> Expr -> (CGenCtx, Term)
cGenExpr ctx expr = case expr of
    NumLit i -> (ctx, Scalar i)
    ArrayLit es -> (ctx', Array ts)
        where
            (ctx', ts) = cGenExprs ctx es
    BinExpr op l r ->
        (ctx'', case op of
            Add -> l' + r'
            Sub -> l' - r'
            Mul -> l' * r'
            Div -> l' / r'
            IntDiv -> cGenConstantBinLift "//" div l' r'
            Mod -> cGenConstantBinLift "%" mod l' r'
            Lt -> cGenConstantCmpLift "<" (<) l' r'
            Gt -> cGenConstantCmpLift ">" (>) l' r'
            Le -> cGenConstantCmpLift "<=" (<=) l' r'
            Ge -> cGenConstantCmpLift "<=" (>=) l' r'
            Eq -> cGenConstantCmpLift "==" (==) l' r'
            Ne -> cGenConstantCmpLift "!=" (/=) l' r'
            And -> cGenConstantBoolBinLift "&&" (&&) l' r'
            Or -> cGenConstantBoolBinLift "||" (||) l' r'
            Shl -> cGenConstantBinLift "<<" Bits.shiftL l' r'
            Shr -> cGenConstantBinLift "<<" Bits.shiftR l' r'
            BitAnd -> cGenConstantBinLift "&" (Bits..&.) l' r'
            BitOr -> cGenConstantBinLift "&" (Bits..|.) l' r'
            BitXor -> cGenConstantBinLift "&" Bits.xor l' r'
            Pow -> cGenConstantBinLift "**" (^) l' r')
        where
            (ctx', l') = cGenExpr ctx l
            (ctx'', r') = cGenExpr ctx' r
    UnExpr op e ->
        case op of
            UnNeg -> (ctx', - t)
            Not -> (ctx', cGenConstantUnLift "!" (\c -> if c /= 0 then 0 else 1) t)
            UnPos -> (ctx', case t of
                Scalar c -> Scalar c
                Array ts -> Scalar (length ts)
                Struct ts _ -> Scalar (Map.size ts)
                Other -> Other
                Sig {} -> Other
                Linear {} -> Other
                Quadratic {} -> Other)
            BitNot -> (ctx', cGenConstantUnLift "~" Bits.complement t)
        where
            (ctx', t) = cGenExpr ctx e
    UnMutExpr op loc -> cGenUnExpr ctx op loc
    Ite c l r ->
        case condT of
            Scalar 0 -> cGenExpr ctx' r
            Scalar _ -> cGenExpr ctx' l
            t -> error $ "Cannot condition on term " ++ show t
        where
            (ctx', condT) = cGenExpr ctx c
    LValue loc ->
            (ctx', ctxGet ctx' lt)
            -- TODO(aozdemir): enforce ctx' == ctx for sanity?
        where (ctx', lt) = cGenLocation ctx loc
    Call name args -> if all termGenTimeConst actualArgs then
            (ctx', ctxToStruct postCtx)
        else
            error $ "One of the arguments to " ++ name ++ " is not a generation-time constant!"
        where
            postCtx = cGenStatements newCtx block
            newCtx = ctx' { env = Map.fromList (zip formalArgs actualArgs) , constraints = []}
            (formalArgs, block) = ctxGetTemplate ctx name
            (ctx', actualArgs) = cGenExprs ctx args


cGenUnExpr :: CGenCtx -> UnMutOp -> Location -> (CGenCtx, Term)
cGenUnExpr ctx op loc = case op of
    PostInc -> (ctx'', term)
    PreInc -> (ctx'', term')
    PostDec -> (ctx'', term)
    PreDec -> (ctx'', term')
    where
        -- TODO(aozdemir): enforce ctx' == ctx for sanity?
        (ctx', lval) = cGenLocation ctx loc
        term = ctxGet ctx' lval
        term' = cGenGetUnMutOp op term
        ctx'' = ctxStore ctx' lval term'

cGenExprs :: CGenCtx -> [Expr] -> (CGenCtx, [Term])
cGenExprs c = foldl (\(c, ts) e -> let (c', t) = cGenExpr c e in (c', t:ts)) (c, [])

cGenStatements :: CGenCtx -> [Statement] -> CGenCtx
cGenStatements = foldl cGenStatement

cGenStatement :: CGenCtx -> Statement -> CGenCtx
cGenStatement ctx statement = case statement of
    -- TODO kind of weird -- we do not assign to signals.
    Assign loc expr -> if termIsSig (ctxGet ctx'' lval) then
                ctx''
            else
                ctxStore ctx'' lval term
        where
            (ctx', lval) = cGenLocation ctx loc
            (ctx'', term) = cGenExpr ctx' expr
    -- TODO Not quite right: evals twice
    OpAssign op loc expr -> cGenStatement ctx $ Assign loc (BinExpr op (LValue loc) expr)
    Constrain l r ->
        case zeroTerm of
            Scalar 0 -> ctx''
            Linear lc -> ctxAddConstraint ctx'' (lcZero, lcZero, lc)
            Quadratic a b c -> ctxAddConstraint ctx'' (a, b, c)
            _ -> error $ "Cannot constain " ++ show zeroTerm ++ " to zero"
        where
            (ctx', lt) = cGenExpr ctx l
            (ctx'', rt) = cGenExpr ctx' r
            zeroTerm = lt - rt
    -- TODO Not quite right: evals twice
    AssignConstrain l e -> cGenStatements ctx [Assign l e, Constrain (LValue l) e]
    VarDeclaration name dims init -> case init of
            Just e -> cGenStatement ctx'' $ Assign (Ident name) e
            Nothing -> ctx''
        where
            ctx'' = ctxInitIdent ctx' name (termZeroArray ts)
            (ctx', ts) = cGenExprs ctx dims
    SigDeclaration name kind dims -> ctxInitIdent ctx' name (termSignalArray name tdims)
        where
            (ctx', tdims) = cGenExprs ctx dims
    SubDeclaration name dims init -> case init of
            Just e -> cGenStatement ctx'' $ Assign (Ident name) e
            Nothing -> ctx''
        where
            ctx'' = ctxInitIdent ctx' name (termZeroArray ts)
            (ctx', ts) = cGenExprs ctx dims
    If cond true false -> case tcond of
            Scalar 0 -> cGenStatements ctx' (concat $ maybeToList false)
            Scalar _ -> cGenStatements ctx' true
            _ -> error $ "Invalid conditional term " ++ show tcond
        where
            (ctx', tcond) = cGenExpr ctx cond
    While cond block -> case tcond of
            Scalar 0 -> ctx'
            Scalar _ -> cGenStatement (cGenStatements ctx block) (While cond block)
            _ -> error $ "Invalid conditional term " ++ show tcond
        where
            (ctx', tcond) = cGenExpr ctx cond
    For init cond step block -> cGenStatements ctx [init, While cond (block ++ [step])]
    DoWhile block expr -> cGenStatements ctx (block ++ [While expr block])
    Compute _ -> ctx
    Ignore e -> fst $ cGenExpr ctx e
    Return {} -> error "NYI"

cGenMain :: MainCircuit -> [Constraint]
cGenMain m =
        constraints ctx'
    where
        ctx' = cGenStatement ctxEmpty (SubDeclaration "main" [] (Just (main m)))
        ctxEmpty = (ctxWithEnv Map.empty) { Codegen.Circom.templates = AST.Circom.templates m }

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module AST.Circom ( File
                  , BinOp(..)
                  , Item(..)
                  , Statement(..)
                  , Expr(..)
                  , Location(..)
                  , SignalKind(..)
                  , UnOp(..)
                  , UnMutOp(..)
                  , Block
                  , collectIncludes
                  , collectFunctions
                  , collectTemplates
                  , collectMains
                  , cGenExpr
                  , CGenCtx(..)
                  , Term(..)
                  , LTerm(..)
                  , Signal(..)
                  , ctxStore
                  , ctxGet
                  ) where


import Data.Maybe       (mapMaybe, fromMaybe, maybeToList)
import qualified Data.Bits as Bits
import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map

data BinOp = Add
           | Sub
           | Mul
           | Div
           | IntDiv
           | Mod
           | Shl
           | Shr
           | Lt
           | Gt
           | Le
           | Ge
           | Eq
           | Ne
           | And
           | Or
           | BitAnd
           | BitOr
           | BitXor
           | Pow
           deriving (Show,Eq)

data Item = Function String [String] Block
          | Template String [String] Block
          | Include String
          | Main Expr
          deriving (Show,Eq)

itemAsFunction :: Item -> Maybe (String, [String], Block)
itemAsFunction (Function a b c) = Just (a, b, c)
itemAsFunction _ = Nothing
itemAsTemplate :: Item -> Maybe (String, [String], Block)
itemAsTemplate (Template a b c) = Just (a, b, c)
itemAsTemplate _ = Nothing
itemAsMain :: Item -> Maybe Expr
itemAsMain (Main c) = Just c
itemAsMain _ = Nothing
itemAsInclude :: Item -> Maybe String
itemAsInclude (Include c) = Just c
itemAsInclude _ = Nothing

collectFunctions :: File -> [(String, [String], Block)]
collectFunctions = mapMaybe itemAsFunction
collectTemplates :: File -> [(String, [String], Block)]
collectTemplates = mapMaybe itemAsTemplate
collectMains :: File -> [Expr]
collectMains = mapMaybe itemAsMain
collectIncludes :: File -> [String]
collectIncludes = mapMaybe itemAsInclude

type File = [Item]

type Block = [Statement]

data Statement = Assign Location Expr
               | OpAssign BinOp Location Expr
               | AssignConstrain Location Expr
               | Constrain Expr Expr
               | VarDeclaration String [Expr] (Maybe Expr)
               | SigDeclaration String SignalKind [Expr]
               | SubDeclaration String [Expr] (Maybe Expr)
               | If Expr Block (Maybe Block)
               | For Statement Expr Statement Block
               | While Expr Block
               | DoWhile Block Expr
               | Compute Block
               | Return Expr
               | Ignore Expr -- Expression statements
               deriving (Show,Eq)

data SignalKind = In
                | Out
                | Local
                deriving (Show,Eq)

data Location = Ident String
              | Pin Location String
              | Index Location Expr
              deriving (Show,Eq)

data UnMutOp = PreInc
          | PostInc
          | PreDec
          | PostDec
          deriving (Show,Eq)

data UnOp = UnNeg
          | BitNot
          | Not
          | UnPos
          deriving (Show,Eq)

data Expr = BinExpr BinOp Expr Expr
          | UnExpr UnOp Expr
          | UnMutExpr UnMutOp Location
          | Ite Expr Expr Expr
          | LValue Location
          | Call String [Expr]
          | ArrayLit [Expr]
          | NumLit Int
          deriving (Show,Eq)

data Signal = SigLocal String [Int]
            -- Subcomponent name, subcomponent indices, signal name, signal indices
            | SigForeign String [Int] Signal
            deriving (Show,Ord,Eq)
type LC = (Map.Map Signal Int, Int) -- A linear combination of signals and gen-time constants

data Term = Linear LC                   -- A linear combination
          | Quadratic LC LC LC          -- A * B + C for LC's A, B, C
          | Scalar Int                  -- a gen-time constant
          | Array [Term]                -- An array of terms
          | Struct (Map.Map String Term)-- A structure
          | Other                       -- A non-gen-time constant that is none of the above.
          deriving (Show,Ord,Eq)

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
termSignalArray ident [] = Linear (Map.fromList [(SigLocal ident [], 1)], 0)
termSignalArray ident ((Scalar n):ts) =
        Array $ [ signalTranform (\t -> case t of
            SigLocal ident idxs -> SigLocal ident idxs
            SigForeign ident idxs s -> SigForeign ident idxs s
        ) rec | i <- [0..(n-1)]]
    where
        rec = termSignalArray ident ts
termSignalArray ident (t:ts) = error $ "Illegal dimension " ++ show t

signalTranform :: (Signal -> Signal) -> Term -> Term
signalTranform f t = case t of
    Linear (m, c) -> Linear (Map.mapKeys f m, c)
    Quadratic (m1, c1) (m2, c2) (m3, c3) -> Quadratic (Map.mapKeys f m1, c1) (Map.mapKeys f m2, c2) (Map.mapKeys f m3, c3)
    Scalar c -> Scalar c
    Array ts -> Array $ map (signalTranform f) ts
    Struct tmap -> Struct $ Map.map (signalTranform f) tmap
    Other -> Other

cGenAdd :: Term -> Term -> Term
cGenAdd s t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
    (a@Struct {}, _) -> error $ "Cannot add struct term " ++ show a ++ " to anything"
    (Other, _) -> Other
    (Linear (m1, c1), Linear (m2, c2)) -> Linear (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Quadratic a b (m2, c2)) -> Quadratic a b (Map.unionWith (+) m1 m2, c1 + c2)
    (Linear (m1, c1), Scalar c) -> Linear (m1, c1 + c)
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic a b (m, c1), Scalar c2) -> Quadratic a b (m, c1 + c2)
    (Scalar c1, Scalar c2) -> Scalar $ c1 + c2
    (l, r) -> cGenAdd r l

lcScale :: LC -> Int -> LC
lcScale (m, c) a = (Map.map (*a) m, a * c)

lcZero :: LC
lcZero = (Map.empty, 0)

cGenMul :: Term -> Term -> Term
cGenMul s t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
    (a@Struct {}, _) -> error $ "Cannot multiply struct term " ++ show a ++ " with anything"
    (Other, _) -> Other
    (Linear l1, Linear l2) -> Quadratic l1 l2 (Map.empty, 0)
    (Linear _, Quadratic {}) -> Other
    (Linear l, Scalar c) -> Linear $ lcScale l c
    (Quadratic {}, Quadratic {}) -> Other
    (Quadratic l1 l2 l3, Scalar c) -> Quadratic (lcScale l1 c) (lcScale l2 c) (lcScale l3 c)
    (Scalar c1 , Scalar c2) -> Scalar $ c1 * c2
    (l, r) -> cGenMul r l

cGenNeg :: Term -> Term
cGenNeg t = case t of
    a@Array {} -> error $ "Cannot negate array term " ++ show a
    a@Struct {} -> error $ "Cannot negate struct term " ++ show a
    Other -> Other
    Linear l1 -> Linear $ lcScale l1 (-1)
    Quadratic l1 l2 l3 -> Quadratic (lcScale l1 (-1)) (lcScale l2 (-1)) (lcScale l3 (-1))
    Scalar c1 -> Scalar $ -c1

cGenDoUnMutOp :: UnMutOp -> Term -> Term
cGenDoUnMutOp op t = case op of
    PreInc -> cGenAdd t (Scalar 1)
    PostInc -> cGenAdd t (Scalar 1)
    PreDec -> cGenAdd t (Scalar (-1))
    PostDec -> cGenAdd t (Scalar (-1))

cGenRecip :: Term -> Term
cGenRecip t = case t of
    a@Array {} -> error $ "Cannot invert array term " ++ show a
    a@Struct {} -> error $ "Cannot invert struct term " ++ show a
    Scalar c1 -> error "NYI"
    Other -> Other
    Linear _ -> Other
    Quadratic {} -> Other

data CGenCtx = CGenCtx { env :: Map.Map String Term
                       , constraints :: [(LC, LC, LC)]
                       }
                       deriving (Show)

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

updateList :: (a -> a) -> Int -> [a] -> Maybe [a]
updateList f i l = case splitAt i l of
    (h, m:t) -> Just $ h ++ (f m : t)
    _ -> Nothing

-- Modifies a context to store a value in a location
ctxStore :: CGenCtx -> LTerm -> Term -> CGenCtx
ctxStore ctx loc value = CGenCtx {
    env = Map.update (pure . replacein ss value) ident (env ctx),
    constraints = constraints ctx }
    where
        (ident, ss) = steps loc

        -- TODO: nicer way to write this?
        replacein :: [Either String Int] -> Term -> Term -> Term
        replacein [] value _ = value
        replacein (Left pin:t) value (Struct m) = Struct $ Map.update (pure . replacein t value) pin m
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
        Struct pins -> pins Map.! pin
        l -> error $ "Non-struct " ++ show l ++ " as location in " ++ show loc
    LTermIdx loc' i -> case ctxGet ctx loc' of
        Array ts -> ts !! i
        l -> error $ "Non-array " ++ show l ++ " as location in " ++ show loc

ctxAddConstraint :: CGenCtx -> (LC, LC, LC) -> CGenCtx
ctxAddConstraint ctx c = ctx { constraints = c : constraints ctx }

ctxInitIdent :: CGenCtx -> String -> Term -> CGenCtx
ctxInitIdent ctx name value = ctx { env = Map.insert name value (env ctx) }

cGenExpr :: CGenCtx -> Expr -> (CGenCtx, Term)
cGenExpr ctx expr = case expr of
    NumLit i -> (ctx, Scalar i)
    ArrayLit es -> (ctx', Array ts)
        where
            (ctx', ts) = foldl (\(ctx, ts) e -> let (ctx', t) = cGenExpr ctx e in (ctx', t:ts)) (ctx, []) es
    BinExpr op l r ->
        (ctx'', case op of
            Add -> cGenAdd l' r'
            Sub -> cGenAdd l' $ cGenNeg r'
            Mul -> cGenMul l' r'
            Div -> cGenMul l' $ cGenRecip r'
            IntDiv -> cGenConstantBinLift "//" div l' r'
            Mod -> cGenConstantBinLift "//" mod l' r'
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
            UnNeg -> (ctx', cGenNeg t)
            Not -> (ctx', cGenConstantUnLift "!" (\c -> if c /= 0 then 0 else 1) t)
            UnPos -> (ctx', case t of
                Scalar c -> Scalar c
                Array ts -> Scalar (length ts)
                Struct ts -> Scalar (Map.size ts)
                Other -> Other
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
    Call _ _ -> error "NYI"


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
        term' = cGenDoUnMutOp op term
        ctx'' = ctxStore ctx' lval term'

cGenExprs :: CGenCtx -> [Expr] -> (CGenCtx, [Term])
cGenExprs c = foldl (\(c, ts) e -> let (c', t) = cGenExpr c e in (c', t:ts)) (c, [])

cGenStatements :: CGenCtx -> [Statement] -> CGenCtx
cGenStatements = foldl cGenStatement

cGenStatement :: CGenCtx -> Statement -> CGenCtx
cGenStatement ctx statement = case statement of
    Assign loc expr ->
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
            zeroTerm = cGenAdd lt (cGenNeg rt)
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
    If cond true false -> case tcond of
            Scalar 0 -> cGenStatements ctx' (concat $ maybeToList false)
            Scalar _ -> cGenStatements ctx' true
            _ -> error $ "Invalid conditional term " ++ show tcond
        where
            (ctx', tcond) = cGenExpr ctx cond
    While cond block -> case tcond of
            Scalar 0 -> ctx'
            Scalar _ -> cGenStatement ctx' $ While cond block
            _ -> error $ "Invalid conditional term " ++ show tcond
        where
            (ctx', tcond) = cGenExpr ctx cond
    For init cond step block -> cGenStatements ctx [init, While cond (block ++ [step])]
    DoWhile block expr -> cGenStatements ctx (block ++ [While expr block])
    Compute _ -> ctx
    Ignore e -> fst $ cGenExpr ctx e
    SubDeclaration ident dims init -> error "NYI"
    Return {} -> error "NYI"

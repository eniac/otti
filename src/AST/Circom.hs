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
                  , CGenCtx
                  , Term(..)
                  , Signal(..)
                  ) where


import Data.Maybe       (mapMaybe)
import qualified Data.Bits as Bits
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
            | SigForeign String [Int] String [Int]
            deriving (Show,Ord,Eq)
type LC = (Map.Map Signal Int, Int) -- A linear combination of signals and gen-time constants
data Term = Linear LC               -- A linear combination
          | Quadratic LC LC LC      -- A * B + C for LC's A, B, C
          | Scalar Int              -- a gen-time constant
          | Array [Term]            -- An array of terms
          | Other                   -- A non-gen-time constant that is none of the above.
          deriving (Show,Ord,Eq)


cGenAdd :: Term -> Term -> Term
cGenAdd s t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot add array term " ++ show a ++ " to anything"
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

cGenMul :: Term -> Term -> Term
cGenMul s t = case (s, t) of
    (a@Array {}, _) -> error $ "Cannot multiply array term " ++ show a ++ " with anything"
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
    Other -> Other
    Linear l1 -> Linear $ lcScale l1 (-1)
    Quadratic l1 l2 l3 -> Quadratic (lcScale l1 (-1)) (lcScale l2 (-1)) (lcScale l3 (-1))
    Scalar c1 -> Scalar $ -c1

cGenRecip :: Term -> Term
cGenRecip t = case t of
    a@Array {} -> error $ "Cannot invert array term " ++ show a
    Scalar c1 -> error "NYI"
    Other -> Other
    Linear _ -> Other
    Quadratic {} -> Other

type CGenCtx = Map.Map String Term

cGenLocation :: CGenCtx -> Location -> (CGenCtx, Term)
cGenLocation ctx loc = case loc of
    Ident s -> (ctx, Map.findWithDefault (error $ "Unknown identifier `" ++ s ++ "`") s ctx)
    Pin loc pin -> error "NYI"
    Index loc' ie -> case cGenLocation ctx loc' of
        (ctx', Array ts) -> case cGenExpr ctx' ie of
            (ctx'', Scalar i) -> (ctx'', ts !! i)
            (_, i) -> error $ "Non-scalar " ++ show i ++ " as index in " ++ show loc
        (_, l) -> error $ "Non-array " ++ show l ++ " as location in " ++ show loc

-- Lifts a fun: Int -> Int -> Int to one that operates over gen-time constant
-- terms
cGenConstantBinLift :: String -> (Int -> Int -> Int) -> Term -> Term -> Term
cGenConstantBinLift name f s t = case (s, t) of
    (Scalar c1 , Scalar c2) -> Scalar $ f c1 c2
    (a@Array {}, _) -> error $ "Cannot perform operation \"" ++ name ++ "\" on array term " ++ show a
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
    Other -> Other
    Linear {} -> Other
    Quadratic {} -> Other


cGenExpr :: CGenCtx -> Expr -> (CGenCtx, Term)
cGenExpr ctx expr = case expr of
    NumLit i -> (ctx, Scalar i)
    ArrayLit es -> (ctx', Array ts)
        where
            (ctx', ts) = foldl (\(ctx, ts) e -> let (ctx', t) = cGenExpr ctx e in (ctx', t:ts)) (ctx, []) es
    BinExpr op l r ->
        case op of
            Add -> (ctx'', cGenAdd l' r')
            Sub -> (ctx'', cGenAdd l' $ cGenNeg r')
            Mul -> (ctx'', cGenMul l' r')
            Div -> (ctx'', cGenMul l' $ cGenRecip r')
            IntDiv -> (ctx'', cGenConstantBinLift "//" div l' r')
            Mod -> (ctx'', cGenConstantBinLift "//" mod l' r')
            Lt -> (ctx'', cGenConstantCmpLift "<" (<) l' r')
            Gt -> (ctx'', cGenConstantCmpLift ">" (>) l' r')
            Le -> (ctx'', cGenConstantCmpLift "<=" (<=) l' r')
            Ge -> (ctx'', cGenConstantCmpLift "<=" (>=) l' r')
            Eq -> (ctx'', cGenConstantCmpLift "==" (==) l' r')
            Ne -> (ctx'', cGenConstantCmpLift "!=" (/=) l' r')
            And -> (ctx'', cGenConstantBoolBinLift "&&" (&&) l' r')
            Or -> (ctx'', cGenConstantBoolBinLift "||" (||) l' r')
            Shl -> (ctx'', cGenConstantBinLift "<<" Bits.shiftL l' r')
            Shr -> (ctx'', cGenConstantBinLift "<<" Bits.shiftR l' r')
            BitAnd -> (ctx'', cGenConstantBinLift "&" (Bits..&.) l' r')
            BitOr -> (ctx'', cGenConstantBinLift "&" (Bits..|.) l' r')
            BitXor -> (ctx'', cGenConstantBinLift "&" Bits.xor l' r')
            Pow -> (ctx'', cGenConstantBinLift "**" (^) l' r')
        where
            (ctx', l') = cGenExpr ctx l
            (ctx'', r') = cGenExpr ctx' r
    UnExpr op e -> error "NYI"
    Ite c l r ->
        case condT of
            Scalar 0 -> cGenExpr ctx' r
            Scalar _ -> cGenExpr ctx' l
            t -> error $ "Cannot condition on term " ++ show t
        where
            (ctx', condT) = cGenExpr ctx c
    LValue loc -> cGenLocation ctx loc
    Call _ _ -> error "NYI"






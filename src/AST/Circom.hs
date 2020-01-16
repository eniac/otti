module AST.Circom ( File
                  , BinOp(..)
                  , Item(..)
                  , Statement(..)
                  , Expr(..)
                  , Location(..)
                  , SignalKind(..)
                  , UnOp(..)
                  , Block
                  , collectIncludes
                  , collectFunctions
                  , collectTemplates
                  , collectMains
                  ) where

import Data.Maybe       (mapMaybe)
import AST.Typed        (Typed)
import Math.NumberTheory.Primes.Testing (millerRabinV)

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

data UnOp = PreInc
          | PostInc
          | PreDec
          | PostDec
          | UnNeg
          | BitNot
          | Not
          | UnPos
          deriving (Show,Eq)

data Expr = BinExpr BinOp Expr Expr
          | UnExpr UnOp Expr
          | Ite Expr Expr Expr
          | LValue Location
          | Call String [Expr]
          | ArrayLit [Expr]
          | NumLit Int
          deriving (Show,Eq)

-- List of base, power pairs
-- Use `makeFieldOrder`.
newtype FieldOrder = FieldOrder [(Int, Int)]

fieldSize :: FieldOrder -> Int
fieldSize (FieldOrder fieldOrder) = foldl (\a p -> uncurry (^) p * a) 1 fieldOrder

fieldBitCount :: FieldOrder -> Int
fieldBitCount f = ceiling $ logBase 2 (fromIntegral (fieldSize f))

fieldBitCapacity :: FieldOrder -> Int
fieldBitCapacity f = floor $ logBase 2 (fromIntegral (fieldSize f))

makeFieldOrder :: [(Int, Int)] -> FieldOrder
makeFieldOrder pairs = if all (\p -> millerRabinV (fst p) 10) pairs
                       then FieldOrder pairs
                       else error $ "The field order " ++ show pairs ++ " contains non-primes"

data CircomType = FiniteField Int
                deriving (Show, Ord, Eq)

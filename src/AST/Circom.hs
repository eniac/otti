{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module AST.Circom ( File
                  , BinOp(..)
                  , Item(..)
                  , Statement(..)
                  , Expr(..)
                  , IndexedIdent
                  , Location(..)
                  , SignalKind(..)
                  , isPublic
                  , isInput
                  , isVisible
                  , UnOp(..)
                  , UnMutOp(..)
                  , unMutOpOp
                  , UnMutOpOp(..)
                  , unMutOpTime
                  , UnMutOpTime(..)
                  , MainCircuit(..)
                  , Block
                  , collectIncludes
                  , collectFunctions
                  , collectTemplates
                  , collectMains
                  ) where


import Data.Maybe       (mapMaybe, fromMaybe, maybeToList)
import qualified Data.Bits as Bits
import qualified Data.Sequence as Sequence
import qualified Data.Map.Strict as Map
import qualified Data.Either as Either

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

data MainCircuit = MainCircuit { main :: Expr
                               , functions :: Map.Map String ([String], Block)
                               , templates :: Map.Map String ([String], Block)
                               }


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
               | Log Expr
               | Ignore Expr -- Expression statements
               deriving (Show,Eq)

data SignalKind = PublicIn
                | PrivateIn
                | Out
                | Local
                deriving (Show,Eq)

isPublic :: SignalKind -> Bool
isPublic s = case s of
    PublicIn -> True
    PrivateIn -> False
    Out -> False
    Local -> False

isInput :: SignalKind -> Bool
isInput s = case s of
    PublicIn -> True
    PrivateIn -> True
    Out -> False
    Local -> False

isVisible :: SignalKind -> Bool
isVisible s = case s of
    PublicIn -> True
    PrivateIn -> True
    Out -> True
    Local -> False

type IndexedIdent = (String, [Expr])
data Location = LocalLocation IndexedIdent
              | ForeignLocation IndexedIdent IndexedIdent
              deriving (Show,Eq)

data UnMutOp = PreInc
          | PostInc
          | PreDec
          | PostDec
          deriving (Show,Eq)

unMutOpTime :: UnMutOp -> UnMutOpTime
unMutOpTime o = case o of
    PreInc -> Pre
    PostInc -> Post
    PreDec -> Pre
    PostDec -> Post

unMutOpOp :: UnMutOp -> UnMutOpOp
unMutOpOp o = case o of
    PreInc -> Inc
    PostInc -> Inc
    PreDec -> Dec
    PostDec -> Dec

data UnMutOpTime = Pre | Post deriving (Show, Eq)
data UnMutOpOp = Inc | Dec deriving (Show, Eq)

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

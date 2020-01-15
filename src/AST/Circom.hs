module AST.Circom (BinOp(..), Item(..), Statement(..), Expr(..), Location(..), Direction(..), UnOp(..)) where

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

type Block = [Statement]

data Statement = Assign Location Expr
               | OpAssign BinOp
               | AssignConstrain Location Expr
               | Constrain Expr Expr
               | VarDeclaration String [Expr] (Maybe Expr)
               | SigDeclaration String Direction [Expr] (Maybe Expr)
               | SubDeclaration String [Expr] (Maybe Expr)
               | If Expr Block (Maybe Block)
               | While Expr Block
               | Compute Block
               | Return Expr
               | Ignore Expr -- Expression statements
               deriving (Show,Eq)

data Direction = In | Out
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
          | LValue Location
          | Call String [Expr]
          | ArrayLit [Expr]
          | NumLit Int
          deriving (Show,Eq)

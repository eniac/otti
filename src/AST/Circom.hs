module AST.Circom
  ( File
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
  , PString
  , PItem
  , PIndexedIdent
  , PStatement
  , PBlock
  , PExpr
  , PLocation
  , PFile
  , PMainCircuit
  , SString
  , SItem
  , SIndexedIdent
  , SStatement
  , SBlock
  , SExpr
  , SLocation
  , SFile
  , SMainCircuit
  , Posn(..)
  , PosnPair(..)
  , Span(..)
  , nullSpan
  , Annotated(..)
  , Interval(..)
  , mapAnns
  )
where

import           AST.Util
import           Data.Bifunctor
import           Data.Maybe                     ( mapMaybe )
import qualified Data.Map.Strict               as Map


-- Syntax trees [An]notated with arbitrary s-types.
type AnString s = Annotated String s
type AnItem s = Annotated (Item s) s
type AnIndexedIdent s = Annotated (IndexedIdent s) s
type AnStatement s = Annotated (Statement s) s
type AnBlock s = Annotated (Block s) s
type AnExpr s = Annotated (Expr s) s
type AnLocation s = Annotated (Location s) s
-- Don't annotate whole-file or multi-file forms.
type AnFile s = File s
type AnMainCircuit s = MainCircuit s

-- Syntax trees annotated with [P]os[n] pairs
type PString = AnString PosnPair
type PItem = AnItem PosnPair
type PIndexedIdent = AnIndexedIdent PosnPair
type PStatement = AnStatement PosnPair
type PBlock = AnBlock PosnPair
type PExpr = AnExpr PosnPair
type PLocation = AnLocation PosnPair
type PFile = AnFile PosnPair
type PMainCircuit = AnMainCircuit PosnPair

-- Syntax trees annotated with [S]pans
type SString = AnString Span
type SItem = AnItem Span
type SIndexedIdent = AnIndexedIdent Span
type SStatement = AnStatement Span
type SBlock = AnBlock Span
type SExpr = AnExpr Span
type SLocation = AnLocation Span
type SFile = AnFile Span
type SMainCircuit = AnMainCircuit Span

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

data Item s = Function !(AnString s) ![AnString s] !(AnBlock s)
            | Template !(AnString s) ![AnString s] !(AnBlock s)
            | Include !(AnString s)
            | Main !(AnStatement s)
            deriving (Show,Eq)

instance Functor Item where
  fmap f i = case i of
    Function name args block ->
      Function (f <$> name) (map (fmap f) args) (mapAnnsL f block)
    Template name args block ->
      Template (f <$> name) (map (fmap f) args) (mapAnnsL f block)
    Include path -> Include (f <$> path)
    Main    s    -> Main (mapAnns f s)

itemAsFunction :: Item s -> Maybe (AnString s, [AnString s], AnBlock s)
itemAsFunction (Function a b c) = Just (a, b, c)
itemAsFunction _                = Nothing
itemAsTemplate :: Item s -> Maybe (AnString s, [AnString s], AnBlock s)
itemAsTemplate (Template a b c) = Just (a, b, c)
itemAsTemplate _                = Nothing
itemAsMain :: Item s -> Maybe (AnStatement s)
itemAsMain (Main c) = Just c
itemAsMain _        = Nothing
itemAsInclude :: Item s -> Maybe (AnString s)
itemAsInclude (Include c) = Just c
itemAsInclude _           = Nothing

collectFunctions :: File s -> [(AnString s, [AnString s], AnBlock s)]
collectFunctions = mapMaybe (itemAsFunction . ast)
collectTemplates :: File s -> [(AnString s, [AnString s], AnBlock s)]
collectTemplates = mapMaybe (itemAsTemplate . ast)
collectMains :: File s -> [AnStatement s]
collectMains = mapMaybe (itemAsMain . ast)
collectIncludes :: File s -> [AnString s]
collectIncludes = mapMaybe (itemAsInclude . ast)

type File s = [AnItem s]

data MainCircuit s = MainCircuit { main :: !(AnStatement s)
                                 , functions :: !(Map.Map String ([String], AnBlock s))
                                 , templates :: !(Map.Map String ([String], AnBlock s))
                                 }

instance Functor MainCircuit where
  fmap f (MainCircuit m fs ts) = MainCircuit
    (mapAnns f m)
    (Map.map (second (mapAnnsL f)) fs)
    (Map.map (second (mapAnnsL f)) ts)


type Block s = [AnStatement s]

data Statement s = Assign !(AnLocation s) !(AnExpr s)
                 | OpAssign BinOp !(AnLocation s) !(AnExpr s)
                 | AssignConstrain !(AnLocation s) !(AnExpr s)
                 | Constrain !(AnExpr s) !(AnExpr s)
                 | VarDeclaration !(AnString s) ![AnExpr s] !(Maybe (AnExpr s))
                 | SigDeclaration !(AnString s) !SignalKind ![AnExpr s]
                 | SubDeclaration !(AnString s) ![AnExpr s] !(Maybe (AnExpr s))
                 | If !(AnExpr s) !(AnBlock s) !(Maybe (AnBlock s))
                 | For !(AnStatement s) !(AnExpr s) !(AnStatement s) !(AnBlock s)
                 | While !(AnExpr s) !(AnBlock s)
                 | DoWhile !(AnBlock s) !(AnExpr s)
                 | Compute !(AnBlock s)
                 | Return !(AnExpr s)
                 | Log !(AnExpr s)
                 | Ignore !(AnExpr s) -- Expression statements
                 deriving (Show,Eq)

instance Functor Statement where
  fmap f s = case s of
    Assign l e          -> Assign (mapAnns f l) (mapAnns f e)
    OpAssign o l e      -> OpAssign o (mapAnns f l) (mapAnns f e)
    AssignConstrain l e -> AssignConstrain (mapAnns f l) (mapAnns f e)
    Constrain       l r -> Constrain (mapAnns f l) (mapAnns f r)
    VarDeclaration n ds i ->
      VarDeclaration (f <$> n) (map (mapAnns f) ds) (fmap (mapAnns f) i)
    SigDeclaration n k ds -> SigDeclaration (f <$> n) k (map (mapAnns f) ds)
    SubDeclaration n ds i ->
      SubDeclaration (f <$> n) (map (mapAnns f) ds) (fmap (mapAnns f) i)
    If c i e -> If (mapAnns f c) (mapAnnsL f i) (fmap (mapAnnsL f) e)
    For i c st b ->
      For (mapAnns f i) (mapAnns f c) (mapAnns f st) (mapAnnsL f b)
    While   c b -> While (mapAnns f c) (mapAnnsL f b)
    DoWhile b c -> DoWhile (mapAnnsL f b) (mapAnns f c)
    Compute b   -> Compute (mapAnnsL f b)
    Return  e   -> Return (mapAnns f e)
    Log     e   -> Log (mapAnns f e)
    Ignore  e   -> Ignore (mapAnns f e)

data SignalKind = PublicIn
                | Out
                | PrivateIn
                | Local
                deriving (Show,Eq,Ord)

isPublic :: SignalKind -> Bool
isPublic s = case s of
  PublicIn  -> True
  PrivateIn -> False
  Out       -> True
  Local     -> False

isInput :: SignalKind -> Bool
isInput s = case s of
  PublicIn  -> True
  PrivateIn -> True
  Out       -> False
  Local     -> False

isVisible :: SignalKind -> Bool
isVisible s = case s of
  PublicIn  -> True
  PrivateIn -> True
  Out       -> True
  Local     -> False

type IndexedIdent s = (AnString s, [AnExpr s])

data Location s = LocalLocation !(AnIndexedIdent s)
                | ForeignLocation !(AnIndexedIdent s) !(AnIndexedIdent s)
                deriving (Show,Eq)

instance Functor Location where
  fmap f l = case l of
    LocalLocation i     -> LocalLocation (mapP i)
    ForeignLocation i j -> ForeignLocation (mapP i) (mapP j)
   where
    mapP (Annotated (n, e) a) = Annotated (f <$> n, map (mapAnns f) e) (f a)

data UnMutOp = PreInc
             | PostInc
             | PreDec
             | PostDec
             deriving (Show,Eq)

unMutOpTime :: UnMutOp -> UnMutOpTime
unMutOpTime o = case o of
  PreInc  -> Pre
  PostInc -> Post
  PreDec  -> Pre
  PostDec -> Post

unMutOpOp :: UnMutOp -> UnMutOpOp
unMutOpOp o = case o of
  PreInc  -> Inc
  PostInc -> Inc
  PreDec  -> Dec
  PostDec -> Dec

data UnMutOpTime = Pre | Post deriving (Show, Eq)
data UnMutOpOp = Inc | Dec deriving (Show, Eq)

data UnOp = UnNeg
          | BitNot
          | Not
          | UnPos
          deriving (Show,Eq)

data Expr s = BinExpr !BinOp !(AnExpr s) !(AnExpr s)
            | UnExpr !UnOp !(AnExpr s)
            | UnMutExpr !UnMutOp !(AnLocation s)
            | Ite !(AnExpr s) !(AnExpr s) !(AnExpr s)
            | LValue !(AnLocation s)
            | Call !(AnString s) ![AnExpr s]
            | ArrayLit ![AnExpr s]
            | NumLit !Int
            deriving (Show,Eq)

instance Functor Expr where
  fmap f e = case e of
    BinExpr o l r  -> BinExpr o (mapAnns f l) (mapAnns f r)
    UnExpr    o e' -> UnExpr o (mapAnns f e')
    UnMutExpr o e' -> UnMutExpr o (mapAnns f e')
    Ite c i e'     -> Ite (mapAnns f c) (mapAnns f i) (mapAnns f e')
    Call n args    -> Call (f <$> n) (map (mapAnns f) args)
    ArrayLit es    -> ArrayLit (map (mapAnns f) es)
    NumLit   i     -> NumLit i
    LValue   l     -> LValue (mapAnns f l)

module AST.Zokrates where

import           AST.Util
import           Data.Bifunctor                 ( Bifunctor(bimap) )
import           Data.Map                       ( Map )

data Literal = BoolLit !Bool
             | IntLit !Integer
             | HexLit !Int      -- ^ nbits
                      !Integer  -- ^ value
            deriving Show

data Prim = Field | Bool | U8 | U16 | U32
              deriving Show

data Type s = Type ![AnInteger s] !(AnPrim s)
            | UStruct !(AnString s)
            deriving Show

data Op = Plus | Minus | Times | Div | Pow | Shl | Shr | BitAnd | BitOr | BitXor | And | Or | Eq | Neq | Gt | Ge | Lt | Le
            deriving Show

data UnOp = Not | Neg
            deriving Show

data Expr s = Ident !(AnString s)
            | LitExpr !(AnLiteral s)
            | IfElse !(AnExpr s) !(AnExpr s) !(AnExpr s)
            | Bin !Op !(AnExpr s) !(AnExpr s)
            | Un !UnOp !(AnExpr s)
            | Call !(AnString s) ![AnExpr s]
            | Array [AnElemExpr s]
            | Repeat !(AnExpr s) !(AnExpr s)
            | Member !(AnExpr s) !(AnString s)
            | Idx !(AnExpr s) !(AnExpr s)
            | Slice !(AnExpr s) !(AnBounds s)
            | Struct !(AnString s) ![(AnString s, AnExpr s)]
            deriving Show

data ElemExpr s = Spread !(AnExpr s) | ElemExpr !(AnExpr s)
            deriving Show

data Bounds s = Bounds !(Maybe (AnExpr s)) !(Maybe (AnExpr s))
  deriving Show

data Stmt s = For !(AnType s) !(AnString s) !(AnBounds s) !(AnBlock s)
            | Assert !(AnExpr s)
            | Declare !(AnType s) !(AnString s) !(AnExpr s)
            | Assign !(AnExpr s) !(AnExpr s)
            | Return !(AnExpr s)
            deriving Show

data Block s = Block ![AnStmt s]
  deriving Show

data Func s = Func !(AnString s)
                   ![(Bool, AnType s, AnString s)] -- ^ (private, type, ident)
                   !(AnType s)
                   !(AnBlock s)
  deriving Show

-- Import (path, srcname, dstname)
data Item s = Import !(AnString s) !(Maybe (AnString s)) !(Maybe (AnString s))
            | FuncItem (Func s)
            | SDef !(AnString s) ![(AnType s, AnString s)]
            deriving Show

instance Functor Stmt where
  fmap f s = case s of
    For t n r b   -> For (mapAnns f t) (f <$> n) (mapAnns f r) (mapAnns f b)
    Assert t      -> Assert (mapAnns f t)
    Declare t n r -> Declare (mapAnns f t) (f <$> n) (mapAnns f r)
    Assign n r    -> Assign (mapAnns f n) (mapAnns f r)
    Return r      -> Return (mapAnns f r)

instance Functor Type where
  fmap f (Type ds prim) = Type ((f <$>) <$> ds) (fmap f prim)
  fmap f (UStruct n   ) = UStruct (f <$> n)

instance Functor ElemExpr where
  fmap f e = case e of
    ElemExpr e' -> ElemExpr (mapAnns f e')
    Spread   e' -> Spread (mapAnns f e')

instance Functor Expr where
  fmap f s = case s of
    Ident   b     -> Ident (f <$> b)
    LitExpr b     -> LitExpr (fmap f b)
    IfElse c t f' -> IfElse (mapAnns f c) (mapAnns f t) (mapAnns f f')
    Bin    o a b  -> Bin o (mapAnns f a) (mapAnns f b)
    Un   o a      -> Un o (mapAnns f a)
    Call o a      -> Call (f <$> o) (mapAnns f <$> a)
    Array a       -> Array (mapAnns f <$> a)
    Repeat a b    -> Repeat (mapAnns f a) (mapAnns f b)
    Member a b    -> Member (mapAnns f a) (fmap f b)
    Idx    a b    -> Idx (mapAnns f a) (mapAnns f b)
    Slice  a b    -> Slice (mapAnns f a) (mapAnns f b)
    Struct a b    -> Struct (f <$> a) (bimap (f <$>) (mapAnns f) <$> b)

instance Functor Bounds where
  fmap f (Bounds a b) = Bounds (mapAnns f <$> a) (mapAnns f <$> b)

instance Functor Block where
  fmap f (Block b) = Block $ fmap (mapAnns f) b

instance Functor Func where
  fmap f (Func n is t b) = Func
    (f <$> n)
    (map (\(a, b', c) -> (a, mapAnns f b', f <$> c)) is)
    (mapAnns f t)
    (mapAnns f b)

instance Functor Item where
  fmap f i = case i of
    Import p n m -> Import (f <$> p) ((f <$>) <$> n) ((f <$>) <$> m)
    FuncItem u   -> FuncItem (f <$> u)
    SDef n mems  -> SDef (f <$> n) (bimap (mapAnns f) (f <$>) <$> mems)

collectImports
  :: [Item s] -> [(AnString s, Maybe (AnString s), Maybe (AnString s))]
collectImports l = case l of
  Import a b c : l' -> (a, b, c) : collectImports l'
  _            : l' -> collectImports l'
  []                -> []

-- Syntax trees [An]notated with arbitrary s-types.
type AnString s = Annotated String s
type AnInt s = Annotated Int s
type AnInteger s = Annotated Integer s
type AnType s = Annotated (Type s) s
type AnPrim s = Annotated Prim s
type AnOp s = Annotated Op s
type AnUnOp s = Annotated UnOp s
type AnBounds s = Annotated (Bounds s) s
type AnLiteral s = Annotated Literal s
type AnItem s = Annotated (Item s) s
type AnStmt s = Annotated (Stmt s) s
type AnBlock s = Annotated (Block s) s
type AnExpr s = Annotated (Expr s) s
type AnElemExpr s = Annotated (ElemExpr s) s

-- Syntax trees annotated with [P]os[n] pairs
type PString = AnString PosnPair
type PInt = AnInt PosnPair
type PInteger = AnInteger PosnPair
type PType = AnType PosnPair
type PPrim = AnPrim PosnPair
type POp = AnOp PosnPair
type PUnOp = AnUnOp PosnPair
type PBounds = AnBounds PosnPair
type PLiteral = AnLiteral PosnPair
type PItem = AnItem PosnPair
type PStmt = AnStmt PosnPair
type PBlock = AnBlock PosnPair
type PExpr = AnExpr PosnPair
type PElemExpr = AnElemExpr PosnPair

-- Syntax trees annotated with [S]pans
type SString = AnString Span
type SInt = AnInt Span
type SInteger = AnInteger Span
type SPrim = AnPrim Span
type SFunc = Func Span
type SType = AnType Span
type SOp = AnOp Span
type SUnOp = AnUnOp Span
type SBounds = AnBounds Span
type SLiteral = AnLiteral Span
type SItem = AnItem Span
type SStmt = AnStmt Span
type SBlock = AnBlock Span
type SExpr = AnExpr Span
type SElemExpr = AnElemExpr Span

type SFiles = Map FilePath [SItem]

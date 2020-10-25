{
module Parser.Zokrates.Parser where

import AST.Zokrates             as A
import AST.Util
import Parser.Zokrates.Lexer    as L (Posnd(..),Token(..),AlexPosn(AlexPn),start,end,str)

}

%name parseZokratesExpr expr
%name parseZokratesStatement stmt
%name parseZokratesBlock block
%name parseZokratesItem item
%name parseZokratesFile items
%tokentype { Posnd Token }
%error { parseError }

--%expect 0

%token
bool { L.Posnd _ L.Bool }
truelit { L.Posnd _ L.TrueLit }
falselit { L.Posnd _ L.FalseLit }
tintlit { L.Posnd _ (L.IntLit _) }
tstrlit { L.Posnd _ (L.StrLit _) }
hexlit { L.Posnd _ (L.HexLit _) }
u8 { L.Posnd _ L.U8 }
u16 { L.Posnd _ L.U16 }
u32 { L.Posnd _ L.U32 }
field { L.Posnd _ L.Field }
import { L.Posnd _ L.Import }
struct { L.Posnd _ L.Struct }
from { L.Posnd _ L.From }
private { L.Posnd _ L.Private }
if { L.Posnd _ L.If }
then { L.Posnd _ L.Then }
else { L.Posnd _ L.Else }
fi { L.Posnd _ L.Fi }
for { L.Posnd _ L.For }
in { L.Posnd _ L.In }
do { L.Posnd _ L.Do }
endfor { L.Posnd _ L.EndFor }
assert { L.Posnd _ L.Assert }
def { L.Posnd _ L.Def }
return { L.Posnd _ L.Return }
as { L.Posnd _ L.As }
tident { L.Posnd _ (L.Ident _) }
'...' { L.Posnd _ L.DotDotDot }
'..' { L.Posnd _ L.DotDot }
'.' { L.Posnd _ L.Dot }
';' { L.Posnd _ L.SemiColon }
':' { L.Posnd _ L.Colon }
',' { L.Posnd _ L.Comma }
'(' { L.Posnd _ L.BeginParen }
'[' { L.Posnd _ L.BeginBracket }
'{' { L.Posnd _ L.BeginBrace }
')' { L.Posnd _ L.EndParen }
']' { L.Posnd _ L.EndBracket }
'}' { L.Posnd _ L.EndBrace }
nl { L.Posnd _ L.Newline }
'->'            { L.Posnd _ (L.Symbols "->"     ) }
'='             { L.Posnd _ (L.Symbols "="      ) }
'&&'            { L.Posnd _ (L.Symbols "&&"     ) }
'||'            { L.Posnd _ (L.Symbols "||"     ) }
'=='            { L.Posnd _ (L.Symbols "=="     ) }
'<='            { L.Posnd _ (L.Symbols "<="     ) }
'>='            { L.Posnd _ (L.Symbols ">="     ) }
'!='            { L.Posnd _ (L.Symbols "!="     ) }
'>>'            { L.Posnd _ (L.Symbols ">>"     ) }
'<<'            { L.Posnd _ (L.Symbols "<<"     ) }
'**'            { L.Posnd _ (L.Symbols "**"     ) }
'+'             { L.Posnd _ (L.Symbols "+"      ) }
'-'             { L.Posnd _ (L.Symbols "-"      ) }
'*'             { L.Posnd _ (L.Symbols "*"      ) }
'/'             { L.Posnd _ (L.Symbols "/"      ) }
'^'             { L.Posnd _ (L.Symbols "^"      ) }
'&'             { L.Posnd _ (L.Symbols "&"      ) }
'|'             { L.Posnd _ (L.Symbols "|"      ) }
'<'             { L.Posnd _ (L.Symbols "<"      ) }
'>'             { L.Posnd _ (L.Symbols ">"      ) }
'!'             { L.Posnd _ (L.Symbols "!"      ) }

%nonassoc LOW
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%nonassoc '<<' '>>'
%left '+' '-'
%left '*' '/'
%right '**'
%right PRE
%left POST
%left '.'
%left '['


%%

opt(p)                  : p                         { Just $1 }
                        |                           { Nothing }

rev_list1_sep(p,sep)    : p                         { [$1] }
                        | rev_list1_sep(p,sep) nl { $1 }
                        | rev_list1_sep(p,sep) sep p { $3 : $1 }
                        | rev_list1_sep(p,sep) sep nl p { $4 : $1 }

rev_list1_sep_strict(p,sep)    : p                         { [$1] }
                        | rev_list1_sep_strict(p,sep) sep p { $3 : $1 }
                        | rev_list1_sep_strict(p,sep) sep { $1 }

list1_sep(p,sep)        : rev_list1_sep(p,sep)      { reverse $1 }
                        | nl list1_sep(p,sep)  { $2 }

list0_sep(p,sep)        : list1_sep(p,sep)          { $1 }
                        |                           { [] }

list1_sep_strict(p,sep)        : rev_list1_sep_strict(p,sep)      { reverse $1 }
                        | nl list1_sep_strict(p,sep)  { $2 }

list0_sep_strict(p,sep)        : list1_sep_strict(p,sep)          { $1 }
                        |                           { [] }

rl1(p)               : p                   { [$1] }
                     | rl1(p) p            { $2 : $1 }
rls1(p,s)            : p                   { [$1] }
                     | rls1(p,s) s p       { $3 : $1 }
rlss1(p,s)           : rlss0(p,s) s p      { $3 : $1 }
                     | rlss0(p,s) s        { $1 }
                     | p                   { [$1] }
rlss0(p,s)           : rlss0(p,s) s p      { $3 : $1 }
                     | rlss0(p,s) s        { $1 }
                     | p                   { [$1] }
                     |                     { [] }
-- reverse list (separated) (ignoring) (terminated w/ separator)
rlssi0t(p,s,i)       : rlssi0(p,s,i) s     { $1 }
                     | rlssi0t(p,s,i) i    { $1 }
                     |                     { [] }

-- reverse list (separated) (ignoring)
rlssi0(p,s,i)        : rlssi0(p,s,i) s i p   { $4 : $1 }
                     | rlssi0(p,s,i) s p     { $3 : $1 }
                     | rlssi0(p,s,i) i       { $1 }
                     | i p                   { [$2] }
                     | p                     { [$1] }
                     | i                     { [] }
                     |                       { [] }

l1(p)                : rl1(p)              { reverse $1 }
ls1(p,s)             : rls1(p,s)           { reverse $1 }
lss1(p,s)            : rlss1(p,s)          { reverse $1 }
l0(p)                : l1(p)               { $1 }
                     |                     { [] }
ls0(p,s)             : ls1(p,s)            { $1 }
                     |                     { [] }
lss0(p,s)            : rlss0(p,s)          { reverse $1 }
lssi0(p,s,i)         : rlssi0(p,s,i)       { reverse $1 }

ident    :: {PString}
         : tident     { let L.Posnd _ (L.Ident s) = $1 in pos s (tStart $1) (tEnd $1) }

intlit :: {PInteger}
       :  tintlit     { let L.Posnd _ (L.IntLit s) = $1 in pos (read s) (tStart $1) (tEnd $1) }

strlit   :: {PString}
         : tstrlit    { let L.Posnd _ (L.StrLit s) = $1 in pos s (tStart $1) (tEnd $1) }

cdimension :: {PInteger}
           : '[' intlit ']'                         { pos (ast $2) (tStart $1) (tEnd $3) }

cdimensions :: {[PInteger]}
            : l0(cdimension)                       { $1 }


lit :: {PLiteral}
    :  intlit     { pos (A.IntLit (ast $1)) (aStart $1) (aEnd $1) }
    |  hexlit     { let L.Posnd _ (L.HexLit s) = $1 in pos (A.HexLit ((length s - 2) * 4) (read s)) (tStart $1) (tEnd $1) }
    |  truelit    { pos (A.BoolLit True) (tStart $1) (tEnd $1) }
    |  falselit   { pos (A.BoolLit False) (tStart $1) (tEnd $1) }

prim :: {PPrim}
    :  field      { pos A.Field (tStart $1) (tEnd $1) }
    |  bool       { pos A.Bool (tStart $1) (tEnd $1) }
    |  u8         { pos A.U8 (tStart $1) (tEnd $1) }
    |  u16        { pos A.U16 (tStart $1) (tEnd $1) }
    |  u32        { pos A.U32 (tStart $1) (tEnd $1) }

type :: {PType}
     :  prim cdimensions { pos (A.Type $2 $1) (aStart $1) (foldr max (aEnd $1) (map aEnd $2)) }
     |  ident      { pos (A.UStruct $1) (aStart $1) (aEnd $1) }

-- The different ops are split out to enable precedence handling
expr :: {PExpr}
     : ident                                        { pos (A.Ident $1) (aStart $1) (aEnd $1) }
     | lit                                          { pos (A.LitExpr $1) (aStart $1) (aEnd $1) }
     | if expr then expr else expr fi               { pos (A.IfElse $2 $4 $6) (tStart $1) (tEnd $7) }
     | expr '&&' expr                               { pos (A.Bin A.And $1 $3) (aStart $1) (aEnd $3) }
     | expr '||' expr                               { pos (A.Bin A.Or $1 $3) (aStart $1) (aEnd $3) }
     | expr '==' expr                               { pos (A.Bin A.Eq $1 $3) (aStart $1) (aEnd $3) }
     | expr '<=' expr                               { pos (A.Bin A.Le $1 $3) (aStart $1) (aEnd $3) }
     | expr '>=' expr                               { pos (A.Bin A.Ge $1 $3) (aStart $1) (aEnd $3) }
     | expr '!=' expr                               { pos (A.Bin A.Neq $1 $3) (aStart $1) (aEnd $3) }
     | expr '>>' expr                               { pos (A.Bin A.Shr $1 $3) (aStart $1) (aEnd $3) }
     | expr '<<' expr                               { pos (A.Bin A.Shl $1 $3) (aStart $1) (aEnd $3) }
     | expr '**' expr                               { pos (A.Bin A.Pow $1 $3) (aStart $1) (aEnd $3) }
     | expr '+' expr                                { pos (A.Bin A.Plus $1 $3) (aStart $1) (aEnd $3) }
     | expr '-' expr                                { pos (A.Bin A.Minus $1 $3) (aStart $1) (aEnd $3) }
     | expr '*' expr                                { pos (A.Bin A.Times $1 $3) (aStart $1) (aEnd $3) }
     | expr '/' expr                                { pos (A.Bin A.Div $1 $3) (aStart $1) (aEnd $3) }
     | expr '^' expr                                { pos (A.Bin A.BitXor $1 $3) (aStart $1) (aEnd $3) }
     | expr '&' expr                                { pos (A.Bin A.BitAnd $1 $3) (aStart $1) (aEnd $3) }
     | expr '|' expr                                { pos (A.Bin A.BitOr $1 $3) (aStart $1) (aEnd $3) }
     | expr '<' expr                                { pos (A.Bin A.Lt $1 $3) (aStart $1) (aEnd $3) }
     | expr '>' expr                                { pos (A.Bin A.Gt $1 $3) (aStart $1) (aEnd $3) }
     | '!' expr             %prec PRE               { pos (A.Un A.Not $2) (tStart $1) (aEnd $2) }
     | '-' expr             %prec PRE               { pos (A.Un A.Neg $2) (tStart $1) (aEnd $2) }
     | ident '(' lss0(expr, ',') ')'                { pos (Call $1 $3) (aStart $1) (tEnd $4) }
     | '[' lssi0(elem_expr, ',', nl) ']'            { pos (Array $2) (tStart $1) (tEnd $3) }
     | expr '.' ident     %prec POST                { pos (A.Member $1 $3) (aStart $1) (aEnd $3) }
     | expr '[' bounds ']' %prec POST               { pos (A.Slice $1 $3) (aStart $1) (tEnd $4) }
     | expr '[' expr ']' %prec POST                 { pos (A.Idx $1 $3) (aStart $1) (tEnd $4) }
     | '[' expr ';' expr ']'                        { pos (A.Repeat $2 $4) (tStart $1) (tEnd $5) }
     | ident '{' lssi0(spair, ',', nl) '}'          { pos (A.Struct $1 $3) (aStart $1) (tEnd $4) }
     | '(' expr ')'         %prec PRE               { pos (ast $2) (tStart $1) (tEnd $3) }

spair :: {(PString, PExpr)}
      : ident ':' expr { ($1, $3) }

bounds :: {PBounds}
       : opt(expr) '..' opt(expr)                   { pos (A.Bounds $1 $3) (foldr min (tStart $2) (fmap aStart $1)) (foldr max (tEnd $2) (fmap aEnd $3)) }

elem_expr :: {PElemExpr}
          : '...' expr { pos (A.Spread $2) (tStart $1) (aEnd $2) }
          | expr       { pos (A.ElemExpr $1) (aStart $1) (aEnd $1) }

stmt :: {PStmt} 
     : for type ident in bounds do nl block endfor nl { pos (A.For $2 $3 $5 $8) (tStart $1) (tEnd $9) }
     | assert '(' expr ')' nl                         { pos (A.Assert $3) (tStart $1) (tEnd $4) }
     | type ident '=' expr nl                         { pos (A.Declare $1 $2 $4) (aStart $1) (aEnd $4) }
     | expr '=' expr nl                               { pos (A.Assign $1 $3) (aStart $1) (aEnd $3) }
     --| return expr %prec LOW                          { pos (A.Return $2) (tStart $1) (aEnd $2) }

is_private :: {Bool}
           : private                           { True }
           |                                   { False }

cblock :: {PBlock}
       : cblock stmt { let Block l = ast $1 in pos (Block (l ++ [$2])) (aStart $1) (aEnd $2) }
       | { pos (Block []) nullPosn nullPosn }

block :: {PBlock}
      : cblock return expr { let Block l = ast $1 in pos
               (Block (l ++ [pos (A.Return $3) (tStart $2) (aEnd $3)]))
               (aStart $1)
               (aEnd $3) }
      | cblock { $1 }

input :: {(Bool, PType, PString)}
      : is_private type ident   { ($1, $2, $3) }

arg_list :: {[(Bool, PType, PString)]} : ls0(input, ',') { $1 }

mem :: {(PType, PString)}
      : type ident   { ($1, $2) }

item :: {PItem}
     : def ident '(' arg_list ')' '->' type ':' l1(nl) block
         { pos (A.FuncItem (A.Func $2 $4 $7 $10)) (tStart $1) (aEnd $10) }
     | import strlit as ident                { pos (A.Import $2 Nothing (Just $4)) (tStart $1) (aEnd $4) }
     | import strlit                         { pos (A.Import $2 Nothing Nothing) (tStart $1) (aEnd $2) }
     | from strlit import ident as ident     { pos (A.Import $2 (Just $4) (Just $6)) (tStart $1) (aEnd $6) }
     | from strlit import ident              { pos (A.Import $2 (Just $4) (Just $4)) (tStart $1) (aEnd $4) }
     | struct ident '{' lss0(mem, nl) '}'    { pos (A.SDef $2 $4) (tStart $1) (tEnd $5) }

items :: {[PItem]} : lss0(item, nl) { $1 }

{

parseError :: [Posnd Token] -> a
parseError []    = error "Parse error around EOF"
parseError (t:_) = error $ "Parse error around line " ++ show l ++ ", column " ++ show c ++ " at token `" ++ L.str t ++ "`"
    where (AlexPn _ l c) = L.start t

fromA (AlexPn a l c) = Posn a l c
tEnd = fromA . L.end
tStart = fromA . L.start
aEnd = pEnd . ann
aStart = pStart . ann
pStart (PosnPair x _) = x
pEnd (PosnPair _ x) = x

pos :: a -> Posn -> Posn -> Annotated a PosnPair
pos a s e = Annotated a (PosnPair s e)
}


{
module Parser.Circom.Parser where

import AST.Circom               as AST
import Parser.Circom.Lexer      as Lexer (Token(..),AlexPosn(AlexPn),tokenStartPosn, tokenEndPosn,tokenStr)
}

%name parseCircomExpr expr
%name parseCircomStatement statement
%name parseCircomFile items
%tokentype { Token }
%error { parseError }

%expect 1

%token
        tnumlit         { Lexer.NumLit _ _          }
        tident          { Lexer.Ident _ _           }
        tstrlit         { Lexer.StrLit _ _          }
        var             { Lexer.Var _               }
        signal          { Lexer.Signal _            }
        private         { Lexer.Private _           }
        public          { Lexer.Public _            }
        input           { Lexer.Input _             }
        output          { Lexer.Output _            }
        component       { Lexer.Component _         }
        template        { Lexer.Template _          }
        function        { Lexer.Function _          }
        include         { Lexer.Include _           }
        if              { Lexer.If _                }
        else            { Lexer.Else _              }
        while           { Lexer.While _             }
        for             { Lexer.For _               }
        compute         { Lexer.Compute _           }
        do              { Lexer.Do _                }
        log             { Lexer.Log _               }
        return          { Lexer.Return _            }
        main            { Lexer.Main _              }
        ';'             { Lexer.SemiColon _         }
        ','             { Lexer.Comma _             }
        '.'             { Lexer.Dot _               }
        '('             { Lexer.BeginParen _        }
        '['             { Lexer.BeginBracket _      }
        '{'             { Lexer.BeginBrace _        }
        ')'             { Lexer.EndParen _          }
        ']'             { Lexer.EndBracket _        }
        '}'             { Lexer.EndBrace _          }
        '='             { Lexer.Symbols _ "="       }
        '==>'           { Lexer.Symbols _ "==>"     }
        '<=='           { Lexer.Symbols _ "<=="     }
        '-->'           { Lexer.Symbols _ "-->"     }
        '<--'           { Lexer.Symbols _ "<--"     }
        '==='           { Lexer.Symbols _ "==="     }
        '>>='           { Lexer.Symbols _ ">>="     }
        '<<='           { Lexer.Symbols _ "<<="     }
        '&&'            { Lexer.Symbols _ "&&"      }
        '||'            { Lexer.Symbols _ "||"      }
        '=='            { Lexer.Symbols _ "=="      }
        '<='            { Lexer.Symbols _ "<="      }
        '>='            { Lexer.Symbols _ ">="      }
        '!='            { Lexer.Symbols _ "!="      }
        '>>'            { Lexer.Symbols _ ">>"      }
        '<<'            { Lexer.Symbols _ "<<"      }
        '**'            { Lexer.Symbols _ "**"      }
        '++'            { Lexer.Symbols _ "++"      }
        '--'            { Lexer.Symbols _ "--"      }
        '+='            { Lexer.Symbols _ "+="      }
        '-='            { Lexer.Symbols _ "-="      }
        '*='            { Lexer.Symbols _ "*="      }
        '/='            { Lexer.Symbols _ "/="      }
        '//='           { Lexer.Symbols _ "//="     }
        '%='            { Lexer.Symbols _ "%="      }
        '|='            { Lexer.Symbols _ "|="      }
        '&='            { Lexer.Symbols _ "&="      }
        '^='            { Lexer.Symbols _ "^="      }
        '+'             { Lexer.Symbols _ "+"       }
        '-'             { Lexer.Symbols _ "-"       }
        '~'             { Lexer.Symbols _ "~"       }
        '*'             { Lexer.Symbols _ "*"       }
        '/'             { Lexer.Symbols _ "/"       }
        '//'            { Lexer.Symbols _ "//"      }
        rev_slash       { Lexer.Symbols _ "\\"      }
        '%'             { Lexer.Symbols _ "%"       }
        '^'             { Lexer.Symbols _ "^"       }
        '&'             { Lexer.Symbols _ "&"       }
        '|'             { Lexer.Symbols _ "|"       }
        '<'             { Lexer.Symbols _ "<"       }
        '>'             { Lexer.Symbols _ ">"       }
        '!'             { Lexer.Symbols _ "!"       }
        '?'             { Lexer.Symbols _ "?"       }
        ':'             { Lexer.Symbols _ ":"       }

%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%nonassoc '==' '!='
%nonassoc '<' '<=' '>' '>='
%nonassoc '<<' '>>'
%left '+' '-'
%left '*' '%' '/' '//' rev_slash
%right '**'
%right PRE
%left POST


%%

numlit   :: {PExpr}
         : tnumlit    { let Lexer.NumLit _ s = $1 in pos (AST.NumLit (read s)) (tStart $1) (tEnd $1) }
ident    :: {PString}
         : tident     { let Lexer.Ident _ s = $1 in pos s (tStart $1) (tEnd $1) }
strlit   :: {PString}
         : tstrlit    { let Lexer.StrLit _ s = $1 in pos s (tStart $1) (tEnd $1) }

opt(p)                  : p                         { Just $1 }
                        |                           { Nothing }

rev_list1_sep(p,sep)    : p                         { [$1] }
                        | rev_list1_sep(p,sep) sep p { $3 : $1 }

list1_sep(p,sep)        : rev_list1_sep(p,sep)      { reverse $1 }

list0_sep(p,sep)        : list1_sep(p,sep)          { $1 }
                        |                           { [] }

rev_list1(p)            : p                         { [$1] }
                        | rev_list1(p) p            { $2 : $1 }

list1(p)                : rev_list1(p)              { reverse $1 }
list0(p)                : list1(p)                  { $1 }
                        |                           { [] }

indexed_id :: {PIndexedIdent}
           : ident dimensions                       { pos ($1, $2) (aStart $1) (foldr max (aEnd $1) (map aEnd $2)) }

location :: {PLocation}
         : indexed_id                { pos (LocalLocation $1) (aStart $1) (aEnd $1) }
         | indexed_id '.' indexed_id { pos (ForeignLocation $1 $3) (aStart $1) (aEnd $3) }

dimension :: {PExpr}
          : '[' expr ']'                            { pos (AST.ast $2) (tStart $1) (tEnd $3) }

dimensions :: {[PExpr]}
           : list0(dimension)                       { $1 }

expr :: {PExpr}
     : location                                     { pos (LValue $1) (aStart $1) (aEnd $1) }
     | expr '+' expr                                { pos (BinExpr Add $1 $3) (aStart $1) (aEnd $3) }
     | expr '-' expr                                { pos (BinExpr Sub $1 $3) (aStart $1) (aEnd $3) }
     | expr '*' expr                                { pos (BinExpr Mul $1 $3) (aStart $1) (aEnd $3) }
     | expr '/' expr                                { pos (BinExpr Div $1 $3) (aStart $1) (aEnd $3) }
     | expr '//' expr                               { pos (BinExpr IntDiv $1 $3) (aStart $1) (aEnd $3) }
     | expr rev_slash expr                          { pos (BinExpr IntDiv $1 $3) (aStart $1) (aEnd $3) }
     | expr '%' expr                                { pos (BinExpr Mod $1 $3) (aStart $1) (aEnd $3) }
     | expr '<<' expr                               { pos (BinExpr Shl $1 $3) (aStart $1) (aEnd $3) }
     | expr '>>' expr                               { pos (BinExpr Shr $1 $3) (aStart $1) (aEnd $3) }
     | expr '<' expr                                { pos (BinExpr Lt $1 $3) (aStart $1) (aEnd $3) }
     | expr '>' expr                                { pos (BinExpr Gt $1 $3) (aStart $1) (aEnd $3) }
     | expr '<=' expr                               { pos (BinExpr Le $1 $3) (aStart $1) (aEnd $3) }
     | expr '>=' expr                               { pos (BinExpr Ge $1 $3) (aStart $1) (aEnd $3) }
     | expr '==' expr                               { pos (BinExpr Eq $1 $3) (aStart $1) (aEnd $3) }
     | expr '!=' expr                               { pos (BinExpr Ne $1 $3) (aStart $1) (aEnd $3) }
     | expr '&&' expr                               { pos (BinExpr And $1 $3) (aStart $1) (aEnd $3) }
     | expr '||' expr                               { pos (BinExpr Or $1 $3) (aStart $1) (aEnd $3) }
     | expr '&' expr                                { pos (BinExpr BitAnd $1 $3) (aStart $1) (aEnd $3) }
     | expr '|' expr                                { pos (BinExpr BitOr $1 $3) (aStart $1) (aEnd $3) }
     | expr '^' expr                                { pos (BinExpr BitXor $1 $3) (aStart $1) (aEnd $3) }
     | expr '**' expr                               { pos (BinExpr Pow $1 $3) (aStart $1) (aEnd $3) }
     | '++' location        %prec PRE               { pos (UnMutExpr PreInc $2) (tStart $1) (aEnd $2) }
     | '--' location        %prec PRE               { pos (UnMutExpr PreDec $2) (tStart $1) (aEnd $2) }
     | '!' expr             %prec PRE               { pos (UnExpr Not $2) (tStart $1) (aEnd $2) }
     | '~' expr             %prec PRE               { pos (UnExpr BitNot $2) (tStart $1) (aEnd $2) }
     | '-' expr             %prec PRE               { pos (UnExpr UnNeg $2) (tStart $1) (aEnd $2) }
     | '+' expr             %prec PRE               { pos (UnExpr UnPos $2) (tStart $1) (aEnd $2) }
     | location '++'        %prec POST              { pos (UnMutExpr PostInc $1) (aStart $1) (tEnd $2) }
     | location '--'        %prec POST              { pos (UnMutExpr PostDec $1) (aStart $1) (tEnd $2) }
     | ident '(' list0_sep(expr, ',') ')'  %prec POST   { pos (Call $1 $3) (aStart $1) (tEnd $4) }
     | '[' list0_sep(expr, ',') ']'        %prec PRE    { pos (ArrayLit $2) (tStart $1) (tEnd $3) }
     | '(' expr ')'         %prec PRE               { pos (AST.ast $2) (tStart $1) (tEnd $3) }
     | numlit                                       { $1 }
     | expr '?' expr ':' expr                       { pos (Ite $1 $3 $5) (aStart $1) (aEnd $5) }

assignment_op :: {BinOp}
              : '+='                            { Add }
              | '-='                            { Sub }
              | '*='                            { Mul }
              | '/='                            { Div }
              | '//='                           { IntDiv }
              | '%='                            { Mod }
              | '|='                            { BitOr }
              | '&='                            { BitAnd }
              | '^='                            { BitXor }
              | '<<='                           { Shl }
              | '>>='                           { Shr }

sig_kind :: {SignalKind}
          : public input                        { PublicIn }
          | private input                       { PrivateIn }
          | input                               { PublicIn }
          | output                              { Out }
          |                                     { Local }

block :: {PBlock}
      : '{' list0(statement) '}'                { pos $2 (tStart $1) (tEnd $3) }

-- an "abbreviatable" block
ablock :: {PBlock}
       : block                            { $1 }
       | statement                        { pos [ $1 ] (aStart $1) (aEnd $1) }



statement :: {PStatement} 
          : line ';'                                    { pos (AST.ast $1) (aStart $1) (tEnd $2) }
          | if '(' expr ')' ablock                      { pos (AST.If $3 $5 Nothing) (tStart $1) (aEnd $5) }
          -- NB: there is a shift-reduce conflict here that is resolved in favor of the else.
          | if '(' expr ')' ablock else ablock          { pos (AST.If $3 $5 (Just $7)) (tStart $1) (aEnd $7) }
          | while '(' expr ')' ablock                   { pos (AST.While $3 $5) (tStart $1) (aEnd $5) }
          | do ablock while '(' expr ')'                { pos (AST.DoWhile $2 $5) (tStart $1) (tEnd $6) }
          | for '(' line ';' expr ';' line ')' ablock   { pos (AST.For $3 $5 $7 $9) (tStart $1) (aEnd $9) }
          | compute ablock                              { pos (AST.Compute $2) (tStart $1) (aEnd $2) }

line :: {PStatement}
     : location '=' expr                                { pos (Assign $1 $3) (aStart $1) (aEnd $3) }
     | location assignment_op expr                      { pos (OpAssign $2 $1 $3) (aStart $1) (aEnd $3) }
     | location '<--' expr                              { pos (Assign $1 $3) (aStart $1) (aEnd $3) }
     | expr '-->' location                              { pos (Assign $3 $1) (aStart $1) (aEnd $3) }
     | location '<==' expr                              { pos (AssignConstrain $1 $3) (aStart $1) (aEnd $3) }
     | expr '==>' location                              { pos (AssignConstrain $3 $1) (aStart $1) (aEnd $3) }
     | expr '===' expr                                  { pos (Constrain $1 $3) (aStart $1) (aEnd $3) }
     | var ident dimensions                             { pos (VarDeclaration $2 $3 Nothing) (tStart $1) (foldr max (aEnd $2) (map aEnd $3)) }
     | var ident dimensions '=' expr                    { pos (VarDeclaration $2 $3 (Just $5)) (tStart $1) (aEnd $5) }
     | signal sig_kind ident dimensions                 { pos (SigDeclaration $3 $2 $4) (tStart $1) (foldr max (aEnd $3) (map aEnd $4)) }
     | component ident dimensions                       { pos (SubDeclaration $2 $3 Nothing) (tStart $1) (foldr max (aEnd $2) (map aEnd $3)) }
     | component ident dimensions '=' expr              { pos (SubDeclaration $2 $3 (Just $5)) (tStart $1) (aEnd $5) }
     | return expr                                      { pos (AST.Return $2) (tStart $1) (aEnd $2) }
     | log '(' expr ')'                                 { pos (AST.Log $3) (tStart $1) (tEnd $4) }
     | expr                                             { pos (AST.Ignore $1) (aStart $1) (aEnd $1) }

ident_list : list0_sep(ident, ',')                      { $1 }

item :: {PItem}
     : function ident '(' ident_list ')' block          { pos (AST.Function $2 $4 $6) (tStart $1) (aEnd $6) }
     | template ident '(' ident_list ')' block          { pos (AST.Template $2 $4 $6) (tStart $1) (aEnd $6) }
     | include strlit ';'                               { pos (AST.Include $2) (tStart $1) (tEnd $3) }
     | include strlit                                   { pos (AST.Include $2) (tStart $1) (aEnd $2) }
     | component main '=' expr ';'                      { pos (AST.Main (pos (AST.SubDeclaration (pos "main" (tStart $2) (tEnd $2)) [] (Just $4)) (tStart $1) (tEnd $5))) (tStart $1) (tEnd $5) }
     | component main '=' expr                          { pos (AST.Main (pos (AST.SubDeclaration (pos "main" (tStart $2) (tEnd $2)) [] (Just $4)) (tStart $1) (aEnd $4))) (tStart $1) (aEnd $4) }

items :: {[PItem]} : list0(item)                        { $1 }

{

parseError :: [Token] -> a
parseError []    = error "Parse error around EOF"
parseError (t:_) = error $ "Parse error around line " ++ show l ++ ", column " ++ show c ++ " at token `" ++ tokenStr t ++ "`"
    where (AlexPn _ l c) = tokenStartPosn t

fromA (AlexPn a l c) = AST.Posn a l c
tEnd = fromA . tokenEndPosn
tStart = fromA . tokenStartPosn
aEnd = pEnd . ann
aStart = pStart . ann
pStart (AST.PosnPair x _) = x
pEnd (AST.PosnPair _ x) = x

pos :: a -> AST.Posn -> AST.Posn -> Annotated a AST.PosnPair
pos a s e = Annotated a (AST.PosnPair s e)
}


{
module Parser.Circom.Parser where

import AST.Circom               as AST
import Parser.Circom.Lexer      as Lexer (Token(..),tokenize,AlexPosn(AlexPn),tokenPosn)
}

%name parseCircomExpr expr
%name parseCircomStatement statement
%name parseCircomFile items
%tokentype { Token }
%error { parseError }

%token
        numlit          { Lexer.NumLit _ $$         }
        ident           { Lexer.Ident _ $$          }
        strlit          { Lexer.StrLit _ $$         }
        var             { Lexer.Var _               }
        signal          { Lexer.Signal _            }
        private         { Lexer.Private _           }
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
%left '*' '%' '/' '//'
%right '**'
%right PRE
%left POST


%%

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


location :: {Location}
         : ident                                    { AST.Ident $1 }
         | location '.' ident     %prec POST        { Pin $1 $3 }
         | location '[' expr ']'  %prec POST        { Index $1 $3 }

dimension : '[' expr ']'                            { $2 }

decl_dimensions :: {[Expr]}
                : list0(dimension)                  { $1 }

expr :: {Expr}
     : location                                     { LValue $1 }
     | expr '+' expr                                { BinExpr Add $1 $3 }
     | expr '-' expr                                { BinExpr Sub $1 $3 }
     | expr '*' expr                                { BinExpr Mul $1 $3 }
     | expr '/' expr                                { BinExpr Div $1 $3 }
     | expr '//' expr                               { BinExpr IntDiv $1 $3 }
     | expr rev_slash expr                          { BinExpr IntDiv $1 $3 }
     | expr '%' expr                                { BinExpr Mod $1 $3 }
     | expr '<<' expr                               { BinExpr Shl $1 $3 }
     | expr '>>' expr                               { BinExpr Shr $1 $3 }
     | expr '<' expr                                { BinExpr Lt $1 $3 }
     | expr '>' expr                                { BinExpr Gt $1 $3 }
     | expr '<=' expr                               { BinExpr Le $1 $3 }
     | expr '>=' expr                               { BinExpr Ge $1 $3 }
     | expr '==' expr                               { BinExpr Eq $1 $3 }
     | expr '!=' expr                               { BinExpr Ne $1 $3 }
     | expr '&&' expr                               { BinExpr And $1 $3 }
     | expr '||' expr                               { BinExpr Or $1 $3 }
     | expr '&' expr                                { BinExpr BitAnd $1 $3 }
     | expr '|' expr                                { BinExpr BitOr $1 $3 }
     | expr '^' expr                                { BinExpr BitXor $1 $3 }
     | expr '**' expr                               { BinExpr Pow $1 $3 }
     | '++' expr            %prec PRE               { UnExpr PreInc $2 }
     | '--' expr            %prec PRE               { UnExpr PreDec $2 }
     | '!' expr             %prec PRE               { UnExpr Not $2 }
     | '~' expr             %prec PRE               { UnExpr BitNot $2 }
     | '-' expr             %prec PRE               { UnExpr UnNeg $2 }
     | '+' expr             %prec PRE               { UnExpr UnPos $2 }
     | expr '++'            %prec POST              { UnExpr PostInc $1 }
     | expr '--'            %prec POST              { UnExpr PostDec $1 }
     | ident '(' list0_sep(expr, ',') ')'  %prec POST   { Call $1 $3 }
     | '[' list0_sep(expr, ',') ']'        %prec PRE    { ArrayLit $2 }
     | '(' expr ')'         %prec PRE               { $2 }
     | numlit                                       { AST.NumLit $1 }
     | expr '?' expr ':' expr                       { Ite $1 $3 $5 }

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
          : input                               { In }
          | output                              { Out }
          |                                     { Local }

block :: {[Statement]}
      : '{' list0(statement) '}'                { $2 }

-- an "abbreviatable" block
ablock :: {[Statement]}
       : block                            { $1 }
       | statement                        { [ $1 ] }



statement :: {Statement} 
          : line ';'                                    { $1 }
          | if '(' expr ')' ablock                      { AST.If $3 $5 Nothing }
          | if '(' expr ')' ablock else ablock          { AST.If $3 $5 (Just $7) }
          | while '(' expr ')' ablock                   { AST.While $3 $5 }
          | do ablock while '(' expr ')'                { AST.DoWhile $2 $5 }
          | for '(' line ';' expr ';' line ')' ablock   { AST.For $3 $5 $7 $9 }
          | compute ablock                              { AST.Compute $2 }

line :: {Statement}
     : location '=' expr                                { Assign $1 $3 }
     | location assignment_op expr                      { OpAssign $2 $1 $3 }
     | location '<--' expr                              { Assign $1 $3 }
     | expr '-->' location                              { Assign $3 $1 }
     | location '<==' expr                              { AssignConstrain $1 $3 }
     | expr '==>' location                              { AssignConstrain $3 $1 }
     | expr '===' expr                                  { Constrain $1 $3 }
     | var ident decl_dimensions                        { VarDeclaration $2 $3 Nothing }
     | var ident decl_dimensions '=' expr               { VarDeclaration $2 $3 (Just $5) }
     | signal private sig_kind ident decl_dimensions    { SigDeclaration $4 $3 $5 }
     | signal         sig_kind ident decl_dimensions    { SigDeclaration $3 $2 $4 }
     | component ident decl_dimensions                  { SubDeclaration $2 $3 Nothing }
     | component ident decl_dimensions '=' expr         { SubDeclaration $2 $3 (Just $5) }
     | return expr                                      { AST.Return $2 }
     | expr                                             { AST.Ignore $1 }

ident_list : list0_sep(ident, ',')                      { $1 }

item :: {Item}
     : function ident '(' ident_list ')' block          { AST.Function $2 $4 $6 }
     | template ident '(' ident_list ')' block          { AST.Template $2 $4 $6 }
     | include strlit ';'                               { AST.Include $2 }
     | component main '=' expr ';'                      { AST.Main $4 }

items :: {[Item]} : list0(item)                         { $1}

{

parseError :: [Token] -> a
parseError []    = error "Parse error around EOF"
parseError (t:_) = error $ "Parse error around line " ++ show l ++ ", column " ++ show c ++ " at token `" ++ show t ++ "`"
    where (AlexPn _ l c) = tokenPosn t
}


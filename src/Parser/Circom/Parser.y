{
module Parser.Circom.Parser where

import AST.Circom               as AST
import Parser.Circom.Lexer      as Lexer (Token(..),tokenize)
}

%name parseCircomExpr expr
%name parseCircomStatement statement
%name parseCircomFile items
%tokentype { Token }
%error { parseError }

%token
        numlit          { Lexer.NumLit $$     }
        ident           { Lexer.Ident $$      }
        strlit          { Lexer.StrLit $$     }
        var             { Lexer.Var           }
        signal          { Lexer.Signal        }
        private         { Lexer.Private       }
        input           { Lexer.Input         }
        output          { Lexer.Output        }
        component       { Lexer.Component     }
        template        { Lexer.Template      }
        function        { Lexer.Function      }
        include         { Lexer.Include       }
        if              { Lexer.If            }
        else            { Lexer.Else          }
        while           { Lexer.While         }
        for             { Lexer.For           }
        compute         { Lexer.Compute       }
        do              { Lexer.Do            }
        return          { Lexer.Return        }
        main            { Lexer.Main          }
        ';'             { Lexer.SemiColon     }
        ','             { Lexer.Comma         }
        '.'             { Lexer.Dot           }
        '('             { Lexer.BeginParen    }
        '['             { Lexer.BeginBracket  }
        '{'             { Lexer.BeginBrace    }
        ')'             { Lexer.EndParen      }
        ']'             { Lexer.EndBracket    }
        '}'             { Lexer.EndBrace      }
        '='             { Lexer.Symbols "="   }
        '==>'           { Lexer.Symbols "==>" }
        '<=='           { Lexer.Symbols "<==" }
        '-->'           { Lexer.Symbols "-->" }
        '<--'           { Lexer.Symbols "<--" }
        '==='           { Lexer.Symbols "===" }
        '>>='           { Lexer.Symbols ">>=" }
        '<<='           { Lexer.Symbols "<<=" }
        '&&'            { Lexer.Symbols "&&"  }
        '||'            { Lexer.Symbols "||"  }
        '=='            { Lexer.Symbols "=="  }
        '<='            { Lexer.Symbols "<="  }
        '>='            { Lexer.Symbols ">="  }
        '!='            { Lexer.Symbols "!="  }
        '>>'            { Lexer.Symbols ">>"  }
        '<<'            { Lexer.Symbols "<<"  }
        '**'            { Lexer.Symbols "**"  }
        '++'            { Lexer.Symbols "++"  }
        '--'            { Lexer.Symbols "--"  }
        '+='            { Lexer.Symbols "+="  }
        '-='            { Lexer.Symbols "-="  }
        '*='            { Lexer.Symbols "*="  }
        '/='            { Lexer.Symbols "/="  }
        '//='           { Lexer.Symbols "//="  }
        '%='            { Lexer.Symbols "%="  }
        '|='            { Lexer.Symbols "|="  }
        '&='            { Lexer.Symbols "&="  }
        '^='            { Lexer.Symbols "^="  }
        '+'             { Lexer.Symbols "+"   }
        '-'             { Lexer.Symbols "-"   }
        '~'             { Lexer.Symbols "~"   }
        '*'             { Lexer.Symbols "*"   }
        '/'             { Lexer.Symbols "/"   }
        '//'            { Lexer.Symbols "//"  }
        '%'             { Lexer.Symbols "%"   }
        '^'             { Lexer.Symbols "^"   }
        '&'             { Lexer.Symbols "&"   }
        '|'             { Lexer.Symbols "|"   }
        '<'             { Lexer.Symbols "<"   }
        '>'             { Lexer.Symbols ">"   }
        '!'             { Lexer.Symbols "!"   }
        '?'             { Lexer.Symbols "?"   }
        ':'             { Lexer.Symbols ":"   }

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
     | expr '%' expr                                { BinExpr Mod $1 $3 }
     | expr '<<' expr                               { BinExpr Shl $1 $3 }
     | expr '>>' expr                               { BinExpr Shr $1 $3 }
     | expr '<' expr                                { BinExpr Lt $1 $3 }
     | expr '>' expr                                { BinExpr Gt $1 $3 }
     | expr '<=' expr                               { BinExpr Le $1 $3 }
     | expr '>=' expr                               { BinExpr Ge $1 $3 }
     | expr '=' expr                                { BinExpr Eq $1 $3 }
     | expr '!' expr                                { BinExpr Ne $1 $3 }
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

direction :: {Direction}
          : input                               { In }
          | output                              { Out }

block :: {[Statement]}
      : '{' list0(statement) '}'                { $2 }



statement :: {Statement} 
          : line ';'                                    { $1 }
          | if '(' expr ')' block                       { AST.If $3 $5 Nothing }
          | if '(' expr ')' block else block            { AST.If $3 $5 (Just $7) }
          | while '(' expr ')' block                    { AST.While $3 $5 }
          | for '(' line ';' expr ';' line ')' block    { AST.For $3 $5 $7 $9 }
          | compute block                               { AST.Compute $2 }

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
     | signal private direction ident decl_dimensions   { SigDeclaration $4 $3 $5 }
     | signal         direction ident decl_dimensions   { SigDeclaration $3 $2 $4 }
     | component ident decl_dimensions                  { SubDeclaration $2 $3 Nothing }
     | component ident decl_dimensions '=' expr         { SubDeclaration $2 $3 (Just $5) }
     | return expr                                      { AST.Return $2 }
     | expr                                             { AST.Ignore $1 }

ident_list : list0_sep(ident, ',')                      { $1 }

item :: {Item}
     : function ident '(' ident_list ')' block          { AST.Function $2 $4 $6 }
     | template ident '(' ident_list ')' block          { AST.Template $2 $4 $6 }
     | include strlit                                   { AST.Include $2 }
     | component main '=' expr ';'                      { AST.Main $4 }

items :: {[Item]} : list0(item)                         { $1}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"
}


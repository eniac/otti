{
module Parser.Circom.Parser where

import AST.Circom               (BinOp(..),Expr(..),Location(..),UnOp(..))
import Parser.Circom.Lexer      (Token(..),tokenize)
}

%name parseCircomExpr expr
%tokentype { Token }
%error { parseError }

%token
        numlit          { Parser.Circom.Lexer.NumLit $$ }
        ident           { Parser.Circom.Lexer.Ident $$  }
        var             { Var           }
        signal          { Signal        }
        private         { Private       }
        input           { Input         }
        output          { Output        }
        component       { Component     }
        template        { Template      }
        function        { Function      }
        if              { If            }
        else            { Else          }
        while           { While         }
        compute         { Compute       }
        do              { Do            }
        return          { Return        }
        ';'             { SemiColon     }
        ','             { Comma         }
        '.'             { Dot           }
        '('             { BeginParen    }
        '['             { BeginBracket  }
        '{'             { BeginBrace    }
        ')'             { EndParen      }
        ']'             { EndBracket    }
        '}'             { EndBrace      }
        '='             { Symbols "="   }
        '==>'           { Symbols "==>" }
        '<=='           { Symbols "<==" }
        '-->'           { Symbols "-->" }
        '<--'           { Symbols "<--" }
        '==='           { Symbols "===" }
        '>>='           { Symbols ">>=" }
        '<<='           { Symbols "<<=" }
        '&&'            { Symbols "&&"  }
        '||'            { Symbols "||"  }
        '=='            { Symbols "=="  }
        '<='            { Symbols "<="  }
        '>='            { Symbols ">="  }
        '!='            { Symbols "!="  }
        '>>'            { Symbols ">>"  }
        '<<'            { Symbols "<<"  }
        '**'            { Symbols "**"  }
        '++'            { Symbols "++"  }
        '--'            { Symbols "--"  }
        '+='            { Symbols "+="  }
        '-='            { Symbols "-="  }
        '*='            { Symbols "*="  }
        '/='            { Symbols "/="  }
        '%='            { Symbols "%="  }
        '|='            { Symbols "|="  }
        '&='            { Symbols "&="  }
        '^='            { Symbols "^="  }
        '+'             { Symbols "+"   }
        '-'             { Symbols "-"   }
        '~'             { Symbols "~"   }
        '*'             { Symbols "*"   }
        '/'             { Symbols "/"   }
        '//'            { Symbols "//"  }
        '%'             { Symbols "%"   }
        '^'             { Symbols "^"   }
        '&'             { Symbols "&"   }
        '|'             { Symbols "|"   }
        '<'             { Symbols "<"   }
        '>'             { Symbols ">"   }
        '!'             { Symbols "!"   }
        '?'             { Symbols "?"   }
        ':'             { Symbols ":"   }

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

location :: {Location}
         : ident                                { AST.Circom.Ident $1 }
         | location '.' ident     %prec POST    { Pin $1 $3 }
         | location '[' expr ']'  %prec POST    { Index $1 $3 }

exprs :: {[Expr]}
      : exprsrev                                { reverse $1 }

exprsrev :: {[Expr]}
         : exprsrev ',' expr                    { $3 : $1 }
         | expr                                 { [$1] }

expr :: {Expr}
     : location                                 { LValue $1 }
     | expr '+' expr                            { BinExpr Add $1 $3 }
     | expr '-' expr                            { BinExpr Sub $1 $3 }
     | expr '*' expr                            { BinExpr Mul $1 $3 }
     | expr '/' expr                            { BinExpr Div $1 $3 }
     | expr '//' expr                           { BinExpr IntDiv $1 $3 }
     | expr '%' expr                            { BinExpr Mod $1 $3 }
     | expr '<<' expr                           { BinExpr Shl $1 $3 }
     | expr '>>' expr                           { BinExpr Shr $1 $3 }
     | expr '<' expr                            { BinExpr Lt $1 $3 }
     | expr '>' expr                            { BinExpr Gt $1 $3 }
     | expr '<=' expr                           { BinExpr Le $1 $3 }
     | expr '>=' expr                           { BinExpr Ge $1 $3 }
     | expr '=' expr                            { BinExpr Eq $1 $3 }
     | expr '!' expr                            { BinExpr Ne $1 $3 }
     | expr '&&' expr                           { BinExpr And $1 $3 }
     | expr '||' expr                           { BinExpr Or $1 $3 }
     | expr '&' expr                            { BinExpr BitAnd $1 $3 }
     | expr '|' expr                            { BinExpr BitOr $1 $3 }
     | expr '^' expr                            { BinExpr BitXor $1 $3 }
     | expr '**' expr                           { BinExpr Pow $1 $3 }
     | '++' expr            %prec PRE           { UnExpr PreInc $2 }
     | '--' expr            %prec PRE           { UnExpr PreDec $2 }
     | '!' expr             %prec PRE           { UnExpr Not $2 }
     | '~' expr             %prec PRE           { UnExpr BitNot $2 }
     | '-' expr             %prec PRE           { UnExpr UnNeg $2 }
     | '+' expr             %prec PRE           { UnExpr UnPos $2 }
     | expr '++'            %prec POST          { UnExpr PostInc $1 }
     | expr '--'            %prec POST          { UnExpr PostDec $1 }
     | ident '(' exprs ')'  %prec POST          { Call $1 $3 }
     | '[' exprs ']'        %prec PRE           { ArrayLit $2 }
     | '(' expr ')'         %prec PRE           { $2 }
     | numlit                                   { AST.Circom.NumLit $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"
}


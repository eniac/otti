{
module Parser.Circom.Lexer (tokenize, tokenPosn, Token(..), AlexPosn(AlexPn)) where
}

%wrapper "posn"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-

  $white+                                           ;
  \/\/[^\r\n]*                                      ;
  \/\*([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+\/    ;
  "var"                                 { \p s -> Var p }
  "signal"                              { \p s -> Signal p }
  "private"                             { \p s -> Private p }
  "public"                              { \p s -> Public p }
  "input"                               { \p s -> Input p }
  "output"                              { \p s -> Output p }
  "component"                           { \p s -> Component p }
  "template"                            { \p s -> Template p }
  "function"                            { \p s -> Function p }
  "include"                             { \p s -> Include p }
  "if"                                  { \p s -> If p }
  "else"                                { \p s -> Else p }
  "while"                               { \p s -> While p }
  "for"                                 { \p s -> For p }
  "compute"                             { \p s -> Compute p }
  "main"                                { \p s -> Main p }
  "do"                                  { \p s -> Do p }
  "log"                                 { \p s -> Log p }
  "return"                              { \p s -> Return p }
  "0x"[0-9 a-f A-F]+                    { \p s -> NumLit p (read s) }
  $digit+                               { \p s -> NumLit p (read s) }
  $alpha [$alpha $digit \_ \']*         { \p s -> Ident p s }
  \" ($printable # \")* \"              { \p s -> StrLit p $ take (length s - 2) $ drop 1 s }
  \;                                    { \p s -> SemiColon p }
  \,                                    { \p s -> Comma p }
  \.                                    { \p s -> Dot p }
  \(                                    { \p s -> BeginParen p }
  \[                                    { \p s -> BeginBracket p }
  \{                                    { \p s -> BeginBrace p }
  \)                                    { \p s -> EndParen p }
  \]                                    { \p s -> EndBracket p }
  \}                                    { \p s -> EndBrace p }
  [\<\>\-\=\!\~\%\^\&\*\/\\\?\:\+\|]+   { \p s -> Symbols p s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = Var AlexPosn
           | Signal AlexPosn
           | Private AlexPosn
           | Public AlexPosn
           | Input AlexPosn
           | Output AlexPosn
           | Component AlexPosn
           | Template AlexPosn
           | Function AlexPosn
           | If AlexPosn
           | Else AlexPosn
           | While AlexPosn
           | For AlexPosn
           | Compute AlexPosn
           | Do AlexPosn
           | Log AlexPosn
           | Return AlexPosn
           | Include AlexPosn
           | Main AlexPosn
           | NumLit AlexPosn Int
           | Ident AlexPosn String
           | Symbols AlexPosn String
           | StrLit AlexPosn String
           | SemiColon AlexPosn
           | Comma AlexPosn
           | Dot AlexPosn
           | BeginParen AlexPosn
           | BeginBracket AlexPosn
           | BeginBrace AlexPosn
           | EndParen AlexPosn
           | EndBracket AlexPosn
           | EndBrace AlexPosn
           deriving (Show)

--tokenize :: String -> [Token]
tokenize = alexScanTokens

tokenPosn :: Token -> AlexPosn
tokenPosn (Var p) = p
tokenPosn (Signal p) = p
tokenPosn (Private p) = p
tokenPosn (Input p) = p
tokenPosn (Output p) = p
tokenPosn (Component p) = p
tokenPosn (Template p) = p
tokenPosn (Function p) = p
tokenPosn (If p) = p
tokenPosn (Else p) = p
tokenPosn (While p) = p
tokenPosn (For p) = p
tokenPosn (Compute p) = p
tokenPosn (Do p) = p
tokenPosn (Return p) = p
tokenPosn (Include p) = p
tokenPosn (Main p) = p
tokenPosn (NumLit p _) = p
tokenPosn (Ident p _) = p
tokenPosn (Symbols p _) = p
tokenPosn (StrLit p _) = p
tokenPosn (SemiColon p) = p
tokenPosn (Comma p) = p
tokenPosn (Dot p) = p
tokenPosn (BeginParen p) = p
tokenPosn (BeginBracket p) = p
tokenPosn (BeginBrace p) = p
tokenPosn (EndParen p) = p
tokenPosn (EndBracket p) = p
tokenPosn (EndBrace p) = p

}

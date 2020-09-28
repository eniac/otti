{
{-# OPTIONS_GHC -Wno-all #-}
module Parser.Circom.Lexer (tokenize, tokenStartPosn, tokenEndPosn, tokenStr, Token(..), AlexPosn(AlexPn)) where
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
  "0x"[0-9 a-f A-F]+                    { \p s -> NumLit p s }
  $digit+                               { \p s -> NumLit p s }
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
           | NumLit AlexPosn String
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

tokenStartPosn :: Token -> AlexPosn
tokenStartPosn t = case t of
    Var p -> p
    Signal p -> p
    Private p -> p
    Input p -> p
    Output p -> p
    Component p -> p
    Template p -> p
    Function p -> p
    If p -> p
    Else p -> p
    While p -> p
    For p -> p
    Compute p -> p
    Do p -> p
    Return p -> p
    Include p -> p
    Main p -> p
    NumLit p _ -> p
    Ident p _ -> p
    Symbols p _ -> p
    StrLit p _ -> p
    SemiColon p -> p
    Comma p -> p
    Dot p -> p
    BeginParen p -> p
    BeginBracket p -> p
    BeginBrace p -> p
    EndParen p -> p
    EndBracket p -> p
    EndBrace p -> p

tokenEndPosn :: Token -> AlexPosn
tokenEndPosn t =
  let len = length $ tokenStr t
      AlexPn charN line col = tokenStartPosn t
  in  AlexPn (charN + len) line (col + len)

tokenStr :: Token -> String
tokenStr t = case t of
    Var p -> "var"
    Signal p -> "signal"
    Private p -> "private"
    Input p -> "input"
    Output p -> "output"
    Component p -> "component"
    Template p -> "template"
    Function p -> "function"
    If p -> "if"
    Else p -> "else"
    While p -> "while"
    For p -> "for"
    Compute p -> "compute"
    Do p -> "do"
    Return p -> "return"
    Include p -> "include"
    Main p -> "main"
    NumLit _ n -> n
    Ident _ i -> i
    Symbols _ s -> s
    StrLit _ l -> l
    SemiColon p -> ";"
    Comma p -> ","
    Dot p -> "."
    BeginParen p -> "("
    BeginBracket p -> "["
    BeginBrace p -> "{"
    EndParen p -> ")"
    EndBracket p -> "]"
    EndBrace p -> "}"

}

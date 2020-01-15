{
module Parser.Circom.Lexer (main, tokenize, Token(..)) where

import Debug.Trace
}

%wrapper "basic"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters

tokens :-

  $white+                               ;
  "var"                                 { \s -> Var }
  "signal"                              { \s -> Signal }
  "private"                             { \s -> Private }
  "input"                               { \s -> Input }
  "output"                              { \s -> Output }
  "component"                           { \s -> Component }
  "template"                            { \s -> Template }
  "function"                            { \s -> Function }
  "if"                                  { \s -> If }
  "else"                                { \s -> Else }
  "while"                               { \s -> While }
  "compute"                             { \s -> Compute }
  "do"                                  { \s -> Do }
  "return"                              { \s -> Return }
  "0x"$digit+                           { \s -> NumLit (read s) }
  $digit+                               { \s -> NumLit (read s) }
  $alpha [$alpha $digit \_ \']*         { \s -> Ident s }
  \;                                    { \s -> SemiColon }
  \,                                    { \s -> Comma }
  \.                                    { \s -> Dot }
  \(                                    { \s -> BeginParen }
  \[                                    { \s -> BeginBracket }
  \{                                    { \s -> BeginBrace }
  \)                                    { \s -> EndParen }
  \]                                    { \s -> EndBracket }
  \}                                    { \s -> EndBrace }
  [\<\>\-\=\!\~\%\^\&\*\/\\\?\:\+]+     { \s -> Symbols s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = Var
           | Signal
           | Private
           | Input
           | Output
           | Component
           | Template
           | Function
           | If
           | Else
           | While
           | Compute
           | Do
           | Return
           | Include
           | NumLit Int
           | Ident String
           | Symbols String
           | SemiColon
           | Comma
           | Dot
           | BeginParen
           | BeginBracket
           | BeginBrace
           | EndParen
           | EndBracket
           | EndBrace
           deriving (Show)

--           | Gets
--           | DoubleGets
--           | Puts
--           | DoublePuts
--           | Eq
--           | Shl
--           | Shr
--           | DoubleEq
--           | TripleEq
--           | Plus
--           | Star
--           | Minus
--           | Bang
--           | Tilde
--           | Slash
--           | RevSlash
--           | DoubleSlash
--           | Mod
--           | Amper
--           | DoubleAmper
--           | Pipe
--           | DoublePipe
--           | Caret
--           | Question
--           | Colon
--           | Symbols String
--  \=\=\=                                { \s -> TripleEq }
--  \<\-\-                                { \s -> Gets }
--  \<\=\=                                { \s -> DoubleGets }
--  \-\-\>                                { \s -> Puts }
--  \=\=\>                                { \s -> DoublePuts }
--  \/\/                                  { \s -> DoubleSlash }
--  \\                                    { \s -> RevSlash }
--  \=\=                                  { \s -> DoubleEq }
--  \&\&                                  { \s -> DoubleAmper }
--  \|\|                                  { \s -> DoublePipe }
--  \=                                    { \s -> Eq }
--  \?                                    { \s -> Question }
--  \:                                    { \s -> Colon }
--  \/                                    { \s -> Slash }
--  \!                                    { \s -> Bang }
--  \~                                    { \s -> Tilde }
--  \+                                    { \s -> Plus }
--  \*                                    { \s -> Star }
--  \-                                    { \s -> Minus }
--  \%                                    { \s -> Mod }
--  \&                                    { \s -> Amper }
--  \|                                    { \s -> Pipe }

main = do
  s <- getContents
  print (alexScanTokens s)

tokenize :: String -> [Token]
tokenize = alexScanTokens
}

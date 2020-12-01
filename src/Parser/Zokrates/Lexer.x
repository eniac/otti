{
{-# OPTIONS_GHC -Wno-all #-}
module Parser.Zokrates.Lexer (tokenize, Posnd(..), Token(..), AlexPosn(AlexPn), start, end, str) where
}

%wrapper "monadUserState"

$digit = 0-9                    -- digits
$alpha = [a-zA-Z]               -- alphabetic characters
$inlinewhite = [\ \t\f\v]       -- inline whitespace

tokens :-

  (\r\n|\n)+                            { tok $ \p s -> Posnd p Newline }
  $inlinewhite                                      ;
  \/\/[^\r\n]*                                      ;
  \#[^\r\n]*                                        ;
  \\                                                ;
  \/\*([^\*]|[\r\n]|(\*+([^\*\/]|[\r\n])))*\*+\/    ;
  \" ($printable # \")* \"              { tok $ \p s -> Posnd p $ StrLit $ take (length s - 2) $ drop 1 s }
  "bool"                                { tok $ \p s -> Posnd p Bool }
  "true"                                { tok $ \p s -> Posnd p TrueLit }
  "false"                               { tok $ \p s -> Posnd p FalseLit }
  $digit+                               { tok $ \p s -> Posnd p $ IntLit s }
  "u8"                                  { tok $ \p s -> Posnd p U8 }
  "u16"                                 { tok $ \p s -> Posnd p U16 }
  "u32"                                 { tok $ \p s -> Posnd p U32 }
  "field"                               { tok $ \p s -> Posnd p Field }
  "0x"[0-9 a-f A-F]+                    { tok $ \p s -> Posnd p $ HexLit s }
  "private"                             { tok $ \p s -> Posnd p Private }
  "if"                                  { tok $ \p s -> Posnd p If }
  "then"                                { tok $ \p s -> Posnd p Then }
  "else"                                { tok $ \p s -> Posnd p Else }
  "fi"                                  { tok $ \p s -> Posnd p Fi }
  "for"                                 { tok $ \p s -> Posnd p For }
  "in"                                  { tok $ \p s -> Posnd p In }
  "do"                                  { tok $ \p s -> Posnd p Do }
  "endfor"                              { tok $ \p s -> Posnd p EndFor }
  "assert"                              { tok $ \p s -> Posnd p Assert }
  "def"                                 { tok $ \p s -> Posnd p Def }
  "return"                              { tok $ \p s -> Posnd p Return }
  "struct"                              { tok $ \p s -> Posnd p Struct }
  "import"                              { tok $ \p s -> Posnd p Import }
  "from"                                { tok $ \p s -> Posnd p From }
  "as"                                  { tok $ \p s -> Posnd p As }
  $alpha [$alpha $digit \_ \']*         { tok $ \p s -> Posnd p $ Ident s }
  \,                                    { tok $ \p s -> Posnd p Comma }
  \.\.\.                                { tok $ \p s -> Posnd p DotDotDot }
  \.\.                                  { tok $ \p s -> Posnd p DotDot }
  \.                                    { tok $ \p s -> Posnd p Dot }
  \;                                    { tok $ \p s -> Posnd p SemiColon }
  \:                                    { tok $ \p s -> Posnd p Colon }
  \(                                    { tok $ \p s -> Posnd p BeginParen }
  \[                                    { tok $ \p s -> Posnd p BeginBracket }
  \{                                    { tok $ \p s -> Posnd p BeginBrace }
  \)                                    { tok $ \p s -> Posnd p EndParen }
  \]                                    { tok $ \p s -> Posnd p EndBracket }
  \}                                    { tok $ \p s -> Posnd p EndBrace }
  [\<\>\-\=\!\~\%\^\&\*\/\?\+\|]+       { tok $ \p s -> Posnd p $ Symbols s }

{

data Posnd a = Posnd AlexPosn a

-- The token type:
data Token = Bool
           | TrueLit
           | FalseLit
           | IntLit String
           | StrLit String
           | HexLit String
           | U8
           | U16
           | U32
           | Field
           | Indent
           | Dedent
           | Private
           | If
           | Then
           | Else
           | Fi
           | For
           | In
           | Do
           | EndFor
           | Assert
           | Def
           | Return
           | Import
           | From
           | As
           | Struct
           | Ident String
           | DotDotDot
           | DotDot
           | Dot
           | SemiColon
           | Colon
           | Symbols String
           | Comma
           | BeginParen
           | BeginBracket
           | BeginBrace
           | EndParen
           | EndBracket
           | Newline
           | EndBrace
           deriving (Show)

start :: Posnd Token -> AlexPosn
start (Posnd p _) = p

str :: Posnd Token -> String
str (Posnd _ t) = case t of
           Bool -> "bool"
           TrueLit -> "true"
           FalseLit -> "false"
           IntLit s -> s
           StrLit s -> "\"" ++ s ++ "\""
           HexLit s -> s
           U8 -> "u8"
           U16 -> "u16"
           U32 -> "u32"
           Field -> "field"
           Indent -> "INDENT"
           Dedent -> "DEDENT"
           Private -> "private"
           If -> "if"
           Then -> "then"
           Else -> "else"
           Fi -> "fi"
           For -> "for"
           In -> "in"
           Do -> "do"
           EndFor ->"endfor"
           Assert -> "assert"
           Def -> "def"
           Return -> "return"
           Struct -> "struct"
           Import -> "import"
           From -> "from"
           As -> "as"
           Ident s -> s
           DotDotDot -> "..."
           DotDot -> ".."
           Dot -> "."
           SemiColon -> ";"
           Colon -> ":"
           Symbols s -> s
           Comma -> ","
           BeginParen -> "("
           BeginBracket -> "["
           BeginBrace -> "{"
           EndParen -> ")"
           EndBracket -> "]"
           EndBrace -> "}"
           Newline -> "\n"

end :: Posnd Token -> AlexPosn
end t =
  let len = length $ str t
      AlexPn charN line col = start t
  in  AlexPn (charN + len) line (col + len)


--tokenize :: String -> [Posnd Token]
--tokenize = alexScanTokens

data AlexUserState = AlexUserState
                   { prevIndent :: Int
                   , curIndent :: Int
                   , indentEnded :: Bool
                   , tokens :: [Posnd Token]
                   }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
                   { prevIndent  = 0
                   , curIndent  = 0
                   , indentEnded = False
                   , tokens = []
                   }

-- s :: AlexUserState -> AlexUserState
-- alex_ust :: AlexState -> AlexUserState
-- -> Returns the current state from AlexState.
modifyUserState :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUserState f = Alex $ \s -> let current = alex_ust s
                                     new     = f current
                                 in
                                   Right (s { alex_ust = new },())

-- Returns the current state.
-- I.e., a list of tokens.
getUserState ::  Alex AlexUserState
getUserState = Alex $ \s -> Right (s,alex_ust s)

-- Each action per token should be a value of result `AlexAction`.
-- type AlexAction a = AlexInput -> Int -> Alex a
-- type AlexInput = (AlexPosn, Char, [Byte], String)

tok :: (AlexPosn -> String -> Posnd Token) -> AlexAction ()
tok f (p, _, _, s) l = pushToken (f p $ take l s) >> alexMonadScan

pushToken :: Posnd Token -> Alex ()
pushToken t = modifyUserState $ \ust -> ust{tokens=t:tokens ust}

alexEOF :: Alex ()
alexEOF = return ()

-- | remove adjacent newlines
uniqNl :: [Posnd Token] -> [Posnd Token]
uniqNl [] = []
uniqNl (x@(Posnd _ Newline):xs) = x:uniqNl (dropWhile isNl xs)
 where
  isNl (Posnd _ Newline) = True
  isNl _ = False
uniqNl (x:xs) = x:uniqNl xs


tokenize :: String -> Either String [Posnd Token]
tokenize s = runAlex s $ alexMonadScan >> (uniqNl . reverse . tokens <$> getUserState)
}

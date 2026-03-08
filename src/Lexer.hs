-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Lexer.hs  -- Lexical analysis: source text -> token stream
-- =============================================================================
module Lexer
  ( tokenize
  , LexError(..)
  ) where

import Types
import Data.Char
import Data.List (isPrefixOf, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)

-- =============================================================================
-- LEXER STATE
-- =============================================================================

data LexState = LexState
  { lsSource  :: String     -- remaining source
  , lsLine    :: Int
  , lsCol     :: Int
  , lsFile    :: String
  , lsIndents :: [Int]      -- indent stack for INDENT/DEDENT
  , lsTokens  :: [TokenInfo]
  } deriving (Show)

data LexError = LexError SourcePos String
  deriving (Show)

initLexState :: String -> String -> LexState
initLexState src file = LexState
  { lsSource  = src
  , lsLine    = 1
  , lsCol     = 1
  , lsFile    = file
  , lsIndents = [0]
  , lsTokens  = []
  }

-- =============================================================================
-- MAIN TOKENIZE FUNCTION
-- =============================================================================

tokenize :: String -> String -> Either LexError [TokenInfo]
tokenize src file =
  let st0 = initLexState src file
  in case runLexer st0 of
    Left  e  -> Left e
    Right ts -> Right (ts ++ [TokenInfo TEOF (SourcePos file (countLines src) 0)])

countLines :: String -> Int
countLines = length . filter (== '\n')

runLexer :: LexState -> Either LexError [TokenInfo]
runLexer st = case lsSource st of
  []  -> closeDedents st
  _   -> do
    (ti, st') <- nextToken st
    rest      <- runLexer st'
    pure (ti : rest)

closeDedents :: LexState -> Either LexError [TokenInfo]
closeDedents st =
  let n = length (lsIndents st) - 1
      pos = curPos st
  in Right (replicate n (TokenInfo TDedent pos))

-- =============================================================================
-- TOKEN DISPATCH
-- =============================================================================

nextToken :: LexState -> Either LexError (TokenInfo, LexState)
nextToken st = case lsSource st of
  []     -> Right (TokenInfo TEOF (curPos st), st)
  -- Skip carriage return
  ('\r':cs) -> nextToken st{ lsSource = cs }
  -- Newline handling + indent tracking
  ('\n':cs) -> handleNewline (advance st '\n') cs
  -- Skip inline whitespace
  (c:_) | c == ' ' || c == '\t' -> nextToken (skipChar st)
  -- Comments
  ('#':cs)  -> nextToken (skipLineComment st{ lsSource = cs })
  ('{':'-':cs) -> case skipBlockComment cs 1 of
    Just (rest, lines', cols') ->
      nextToken st{ lsSource = rest
                  , lsLine   = lsLine st + lines'
                  , lsCol    = if lines' > 0 then cols' else lsCol st + cols'
                  }
    Nothing -> Left (LexError (curPos st) "Unterminated block comment")
  -- String literals
  ('"':_)   -> lexString st
  ('\'':_)  -> lexStringSingle st
  ('f':'"':_) -> lexFString st
  -- Numbers
  (c:_) | isDigit c  -> lexNumber st
  -- Identifiers and keywords
  (c:_) | isAlpha c || c == '_' -> lexIdent st
  -- Operators and punctuation
  _    -> lexSymbol st

-- =============================================================================
-- NEWLINE / INDENTATION
-- =============================================================================

handleNewline :: LexState -> String -> Either LexError (TokenInfo, LexState)
handleNewline st cs =
  let (spaces, rest) = span (\c -> c == ' ' || c == '\t') cs
      indent         = length spaces   -- naive: 1 space = 1 level
      top            = head (lsIndents st)
      pos            = curPos st
      st1            = st{ lsSource = rest
                         , lsLine   = lsLine st + 1
                         , lsCol    = indent + 1
                         }
  in if indent > top
       then do
         let st2 = st1{ lsIndents = indent : lsIndents st1 }
         Right (TokenInfo TIndent pos, st2)
       else if indent < top
         then do
           let (popped, remaining) = span (> indent) (lsIndents st)
               nDedents = length popped
               st2 = st1{ lsIndents = remaining }
           -- We emit the first DEDENT here; the rest come on next calls
           -- (simplified: emit all at once as list, but our interface is single-token)
           -- For simplicity we just emit TNewline here and handle dedent in Parser
           Right (TokenInfo TNewline pos, st1)
         else
           Right (TokenInfo TNewline pos, st1)

-- =============================================================================
-- SKIP HELPERS
-- =============================================================================

skipChar :: LexState -> LexState
skipChar st = case lsSource st of
  []     -> st
  (c:cs) -> st{ lsSource = cs, lsCol = lsCol st + 1 }
  where
    _ = lsSource st

advance :: LexState -> Char -> LexState
advance st '\n' = st{ lsLine = lsLine st + 1, lsCol = 1 }
advance st _    = st{ lsCol  = lsCol st + 1  }

skipLineComment :: LexState -> LexState
skipLineComment st = st{ lsSource = dropWhile (/= '\n') (lsSource st) }

skipBlockComment :: String -> Int -> Maybe (String, Int, Int)
skipBlockComment []         _ = Nothing
skipBlockComment ('-':'}':cs) 1 = Just (cs, 0, 2)
skipBlockComment ('{':'-':cs) n = skipBlockComment cs (n+1)
skipBlockComment ('-':'}':cs) n = skipBlockComment cs (n-1)
skipBlockComment ('\n':cs) n    = fmap (\(r,l,c) -> (r, l+1, c)) (skipBlockComment cs n)
skipBlockComment (_:cs)    n    = skipBlockComment cs n

curPos :: LexState -> SourcePos
curPos st = SourcePos (lsFile st) (lsLine st) (lsCol st)

-- =============================================================================
-- STRING LEXING
-- =============================================================================

lexString :: LexState -> Either LexError (TokenInfo, LexState)
lexString st = do
  let pos = curPos st
  let (_:cs) = lsSource st   -- consume opening "
  (s, rest, linesAdded) <- lexStringContent '"' cs pos
  let st' = st{ lsSource = rest
              , lsLine   = lsLine st + linesAdded
              , lsCol    = lsCol st + length s + 2
              }
  Right (TokenInfo (TStr s) pos, st')

lexStringSingle :: LexState -> Either LexError (TokenInfo, LexState)
lexStringSingle st = do
  let pos = curPos st
  let (_:cs) = lsSource st
  (s, rest, linesAdded) <- lexStringContent '\'' cs pos
  let st' = st{ lsSource = rest
              , lsLine   = lsLine st + linesAdded
              , lsCol    = lsCol st + length s + 2
              }
  Right (TokenInfo (TStr s) pos, st')

lexStringContent :: Char -> String -> SourcePos -> Either LexError (String, String, Int)
lexStringContent q = go 0
  where
    go n []           = Left (LexError noPos "Unterminated string literal")
    go n (c:cs)
      | c == q        = Right ("", cs, n)
      | c == '\\'     = case cs of
          []          -> Left (LexError noPos "Unexpected end after '\\'")
          (e:es)      -> do
            (rest, rs, nl) <- go n es
            let ch = escapeChar e
            Right (ch : rest, rs, nl)
      | c == '\n'     = do
          (rest, rs, nl) <- go (n+1) cs
          Right (c : rest, rs, nl + 1)
      | otherwise     = do
          (rest, rs, nl) <- go n cs
          Right (c : rest, rs, nl)

escapeChar :: Char -> Char
escapeChar 'n'  = '\n'
escapeChar 't'  = '\t'
escapeChar 'r'  = '\r'
escapeChar '\'' = '\''
escapeChar '"'  = '"'
escapeChar '\\' = '\\'
escapeChar '0'  = '\0'
escapeChar 'a'  = '\a'
escapeChar 'b'  = '\b'
escapeChar 'f'  = '\f'
escapeChar 'v'  = '\v'
escapeChar c    = c

-- f-string lexing
lexFString :: LexState -> Either LexError (TokenInfo, LexState)
lexFString st = do
  let pos = curPos st
  let (_:_:cs) = lsSource st   -- consume f and "
  (parts, rest) <- lexFStringParts cs pos
  let st' = st{ lsSource = rest }
  Right (TokenInfo (TFStr parts) pos, st')

lexFStringParts :: String -> SourcePos -> Either LexError ([FStrPart], String)
lexFStringParts [] _   = Left (LexError noPos "Unterminated f-string")
lexFStringParts ('"':cs) _ = Right ([], cs)
lexFStringParts ('{':'{':cs) pos = do
  (rest, rs) <- lexFStringParts cs pos
  let prepend (FStrLit s : ps) = FStrLit ('{' : s) : ps
      prepend ps               = FStrLit "{" : ps
  Right (prepend rest, rs)
lexFStringParts ('}':'}':cs) pos = do
  (rest, rs) <- lexFStringParts cs pos
  let prepend (FStrLit s : ps) = FStrLit ('}' : s) : ps
      prepend ps               = FStrLit "}" : ps
  Right (prepend rest, rs)
lexFStringParts ('{':cs) pos = do
  (expr, afterExpr) <- lexFStringExpr cs 0
  (rest, rs)        <- lexFStringParts afterExpr pos
  Right (FStrExpr expr : rest, rs)
lexFStringParts (c:cs) pos = do
  (rest, rs) <- lexFStringParts cs pos
  let prepend (FStrLit s : ps) = FStrLit (c : s) : ps
      prepend ps               = FStrLit [c] : ps
  Right (prepend rest, rs)

lexFStringExpr :: String -> Int -> Either LexError (String, String)
lexFStringExpr [] _         = Left (LexError noPos "Unterminated expression in f-string")
lexFStringExpr ('}':cs) 0   = Right ("", cs)
lexFStringExpr ('{':cs) n   = do
  (e, r) <- lexFStringExpr cs (n+1)
  Right ('{' : e, r)
lexFStringExpr ('}':cs) n   = do
  (e, r) <- lexFStringExpr cs (n-1)
  Right ('}' : e, r)
lexFStringExpr (c:cs) n     = do
  (e, r) <- lexFStringExpr cs n
  Right (c : e, r)

-- =============================================================================
-- NUMBER LEXING
-- =============================================================================

lexNumber :: LexState -> Either LexError (TokenInfo, LexState)
lexNumber st = do
  let pos = curPos st
  let src = lsSource st
  let (raw, rest) = spanNum src
  let cleaned = filter (/= '_') raw
  case reads cleaned of
    [(n, "")] -> Right (TokenInfo (TInt n) pos,
                        st{ lsSource = rest, lsCol = lsCol st + length raw })
    _         -> case reads cleaned of
      [(f, "")] -> Right (TokenInfo (TFloat f) pos,
                          st{ lsSource = rest, lsCol = lsCol st + length raw })
      _         -> Left (LexError pos ("Invalid number literal: " ++ raw))

spanNum :: String -> (String, String)
spanNum s =
  let (intPart, r1) = span (\c -> isDigit c || c == '_') s
  in case r1 of
    ('.':r2) | not (null r2) && (isDigit (head r2)) ->
      let (fracPart, r3) = span (\c -> isDigit c || c == '_') r2
          (expPart, r4) = case r3 of
            ('e':rs) -> let (ep, r5) = span isDigit rs in ("e"++ep, r5)
            ('E':rs) -> let (ep, r5) = span isDigit rs in ("E"++ep, r5)
            _        -> ("", r3)
      in (intPart ++ "." ++ fracPart ++ expPart, r4)
    ('e':r2) ->
      let (ep, r3) = span isDigit r2 in (intPart ++ "e" ++ ep, r3)
    ('E':r2) ->
      let (ep, r3) = span isDigit r2 in (intPart ++ "E" ++ ep, r3)
    _ -> (intPart, r1)

-- =============================================================================
-- IDENTIFIER AND KEYWORD LEXING
-- =============================================================================

keywords :: Map.Map String Token
keywords = Map.fromList
  [ ("def",      TDef)
  , ("let",      TLet)
  , ("in",       TIn)
  , ("if",       TIf)
  , ("elif",     TElif)
  , ("else",     TElse)
  , ("for",      TFor)
  , ("while",    TWhile)
  , ("return",   TReturn)
  , ("break",    TBreak)
  , ("continue", TContinue)
  , ("match",    TMatch)
  , ("case",     TCase)
  , ("record",   TRecord)
  , ("try",      TTry)
  , ("except",   TExcept)
  , ("finally",  TFinally)
  , ("import",   TImport)
  , ("from",     TFrom)
  , ("as",       TAs)
  , ("lambda",   TLambda)
  , ("and",      TAnd)
  , ("or",       TOr)
  , ("not",      TNot)
  , ("True",     TBool True)
  , ("False",    TBool False)
  , ("None",     TNone)
  , ("pass",     TIdent "pass")
  , ("global",   TIdent "global")
  , ("nonlocal", TIdent "nonlocal")
  , ("del",      TIdent "del")
  , ("raise",    TIdent "raise")
  , ("assert",   TIdent "assert")
  , ("class",    TIdent "class")
  ]

import qualified Data.Map.Strict as Map

lexIdent :: LexState -> Either LexError (TokenInfo, LexState)
lexIdent st = do
  let pos = curPos st
  let src = lsSource st
  let (word, rest) = span (\c -> isAlphaNum c || c == '_') src
  let tok = Map.findWithDefault (TIdent word) word keywords
  Right (TokenInfo tok pos,
         st{ lsSource = rest, lsCol = lsCol st + length word })

-- =============================================================================
-- SYMBOL / OPERATOR LEXING
-- =============================================================================

lexSymbol :: LexState -> Either LexError (TokenInfo, LexState)
lexSymbol st = do
  let pos = curPos st
  let src = lsSource st
  let (tok, n) = matchSymbol src
  case tok of
    Nothing -> Left (LexError pos ("Unexpected character: " ++ [head src]))
    Just t  -> Right (TokenInfo t pos,
                      st{ lsSource = drop n src, lsCol = lsCol st + n })

matchSymbol :: String -> (Maybe Token, Int)
matchSymbol ('|':'>':_) = (Just TPipeRight,  2)
matchSymbol ('>':'>':_) = (Just TBind,       2)
matchSymbol ('-':'>':_) = (Just TArrow,      2)
matchSymbol ('=':'>':_) = (Just TFatArrow,   2)
matchSymbol ('<':'<':_) = (Just TShiftL,     2)
matchSymbol ('>':'>':_) = (Just TShiftR,     2)  -- shadowed by TBind above, order matters
matchSymbol ('*':'*':_) = (Just TDStar,      2)
matchSymbol ('/':'/':_) = (Just TDSlash,     2)
matchSymbol ('<':'=':_) = (Just TLtEq,       2)
matchSymbol ('>':'=':_) = (Just TGtEq,       2)
matchSymbol ('=':'=':_) = (Just TEq,         2)
matchSymbol ('!':'=':_) = (Just TNeq,        2)
matchSymbol ('+':'=':_) = (Just TPlusEq,     2)
matchSymbol ('-':'=':_) = (Just TMinusEq,    2)
matchSymbol ('*':'=':_) = (Just TStarEq,     2)
matchSymbol ('/':'=':_) = (Just TSlashEq,    2)
matchSymbol ('.':'.':'.':_) = (Just TDotDotDot, 3)
matchSymbol ('.':'.':_) = (Just TDotDot,     2)
matchSymbol ('(':_)     = (Just TLParen,     1)
matchSymbol (')':_)     = (Just TRParen,     1)
matchSymbol ('{':_)     = (Just TLBrace,     1)
matchSymbol ('}':_)     = (Just TRBrace,     1)
matchSymbol ('[':_)     = (Just TLBracket,   1)
matchSymbol (']':_)     = (Just TRBracket,   1)
matchSymbol (',':_)     = (Just TComma,      1)
matchSymbol (':':_)     = (Just TColon,      1)
matchSymbol (';':_)     = (Just TSemicolon,  1)
matchSymbol ('@':_)     = (Just TAt,         1)
matchSymbol ('+':_)     = (Just TPlus,       1)
matchSymbol ('-':_)     = (Just TMinus,      1)
matchSymbol ('*':_)     = (Just TStar,       1)
matchSymbol ('/':_)     = (Just TSlash,      1)
matchSymbol ('%':_)     = (Just TPercent,    1)
matchSymbol ('=':_)     = (Just TAssign,     1)
matchSymbol ('<':_)     = (Just TLt,         1)
matchSymbol ('>':_)     = (Just TGt,         1)
matchSymbol ('&':_)     = (Just TAmp,        1)
matchSymbol ('|':_)     = (Just TPipeSym,    1)
matchSymbol ('^':_)     = (Just TCaret,      1)
matchSymbol ('~':_)     = (Just TTilde,      1)
matchSymbol ('.':_)     = (Just TDot,        1)
matchSymbol ('\\':_)    = (Just TBackslash,  1)
matchSymbol ('_':_)     = (Just TUnderscore, 1)
matchSymbol _           = (Nothing,          0)

-- =============================================================================
-- POST-PROCESSING: Insert INDENT/DEDENT tokens based on indentation
-- =============================================================================

-- After initial tokenization, we do a second pass to properly handle
-- indentation-based blocks. The lexer above emits TNewline tokens;
-- this function converts them into proper INDENT/DEDENT sequences.

insertIndentDedent :: [TokenInfo] -> [TokenInfo]
insertIndentDedent tis = go [0] tis
  where
    go _ [] = []
    go stack (ti:tis) = case tiToken ti of
      TNewline ->
        let pos     = tiPos ti
            nextCol = case tis of
              (t:_) -> spCol (tiPos t) - 1
              []    -> 0
            top     = head stack
        in if nextCol > top
             then TokenInfo TIndent pos : TokenInfo TNewline pos
                  : go (nextCol : stack) tis
           else if nextCol < top
             then let (popped, newStack) = span (> nextCol) stack
                      nDedents = length popped
                  in replicate nDedents (TokenInfo TDedent pos)
                     ++ [TokenInfo TNewline pos]
                     ++ go newStack tis
           else ti : go stack tis
      _ -> ti : go stack tis

-- =============================================================================
-- PRETTY-PRINT TOKENS (for debugging)
-- =============================================================================

showToken :: Token -> String
showToken (TInt n)    = "INT(" ++ show n ++ ")"
showToken (TFloat f)  = "FLOAT(" ++ show f ++ ")"
showToken (TStr s)    = "STR(" ++ show s ++ ")"
showToken (TBool b)   = "BOOL(" ++ show b ++ ")"
showToken TNone       = "NONE"
showToken (TIdent i)  = "IDENT(" ++ i ++ ")"
showToken TDef        = "DEF"
showToken TLet        = "LET"
showToken TIf         = "IF"
showToken TElif       = "ELIF"
showToken TElse       = "ELSE"
showToken TFor        = "FOR"
showToken TWhile      = "WHILE"
showToken TReturn     = "RETURN"
showToken TMatch      = "MATCH"
showToken TRecord     = "RECORD"
showToken TPipeRight  = "PIPE_RIGHT(|>)"
showToken TCompose    = "COMPOSE(.)"
showToken TBind       = "BIND(>>)"
showToken TArrow      = "ARROW(->)"
showToken TNewline    = "NEWLINE"
showToken TIndent     = "INDENT"
showToken TDedent     = "DEDENT"
showToken TEOF        = "EOF"
showToken t           = show t

showTokenStream :: [TokenInfo] -> String
showTokenStream = unlines . map (\ti ->
  show (tiPos ti) ++ "  " ++ showToken (tiToken ti))

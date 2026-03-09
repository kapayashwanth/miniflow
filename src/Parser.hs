-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Parser.hs  -- Recursive-descent parser: token stream -> AST
-- =============================================================================
module Parser
  ( parseProgram
  , parseExpr
  , parseExprTokens
  , ParseError(..)
  ) where

import Types
import Data.IORef
import Control.Monad (when, unless, void)
import Data.Char (isUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, catMaybes)

-- =============================================================================
-- PARSER MONAD
-- =============================================================================

data ParseError = ParseError SourcePos String
  deriving (Show)

data ParserState = ParserState
  { psTokens :: [TokenInfo]
  , psPos    :: SourcePos
  } deriving (Show)

newtype Parser a = Parser
  { runParser :: ParserState -> Either ParseError (a, ParserState) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \st ->
    case p st of
      Left  e       -> Left e
      Right (a, st') -> Right (f a, st')

instance Applicative Parser where
  pure a = Parser $ \st -> Right (a, st)
  Parser pf <*> Parser px = Parser $ \st ->
    case pf st of
      Left e         -> Left e
      Right (f, st') -> case px st' of
        Left e          -> Left e
        Right (x, st'') -> Right (f x, st'')

instance Monad Parser where
  return = pure
  Parser p >>= f = Parser $ \st ->
    case p st of
      Left e         -> Left e
      Right (a, st') -> runParser (f a) st'

-- =============================================================================
-- PARSER COMBINATORS
-- =============================================================================

peek :: Parser Token
peek = Parser $ \st ->
  case psTokens st of
    []     -> Right (TEOF, st)
    (t:_)  -> Right (tiToken t, st)

peekInfo :: Parser TokenInfo
peekInfo = Parser $ \st ->
  case psTokens st of
    []    -> Right (TokenInfo TEOF noPos, st)
    (t:_) -> Right (t, st)

consume :: Parser TokenInfo
consume = Parser $ \st ->
  case psTokens st of
    []     -> Left (ParseError (psPos st) "Unexpected end of input")
    (t:ts) -> Right (t, st{ psTokens = ts, psPos = tiPos t })

expect :: Token -> Parser TokenInfo
expect tok = do
  ti <- peekInfo
  if tiToken ti == tok
    then consume
    else failP (tiPos ti) ("Expected " ++ show tok ++ ", got " ++ show (tiToken ti))

expectIdent :: Parser String
expectIdent = do
  ti <- peekInfo
  case tiToken ti of
    TIdent name -> consume >> return name
    _           -> failP (tiPos ti) ("Expected identifier, got " ++ show (tiToken ti))

failP :: SourcePos -> String -> Parser a
failP pos msg = Parser $ \_ -> Left (ParseError pos msg)

try :: Parser a -> Parser (Maybe a)
try p = Parser $ \st ->
  case runParser p st of
    Left  _        -> Right (Nothing, st)
    Right (a, st') -> Right (Just a, st')

optional :: Parser a -> Parser (Maybe a)
optional = try

skipNewlines :: Parser ()
skipNewlines = do
  t <- peek
  case t of
    TNewline -> consume >> skipNewlines
    _        -> return ()

-- Skip newlines, indents, and dedents (safe to use inside paren-delimited contexts)
skipWhitespace :: Parser ()
skipWhitespace = do
  t <- peek
  case t of
    TNewline -> consume >> skipWhitespace
    TIndent  -> consume >> skipWhitespace
    TDedent  -> consume >> skipWhitespace
    _        -> return ()

-- Returns True if a TIndent was consumed (caller must consume matching TDedent)
skipNewlineIndent :: Parser Bool
skipNewlineIndent = Parser $ \st ->
  case skipTowardPipe False (psTokens st) of
    Just (ts', hadIndent) -> Right (hadIndent, st { psTokens = ts' })
    Nothing               -> Right (False, st)

skipTowardPipe :: Bool -> [TokenInfo] -> Maybe ([TokenInfo], Bool)
skipTowardPipe hadIndent [] = Nothing
skipTowardPipe hadIndent (ti:rest) = case tiToken ti of
  TNewline   -> skipTowardPipe hadIndent rest
  TIndent    -> skipTowardPipe True rest
  TDedent    -> Nothing  -- end of block, stop
  TPipeRight -> Just (ti:rest, hadIndent)
  _          -> Nothing

matchToken :: Token -> Parser Bool
matchToken tok = do
  t <- peek
  if t == tok
    then consume >> return True
    else return False

-- =============================================================================
-- MAIN PARSE ENTRY POINT
-- =============================================================================

parseProgram :: [TokenInfo] -> Either ParseError Program
parseProgram toks =
  let st = ParserState{ psTokens = toks, psPos = noPos }
  in case runParser parseStmts st of
    Left  e        -> Left e
    Right (ss, _)  -> Right (Program ss)

parseExprTokens :: [TokenInfo] -> Either ParseError Expr
parseExprTokens toks =
  let st = ParserState{ psTokens = toks, psPos = noPos }
  in case runParser parsePipeExpr st of
    Left  e       -> Left e
    Right (e, _)  -> Right e

parseStmts :: Parser [Stmt]
parseStmts = do
  skipNewlines
  t <- peek
  case t of
    TEOF   -> return []
    TDedent -> return []
    _       -> do
      s  <- parseStmt
      skipNewlines
      ss <- parseStmts
      return (s : ss)

parseBlock :: Parser [Stmt]
parseBlock = do
  t <- peek
  case t of
    TColon -> do
      consume
      skipNewlines
      t2 <- peek
      case t2 of
        TIndent -> do
          consume
          stmts <- parseStmts
          expect TDedent
          return stmts
        -- Single-line block
        _ -> do
          s <- parseStmt
          return [s]
    _ -> failP noPos "Expected ':' to start block"

-- =============================================================================
-- STATEMENT PARSING
-- =============================================================================

parseStmt :: Parser Stmt
parseStmt = do
  ti <- peekInfo
  let pos = tiPos ti
  case tiToken ti of
    TDef      -> parseFunDef
    TLet      -> parseLetDef
    TIf       -> parseIfStmt
    TWhile    -> parseWhileStmt
    TFor      -> parseForStmt
    TReturn   -> parseReturnStmt
    TBreak    -> consume >> return SBreak
    TContinue -> consume >> return SContinue
    TMatch    -> parseMatchStmt
    TTry      -> parseTryCatch
    TRecord   -> parseRecordDef
    TImport   -> parseImport
    TFrom     -> parseFromImport
    TIdent "pass" -> consume >> return SPass
    TIdent "global"   -> parseGlobal
    TIdent "nonlocal" -> parseNonlocal
    TIdent "del"      -> parseDelete
    TIdent "raise"    -> parseRaise
    TIdent "assert"   -> parseAssert
    _         -> parseExprOrAssign

parseFunDef :: Parser Stmt
parseFunDef = do
  expect TDef
  name   <- expectIdent
  params <- parseParamList
  retTy  <- optional parseReturnType
  t      <- peek
  body   <- case t of
    TAssign -> consume >> parseExpr >>= \e -> return [SReturn (Just e)]
    _       -> parseBlock
  return (SFunDef name params retTy body)

parseReturnType :: Parser TypeExpr
parseReturnType = do
  expect TArrow
  parseTypeExpr

parseLetDef :: Parser Stmt
parseLetDef = do
  expect TLet
  t <- peek
  case t of
    TLParen -> do
      -- tuple destructuring: let (a, b) = expr
      pat  <- parsePattern
      expect TAssign
      expr <- parseExpr
      return (SAssign pat expr)
    _ -> do
      name  <- expectIdent
      annot <- optional (expect TColon >> parseTypeExpr)
      expect TAssign
      expr  <- parseExpr
      return (SLetDef name annot expr)

parseIfStmt :: Parser Stmt
parseIfStmt = do
  expect TIf
  cond  <- parseExpr
  body  <- parseBlock
  elifs <- parseElifs
  else_ <- parseElse
  return (SIf ((cond, body) : elifs) else_)

parseElifs :: Parser [(Expr, [Stmt])]
parseElifs = do
  skipNewlines
  t <- peek
  case t of
    TElif -> do
      consume
      cond  <- parseExpr
      body  <- parseBlock
      rest  <- parseElifs
      return ((cond, body) : rest)
    _ -> return []

parseElse :: Parser (Maybe [Stmt])
parseElse = do
  skipNewlines
  t <- peek
  case t of
    TElse -> do
      consume
      body <- parseBlock
      return (Just body)
    _ -> return Nothing

parseWhileStmt :: Parser Stmt
parseWhileStmt = do
  expect TWhile
  cond    <- parseExpr
  body    <- parseBlock
  elsePart <- parseElse
  return (SWhile cond body elsePart)

parseForStmt :: Parser Stmt
parseForStmt = do
  expect TFor
  pat     <- parsePattern
  expect TIn
  iter    <- parseExpr
  body    <- parseBlock
  elsePart <- parseElse
  return (SFor pat iter body elsePart)

parseReturnStmt :: Parser Stmt
parseReturnStmt = do
  expect TReturn
  t <- peek
  case t of
    TNewline -> return (SReturn Nothing)
    TEOF     -> return (SReturn Nothing)
    _        -> do
      e <- parseExpr
      return (SReturn (Just e))

parseMatchStmt :: Parser Stmt
parseMatchStmt = do
  expect TMatch
  scrutinee <- parseExpr
  expect TColon
  skipNewlines
  expect TIndent
  arms <- parseMatchArms
  expect TDedent
  return (SMatch scrutinee arms)

parseMatchArms :: Parser [MatchArm]
parseMatchArms = do
  skipNewlines
  t <- peek
  case t of
    TDedent -> return []
    TCase -> do
      arm  <- parseMatchArm
      skipNewlines
      rest <- parseMatchArms
      return (arm : rest)
    _ -> return []

parseMatchArm :: Parser MatchArm
parseMatchArm = do
  expect TCase
  pat   <- parsePattern
  guard <- optional (expect TIf >> parseExpr)
  body  <- parseBlock
  return (MatchArm pat guard body)

parseTryCatch :: Parser Stmt
parseTryCatch = do
  expect TTry
  tryBody    <- parseBlock
  handlers   <- parseExceptClauses
  elsePart   <- parseElse
  finallyPart <- parseFinallyClause
  return (STryCatch tryBody handlers elsePart finallyPart)

parseExceptClauses :: Parser [(Maybe String, String, [Stmt])]
parseExceptClauses = do
  skipNewlines
  t <- peek
  case t of
    TExcept -> do
      consume
      exType <- optional expectIdent
      name   <- do
        t2 <- peek
        case t2 of
          TAs -> consume >> fmap Just expectIdent
          _   -> return Nothing
      body   <- parseBlock
      rest   <- parseExceptClauses
      return ((exType, fromMaybe "_" name, body) : rest)
    _ -> return []

parseFinallyClause :: Parser (Maybe [Stmt])
parseFinallyClause = do
  skipNewlines
  t <- peek
  case t of
    TIdent "finally" -> consume >> fmap Just parseBlock
    TFinally         -> consume >> fmap Just parseBlock
    _                -> return Nothing

parseRecordDef :: Parser Stmt
parseRecordDef = do
  expect TRecord
  name   <- expectIdent
  expect TLBrace
  fields <- parseFieldDefs
  expect TRBrace
  return (SRecordDef name fields)

parseFieldDefs :: Parser [FieldDef]
parseFieldDefs = do
  t <- peek
  case t of
    TRBrace -> return []
    TNewline -> skipNewlines >> parseFieldDefs
    _ -> do
      field <- parseFieldDef
      t2    <- peek
      case t2 of
        TComma -> consume >> parseFieldDefs >>= \rest -> return (field : rest)
        _      -> return [field]

parseFieldDef :: Parser FieldDef
parseFieldDef = do
  name <- expectIdent
  expect TColon
  ty   <- parseTypeExpr
  defVal <- optional (expect TAssign >> parseExpr)
  return (FieldDef name ty defVal)

parseImport :: Parser Stmt
parseImport = do
  expect TImport
  name <- expectIdent
  alias <- optional (expect TAs >> expectIdent)
  return (SImport (ImportModule name alias))

parseFromImport :: Parser Stmt
parseFromImport = do
  expect TFrom
  modName <- expectIdent
  expect TImport
  t <- peek
  case t of
    TStar -> consume >> return (SImport (ImportAll modName))
    _ -> do
      names <- parseImportNames
      alias <- optional (expect TAs >> expectIdent)
      return (SImport (ImportFrom modName names alias))

parseImportNames :: Parser [String]
parseImportNames = do
  name <- expectIdent
  t    <- peek
  case t of
    TComma -> consume >> fmap (name :) parseImportNames
    _      -> return [name]

parseGlobal :: Parser Stmt
parseGlobal = do
  consume  -- 'global' keyword
  names <- parseCommaSepIdents
  return (SGlobal names)

parseNonlocal :: Parser Stmt
parseNonlocal = do
  consume
  names <- parseCommaSepIdents
  return (SNonlocal names)

parseDelete :: Parser Stmt
parseDelete = do
  consume
  exprs <- parseCommaSepExprs
  return (SDelete exprs)

parseRaise :: Parser Stmt
parseRaise = do
  consume
  t <- peek
  case t of
    TNewline -> return (SRaise Nothing)
    TEOF     -> return (SRaise Nothing)
    _        -> fmap (SRaise . Just) parseExpr

parseAssert :: Parser Stmt
parseAssert = do
  consume
  cond <- parseExpr
  msg  <- optional (expect TComma >> parseExpr)
  return (SAssert cond msg)

parseCommaSepIdents :: Parser [String]
parseCommaSepIdents = do
  name <- expectIdent
  t    <- peek
  case t of
    TComma -> consume >> fmap (name :) parseCommaSepIdents
    _      -> return [name]

parseCommaSepExprs :: Parser [Expr]
parseCommaSepExprs = do
  e <- parseExpr
  t <- peek
  case t of
    TComma -> consume >> fmap (e :) parseCommaSepExprs
    _      -> return [e]

-- Expression or assignment statement
parseExprOrAssign :: Parser Stmt
parseExprOrAssign = do
  e  <- parseExpr
  t  <- peek
  case t of
    TAssign -> do
      consume
      rhs <- parseExpr
      case exprToPattern e of
        Just pat -> return (SAssign pat rhs)
        Nothing  -> case e of
          EVar name -> return (SAssign (PVar name) rhs)
          _         -> return (SExpr (EBinOp OpEq e rhs))  -- fallback
    TPlusEq -> consume >> fmap (SAugAssign (exprName e) OpAdd) parseExpr
    TMinusEq -> consume >> fmap (SAugAssign (exprName e) OpSub) parseExpr
    TStarEq  -> consume >> fmap (SAugAssign (exprName e) OpMul) parseExpr
    TSlashEq -> consume >> fmap (SAugAssign (exprName e) OpDiv) parseExpr
    _        -> return (SExpr e)

exprName :: Expr -> String
exprName (EVar n) = n
exprName _        = "__unknown__"

exprToPattern :: Expr -> Maybe Pattern
exprToPattern (EVar n)      = Just (PVar n)
exprToPattern (ETuple es)   = fmap PTuple (mapM exprToPattern es)
exprToPattern (EList  es)   = fmap PList  (mapM exprToPattern es)
exprToPattern (EIndex c k)  = Just (PIndex c k)
exprToPattern (EField e f)  = Just (PField e f)
exprToPattern _             = Nothing

-- =============================================================================
-- EXPRESSION PARSING  (operator precedence via recursive descent)
-- =============================================================================

parseExpr :: Parser Expr
parseExpr = parsePipeExpr

-- Pipe: left-to-right chaining  a |> f |> g
parsePipeExpr :: Parser Expr
parsePipeExpr = do
  lhs <- parseLambdaExpr
  parsePipeRest lhs

parsePipeRest :: Expr -> Parser Expr
parsePipeRest lhs = do
  hadIndent <- skipNewlineIndent
  t <- peek
  case t of
    TPipeRight -> do
      consume
      rhs <- parseLambdaExpr
      result <- parsePipeRest (EPipe lhs rhs)
      -- If we entered an indent context, consume the closing dedent
      when hadIndent $ do
        skipNewlines
        t2 <- peek
        case t2 of
          TDedent -> consume >> return ()
          _       -> return ()
      return result
    TBind -> do
      consume
      rhs <- parseLambdaExpr
      result <- parsePipeRest (EBind lhs rhs)
      when hadIndent $ do
        skipNewlines
        t2 <- peek
        case t2 of
          TDedent -> consume >> return ()
          _       -> return ()
      return result
    _ -> return lhs

-- Lambda expression
parseLambdaExpr :: Parser Expr
parseLambdaExpr = do
  t <- peek
  case t of
    TLambda -> do
      consume
      params <- parseLambdaParams
      t2 <- peek
      case t2 of
        TArrow -> do { _ <- consume; return () }
        TColon -> do { _ <- consume; return () }
        _      -> return ()
      body <- parseExpr
      return (ELambda params body)
    TBackslash -> do
      consume
      params <- parseLambdaParams
      expect TArrow
      body <- parseExpr
      return (ELambda params body)
    _ -> parseOrExpr

parseLambdaParams :: Parser [Param]
parseLambdaParams = do
  t <- peek
  case t of
    TArrow -> return []
    TColon -> return []
    _ -> do
      name <- expectIdent
      let p = ParamSimple name
      t2   <- peek
      case t2 of
        TComma -> consume >> fmap (p :) parseLambdaParams
        _      -> return [p]

-- Logical OR
parseOrExpr :: Parser Expr
parseOrExpr = do
  lhs <- parseAndExpr
  parseOrRest lhs

parseOrRest :: Expr -> Parser Expr
parseOrRest lhs = do
  t <- peek
  case t of
    TOr -> consume >> parseAndExpr >>= \rhs -> parseOrRest (EBinOp OpOr lhs rhs)
    _   -> return lhs

-- Logical AND
parseAndExpr :: Parser Expr
parseAndExpr = do
  lhs <- parseNotExpr
  parseAndRest lhs

parseAndRest :: Expr -> Parser Expr
parseAndRest lhs = do
  t <- peek
  case t of
    TAnd -> consume >> parseNotExpr >>= \rhs -> parseAndRest (EBinOp OpAnd lhs rhs)
    _    -> return lhs

-- Logical NOT
parseNotExpr :: Parser Expr
parseNotExpr = do
  t <- peek
  case t of
    TNot -> do
      consume
      t2 <- peek
      case t2 of
        TDot -> do
          -- 'not' used as first-class function before compose (e.g. not . even)
          e <- parsePostfixRest (EVar "not")
          parseCmpRest e
        _ -> do
          e <- parseNotExpr
          return (EUnOp OpNot e)
    _ -> parseCmpExpr

-- Comparison
parseCmpExpr :: Parser Expr
parseCmpExpr = do
  lhs <- parseBitorExpr
  parseCmpRest lhs

parseCmpRest :: Expr -> Parser Expr
parseCmpRest lhs = do
  t <- peek
  case t of
    TEq   -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpEq  lhs r)
    TNeq  -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpNeq lhs r)
    TLt   -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpLt  lhs r)
    TGt   -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpGt  lhs r)
    TLtEq -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpLtEq lhs r)
    TGtEq -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpGtEq lhs r)
    TIn   -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpIn lhs r)
    TNot  -> do        -- not in
      consume
      t2 <- peek
      case t2 of
        TIn -> consume >> parseBitorExpr >>= \r -> parseCmpRest (EBinOp OpNotIn lhs r)
        _   -> failP noPos "Expected 'in' after 'not'"
    _     -> return lhs

-- Bitwise OR
parseBitorExpr :: Parser Expr
parseBitorExpr = do
  lhs <- parseBitxorExpr
  parseBitorRest lhs

parseBitorRest :: Expr -> Parser Expr
parseBitorRest lhs = do
  t <- peek
  case t of
    TPipeSym -> consume >> parseBitxorExpr >>= \r -> parseBitorRest (EBinOp OpBitOr lhs r)
    _        -> return lhs

-- Bitwise XOR
parseBitxorExpr :: Parser Expr
parseBitxorExpr = do
  lhs <- parseBitandExpr
  parseBitxorRest lhs

parseBitxorRest :: Expr -> Parser Expr
parseBitxorRest lhs = do
  t <- peek
  case t of
    TCaret -> consume >> parseBitandExpr >>= \r -> parseBitxorRest (EBinOp OpBitXor lhs r)
    _      -> return lhs

-- Bitwise AND
parseBitandExpr :: Parser Expr
parseBitandExpr = do
  lhs <- parseShiftExpr
  parseBitandRest lhs

parseBitandRest :: Expr -> Parser Expr
parseBitandRest lhs = do
  t <- peek
  case t of
    TAmp -> consume >> parseShiftExpr >>= \r -> parseBitandRest (EBinOp OpBitAnd lhs r)
    _    -> return lhs

-- Bit shifts
parseShiftExpr :: Parser Expr
parseShiftExpr = do
  lhs <- parseAddExpr
  parseShiftRest lhs

parseShiftRest :: Expr -> Parser Expr
parseShiftRest lhs = do
  t <- peek
  case t of
    TShiftL -> consume >> parseAddExpr >>= \r -> parseShiftRest (EBinOp OpShiftL lhs r)
    TShiftR -> consume >> parseAddExpr >>= \r -> parseShiftRest (EBinOp OpShiftR lhs r)
    _       -> return lhs

-- Addition/subtraction
parseAddExpr :: Parser Expr
parseAddExpr = do
  lhs <- parseMulExpr
  parseAddRest lhs

parseAddRest :: Expr -> Parser Expr
parseAddRest lhs = do
  t <- peek
  case t of
    TPlus  -> consume >> parseMulExpr >>= \r -> parseAddRest (EBinOp OpAdd lhs r)
    TMinus -> consume >> parseMulExpr >>= \r -> parseAddRest (EBinOp OpSub lhs r)
    _      -> return lhs

-- Multiplication / division / modulo
parseMulExpr :: Parser Expr
parseMulExpr = do
  lhs <- parseUnaryExpr
  parseMulRest lhs

parseMulRest :: Expr -> Parser Expr
parseMulRest lhs = do
  t <- peek
  case t of
    TStar    -> consume >> parseUnaryExpr >>= \r -> parseMulRest (EBinOp OpMul     lhs r)
    TSlash   -> consume >> parseUnaryExpr >>= \r -> parseMulRest (EBinOp OpDiv     lhs r)
    TDSlash  -> consume >> parseUnaryExpr >>= \r -> parseMulRest (EBinOp OpFloorDiv lhs r)
    TPercent -> consume >> parseUnaryExpr >>= \r -> parseMulRest (EBinOp OpMod     lhs r)
    TDStar   -> consume >> parseUnaryExpr >>= \r -> parseMulRest (EBinOp OpPow     lhs r)
    _        -> return lhs

-- Unary operators
parseUnaryExpr :: Parser Expr
parseUnaryExpr = do
  t <- peek
  case t of
    TMinus -> consume >> fmap (EUnOp OpNeg) parseUnaryExpr
    TTilde -> consume >> fmap (EUnOp OpBitNot) parseUnaryExpr
    TPlus  -> consume >> parseUnaryExpr
    _      -> parsePostfixExpr

-- Postfix: function calls, indexing, field access, composition
parsePostfixExpr :: Parser Expr
parsePostfixExpr = do
  e <- parsePrimaryExpr
  parsePostfixRest e

parsePostfixRest :: Expr -> Parser Expr
parsePostfixRest e = do
  t <- peek
  case t of
    TLParen -> do
      args <- parseArgList
      parsePostfixRest (EApp e args)
    TLBracket -> do
      consume
      idx <- parseSliceOrIndex
      expect TRBracket
      parsePostfixRest (applyIndex e idx)
    TDot -> do
      dotInfo <- peekInfo
      consume
      t2Info <- peekInfo
      let dotCol  = spCol (tiPos dotInfo)
          nextCol = spCol (tiPos t2Info)
          adjacent = nextCol == dotCol + 1
      case tiToken t2Info of
        TIdent field
          | adjacent  -> consume >> parsePostfixRest (EField e field)
          | otherwise -> do
              -- composition operator f . g (space-separated)
              rhs <- parsePostfixExpr
              parsePostfixRest (ECompose e rhs)
        _            -> do
          -- composition operator f . g
          rhs <- parsePostfixExpr
          parsePostfixRest (ECompose e rhs)
    TLBrace
      | isRecordName e -> do
          consume
          fields <- parseRecordFields
          expect TRBrace
          case e of
            EVar name -> parsePostfixRest (ERecordCreate name fields)
            _         -> parsePostfixRest e
    _ -> return e

isRecordName :: Expr -> Bool
isRecordName (EVar (c:_)) = isUpper c
isRecordName _             = False

parseRecordFields :: Parser [(String, Expr)]
parseRecordFields = do
  t <- peek
  case t of
    TRBrace -> return []
    _ -> do
      ti <- peekInfo
      case tiToken ti of
        TIdent fname -> do
          consume
          expect TColon
          val <- parseExpr
          t2 <- peek
          case t2 of
            TComma  -> consume >> fmap ((fname, val) :) parseRecordFields
            _       -> return [(fname, val)]
        _ -> return []

applyIndex :: Expr -> Either Expr (Maybe Expr, Maybe Expr, Maybe Expr) -> Expr
applyIndex e (Left  idx)      = EIndex e idx
applyIndex e (Right (a,b,c))  = ESlice e a b c

parseSliceOrIndex :: Parser (Either Expr (Maybe Expr, Maybe Expr, Maybe Expr))
parseSliceOrIndex = do
  t <- peek
  case t of
    TColon -> do
      consume
      end  <- optional (peekNot TColon >> peekNot TRBracket >> parseExpr)
      step <- optional (expect TColon >> parseExpr)
      return (Right (Nothing, end, step))
    _ -> do
      start <- parseExpr
      t2    <- peek
      case t2 of
        TColon -> do
          consume
          end  <- optional (peekNot TColon >> peekNot TRBracket >> parseExpr)
          step <- optional (expect TColon >> parseExpr)
          return (Right (Just start, end, step))
        _ -> return (Left start)

peekNot :: Token -> Parser ()
peekNot tok = do
  t <- peek
  when (t == tok) (failP noPos "unexpected token")

-- =============================================================================
-- PRIMARY EXPRESSION PARSING
-- =============================================================================

parsePrimaryExpr :: Parser Expr
parsePrimaryExpr = do
  ti <- peekInfo
  let pos = tiPos ti
  case tiToken ti of
    TInt n      -> consume >> return (EInt n)
    TFloat f    -> consume >> return (EFloat f)
    TStr s      -> consume >> return (EStr s)
    TBool b     -> consume >> return (EBool b)
    TNone       -> consume >> return ENone
    TFStr parts -> consume >> return (EFStr parts)
    TDotDotDot  -> consume >> return EEllipsis
    TIdent name -> consume >> return (EVar name)
    TLParen     -> parseTupleOrParenExpr
    TLBracket   -> parseListOrListComp
    TLBrace     -> parseDictOrSetExpr
    TIf         -> parseIfThenElse
    TMatch      -> parseMatchExpr
    TBackslash  -> parseLambdaExprPrimary
    TLambda     -> parseLambdaExprPrimary
    _           -> failP pos ("Unexpected token in expression: " ++ show (tiToken ti))

parseIfThenElse :: Parser Expr
parseIfThenElse = do
  expect TIf
  cond  <- parseExpr
  expect TElse  -- simplified: if cond else false_val  (Python ternary)
  alt   <- parseExpr
  return (EIf cond ENone alt)  -- simplified; real: if c then t else f

parseTupleOrParenExpr :: Parser Expr
parseTupleOrParenExpr = do
  expect TLParen
  t <- peek
  case t of
    TRParen -> consume >> return (ETuple [])
    _ -> do
      e  <- parseExpr
      t2 <- peek
      case t2 of
        TRParen -> consume >> return e
        TComma  -> do
          consume
          rest <- parseCommaSepExprs
          expect TRParen
          return (ETuple (e : rest))
        _ -> failP noPos "Expected ')' or ',' in parenthesized expression"

parseListOrListComp :: Parser Expr
parseListOrListComp = do
  expect TLBracket
  t <- peek
  case t of
    TRBracket -> consume >> return (EList [])
    _ -> do
      first <- parseExpr
      t2    <- peek
      case t2 of
        TFor -> do
          clauses <- parseCompClauses
          expect TRBracket
          return (EListComp first clauses)
        TComma -> do
          consume
          rest <- parseListElems
          expect TRBracket
          return (EList (first : rest))
        TRBracket -> consume >> return (EList [first])
        TDotDot   -> do
          consume
          end  <- parseExpr
          step <- optional (expect TColon >> parseExpr)
          expect TRBracket
          return (ERange first end step)
        _ -> failP noPos "Expected ']', ',', 'for', or '..' in list literal"

parseListElems :: Parser [Expr]
parseListElems = do
  t <- peek
  case t of
    TRBracket -> return []
    _ -> do
      e  <- parseExpr
      t2 <- peek
      case t2 of
        TComma    -> consume >> fmap (e :) parseListElems
        TRBracket -> return [e]
        _         -> return [e]

parseDictOrSetExpr :: Parser Expr
parseDictOrSetExpr = do
  expect TLBrace
  t <- peek
  case t of
    TRBrace -> consume >> return (EDict [])
    _ -> do
      first <- parseExpr
      t2    <- peek
      case t2 of
        TColon -> do
          consume
          val   <- parseExpr
          t3    <- peek
          case t3 of
            TFor -> do
              clauses <- parseCompClauses
              expect TRBrace
              return (EDictComp first val clauses)
            _ -> do
              rest <- parseDictPairs
              expect TRBrace
              return (EDict ((first, val) : rest))
        TComma -> do
          consume
          rest <- parseSetElems
          expect TRBrace
          return (ESet (first : rest))
        TFor -> do
          clauses <- parseCompClauses
          expect TRBrace
          return (ESetComp first clauses)
        TRBrace -> consume >> return (ESet [first])
        _ -> failP noPos "Expected ':' or ',' in dict/set literal"

parseDictPairs :: Parser [(Expr, Expr)]
parseDictPairs = do
  t <- peek
  case t of
    TRBrace -> return []
    TComma  -> do
      consume
      t2 <- peek
      case t2 of
        TRBrace -> return []
        _ -> do
          k <- parseExpr
          expect TColon
          v <- parseExpr
          rest <- parseDictPairs
          return ((k, v) : rest)
    _ -> return []

parseSetElems :: Parser [Expr]
parseSetElems = do
  t <- peek
  case t of
    TRBrace -> return []
    _ -> do
      e  <- parseExpr
      t2 <- peek
      case t2 of
        TComma  -> consume >> fmap (e :) parseSetElems
        TRBrace -> return [e]
        _       -> return [e]

parseCompClauses :: Parser [CompClause]
parseCompClauses = do
  t <- peek
  case t of
    TFor -> do
      consume
      pat  <- parsePattern
      expect TIn
      iter <- parseExpr
      rest <- parseCompClauses
      return (CCFor pat iter : rest)
    TIf -> do
      consume
      cond <- parseExpr
      rest <- parseCompClauses
      return (CCIf cond : rest)
    _ -> return []

parseMatchExpr :: Parser Expr
parseMatchExpr = do
  expect TMatch
  scrut <- parseExpr
  expect TColon
  skipNewlines
  expect TIndent
  arms <- parseMatchArms
  expect TDedent
  return (EMatch scrut arms)

parseLambdaExprPrimary :: Parser Expr
parseLambdaExprPrimary = do
  t <- peek
  case t of
    TLambda    -> do { _ <- consume; return () }
    TBackslash -> do { _ <- consume; return () }
    _          -> return ()
  params <- parseLambdaParams
  t2 <- peek
  case t2 of
    TArrow -> do { _ <- consume; return () }
    TColon -> do { _ <- consume; return () }
    _      -> return ()
  body <- parseExpr
  return (ELambda params body)

-- =============================================================================
-- PARAMETER AND ARGUMENT LISTS
-- =============================================================================

parseParamList :: Parser [Param]
parseParamList = do
  expect TLParen
  t <- peek
  case t of
    TRParen -> consume >> return []
    _ -> do
      params <- parseParams
      expect TRParen
      return params

parseParams :: Parser [Param]
parseParams = do
  p  <- parseSingleParam
  t  <- peek
  case t of
    TComma -> consume >> fmap (p :) parseParams
    _      -> return [p]

parseSingleParam :: Parser Param
parseSingleParam = do
  t <- peek
  case t of
    TStar -> do
      consume
      name <- expectIdent
      return (ParamStar name)
    TDStar -> do
      consume
      name <- expectIdent
      return (ParamDStar name)
    _ -> do
      name <- expectIdent
      t2   <- peek
      case t2 of
        TColon -> do
          consume
          ty <- parseTypeExpr
          t3 <- peek
          case t3 of
            TAssign -> do
              consume
              defVal <- parseExpr
              return (ParamDefault name defVal)
            _ -> return (ParamTyped name ty)
        TAssign -> do
          consume
          defVal <- parseExpr
          return (ParamDefault name defVal)
        _ -> return (ParamSimple name)

parseArgList :: Parser [Arg]
parseArgList = do
  expect TLParen
  t <- peek
  case t of
    TRParen -> consume >> return []
    _ -> do
      args <- parseArgs
      expect TRParen
      return args

parseArgs :: Parser [Arg]
parseArgs = do
  arg <- parseSingleArg
  t   <- peek
  case t of
    TComma -> do
      consume
      t2 <- peek
      case t2 of
        TRParen -> return [arg]  -- trailing comma
        _       -> fmap (arg :) parseArgs
    _      -> return [arg]

parseSingleArg :: Parser Arg
parseSingleArg = do
  t <- peek
  case t of
    TStar  -> consume >> fmap ArgStar parseExpr
    TDStar -> consume >> fmap ArgDStar parseExpr
    TIdent name -> do
      -- Peek ahead to see if this is a keyword argument
      consume
      t2 <- peek
      case t2 of
        TAssign -> consume >> fmap (ArgKw name) parseExpr
        _       -> do
          -- Put it back by treating as expression
          e <- parsePostfixRest (EVar name)
          e' <- parseMulRest e
          e'' <- parseAddRest e'
          e''' <- parseShiftRest e''
          e4 <- parseBitandRest e'''
          e5 <- parseBitxorRest e4
          e6 <- parseBitorRest e5
          e7 <- parseCmpRest e6
          return (ArgPos e7)
    _ -> fmap ArgPos parseExpr

-- =============================================================================
-- PATTERN PARSING
-- =============================================================================

parsePattern :: Parser Pattern
parsePattern = parseOrPattern

parseOrPattern :: Parser Pattern
parseOrPattern = do
  p   <- parseSimplePattern
  t   <- peek
  case t of
    TPipeSym -> consume >> fmap (POr p) parseOrPattern
    _        -> return p

parseSimplePattern :: Parser Pattern
parseSimplePattern = do
  ti <- peekInfo
  let pos = tiPos ti
  case tiToken ti of
    TUnderscore -> consume >> return PWild
    TIdent name -> do
      consume
      t <- peek
      case t of
        TLBrace -> parseRecordPattern name
        TAs     -> consume >> fmap (PAs (PVar name)) expectIdent
        _       -> return (PVar name)
    TInt n    -> consume >> return (PNum (fromIntegral n))
    TFloat f  -> consume >> return (PNum f)
    TStr s    -> consume >> return (PStr s)
    TBool b   -> consume >> return (PBool b)
    TNone     -> consume >> return PNone
    TLBracket -> parseListPattern
    TLParen   -> parseTuplePattern
    _         -> failP pos ("Unexpected token in pattern: " ++ show (tiToken ti))

parseListPattern :: Parser Pattern
parseListPattern = do
  expect TLBracket
  t <- peek
  case t of
    TRBracket -> consume >> return (PList [])
    _ -> do
      pats <- parsePatternList
      expect TRBracket
      return (PList pats)

parseTuplePattern :: Parser Pattern
parseTuplePattern = do
  expect TLParen
  t <- peek
  case t of
    TRParen -> consume >> return (PTuple [])
    _ -> do
      pats <- parsePatternList
      expect TRParen
      return (PTuple pats)

parsePatternList :: Parser [Pattern]
parsePatternList = do
  p  <- parseSimplePattern
  t  <- peek
  case t of
    TComma -> consume >> fmap (p :) parsePatternList
    _      -> return [p]

parseRecordPattern :: String -> Parser Pattern
parseRecordPattern name = do
  expect TLBrace
  fields <- parseRecordPatternFields
  expect TRBrace
  return (PRecord name fields)

parseRecordPatternFields :: Parser [(String, Pattern)]
parseRecordPatternFields = do
  t <- peek
  case t of
    TRBrace -> return []
    _ -> do
      fname <- expectIdent
      expect TColon
      pat   <- parsePattern
      t2    <- peek
      case t2 of
        TComma -> consume >> fmap ((fname, pat) :) parseRecordPatternFields
        _      -> return [(fname, pat)]

-- =============================================================================
-- TYPE EXPRESSION PARSING
-- =============================================================================

parseTypeExpr :: Parser TypeExpr
parseTypeExpr = do
  base <- parseBaseType
  t    <- peek
  case t of
    TArrow -> consume >> fmap (TyFun [base]) parseTypeExpr
    _      -> return base

parseBaseType :: Parser TypeExpr
parseBaseType = do
  ti <- peekInfo
  case tiToken ti of
    TIdent "Int"    -> consume >> return TyInt
    TIdent "Float"  -> consume >> return TyFloat
    TIdent "Str"    -> consume >> return TyStr
    TIdent "String" -> consume >> return TyStr
    TIdent "Bool"   -> consume >> return TyBool
    TIdent "None"   -> consume >> return TyNone
    TIdent "Any"    -> consume >> return TyAny
    TIdent "List"   -> do
      consume
      t <- peek
      case t of
        TLBracket -> do
          consume
          inner <- parseTypeExpr
          expect TRBracket
          return (TyList inner)
        _ -> return (TyList TyAny)
    TIdent "Dict"   -> do
      consume
      expect TLBracket
      k <- parseTypeExpr
      expect TComma
      v <- parseTypeExpr
      expect TRBracket
      return (TyDict k v)
    TIdent "Tuple"  -> do
      consume
      expect TLBracket
      ts <- parseTypeList
      expect TRBracket
      return (TyTuple ts)
    TIdent "Set"    -> do
      consume
      expect TLBracket
      t <- parseTypeExpr
      expect TRBracket
      return (TySet t)
    TIdent "Maybe"  -> do
      consume
      expect TLBracket
      t <- parseTypeExpr
      expect TRBracket
      return (TyMaybe t)
    TIdent name     -> consume >> return (TyNamed name [])
    TLParen         -> parseTupleType
    _ -> failP (tiPos ti) ("Expected type, got " ++ show (tiToken ti))

parseTypeList :: Parser [TypeExpr]
parseTypeList = do
  t  <- parseTypeExpr
  t2 <- peek
  case t2 of
    TComma -> consume >> fmap (t :) parseTypeList
    _      -> return [t]

parseTupleType :: Parser TypeExpr
parseTupleType = do
  expect TLParen
  ts <- parseTypeList
  expect TRParen
  return (TyTuple ts)

-- =============================================================================
-- MiniFlow Language Interpreter
-- src/REPL.hs  -- Read-Eval-Print Loop
-- =============================================================================
module REPL
  ( startRepl
  , runReplLine
  ) where

import Types
import Environment
import Lexer
import Parser
import Evaluator
import Pretty
import Data.IORef
import Data.List (isPrefixOf, intercalate)
import Control.Exception (try, catch, SomeException, evaluate)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hSetEcho)

-- =============================================================================
-- REPL ENTRY POINT
-- =============================================================================

data ReplState = ReplState
  { rsEnv       :: Env
  , rsHistory   :: [String]
  , rsMultiline :: [String]   -- accumulated lines for multi-line input
  }

startRepl :: IO ()
startRepl = do
  env <- initGlobalEnv
  printBanner
  let st = ReplState { rsEnv = env, rsHistory = [], rsMultiline = [] }
  replLoop st

printBanner :: IO ()
printBanner = do
  putStrLn "╔══════════════════════════════════════════════════════════════╗"
  putStrLn "║           MiniFlow Language Interpreter v1.0                ║"
  putStrLn "║   A unified Python + Haskell inspired language              ║"
  putStrLn "║   Type :help for help, :q to quit                           ║"
  putStrLn "╚══════════════════════════════════════════════════════════════╝"
  putStrLn ""

-- =============================================================================
-- MAIN REPL LOOP
-- =============================================================================

replLoop :: ReplState -> IO ()
replLoop st = do
  let prompt = if null (rsMultiline st)
                 then "miniflow> "
                 else "       | "
  putStr prompt
  hFlush stdout
  line <- getLine
  let line' = rstrip line

  -- Check for REPL commands
  case line' of
    ":q"    -> putStrLn "Goodbye!" >> return ()
    ":quit" -> putStrLn "Goodbye!" >> return ()
    ":exit" -> putStrLn "Goodbye!" >> return ()
    ":help" -> do
      printHelp
      replLoop st
    ":env"  -> do
      dump <- dumpEnv (rsEnv st)
      putStrLn dump
      replLoop st
    ":reset" -> do
      newEnv <- initGlobalEnv
      putStrLn "Environment reset."
      replLoop st{ rsEnv = newEnv, rsHistory = [] }
    _ | ":t " `isPrefixOf` line' -> do
        let expr = drop 3 line'
        handleTypeQuery (rsEnv st) expr
        replLoop st
    _ | ":load " `isPrefixOf` line' -> do
        let filename = drop 6 line'
        st' <- loadFile st filename
        replLoop st'
    _ | ":history" == line' -> do
        mapM_ putStrLn (take 20 (reverse (rsHistory st)))
        replLoop st
    -- Multi-line detection: ends with :, \, or keyword that opens block
    _ | isMultilineStart line' -> do
        let accumulated = rsMultiline st ++ [line']
        replLoop st{ rsMultiline = accumulated }
    -- Empty line flushes multi-line buffer
    "" | not (null (rsMultiline st)) -> do
        let fullInput = unlines (rsMultiline st)
        st' <- execInput st fullInput
        replLoop st'{ rsMultiline = [] }
    -- Normal single line
    _ -> do
      let fullInput = unlines (rsMultiline st ++ [line'])
      st' <- execInput st fullInput
      replLoop st'{ rsMultiline = [] }

-- =============================================================================
-- INPUT EXECUTION
-- =============================================================================

execInput :: ReplState -> String -> IO ReplState
execInput st input = do
  let trimmed = trim input
  if null trimmed
    then return st
    else do
      result <- try (runReplLine (rsEnv st) trimmed)
      case result of
        Left (ex :: MiniFlowError) -> do
          putStrLn (prettyError ex)
          return st
        Left (ex :: SomeException) -> do
          putStrLn ("Error: " ++ show ex)
          return st
        Right mval -> do
          case mval of
            Nothing  -> return ()
            Just val -> case val of
              VNone -> return ()
              _     -> do
                s <- prettyValueIO val
                putStrLn s
          return st{ rsHistory = trimmed : rsHistory st }

runReplLine :: Env -> String -> IO (Maybe Value)
runReplLine env input = do
  -- Try to lex
  tokens <- case tokenize input "<repl>" of
    Left (LexError pos msg) -> throwMF (MFSyntaxError msg pos)
    Right ts -> return ts
  -- Try to parse as expression first, then as statement
  case parseExpr tokens of
    Right expr -> do
      val <- evalExpr env expr
      return (Just val)
    Left _ -> case parseProgram tokens of
      Left (ParseError pos msg) -> throwMF (MFSyntaxError msg pos)
      Right prog -> do
        evalProgram env prog
        return Nothing

parseExpr :: [TokenInfo] -> Either ParseError Expr
parseExpr toks =
  let st = ParserState{ psTokens = toks, psPos = noPos }
  in case runParser (parsePipeExpr <* expectEOF) st of
    Left  e       -> Left e
    Right (e, _)  -> Right e

expectEOF :: Parser ()
expectEOF = do
  t <- peek
  case t of
    TEOF     -> return ()
    TNewline -> consume >> expectEOF
    _        -> return ()  -- allow trailing content

throwMF :: MiniFlowError -> IO a
throwMF = Control.Exception.throwIO

-- =============================================================================
-- FILE LOADING
-- =============================================================================

loadFile :: ReplState -> String -> IO ReplState
loadFile st filename = do
  result <- try (readFile filename)
  case result of
    Left (ex :: SomeException) -> do
      putStrLn ("Error loading file: " ++ show ex)
      return st
    Right content -> do
      execResult <- try (runFile (rsEnv st) filename content)
      case execResult of
        Left (ex :: MiniFlowError) -> do
          putStrLn (prettyError ex)
          return st
        Left (ex :: SomeException) -> do
          putStrLn ("Error: " ++ show ex)
          return st
        Right _ -> do
          putStrLn ("Loaded: " ++ filename)
          return st

runFile :: Env -> String -> String -> IO ()
runFile env filename content = do
  tokens <- case tokenize content filename of
    Left (LexError pos msg) -> throwMF (MFSyntaxError msg pos)
    Right ts -> return ts
  prog <- case parseProgram tokens of
    Left (ParseError pos msg) -> throwMF (MFSyntaxError msg pos)
    Right p -> return p
  evalProgram env prog
  return ()

-- =============================================================================
-- TYPE QUERY
-- =============================================================================

handleTypeQuery :: Env -> String -> IO ()
handleTypeQuery env exprStr = do
  result <- try (do
    tokens <- case tokenize exprStr "<type_query>" of
      Left (LexError pos msg) -> throwMF (MFSyntaxError msg pos)
      Right ts -> return ts
    case parseExpr tokens of
      Left (ParseError pos msg) -> throwMF (MFSyntaxError msg pos)
      Right expr -> do
        val <- evalExpr env expr
        return (typeNameOf val)) :: IO (Either SomeException String)
  case result of
    Left ex  -> putStrLn ("Error: " ++ show ex)
    Right ty -> putStrLn (exprStr ++ " :: " ++ ty)

-- =============================================================================
-- HELPERS
-- =============================================================================

isMultilineStart :: String -> Bool
isMultilineStart s = any (`isSuffixOf` rstrip s)
  [":", "\\", "->", "do", "then", "else"]
  || any (`isPrefixOf` s) ["def ", "if ", "for ", "while ", "match ", "try:"]
  where isSuffixOf suf str = drop (length str - length suf) str == suf

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')

rstrip :: String -> String
rstrip = reverse . dropWhile (== ' ') . reverse

printHelp :: IO ()
printHelp = do
  putStrLn ""
  putStrLn "MiniFlow REPL Commands:"
  putStrLn "  :q, :quit, :exit  -- Exit the REPL"
  putStrLn "  :help             -- Show this help message"
  putStrLn "  :env              -- Show all current bindings"
  putStrLn "  :reset            -- Reset environment to built-ins only"
  putStrLn "  :t <expr>         -- Show type of expression"
  putStrLn "  :load <file.mf>   -- Load and execute a MiniFlow file"
  putStrLn "  :history          -- Show last 20 commands"
  putStrLn ""
  putStrLn "Language Features:"
  putStrLn "  let x = 10                    -- Variable definition"
  putStrLn "  def f(x) = x * 2             -- Function definition"
  putStrLn "  [1,2,3] |> map(double)       -- Pipe operator"
  putStrLn "  [1,2,3] |> filter(odd) |> sum -- Pipeline"
  putStrLn "  \\x -> x * 2                  -- Lambda"
  putStrLn "  [x*x for x in range(10)]     -- List comprehension"
  putStrLn "  match x { case 0: ... }      -- Pattern matching"
  putStrLn "  foldr (+) 0 [1..10]          -- Haskell-style"
  putStrLn "  map, filter, reduce, zip...  -- Python-style"
  putStrLn ""

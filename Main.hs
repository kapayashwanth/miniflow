-- =============================================================================
-- MiniFlow Language Interpreter
-- Main.hs  -- Entry point
-- =============================================================================
module Main where

import Types
import Lexer
import Parser
import Evaluator
import REPL
import Pretty
import Environment
import Data.IORef
import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import Control.Exception (try, catch, SomeException)

-- =============================================================================
-- ENTRY POINT
-- =============================================================================

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> startRepl
    ["-"]        -> startRepl
    ["--repl"]   -> startRepl
    [filename]   -> runFile filename
    ("--eval":code:_) -> runCode "<eval>" code
    _            -> printUsage

-- =============================================================================
-- FILE EXECUTION MODE
-- =============================================================================

runFile :: FilePath -> IO ()
runFile filename = do
  content <- readFile filename `catch` \(ex :: SomeException) -> do
    hPutStrLn stderr ("Error: Cannot open file '" ++ filename ++ "': " ++ show ex)
    return ""
  if null content
    then return ()
    else runCode filename content

runCode :: String -> String -> IO ()
runCode filename content = do
  -- 1. Lex
  tokens <- case tokenize content filename of
    Left (LexError pos msg) -> do
      hPutStrLn stderr ("SyntaxError at " ++ show pos ++ ": " ++ msg)
      return []
    Right ts -> return ts

  if null tokens
    then return ()
    else do
      -- 2. Parse
      prog <- case parseProgram tokens of
        Left (ParseError pos msg) -> do
          hPutStrLn stderr ("ParseError at " ++ show pos ++ ": " ++ msg)
          return (Program [])
        Right p -> return p

      case progStmts prog of
        [] -> return ()
        _  -> do
          -- 3. Initialize environment
          env <- initGlobalEnv

          -- 4. Evaluate
          result <- try (evalProgram env prog) :: IO (Either MiniFlowError Value)
          case result of
            Left ex -> hPutStrLn stderr (prettyError ex)
            Right _ -> return ()

-- =============================================================================
-- USAGE
-- =============================================================================

printUsage :: IO ()
printUsage = do
  putStrLn "MiniFlow Language Interpreter"
  putStrLn ""
  putStrLn "Usage:"
  putStrLn "  runhaskell Main.hs                    -- Start REPL"
  putStrLn "  runhaskell Main.hs <file.mf>          -- Execute file"
  putStrLn "  runhaskell Main.hs --eval '<code>'    -- Evaluate code"
  putStrLn "  runhaskell Main.hs --repl             -- Force REPL mode"
  putStrLn ""
  putStrLn "Examples:"
  putStrLn "  runhaskell Main.hs examples/sample.mf"
  putStrLn "  runhaskell Main.hs examples/fibonacci.mf"
  putStrLn "  runhaskell Main.hs examples/pipes.mf"

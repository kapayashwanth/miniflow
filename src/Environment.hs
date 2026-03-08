-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Environment.hs  -- Scoped runtime environment with IORef-based mutation
-- =============================================================================
module Environment
  ( newGlobalEnv
  , newChildEnv
  , lookupVar
  , lookupVarIO
  , defineVar
  , assignVar
  , envToList
  , extendEnv
  , envDepth
  , dumpEnv
  , withScope
  , copyEnv
  ) where

import Types
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (throwIO)

-- =============================================================================
-- ENVIRONMENT CREATION
-- =============================================================================

-- | Create a new top-level (global) environment with no parent
newGlobalEnv :: IO Env
newGlobalEnv = newIORef EnvFrame
  { envBindings = Map.empty
  , envParent   = Nothing
  , envName     = "<global>"
  }

-- | Create a child scope (e.g. function body, block) with the given parent
newChildEnv :: String -> Env -> IO Env
newChildEnv name parent = newIORef EnvFrame
  { envBindings = Map.empty
  , envParent   = Just parent
  , envName     = name
  }

-- =============================================================================
-- VARIABLE LOOKUP
-- =============================================================================

-- | Look up a variable in the environment chain. Returns Nothing if not found.
lookupVar :: String -> Env -> IO (Maybe Value)
lookupVar name envRef = do
  frame <- readIORef envRef
  case Map.lookup name (envBindings frame) of
    Just ref -> Just <$> readIORef ref
    Nothing  -> case envParent frame of
      Nothing     -> return Nothing
      Just parent -> lookupVar name parent

-- | Like lookupVar but throws NameError if not found
lookupVarIO :: SourcePos -> String -> Env -> IO Value
lookupVarIO pos name env = do
  result <- lookupVar name env
  case result of
    Just v  -> return v
    Nothing -> throwIO (MFNameError
      { mfMsg = "name '" ++ name ++ "' is not defined"
      , mfPos = pos
      })

-- =============================================================================
-- VARIABLE DEFINITION AND ASSIGNMENT
-- =============================================================================

-- | Define (or redefine) a variable in the *current* frame only
defineVar :: String -> Value -> Env -> IO ()
defineVar name val envRef = do
  frame <- readIORef envRef
  ref   <- newIORef val
  writeIORef envRef frame
    { envBindings = Map.insert name ref (envBindings frame) }

-- | Assign to an existing variable. Walks up the chain to find it.
--   Returns True if found and updated, False if not found anywhere.
assignVar :: String -> Value -> Env -> IO Bool
assignVar name val envRef = do
  frame <- readIORef envRef
  case Map.lookup name (envBindings frame) of
    Just ref -> do
      writeIORef ref val
      return True
    Nothing  -> case envParent frame of
      Nothing     -> return False
      Just parent -> assignVar name val parent

-- | Like assignVar but creates in global scope if not found
assignOrDefineGlobal :: String -> Value -> Env -> IO ()
assignOrDefineGlobal name val envRef = do
  found <- assignVar name val envRef
  if found
    then return ()
    else do
      global <- findGlobalEnv envRef
      defineVar name val global

-- | Walk up to find the global (outermost) environment
findGlobalEnv :: Env -> IO Env
findGlobalEnv envRef = do
  frame <- readIORef envRef
  case envParent frame of
    Nothing -> return envRef
    Just p  -> findGlobalEnv p

-- =============================================================================
-- BULK OPERATIONS
-- =============================================================================

-- | Get all bindings visible from this environment (including parents)
envToList :: Env -> IO [(String, Value)]
envToList envRef = do
  frame <- readIORef envRef
  localBindings <- mapM (\(k, ref) -> do
    v <- readIORef ref
    return (k, v)) (Map.toList (envBindings frame))
  case envParent frame of
    Nothing -> return localBindings
    Just p  -> do
      parentBindings <- envToList p
      -- Local bindings shadow parent
      let localKeys = map fst localBindings
          parentFiltered = filter (\(k,_) -> k `notElem` localKeys) parentBindings
      return (localBindings ++ parentFiltered)

-- | Get only local (current frame) bindings
localBindings :: Env -> IO [(String, Value)]
localBindings envRef = do
  frame <- readIORef envRef
  mapM (\(k, ref) -> do
    v <- readIORef ref
    return (k, v)) (Map.toList (envBindings frame))

-- | Create a child environment pre-populated with the given bindings
extendEnv :: String -> [(String, Value)] -> Env -> IO Env
extendEnv name bindings parent = do
  child <- newChildEnv name parent
  mapM_ (\(k, v) -> defineVar k v child) bindings
  return child

-- =============================================================================
-- SCOPE UTILITIES
-- =============================================================================

-- | Run an action in a fresh child scope, then return the result
withScope :: String -> Env -> IO a -> IO a
withScope name parentEnv action = do
  _child <- newChildEnv name parentEnv
  action

-- | Get the depth of the environment chain (for debugging / stack overflow detection)
envDepth :: Env -> IO Int
envDepth envRef = do
  frame <- readIORef envRef
  case envParent frame of
    Nothing -> return 1
    Just p  -> (1 +) <$> envDepth p

-- | Copy an environment (shallow copy of current frame, sharing parent)
copyEnv :: Env -> IO Env
copyEnv envRef = do
  frame <- readIORef envRef
  -- Deep-copy the bindings map (each IORef is new, same initial value)
  newBindings <- mapM (\ref -> do
    v <- readIORef ref
    newIORef v) (envBindings frame)
  newIORef frame{ envBindings = newBindings }

-- =============================================================================
-- DEBUGGING
-- =============================================================================

-- | Pretty-print the current environment (for :env REPL command)
dumpEnv :: Env -> IO String
dumpEnv envRef = do
  frame <- readIORef envRef
  bindings <- localBindings envRef
  let lines' = map (\(k, v) -> "  " ++ k ++ " = " ++ showValue v) bindings
  parentStr <- case envParent frame of
    Nothing -> return ""
    Just p  -> do
      parentDump <- dumpEnv p
      return ("\n--- parent scope: " ++ envName frame ++ " ---\n" ++ parentDump)
  return ("=== " ++ envName frame ++ " ===\n" ++ unlines lines' ++ parentStr)

showValue :: Value -> String
showValue (VInt n)       = show n
showValue (VFloat f)     = show f
showValue (VBool True)   = "True"
showValue (VBool False)  = "False"
showValue VNone          = "None"
showValue (VStr s)       = show s
showValue (VList _)      = "<list>"
showValue (VTuple vs)    = "(" ++ concatMap (\v -> showValue v ++ ",") vs ++ ")"
showValue (VDict _)      = "<dict>"
showValue (VSet _)       = "<set>"
showValue (VClosure{vcName=n}) = "<function " ++ n ++ ">"
showValue (VBuiltin n _) = "<builtin " ++ n ++ ">"
showValue (VRecord n _)  = "<record " ++ n ++ ">"
showValue (VType t)      = "<type " ++ t ++ ">"
showValue (VLazyList _)  = "<lazy_list>"
showValue (VIterator _)  = "<iterator>"
showValue (VModule n _)  = "<module " ++ n ++ ">"
showValue (VNativeExn e) = "<exception " ++ e ++ ">"

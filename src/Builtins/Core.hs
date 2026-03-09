-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Builtins/Core.hs  -- Shared infrastructure for built-in functions
-- =============================================================================
module Builtins.Core
  ( registerAllBuiltins
  , coerceNum
  , coerceInt
  , coerceBool
  , coerceList
  , coerceStr
  , valueToStr
  , valueRepr
  , typeOf
  , deepEqual
  , compareValues
  , requireArgs
  , requireArgsRange
  , mkBuiltin
  , mkBuiltin0
  , mkBuiltin1
  , mkBuiltin2
  , mkBuiltin3
  , numericBinOp
  , applyFn
  , iterateIO
  , listFromIO
  , lazyTake
  , lazyMap
  , lazyFilter
  , lazyToList
  ) where

import Types
import Environment
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, nub, sort, sortBy, groupBy, isPrefixOf,
                  isSuffixOf, isInfixOf, tails, inits, permutations,
                  subsequences, transpose, intercalate, intersperse)
import Data.Char (toUpper, toLower, isAlpha, isDigit, isSpace, ord, chr)
import Control.Exception (throwIO, catch, SomeException, evaluate)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..))

-- =============================================================================
-- REGISTRATION
-- =============================================================================

registerAllBuiltins :: Env -> IO ()
registerAllBuiltins env = do
  -- Python-style builtins
  mapM_ (\(n, f) -> defineVar n (VBuiltin n f) env) pythonBuiltins
  -- Haskell-style builtins
  mapM_ (\(n, f) -> defineVar n (VBuiltin n f) env) haskellBuiltins
  -- Math constants
  defineVar "pi"   (VFloat 3.141592653589793) env
  defineVar "e"    (VFloat 2.718281828459045) env
  defineVar "inf"  (VFloat (1/0))             env
  defineVar "nan"  (VFloat (0/0))             env
  defineVar "True"  (VBool True)              env
  defineVar "False" (VBool False)             env
  defineVar "None"  VNone                     env

-- Declared in Python.hs and Haskell.hs; forward declarations here
pythonBuiltins :: [(String, [Value] -> [(String, Value)] -> IO Value)]
pythonBuiltins = []  -- populated in Python.hs

haskellBuiltins :: [(String, [Value] -> [(String, Value)] -> IO Value)]
haskellBuiltins = []  -- populated in Haskell.hs

-- =============================================================================
-- TYPE COERCION
-- =============================================================================

coerceNum :: SourcePos -> String -> Value -> IO Double
coerceNum pos ctx v = case v of
  VInt n    -> return (fromIntegral n)
  VFloat f  -> return f
  VStr s    -> case reads s of
    [(n, "")] -> return n
    _         -> throwIO (MFTypeError
      { mfMsg = ctx ++ ": cannot convert str '" ++ s ++ "' to number"
      , mfPos = pos })
  VBool True  -> return 1.0
  VBool False -> return 0.0
  _           -> throwIO (MFTypeError
    { mfMsg = ctx ++ ": expected number, got " ++ typeOf v
    , mfPos = pos })

coerceInt :: SourcePos -> String -> Value -> IO Int
coerceInt pos ctx v = case v of
  VInt n    -> return n
  VFloat f  -> return (round f)
  VStr s    -> case reads s of
    [(n, "")] -> return n
    _         -> throwIO (MFTypeError
      { mfMsg = ctx ++ ": cannot convert '" ++ s ++ "' to int"
      , mfPos = pos })
  VBool True  -> return 1
  VBool False -> return 0
  _           -> throwIO (MFTypeError
    { mfMsg = ctx ++ ": expected int, got " ++ typeOf v
    , mfPos = pos })

coerceBool :: Value -> Bool
coerceBool = isTruthy

coerceList :: SourcePos -> String -> Value -> IO [Value]
coerceList pos ctx v = case v of
  VList ref  -> readIORef ref
  VTuple vs  -> return vs
  VStr s     -> return (map (VStr . (:[])) s)
  VSet ref   -> readIORef ref
  VDict ref  -> do
    m <- readIORef ref
    return (map fst (Map.toList m))
  VIterator ref -> readIORef ref
  _         -> throwIO (MFTypeError
    { mfMsg = ctx ++ ": object is not iterable: " ++ typeOf v
    , mfPos = pos })

coerceStr :: Value -> String
coerceStr = valueToStr

-- =============================================================================
-- VALUE DISPLAY
-- =============================================================================

valueToStr :: Value -> String
valueToStr VNone          = "None"
valueToStr (VBool True)   = "True"
valueToStr (VBool False)  = "False"
valueToStr (VInt n)       = show n
valueToStr (VFloat f)     = formatFlt f
valueToStr (VStr s)       = s
valueToStr (VTuple vs)    =
  let strs = map valueRepr vs
  in case strs of
    [s] -> "(" ++ s ++ ",)"
    _   -> "(" ++ intercalate ", " strs ++ ")"
valueToStr (VList _)      = "<list>"
valueToStr (VDict _)      = "<dict>"
valueToStr (VSet _)       = "<set>"
valueToStr (VClosure{vcName=n}) = "<function " ++ n ++ ">"
valueToStr (VBuiltin n _) = "<built-in function " ++ n ++ ">"
valueToStr (VRecord n _)  = "<" ++ n ++ " instance>"
valueToStr (VType t)      = "<class '" ++ t ++ "'>"
valueToStr (VLazyList _)  = "<lazy_list>"
valueToStr (VIterator _)  = "<iterator>"
valueToStr (VModule n _)  = "<module '" ++ n ++ "'>"
valueToStr (VNativeExn e) = e

-- | Python repr() style
valueRepr :: Value -> String
valueRepr (VStr s)      = show s
valueRepr (VBool True)  = "True"
valueRepr (VBool False) = "False"
valueRepr VNone         = "None"
valueRepr v             = valueToStr v

formatFlt :: Double -> String
formatFlt f
  | isInfinite f && f > 0 = "inf"
  | isInfinite f && f < 0 = "-inf"
  | isNaN f               = "nan"
  | f == fromIntegral (round f :: Int) && abs f < 1e15
                          = show (round f :: Int) ++ ".0"
  | otherwise             = show f

-- =============================================================================
-- TYPE INSPECTION
-- =============================================================================

typeOf :: Value -> String
typeOf = typeNameOf

-- =============================================================================
-- COMPARISON
-- =============================================================================

deepEqual :: Value -> Value -> IO Bool
deepEqual (VList r1) (VList r2) = do
  l1 <- readIORef r1
  l2 <- readIORef r2
  if length l1 /= length l2
    then return False
    else fmap and (zipWithM deepEqual l1 l2)
deepEqual (VDict r1) (VDict r2) = do
  m1 <- readIORef r1
  m2 <- readIORef r2
  if Map.keys m1 /= Map.keys m2
    then return False
    else fmap and (zipWithM deepEqual (Map.elems m1) (Map.elems m2))
deepEqual v1 v2 = return (v1 == v2)

zipWithM :: Monad m => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithM _ [] _      = return []
zipWithM _ _ []      = return []
zipWithM f (a:as) (b:bs) = do
  c  <- f a b
  cs <- zipWithM f as bs
  return (c : cs)

compareValues :: Value -> Value -> IO Ordering
compareValues v1 v2 = do
  eq <- deepEqual v1 v2
  if eq
    then return EQ
    else return (compare v1 v2)

-- =============================================================================
-- ARGUMENT VALIDATION
-- =============================================================================

requireArgs :: String -> Int -> [Value] -> IO ()
requireArgs name n args =
  when (length args /= n) $
    throwIO (MFTypeError
      { mfMsg = name ++ "() takes exactly " ++ show n ++
                " argument(s) (" ++ show (length args) ++ " given)"
      , mfPos = noPos })

requireArgsRange :: String -> Int -> Int -> [Value] -> IO ()
requireArgsRange name lo hi args =
  when (length args < lo || length args > hi) $
    throwIO (MFTypeError
      { mfMsg = name ++ "() takes " ++ show lo ++ " to " ++ show hi ++
                " arguments (" ++ show (length args) ++ " given)"
      , mfPos = noPos })

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = return ()

-- =============================================================================
-- BUILTIN CONSTRUCTORS
-- =============================================================================

mkBuiltin :: String -> ([Value] -> [(String, Value)] -> IO Value) -> (String, [Value] -> [(String, Value)] -> IO Value)
mkBuiltin = (,)

mkBuiltin0 :: String -> IO Value -> (String, [Value] -> [(String, Value)] -> IO Value)
mkBuiltin0 name action = (name, \_ _ -> action)

mkBuiltin1 :: String -> (Value -> IO Value) -> (String, [Value] -> [(String, Value)] -> IO Value)
mkBuiltin1 name f = (name, \args _ -> do
  requireArgs name 1 args
  f (head args))

mkBuiltin2 :: String -> (Value -> Value -> IO Value) -> (String, [Value] -> [(String, Value)] -> IO Value)
mkBuiltin2 name f = (name, \args _ -> do
  requireArgs name 2 args
  f (args !! 0) (args !! 1))

mkBuiltin3 :: String -> (Value -> Value -> Value -> IO Value) -> (String, [Value] -> [(String, Value)] -> IO Value)
mkBuiltin3 name f = (name, \args _ -> do
  requireArgs name 3 args
  f (args !! 0) (args !! 1) (args !! 2))

-- =============================================================================
-- NUMERIC BINARY OPERATION
-- =============================================================================

numericBinOp :: String -> (Double -> Double -> Double) -> Value -> Value -> IO Value
numericBinOp _ f (VInt a) (VInt b) =
  let result = f (fromIntegral a) (fromIntegral b)
  in return (if result == fromIntegral (round result :: Int) && abs result < 2^31
             then VInt (round result)
             else VFloat result)
numericBinOp _ f (VFloat a) (VFloat b) = return (VFloat (f a b))
numericBinOp _ f (VInt a) (VFloat b)   = return (VFloat (f (fromIntegral a) b))
numericBinOp _ f (VFloat a) (VInt b)   = return (VFloat (f a (fromIntegral b)))
numericBinOp name _ a b = throwIO (MFTypeError
  { mfMsg = name ++ ": unsupported types " ++ typeOf a ++ " and " ++ typeOf b
  , mfPos = noPos })

-- =============================================================================
-- FUNCTION APPLICATION HELPER
-- =============================================================================

-- | Apply a MiniFlow value as a function to argument values.
--   This is the central dispatch used by all higher-order builtins.
applyFn :: Value -> [Value] -> IO Value
applyFn (VBuiltin _ f) args = f args []
applyFn (VClosure params body closureEnv name) args = do
  -- Import here to avoid circular dependency; in practice, Evaluator.applyFn is used
  throwIO (MFRuntimeError
    { mfMsg = "applyFn: closure application must go through Evaluator"
    , mfPos = noPos })
applyFn v _ = throwIO (MFTypeError
  { mfMsg = "'" ++ typeOf v ++ "' object is not callable"
  , mfPos = noPos })

-- =============================================================================
-- LAZY LIST HELPERS
-- =============================================================================

iterateIO :: (Value -> IO Value) -> Value -> IO Value
iterateIO f seed = do
  ref <- newIORef (makeLazy seed)
  return (VLazyList ref)
  where
    makeLazy v = LCons v (f v >>= \v' -> return (makeLazy v'))

lazyTake :: Int -> IORef LazyList -> IO [Value]
lazyTake 0 _   = return []
lazyTake n ref = do
  ll <- readIORef ref
  case ll of
    LNil          -> return []
    LCons v mkRest -> do
      rest    <- mkRest
      restRef <- newIORef rest
      vs      <- lazyTake (n-1) restRef
      return (v : vs)

-- | Map a function lazily over a LazyList, returning a new VLazyList
lazyMap :: (Value -> IO Value) -> IORef LazyList -> IO Value
lazyMap f ref = do
  ll <- readIORef ref
  buildMapped f ll

buildMapped :: (Value -> IO Value) -> LazyList -> IO Value
buildMapped _ LNil = do
  r <- newIORef LNil
  return (VLazyList r)
buildMapped f (LCons v mkRest) = do
  v' <- f v
  let next = mkRest >>= \rest -> do
        VLazyList r <- buildMapped f rest
        readIORef r
  r <- newIORef (LCons v' next)
  return (VLazyList r)

-- | Filter a LazyList lazily, returning a new VLazyList
lazyFilter :: (Value -> IO Bool) -> IORef LazyList -> IO Value
lazyFilter p ref = do
  ll <- readIORef ref
  buildFiltered p ll

buildFiltered :: (Value -> IO Bool) -> LazyList -> IO Value
buildFiltered _ LNil = newIORef LNil >>= \r -> return (VLazyList r)
buildFiltered p (LCons v mkRest) = do
  keep <- p v
  if keep
    then do
      let next = mkRest >>= \rest -> do
            VLazyList r <- buildFiltered p rest
            readIORef r
      r <- newIORef (LCons v next)
      return (VLazyList r)
    else mkRest >>= buildFiltered p

-- | Force a lazy list to a Haskell list (only safe for finite lists)
lazyToList :: IORef LazyList -> IO [Value]
lazyToList ref = do
  ll <- readIORef ref
  case ll of
    LNil -> return []
    LCons v mkRest -> do
      rest <- mkRest
      restRef <- newIORef rest
      vs <- lazyToList restRef
      return (v : vs)

listFromIO :: Value -> IO [Value]
listFromIO (VList ref)   = readIORef ref
listFromIO (VTuple vs)   = return vs
listFromIO (VStr s)      = return (map (VStr . (:[])) s)
listFromIO (VLazyList r) = lazyToList r
listFromIO v             = throwIO (MFTypeError
  { mfMsg = "not iterable: " ++ typeOf v
  , mfPos = noPos })

-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Builtins/Python.hs  -- All Python-compatible built-in functions
-- =============================================================================
module Builtins.Python
  ( pythonBuiltinList
  ) where

import Types
import Builtins.Core
import Environment
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, nub, sort, sortBy, groupBy, isPrefixOf,
                  isSuffixOf, isInfixOf, elemIndex, findIndex, partition,
                  stripPrefix, stripSuffix)
import Data.Char (toUpper, toLower, isAlpha, isAlphaNum, isDigit, isSpace,
                  isUpper, isLower, isPunctuation, ord, chr, digitToInt)
import Control.Exception (throwIO, catch, SomeException)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, stderr)
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (printf)

-- We import applyFn from Evaluator at runtime via an IORef callback
-- to avoid circular imports. The evaluator sets this on startup.
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)

-- Global callback: set by Evaluator.hs on startup
applyCallback :: IORef (Value -> [Value] -> IO Value)
applyCallback = unsafePerformIO $ newIORef defaultApply
{-# NOINLINE applyCallback #-}

defaultApply :: Value -> [Value] -> IO Value
defaultApply (VBuiltin _ f) args = f args []
defaultApply v _ = throwIO (MFTypeError
  { mfMsg = "not callable: " ++ typeNameOf v, mfPos = noPos })

setApplyCallback :: (Value -> [Value] -> IO Value) -> IO ()
setApplyCallback fn = writeIORef applyCallback fn

callFn :: Value -> [Value] -> IO Value
callFn fn args = do
  cb <- readIORef applyCallback
  cb fn args

-- =============================================================================
-- PYTHON BUILTIN TABLE
-- =============================================================================

pythonBuiltinList :: [(String, [Value] -> [(String, Value)] -> IO Value)]
pythonBuiltinList =
  [ -- I/O
    ("print",      builtinPrint)
  , ("input",      builtinInput)
  , ("open",       builtinOpen)
  -- Type conversions
  , ("int",        builtinInt)
  , ("float",      builtinFloat)
  , ("str",        builtinStr)
  , ("bool",       builtinBool)
  , ("list",       builtinList)
  , ("tuple",      builtinTuple)
  , ("dict",       builtinDict)
  , ("set",        builtinSet)
  , ("frozenset",  builtinFrozenset)
  , ("bytes",      builtinBytes)
  , ("bytearray",  builtinBytearray)
  , ("memoryview", builtinMemoryview)
  , ("complex",    builtinComplex)
  -- Numeric
  , ("abs",        builtinAbs)
  , ("round",      builtinRound)
  , ("min",        builtinMin)
  , ("max",        builtinMax)
  , ("sum",        builtinSum)
  , ("pow",        builtinPow)
  , ("divmod",     builtinDivmod)
  , ("bin",        builtinBin)
  , ("oct",        builtinOct)
  , ("hex",        builtinHex)
  -- String / char
  , ("chr",        builtinChr)
  , ("ord",        builtinOrd)
  , ("format",     builtinFormat)
  , ("repr",       builtinRepr)
  -- Collections
  , ("len",        builtinLen)
  , ("range",      builtinRange)
  , ("enumerate",  builtinEnumerate)
  , ("zip",        builtinZip)
  , ("map",        builtinMap)
  , ("filter",     builtinFilter)
  , ("sorted",     builtinSorted)
  , ("reversed",   builtinReversed)
  , ("any",        builtinAny)
  , ("all",        builtinAll)
  , ("next",       builtinNext)
  , ("iter",       builtinIter)
  , ("slice",      builtinSlice)
  -- Inspection
  , ("type",       builtinType)
  , ("isinstance", builtinIsinstance)
  , ("issubclass", builtinIssubclass)
  , ("callable",   builtinCallable)
  , ("hasattr",    builtinHasattr)
  , ("getattr",    builtinGetattr)
  , ("setattr",    builtinSetattr)
  , ("delattr",    builtinDelattr)
  , ("dir",        builtinDir)
  , ("vars",       builtinVars)
  , ("id",         builtinId)
  , ("hash",       builtinHash)
  -- Functional
  , ("reduce",     builtinReduce)
  , ("zip_longest",builtinZipLongest)
  -- Output formatting
  , ("print_r",    builtinPrintR)
  -- Error handling
  , ("exit",       builtinExit)
  , ("quit",       builtinExit)
  -- Eval (limited)
  , ("eval",       builtinEval)
  -- Math
  , ("abs",        builtinAbs)
  , ("product",    builtinProduct)
  ]

-- =============================================================================
-- I/O
-- =============================================================================

builtinPrint :: [Value] -> [(String, Value)] -> IO Value
builtinPrint args kwargs = do
  let sep = case lookup "sep" kwargs of
              Just (VStr s) -> s
              _             -> " "
  let end_ = case lookup "end" kwargs of
              Just (VStr s) -> s
              _             -> "\n"
  strs <- mapM displayVal args
  putStr (intercalate sep strs ++ end_)
  hFlush stdout
  return VNone

displayVal :: Value -> IO String
displayVal (VStr s)       = return s
displayVal VNone          = return "None"
displayVal (VBool True)   = return "True"
displayVal (VBool False)  = return "False"
displayVal (VInt n)       = return (show n)
displayVal (VFloat f)     = return (valueToStr (VFloat f))
displayVal (VList ref)    = do
  vs <- readIORef ref
  strs <- mapM reprVal vs
  return ("[" ++ intercalate ", " strs ++ "]")
displayVal (VTuple vs)    = do
  strs <- mapM reprVal vs
  case strs of
    [s] -> return ("(" ++ s ++ ",)")
    _   -> return ("(" ++ intercalate ", " strs ++ ")")
displayVal (VDict ref)    = do
  m <- readIORef ref
  pairs <- mapM (\(k,v) -> do
    ks <- reprVal k
    vs <- reprVal v
    return (ks ++ ": " ++ vs)) (Map.toAscList m)
  return ("{" ++ intercalate ", " pairs ++ "}")
displayVal (VSet ref)     = do
  vs <- readIORef ref
  strs <- mapM reprVal vs
  case strs of
    [] -> return "set()"
    _  -> return ("{" ++ intercalate ", " strs ++ "}")
displayVal v              = return (valueToStr v)

reprVal :: Value -> IO String
reprVal (VStr s)    = return (show s)
reprVal (VBool True) = return "True"
reprVal (VBool False) = return "False"
reprVal VNone       = return "None"
reprVal v           = displayVal v

builtinInput :: [Value] -> [(String, Value)] -> IO Value
builtinInput args _ = do
  hSetBuffering stdout NoBuffering
  case args of
    [VStr prompt] -> putStr prompt >> hFlush stdout
    []            -> return ()
    (v:_)         -> putStr (valueToStr v) >> hFlush stdout
  line <- getLine
  return (VStr line)

builtinOpen :: [Value] -> [(String, Value)] -> IO Value
builtinOpen args kwargs = do
  case args of
    (VStr path : rest) -> do
      let mode = case rest of
                   (VStr m : _) -> m
                   _            -> case lookup "mode" kwargs of
                                     Just (VStr m) -> m
                                     _             -> "r"
      -- Return a file-like dict with read/write methods
      contents <- case mode of
        "r"  -> readFile path
        _    -> return ""
      ref <- newIORef (VStr contents)
      return (VStr ("file:" ++ path))  -- simplified
    _ -> throwIO (MFTypeError { mfMsg = "open() requires a filename", mfPos = noPos })

-- =============================================================================
-- TYPE CONVERSIONS
-- =============================================================================

builtinInt :: [Value] -> [(String, Value)] -> IO Value
builtinInt [] _ = return (VInt 0)
builtinInt (v:rest) kwargs = case v of
  VInt n      -> return (VInt n)
  VFloat f    -> return (VInt (truncate f))
  VBool True  -> return (VInt 1)
  VBool False -> return (VInt 0)
  VStr s      -> do
    let base = case rest of
                 (VInt b:_) -> b
                 _           -> case lookup "base" kwargs of
                                  Just (VInt b) -> b
                                  _             -> 10
    case parseIntBase base (trim s) of
      Just n  -> return (VInt n)
      Nothing -> throwIO (MFValueError
        { mfMsg = "int() invalid literal: '" ++ s ++ "'"
        , mfPos = noPos })
  _ -> throwIO (MFTypeError
    { mfMsg = "int() argument must be a string or number, not '" ++ typeNameOf v ++ "'"
    , mfPos = noPos })

parseIntBase :: Int -> String -> Maybe Int
parseIntBase 10 s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing
parseIntBase 16 ('0':'x':s) = case reads ("0x" ++ s) of
  [(n, "")] -> Just n
  _         -> Nothing
parseIntBase 16 s = case reads ("0x" ++ s) of
  [(n, "")] -> Just n
  _         -> Nothing
parseIntBase 8 ('0':'o':s) = case reads ("0o" ++ s) of
  [(n, "")] -> Just n
  _         -> Nothing
parseIntBase 2 ('0':'b':s) = case reads ("0b" ++ s) of
  [(n, "")] -> Just n
  _         -> Nothing
parseIntBase _ s = Nothing

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

builtinFloat :: [Value] -> [(String, Value)] -> IO Value
builtinFloat [] _ = return (VFloat 0.0)
builtinFloat (v:_) _ = case v of
  VFloat f    -> return (VFloat f)
  VInt n      -> return (VFloat (fromIntegral n))
  VBool True  -> return (VFloat 1.0)
  VBool False -> return (VFloat 0.0)
  VStr s      -> case reads (trim s) of
    [(f, "")] -> return (VFloat f)
    _         -> throwIO (MFValueError
      { mfMsg = "float() invalid literal: '" ++ s ++ "'"
      , mfPos = noPos })
  _ -> throwIO (MFTypeError
    { mfMsg = "float() argument must be a string or number"
    , mfPos = noPos })

builtinStr :: [Value] -> [(String, Value)] -> IO Value
builtinStr [] _ = return (VStr "")
builtinStr (v:_) _ = VStr <$> displayVal v

builtinBool :: [Value] -> [(String, Value)] -> IO Value
builtinBool []    _ = return (VBool False)
builtinBool (v:_) _ = case v of
  VList ref -> do
    vs <- readIORef ref
    return (VBool (not (null vs)))
  VDict ref -> do
    m <- readIORef ref
    return (VBool (not (Map.null m)))
  VSet ref -> do
    vs <- readIORef ref
    return (VBool (not (null vs)))
  _ -> return (VBool (isTruthy v))

builtinList :: [Value] -> [(String, Value)] -> IO Value
builtinList [] _ = do
  ref <- newIORef []
  return (VList ref)
builtinList (v:_) _ = do
  elems <- toListElems v
  ref   <- newIORef elems
  return (VList ref)

toListElems :: Value -> IO [Value]
toListElems (VList ref)   = readIORef ref
toListElems (VTuple vs)   = return vs
toListElems (VStr s)      = return (map (VStr . (:[])) s)
toListElems (VSet ref)    = readIORef ref
toListElems (VDict ref)   = do
  m <- readIORef ref
  return (map fst (Map.toList m))
toListElems (VIterator ref) = readIORef ref
toListElems v = throwIO (MFTypeError
  { mfMsg = "'" ++ typeNameOf v ++ "' object is not iterable"
  , mfPos = noPos })

builtinTuple :: [Value] -> [(String, Value)] -> IO Value
builtinTuple [] _ = return (VTuple [])
builtinTuple (v:_) _ = fmap VTuple (toListElems v)

builtinDict :: [Value] -> [(String, Value)] -> IO Value
builtinDict [] kwargs = do
  let m = Map.fromList [(VStr k, v) | (k, v) <- kwargs]
  ref <- newIORef m
  return (VDict ref)
builtinDict (v:_) kwargs = case v of
  VList ref -> do
    pairs <- readIORef ref
    kvs   <- mapM toPair pairs
    let m = Map.fromList (kvs ++ [(VStr k, v') | (k, v') <- kwargs])
    mref  <- newIORef m
    return (VDict mref)
  VDict ref -> do
    m   <- readIORef ref
    let m2 = Map.union m (Map.fromList [(VStr k, v') | (k, v') <- kwargs])
    mref <- newIORef m2
    return (VDict mref)
  _ -> throwIO (MFTypeError { mfMsg = "dict() argument must be iterable", mfPos = noPos })

toPair :: Value -> IO (Value, Value)
toPair (VTuple [k, v]) = return (k, v)
toPair (VList ref)     = do
  vs <- readIORef ref
  case vs of
    [k, v] -> return (k, v)
    _      -> throwIO (MFValueError { mfMsg = "dict key-value pairs must have exactly 2 elements", mfPos = noPos })
toPair v = throwIO (MFTypeError { mfMsg = "cannot convert to dict pair: " ++ typeNameOf v, mfPos = noPos })

builtinSet :: [Value] -> [(String, Value)] -> IO Value
builtinSet [] _ = do
  ref <- newIORef []
  return (VSet ref)
builtinSet (v:_) _ = do
  elems <- toListElems v
  let unique = nub elems
  ref <- newIORef unique
  return (VSet ref)

builtinFrozenset :: [Value] -> [(String, Value)] -> IO Value
builtinFrozenset args kwargs = builtinSet args kwargs  -- simplified

builtinBytes :: [Value] -> [(String, Value)] -> IO Value
builtinBytes args _ = return (VStr "<bytes>")

builtinBytearray :: [Value] -> [(String, Value)] -> IO Value
builtinBytearray _ _ = return (VStr "<bytearray>")

builtinMemoryview :: [Value] -> [(String, Value)] -> IO Value
builtinMemoryview _ _ = return (VStr "<memoryview>")

builtinComplex :: [Value] -> [(String, Value)] -> IO Value
builtinComplex args _ = return (VStr "<complex>")

-- =============================================================================
-- NUMERIC FUNCTIONS
-- =============================================================================

builtinAbs :: [Value] -> [(String, Value)] -> IO Value
builtinAbs [VInt n]   _ = return (VInt (abs n))
builtinAbs [VFloat f] _ = return (VFloat (abs f))
builtinAbs [VBool b]  _ = return (VInt (if b then 1 else 0))
builtinAbs [v]        _ = throwIO (MFTypeError
  { mfMsg = "abs() argument must be a number, not '" ++ typeNameOf v ++ "'"
  , mfPos = noPos })
builtinAbs args _ = throwIO (MFTypeError
  { mfMsg = "abs() takes exactly one argument (" ++ show (length args) ++ " given)"
  , mfPos = noPos })

builtinRound :: [Value] -> [(String, Value)] -> IO Value
builtinRound [v] _ = case v of
  VInt n   -> return (VInt n)
  VFloat f -> return (VInt (round f))
  _        -> throwIO (MFTypeError { mfMsg = "round() requires a number", mfPos = noPos })
builtinRound [v, VInt ndigits] _ = case v of
  VInt n   -> return (VInt n)
  VFloat f -> let factor = 10 ^ ndigits :: Int
              in return (VFloat (fromIntegral (round (f * fromIntegral factor)) / fromIntegral factor))
  _        -> throwIO (MFTypeError { mfMsg = "round() requires a number", mfPos = noPos })
builtinRound args _ = throwIO (MFTypeError
  { mfMsg = "round() takes 1 or 2 arguments", mfPos = noPos })

builtinMin :: [Value] -> [(String, Value)] -> IO Value
builtinMin [] _ = throwIO (MFValueError { mfMsg = "min() arg is an empty sequence", mfPos = noPos })
builtinMin [VList ref] kwargs = do
  vs <- readIORef ref
  minByKey vs kwargs
builtinMin args kwargs = minByKey args kwargs

minByKey :: [Value] -> [(String, Value)] -> IO Value
minByKey [] _ = throwIO (MFValueError { mfMsg = "min() arg is an empty sequence", mfPos = noPos })
minByKey vs kwargs = case lookup "key" kwargs of
  Nothing  -> return (minimum vs)
  Just keyFn -> do
    keyed <- mapM (\v -> do k <- callFn keyFn [v]; return (k, v)) vs
    return (snd (minimumBy (\(k1,_) (k2,_) -> compare k1 k2) keyed))

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ [x] = x
minimumBy cmp (x:xs) =
  let m = minimumBy cmp xs
  in if cmp x m == GT then m else x
minimumBy _ [] = error "minimumBy: empty list"

maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ [x] = x
maximumBy cmp (x:xs) =
  let m = maximumBy cmp xs
  in if cmp x m == LT then m else x
maximumBy _ [] = error "maximumBy: empty list"

builtinMax :: [Value] -> [(String, Value)] -> IO Value
builtinMax [] _ = throwIO (MFValueError { mfMsg = "max() arg is an empty sequence", mfPos = noPos })
builtinMax [VList ref] kwargs = do
  vs <- readIORef ref
  maxByKey vs kwargs
builtinMax args kwargs = maxByKey args kwargs

maxByKey :: [Value] -> [(String, Value)] -> IO Value
maxByKey [] _ = throwIO (MFValueError { mfMsg = "max() arg is an empty sequence", mfPos = noPos })
maxByKey vs kwargs = case lookup "key" kwargs of
  Nothing  -> return (maximum vs)
  Just keyFn -> do
    keyed <- mapM (\v -> do k <- callFn keyFn [v]; return (k, v)) vs
    return (snd (maximumBy (\(k1,_) (k2,_) -> compare k1 k2) keyed))

builtinSum :: [Value] -> [(String, Value)] -> IO Value
builtinSum [VList ref] kwargs = do
  vs <- readIORef ref
  let start = case lookup "start" kwargs of
                Just v  -> v
                Nothing -> VInt 0
  foldM addVals start vs
builtinSum (VList ref : rest) kwargs = builtinSum [VList ref] kwargs
builtinSum args _ = throwIO (MFTypeError
  { mfMsg = "sum() requires a list as first argument", mfPos = noPos })

foldM :: (b -> a -> IO b) -> b -> [a] -> IO b
foldM _ acc []     = return acc
foldM f acc (x:xs) = f acc x >>= \acc' -> foldM f acc' xs

addVals :: Value -> Value -> IO Value
addVals (VInt a)   (VInt b)   = return (VInt   (a + b))
addVals (VFloat a) (VFloat b) = return (VFloat (a + b))
addVals (VInt a)   (VFloat b) = return (VFloat (fromIntegral a + b))
addVals (VFloat a) (VInt b)   = return (VFloat (a + fromIntegral b))
addVals (VStr a)   (VStr b)   = return (VStr   (a ++ b))
addVals a b = throwIO (MFTypeError
  { mfMsg = "unsupported operand types for +: '" ++ typeNameOf a ++ "' and '" ++ typeNameOf b ++ "'"
  , mfPos = noPos })

builtinPow :: [Value] -> [(String, Value)] -> IO Value
builtinPow [base, exp_] _ = numericBinOp "pow" (**) base exp_
builtinPow [base, exp_, modV] _ = do
  b <- coerceInt noPos "pow" base
  e <- coerceInt noPos "pow" exp_
  m <- coerceInt noPos "pow" modV
  return (VInt ((b ^ e) `mod` m))
builtinPow _ _ = throwIO (MFTypeError { mfMsg = "pow() takes 2 or 3 arguments", mfPos = noPos })

builtinDivmod :: [Value] -> [(String, Value)] -> IO Value
builtinDivmod [VInt a, VInt b] _ =
  if b == 0
    then throwIO (MFDivisionByZero { mfPos = noPos })
    else return (VTuple [VInt (a `div` b), VInt (a `mod` b)])
builtinDivmod [a, b] _ = do
  fa <- coerceNum noPos "divmod" a
  fb <- coerceNum noPos "divmod" b
  if fb == 0
    then throwIO (MFDivisionByZero { mfPos = noPos })
    else return (VTuple [VFloat (fa / fb), VFloat (fa - (fromIntegral (floor (fa / fb))) * fb)])
builtinDivmod _ _ = throwIO (MFTypeError { mfMsg = "divmod() takes exactly 2 arguments", mfPos = noPos })

builtinBin :: [Value] -> [(String, Value)] -> IO Value
builtinBin [v] _ = do
  n <- coerceInt noPos "bin" v
  return (VStr (if n < 0
    then "-0b" ++ showBin (abs n)
    else "0b"  ++ showBin n))
builtinBin _ _ = throwIO (MFTypeError { mfMsg = "bin() takes exactly 1 argument", mfPos = noPos })

showBin :: Int -> String
showBin 0 = "0"
showBin n = reverse (go n)
  where go 0 = ""
        go x = (if x `mod` 2 == 0 then '0' else '1') : go (x `div` 2)

builtinOct :: [Value] -> [(String, Value)] -> IO Value
builtinOct [v] _ = do
  n <- coerceInt noPos "oct" v
  return (VStr ("0o" ++ showOctStr n))
  where showOctStr x = if x < 0 then "-" ++ show (abs x) else showOct' x
        showOct' 0 = "0"
        showOct' x = reverse (go x)
          where go 0 = ""
                go n = let (q,r) = n `divMod` 8 in (head (show r)) : go q
builtinOct _ _ = throwIO (MFTypeError { mfMsg = "oct() takes exactly 1 argument", mfPos = noPos })

builtinHex :: [Value] -> [(String, Value)] -> IO Value
builtinHex [v] _ = do
  n <- coerceInt noPos "hex" v
  return (VStr (if n < 0
    then "-0x" ++ showHexStr (abs n)
    else "0x"  ++ showHexStr n))
  where showHexStr x = let hex = concatMap showHexDigit (toBase 16 x)
                           showHexDigit d
                             | d < 10    = [toEnum (d + fromEnum '0')]
                             | otherwise = [toEnum (d - 10 + fromEnum 'a')]
                           toBase _ 0 = [0]
                           toBase b n = reverse (go n)
                             where go 0 = []
                                   go x = let (q,r) = x `divMod` b in r : go q
                       in hex
builtinHex _ _ = throwIO (MFTypeError { mfMsg = "hex() takes exactly 1 argument", mfPos = noPos })

-- =============================================================================
-- STRING / CHAR FUNCTIONS
-- =============================================================================

builtinChr :: [Value] -> [(String, Value)] -> IO Value
builtinChr [v] _ = do
  n <- coerceInt noPos "chr" v
  if n < 0 || n > 0x10FFFF
    then throwIO (MFValueError { mfMsg = "chr() arg not in range(0x110000)", mfPos = noPos })
    else return (VStr [toEnum n])
builtinChr _ _ = throwIO (MFTypeError { mfMsg = "chr() takes exactly 1 argument", mfPos = noPos })

builtinOrd :: [Value] -> [(String, Value)] -> IO Value
builtinOrd [VStr [c]] _ = return (VInt (fromEnum c))
builtinOrd [VStr s]   _ = throwIO (MFTypeError
  { mfMsg = "ord() expected a character, but string of length " ++ show (length s) ++ " found"
  , mfPos = noPos })
builtinOrd _ _ = throwIO (MFTypeError { mfMsg = "ord() expected a string of length 1", mfPos = noPos })

builtinFormat :: [Value] -> [(String, Value)] -> IO Value
builtinFormat (VStr fmt : args) kwargs = do
  result <- applyFormat fmt args kwargs
  return (VStr result)
builtinFormat _ _ = throwIO (MFTypeError { mfMsg = "format() requires a format string", mfPos = noPos })

applyFormat :: String -> [Value] -> [(String, Value)] -> IO String
applyFormat [] _ _ = return ""
applyFormat ('{':'}':rest) (a:args) kwargs = do
  s <- displayVal a
  r <- applyFormat rest args kwargs
  return (s ++ r)
applyFormat ('{':cs) args kwargs =
  let (key, rest) = break (== '}') cs
  in case rest of
    ('}':r) -> do
      val <- case reads key of
               [(n, "")] -> return (args !! n)
               _         -> case lookup key kwargs of
                              Just v  -> return v
                              Nothing -> return VNone
      s <- displayVal val
      r' <- applyFormat r args kwargs
      return (s ++ r')
    _ -> fmap ('{' :) (applyFormat cs args kwargs)
applyFormat (c:cs) args kwargs = fmap (c:) (applyFormat cs args kwargs)

builtinRepr :: [Value] -> [(String, Value)] -> IO Value
builtinRepr [v] _ = do
  s <- reprVal v
  return (VStr s)
builtinRepr _ _ = throwIO (MFTypeError { mfMsg = "repr() takes exactly 1 argument", mfPos = noPos })

-- =============================================================================
-- COLLECTION FUNCTIONS
-- =============================================================================

builtinLen :: [Value] -> [(String, Value)] -> IO Value
builtinLen [v] _ = case v of
  VStr s    -> return (VInt (length s))
  VList ref -> fmap (VInt . length) (readIORef ref)
  VTuple vs -> return (VInt (length vs))
  VDict ref -> fmap (VInt . Map.size) (readIORef ref)
  VSet  ref -> fmap (VInt . length) (readIORef ref)
  _         -> throwIO (MFTypeError
    { mfMsg = "object of type '" ++ typeNameOf v ++ "' has no len()"
    , mfPos = noPos })
builtinLen _ _ = throwIO (MFTypeError { mfMsg = "len() takes exactly one argument", mfPos = noPos })

builtinRange :: [Value] -> [(String, Value)] -> IO Value
builtinRange args _ = do
  (start, stop, step) <- case args of
    [VInt n]                   -> return (0, n, 1)
    [VInt a, VInt b]           -> return (a, b, 1)
    [VInt a, VInt b, VInt c]   -> return (a, b, c)
    [v]                        -> do n <- coerceInt noPos "range" v; return (0, n, 1)
    [a, b]                     -> do
      s <- coerceInt noPos "range" a
      e <- coerceInt noPos "range" b
      return (s, e, 1)
    [a, b, c]                  -> do
      s  <- coerceInt noPos "range" a
      e  <- coerceInt noPos "range" b
      st <- coerceInt noPos "range" c
      return (s, e, st)
    _ -> throwIO (MFTypeError { mfMsg = "range() takes 1 to 3 arguments", mfPos = noPos })
  when (step == 0) $ throwIO (MFValueError { mfMsg = "range() step argument must not be zero", mfPos = noPos })
  let nums = if step > 0
               then [start, start+step .. stop-1]
               else [start, start+step .. stop+1]
  ref <- newIORef (map VInt nums)
  return (VList ref)

builtinEnumerate :: [Value] -> [(String, Value)] -> IO Value
builtinEnumerate (v:rest) kwargs = do
  elems <- toListElems v
  let startIdx = case rest of
                   (VInt n:_) -> n
                   _          -> case lookup "start" kwargs of
                                   Just (VInt n) -> n
                                   _             -> 0
  let pairs = zipWith (\i e -> VTuple [VInt i, e]) [startIdx..] elems
  ref <- newIORef pairs
  return (VList ref)
builtinEnumerate _ _ = throwIO (MFTypeError { mfMsg = "enumerate() requires at least 1 argument", mfPos = noPos })

builtinZip :: [Value] -> [(String, Value)] -> IO Value
builtinZip [] _ = do ref <- newIORef []; return (VList ref)
builtinZip lists _ = do
  elems <- mapM toListElems lists
  let zipped = map VTuple (transposeShort elems)
  ref <- newIORef zipped
  return (VList ref)

transposeShort :: [[a]] -> [[a]]
transposeShort [] = []
transposeShort xs = if any null xs
  then []
  else map head xs : transposeShort (map tail xs)

builtinMap :: [Value] -> [(String, Value)] -> IO Value
builtinMap (fn : iterables) _ = do
  elems <- mapM toListElems iterables
  let zipped = transposeShort elems
  results <- mapM (\args -> callFn fn args) zipped
  ref <- newIORef results
  return (VList ref)
builtinMap _ _ = throwIO (MFTypeError { mfMsg = "map() requires at least 2 arguments", mfPos = noPos })

builtinFilter :: [Value] -> [(String, Value)] -> IO Value
builtinFilter [fn, v] _ = do
  elems <- toListElems v
  kept  <- filterM (\x -> fmap isTruthy (callFn fn [x])) elems
  ref   <- newIORef kept
  return (VList ref)
builtinFilter _ _ = throwIO (MFTypeError { mfMsg = "filter() requires exactly 2 arguments", mfPos = noPos })

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ []     = return []
filterM p (x:xs) = do
  b  <- p x
  ys <- filterM p xs
  return (if b then x:ys else ys)

builtinSorted :: [Value] -> [(String, Value)] -> IO Value
builtinSorted (v:_) kwargs = do
  elems <- toListElems v
  let rev = case lookup "reverse" kwargs of
              Just (VBool b) -> b
              _              -> False
  sorted <- case lookup "key" kwargs of
    Nothing  -> return (if rev then reverse (sort elems) else sort elems)
    Just keyFn -> do
      keyed  <- mapM (\e -> do k <- callFn keyFn [e]; return (k, e)) elems
      let sorted' = sortBy (\(k1,_) (k2,_) -> compare k1 k2) keyed
      return (map snd (if rev then reverse sorted' else sorted'))
  ref <- newIORef sorted
  return (VList ref)
builtinSorted _ _ = throwIO (MFTypeError { mfMsg = "sorted() requires at least 1 argument", mfPos = noPos })

builtinReversed :: [Value] -> [(String, Value)] -> IO Value
builtinReversed [v] _ = do
  elems <- toListElems v
  ref   <- newIORef (reverse elems)
  return (VList ref)
builtinReversed _ _ = throwIO (MFTypeError { mfMsg = "reversed() takes exactly 1 argument", mfPos = noPos })

builtinAny :: [Value] -> [(String, Value)] -> IO Value
builtinAny [v] _ = do
  elems <- toListElems v
  let result = any isTruthy elems
  return (VBool result)
builtinAny [fn, v] _ = do
  elems  <- toListElems v
  result <- anyM (\x -> fmap isTruthy (callFn fn [x])) elems
  return (VBool result)
builtinAny _ _ = throwIO (MFTypeError { mfMsg = "any() takes 1 or 2 arguments", mfPos = noPos })

anyM :: Monad m => (a -> m Bool) -> [a] -> m Bool
anyM _ []     = return False
anyM p (x:xs) = do
  b <- p x
  if b then return True else anyM p xs

builtinAll :: [Value] -> [(String, Value)] -> IO Value
builtinAll [v] _ = do
  elems <- toListElems v
  return (VBool (all isTruthy elems))
builtinAll [fn, v] _ = do
  elems  <- toListElems v
  result <- allM (\x -> fmap isTruthy (callFn fn [x])) elems
  return (VBool result)
builtinAll _ _ = throwIO (MFTypeError { mfMsg = "all() takes 1 or 2 arguments", mfPos = noPos })

allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
allM _ []     = return True
allM p (x:xs) = do
  b <- p x
  if b then allM p xs else return False

builtinNext :: [Value] -> [(String, Value)] -> IO Value
builtinNext [VIterator ref] _ = do
  vs <- readIORef ref
  case vs of
    []     -> throwIO MFStopIteration
    (x:xs) -> writeIORef ref xs >> return x
builtinNext [VIterator ref, def] _ = do
  vs <- readIORef ref
  case vs of
    []     -> return def
    (x:xs) -> writeIORef ref xs >> return x
builtinNext [v] _ = throwIO (MFTypeError
  { mfMsg = "'" ++ typeNameOf v ++ "' object is not an iterator"
  , mfPos = noPos })
builtinNext _ _ = throwIO (MFTypeError { mfMsg = "next() takes 1 or 2 arguments", mfPos = noPos })

builtinIter :: [Value] -> [(String, Value)] -> IO Value
builtinIter [v] _ = do
  elems <- toListElems v
  ref   <- newIORef elems
  return (VIterator ref)
builtinIter _ _ = throwIO (MFTypeError { mfMsg = "iter() takes exactly 1 argument", mfPos = noPos })

builtinSlice :: [Value] -> [(String, Value)] -> IO Value
builtinSlice args _ = return (VStr ("<slice " ++ show (length args) ++ ">"))

-- =============================================================================
-- INSPECTION FUNCTIONS
-- =============================================================================

builtinType :: [Value] -> [(String, Value)] -> IO Value
builtinType [v] _ = return (VType (typeNameOf v))
builtinType _ _   = throwIO (MFTypeError { mfMsg = "type() takes exactly 1 argument", mfPos = noPos })

builtinIsinstance :: [Value] -> [(String, Value)] -> IO Value
builtinIsinstance [v, VType t] _ = return (VBool (typeNameOf v == t))
builtinIsinstance [v, VTuple ts] _ =
  return (VBool (any (\t -> case t of VType tn -> typeNameOf v == tn; _ -> False) ts))
builtinIsinstance _ _ = return (VBool False)

builtinIssubclass :: [Value] -> [(String, Value)] -> IO Value
builtinIssubclass [VType t1, VType t2] _ = return (VBool (t1 == t2))
builtinIssubclass _ _ = return (VBool False)

builtinCallable :: [Value] -> [(String, Value)] -> IO Value
builtinCallable [v] _ = return (VBool (isCallable v))
  where isCallable (VClosure{}) = True
        isCallable (VBuiltin{}) = True
        isCallable _            = False
builtinCallable _ _ = return (VBool False)

builtinHasattr :: [Value] -> [(String, Value)] -> IO Value
builtinHasattr [VRecord _ ref, VStr fname] _ = do
  fields <- readIORef ref
  return (VBool (Map.member fname fields))
builtinHasattr [VModule _ exports, VStr name] _ =
  return (VBool (Map.member name exports))
builtinHasattr _ _ = return (VBool False)

builtinGetattr :: [Value] -> [(String, Value)] -> IO Value
builtinGetattr [VRecord _ ref, VStr fname] _ = do
  fields <- readIORef ref
  case Map.lookup fname fields of
    Just v  -> return v
    Nothing -> throwIO (MFKeyError { mfMsg = "no attribute '" ++ fname ++ "'", mfPos = noPos })
builtinGetattr [VRecord _ ref, VStr fname, defVal] _ = do
  fields <- readIORef ref
  return (Map.findWithDefault defVal fname fields)
builtinGetattr _ _ = throwIO (MFTypeError { mfMsg = "getattr() takes 2 or 3 arguments", mfPos = noPos })

builtinSetattr :: [Value] -> [(String, Value)] -> IO Value
builtinSetattr [VRecord _ ref, VStr fname, v] _ = do
  modifyIORef ref (Map.insert fname v)
  return VNone
builtinSetattr _ _ = throwIO (MFTypeError { mfMsg = "setattr() takes 3 arguments", mfPos = noPos })

builtinDelattr :: [Value] -> [(String, Value)] -> IO Value
builtinDelattr [VRecord _ ref, VStr fname] _ = do
  modifyIORef ref (Map.delete fname)
  return VNone
builtinDelattr _ _ = return VNone

builtinDir :: [Value] -> [(String, Value)] -> IO Value
builtinDir [VRecord _ ref] _ = do
  fields <- readIORef ref
  let names = map (VStr . fst) (Map.toAscList fields)
  nameRef <- newIORef names
  return (VList nameRef)
builtinDir [VModule _ exports] _ = do
  let names = map VStr (Map.keys exports)
  nameRef <- newIORef names
  return (VList nameRef)
builtinDir _ _ = do
  ref <- newIORef []
  return (VList ref)

builtinVars :: [Value] -> [(String, Value)] -> IO Value
builtinVars [VRecord _ ref] _ = do
  fields <- readIORef ref
  dictRef <- newIORef (Map.mapKeys VStr fields)
  return (VDict dictRef)
builtinVars _ _ = do
  ref <- newIORef Map.empty
  return (VDict ref)

builtinId :: [Value] -> [(String, Value)] -> IO Value
builtinId [_] _ = return (VInt 0)  -- simplified; real IDs need stable pointers
builtinId _ _ = throwIO (MFTypeError { mfMsg = "id() takes exactly 1 argument", mfPos = noPos })

builtinHash :: [Value] -> [(String, Value)] -> IO Value
builtinHash [v] _ = case v of
  VInt n   -> return (VInt n)
  VFloat f -> return (VInt (round f))
  VStr s   -> return (VInt (hashStr s))
  VBool b  -> return (VInt (if b then 1 else 0))
  VNone    -> return (VInt 0)
  _        -> throwIO (MFTypeError
    { mfMsg = "unhashable type: '" ++ typeNameOf v ++ "'"
    , mfPos = noPos })
builtinHash _ _ = throwIO (MFTypeError { mfMsg = "hash() takes exactly 1 argument", mfPos = noPos })

hashStr :: String -> Int
hashStr s = foldl (\h c -> h * 31 + fromEnum c) 5381 s

-- =============================================================================
-- FUNCTIONAL
-- =============================================================================

builtinReduce :: [Value] -> [(String, Value)] -> IO Value
builtinReduce [fn, v] _ = do
  elems <- toListElems v
  case elems of
    []     -> throwIO (MFTypeError { mfMsg = "reduce() of empty sequence with no initial value", mfPos = noPos })
    [x]    -> return x
    (x:xs) -> foldM (\acc e -> callFn fn [acc, e]) x xs
builtinReduce [fn, v, initial] _ = do
  elems <- toListElems v
  foldM (\acc e -> callFn fn [acc, e]) initial elems
builtinReduce _ _ = throwIO (MFTypeError { mfMsg = "reduce() takes 2 or 3 arguments", mfPos = noPos })

builtinZipLongest :: [Value] -> [(String, Value)] -> IO Value
builtinZipLongest lists kwargs = do
  let fillval = case lookup "fillvalue" kwargs of
                  Just v  -> v
                  Nothing -> VNone
  elems <- mapM toListElems lists
  let maxLen = maximum (map length elems)
      padded = map (\xs -> xs ++ replicate (maxLen - length xs) fillval) elems
      zipped = map VTuple (transposeExact padded)
  ref <- newIORef zipped
  return (VList ref)

transposeExact :: [[a]] -> [[a]]
transposeExact xs = if null (head xs) then [] else map head xs : transposeExact (map tail xs)

builtinPrintR :: [Value] -> [(String, Value)] -> IO Value
builtinPrintR args _ = do
  strs <- mapM reprVal args
  putStrLn (unwords strs)
  return VNone

-- =============================================================================
-- EXIT
-- =============================================================================

builtinExit :: [Value] -> [(String, Value)] -> IO Value
builtinExit []      _ = exitWith ExitSuccess
builtinExit [VInt 0] _ = exitWith ExitSuccess
builtinExit [VInt n] _ = exitWith (ExitFailure n)
builtinExit _       _ = exitWith ExitSuccess

-- =============================================================================
-- EVAL (limited)
-- =============================================================================

builtinEval :: [Value] -> [(String, Value)] -> IO Value
builtinEval [VStr _] _ = return VNone  -- eval is a no-op for safety
builtinEval _ _ = throwIO (MFTypeError { mfMsg = "eval() requires a string", mfPos = noPos })

-- =============================================================================
-- PRODUCT
-- =============================================================================

builtinProduct :: [Value] -> [(String, Value)] -> IO Value
builtinProduct [VList ref] kwargs = do
  vs <- readIORef ref
  let start = case lookup "start" kwargs of
                Just v  -> v
                Nothing -> VInt 1
  foldM mulVals start vs
builtinProduct args _ = throwIO (MFTypeError
  { mfMsg = "product() requires a list", mfPos = noPos })

mulVals :: Value -> Value -> IO Value
mulVals (VInt a)   (VInt b)   = return (VInt   (a * b))
mulVals (VFloat a) (VFloat b) = return (VFloat (a * b))
mulVals (VInt a)   (VFloat b) = return (VFloat (fromIntegral a * b))
mulVals (VFloat a) (VInt b)   = return (VFloat (a * fromIntegral b))
mulVals a b = throwIO (MFTypeError
  { mfMsg = "unsupported operand types for *: '" ++ typeNameOf a ++ "' and '" ++ typeNameOf b ++ "'"
  , mfPos = noPos })

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

stripSuffix :: String -> String -> Maybe String
stripSuffix suffix str =
  if suffix `isSuffixOf` str
    then Just (take (length str - length suffix) str)
    else Nothing

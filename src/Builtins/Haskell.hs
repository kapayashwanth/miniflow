-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Builtins/Haskell.hs  -- All Haskell-style built-in functions
-- =============================================================================
module Builtins.Haskell
  ( haskellBuiltinList
  ) where

import Types
import Builtins.Core
import Environment
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List
  ( isPrefixOf, isSuffixOf, isInfixOf
  , nub, nubBy, sort, sortBy, sortOn
  , groupBy, group, tails, inits
  , permutations, subsequences
  , transpose, intercalate, intersperse
  , partition, find, findIndex, elemIndex
  , stripPrefix, maximumBy, minimumBy
  )
import Data.Char (toLower, toUpper, isAlpha, isDigit, isSpace, isUpper, isLower, ord, chr)
import Control.Exception (throwIO, catch, SomeException)
import Data.Maybe (fromMaybe, mapMaybe, catMaybes, isJust, isNothing, listToMaybe)
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- Reuse the apply callback from Python.hs
applyCallback :: IORef (Value -> [Value] -> IO Value)
applyCallback = unsafePerformIO $ newIORef defaultApply
{-# NOINLINE applyCallback #-}

defaultApply :: Value -> [Value] -> IO Value
defaultApply (VBuiltin _ f) args = f args []
defaultApply v _ = throwIO (MFTypeError { mfMsg = "not callable: " ++ typeNameOf v, mfPos = noPos })

callFn :: Value -> [Value] -> IO Value
callFn fn args = do
  cb <- readIORef applyCallback
  cb fn args

-- =============================================================================
-- HASKELL BUILTIN TABLE
-- =============================================================================

haskellBuiltinList :: [(String, [Value] -> [(String, Value)] -> IO Value)]
haskellBuiltinList =
  [ -- List manipulation (Prelude)
    ("head",        builtinHead)
  , ("tail",        builtinTail)
  , ("init",        builtinInit)
  , ("last",        builtinLast)
  , ("null",        builtinNull)
  , ("length",      builtinLength)
  , ("reverse",     builtinReverse)
  , ("concat",      builtinConcat)
  , ("concatMap",   builtinConcatMap)
  , ("take",        builtinTake)
  , ("drop",        builtinDrop)
  , ("takeWhile",   builtinTakeWhile)
  , ("dropWhile",   builtinDropWhile)
  , ("span",        builtinSpan)
  , ("break_",      builtinBreakH)     -- renamed to avoid keyword clash
  , ("splitAt",     builtinSplitAt)
  , ("elem",        builtinElem)
  , ("notElem",     builtinNotElem)
  , ("lookup",      builtinLookup)
  , ("maximum",     builtinMaximum)
  , ("minimum",     builtinMinimum)
  , ("sum",         builtinSumH)
  , ("product",     builtinProductH)
  -- Higher-order functions
  , ("foldr",       builtinFoldr)
  , ("foldl",       builtinFoldl)
  , ("foldl1",      builtinFoldl1)
  , ("foldr1",      builtinFoldr1)
  , ("scanl",       builtinScanl)
  , ("scanr",       builtinScanr)
  , ("scanl1",      builtinScanl1)
  , ("scanr1",      builtinScanr1)
  , ("mapM_",       builtinMapM_)
  , ("forM_",       builtinForM_)
  , ("mapM",        builtinMapM)
  , ("forM",        builtinForM)
  , ("sequence",    builtinSequence)
  , ("sequence_",   builtinSequence_)
  -- Zip functions
  , ("zip",         builtinZip2)
  , ("zip3",        builtinZip3)
  , ("unzip",       builtinUnzip)
  , ("unzip3",      builtinUnzip3)
  , ("zipWith",     builtinZipWith)
  , ("zipWith3",    builtinZipWith3)
  -- Predicates
  , ("any",         builtinAnyH)
  , ("all",         builtinAllH)
  -- Number utilities
  , ("odd",         builtinOdd)
  , ("even",        builtinEven)
  , ("gcd",         builtinGcd)
  , ("lcm",         builtinLcm)
  , ("abs",         builtinAbsH)
  , ("signum",      builtinSignum)
  , ("negate",      builtinNegate)
  , ("floor",       builtinFloor)
  , ("ceiling",     builtinCeiling)
  , ("truncate",    builtinTruncate)
  , ("round",       builtinRoundH)
  , ("fromIntegral",builtinFromIntegral)
  , ("toInteger",   builtinToInteger)
  , ("fromInteger", builtinFromInteger)
  -- Infinite lists
  , ("iterate",     builtinIterate)
  , ("repeat",      builtinRepeat)
  , ("replicate",   builtinReplicate)
  , ("cycle",       builtinCycle)
  -- String functions
  , ("words",       builtinWords)
  , ("unwords",     builtinUnwords)
  , ("lines",       builtinLines)
  , ("unlines",     builtinUnlines)
  , ("show",        builtinShow)
  , ("read",        builtinRead)
  -- Data.List functions
  , ("nub",         builtinNub)
  , ("sort",        builtinSort)
  , ("sortBy",      builtinSortBy)
  , ("sortOn",      builtinSortOn)
  , ("groupBy",     builtinGroupBy)
  , ("group",       builtinGroup)
  , ("tails",       builtinTails)
  , ("inits",       builtinInits)
  , ("permutations",builtinPermutations)
  , ("subsequences",builtinSubsequences)
  , ("transpose",   builtinTranspose)
  , ("intercalate", builtinIntercalate)
  , ("intersperse", builtinIntersperse)
  , ("isPrefixOf",  builtinIsPrefixOf)
  , ("isSuffixOf",  builtinIsSuffixOf)
  , ("isInfixOf",   builtinIsInfixOf)
  , ("stripPrefix", builtinStripPrefix)
  , ("nubBy",       builtinNubBy)
  , ("partition",   builtinPartition)
  , ("find",        builtinFind)
  , ("findIndex",   builtinFindIndex)
  , ("elemIndex",   builtinElemIndex)
  -- Function combinators
  , ("id",          builtinIdH)
  , ("const",       builtinConst)
  , ("flip",        builtinFlip)
  , ("curry",       builtinCurry)
  , ("uncurry",     builtinUncurry)
  , ("until",       builtinUntil)
  , ("fix",         builtinFix)
  , ("on",          builtinOn)
  , ("comparing",   builtinComparing)
  -- Maybe/Either-style
  , ("fromMaybe",   builtinFromMaybe)
  , ("maybe",       builtinMaybe)
  , ("catMaybes",   builtinCatMaybes)
  , ("mapMaybe",    builtinMapMaybe)
  , ("listToMaybe", builtinListToMaybe)
  , ("maybeToList", builtinMaybeToList)
  -- Math
  , ("sqrt",        builtinSqrt)
  , ("log",         builtinLog)
  , ("exp",         builtinExp)
  , ("sin",         builtinSin)
  , ("cos",         builtinCos)
  , ("tan",         builtinTan)
  , ("asin",        builtinAsin)
  , ("acos",        builtinAcos)
  , ("atan",        builtinAtan)
  , ("atan2",       builtinAtan2)
  , ("sinh",        builtinSinh)
  , ("cosh",        builtinCosh)
  , ("tanh",        builtinTanh)
  , ("logBase",     builtinLogBase)
  -- Char/String
  , ("isAlpha",     builtinIsAlpha)
  , ("isDigit",     builtinIsDigit)
  , ("isSpace",     builtinIsSpace)
  , ("isUpper",     builtinIsUpper)
  , ("isLower",     builtinIsLower)
  , ("toUpper",     builtinToUpper)
  , ("toLower",     builtinToLower)
  , ("digitToInt",  builtinDigitToInt)
  , ("intToDigit",  builtinIntToDigit)
  -- Bool
  , ("not",         builtinNot)
  , ("and",         builtinAndH)
  , ("or",          builtinOrH)
  -- IO
  , ("putStr",      builtinPutStr)
  , ("putStrLn",    builtinPutStrLn)
  , ("getLine",     builtinGetLine)
  , ("readLn",      builtinReadLn)
  , ("writeFile",   builtinWriteFile)
  , ("readFile",    builtinReadFile)
  , ("appendFile",  builtinAppendFile)
  ]

-- =============================================================================
-- LIST MANIPULATION
-- =============================================================================

builtinHead :: [Value] -> [(String, Value)] -> IO Value
builtinHead [v] _ = do
  elems <- toList v
  case elems of
    []    -> throwIO (MFRuntimeError { mfMsg = "head: empty list", mfPos = noPos })
    (x:_) -> return x
builtinHead _ _ = err "head" 1

builtinTail :: [Value] -> [(String, Value)] -> IO Value
builtinTail [v] _ = do
  elems <- toList v
  case elems of
    []     -> throwIO (MFRuntimeError { mfMsg = "tail: empty list", mfPos = noPos })
    (_:xs) -> mkList xs
builtinTail _ _ = err "tail" 1

builtinInit :: [Value] -> [(String, Value)] -> IO Value
builtinInit [v] _ = do
  elems <- toList v
  case elems of
    [] -> throwIO (MFRuntimeError { mfMsg = "init: empty list", mfPos = noPos })
    _  -> mkList (init elems)
builtinInit _ _ = err "init" 1

builtinLast :: [Value] -> [(String, Value)] -> IO Value
builtinLast [v] _ = do
  elems <- toList v
  case elems of
    [] -> throwIO (MFRuntimeError { mfMsg = "last: empty list", mfPos = noPos })
    _  -> return (last elems)
builtinLast _ _ = err "last" 1

builtinNull :: [Value] -> [(String, Value)] -> IO Value
builtinNull [v] _ = case v of
  VList ref -> fmap (VBool . null) (readIORef ref)
  VTuple vs -> return (VBool (null vs))
  VStr s    -> return (VBool (null s))
  VDict ref -> fmap (VBool . Map.null) (readIORef ref)
  VSet ref  -> fmap (VBool . null) (readIORef ref)
  _         -> return (VBool False)
builtinNull _ _ = err "null" 1

builtinLength :: [Value] -> [(String, Value)] -> IO Value
builtinLength [v] _ = case v of
  VList ref -> fmap (VInt . length) (readIORef ref)
  VTuple vs -> return (VInt (length vs))
  VStr s    -> return (VInt (length s))
  VDict ref -> fmap (VInt . Map.size) (readIORef ref)
  VSet ref  -> fmap (VInt . length) (readIORef ref)
  _         -> return (VInt 0)
builtinLength _ _ = err "length" 1

builtinReverse :: [Value] -> [(String, Value)] -> IO Value
builtinReverse [v] _ = case v of
  VStr s    -> return (VStr (reverse s))
  _         -> toList v >>= mkList . reverse
builtinReverse _ _ = err "reverse" 1

builtinConcat :: [Value] -> [(String, Value)] -> IO Value
builtinConcat [v] _ = do
  outer <- toList v
  inner <- mapM toList outer
  mkList (concat inner)
builtinConcat _ _ = err "concat" 1

builtinConcatMap :: [Value] -> [(String, Value)] -> IO Value
builtinConcatMap [fn, v] _ = do
  elems   <- toList v
  results <- mapM (\e -> callFn fn [e] >>= toList) elems
  mkList (concat results)
builtinConcatMap _ _ = err "concatMap" 2

builtinTake :: [Value] -> [(String, Value)] -> IO Value
builtinTake [VInt n, v] _ = toList v >>= mkList . take n
builtinTake [nv, v] _ = do
  n <- coerceInt noPos "take" nv
  toList v >>= mkList . take n
builtinTake _ _ = err "take" 2

builtinDrop :: [Value] -> [(String, Value)] -> IO Value
builtinDrop [VInt n, v] _ = toList v >>= mkList . drop n
builtinDrop [nv, v] _ = do
  n <- coerceInt noPos "drop" nv
  toList v >>= mkList . drop n
builtinDrop _ _ = err "drop" 2

builtinTakeWhile :: [Value] -> [(String, Value)] -> IO Value
builtinTakeWhile [fn, v] _ = do
  elems  <- toList v
  result <- takeWhileM (\x -> fmap isTruthy (callFn fn [x])) elems
  mkList result
builtinTakeWhile _ _ = err "takeWhile" 2

takeWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
takeWhileM _ []     = return []
takeWhileM p (x:xs) = do
  b <- p x
  if b then fmap (x:) (takeWhileM p xs) else return []

builtinDropWhile :: [Value] -> [(String, Value)] -> IO Value
builtinDropWhile [fn, v] _ = do
  elems  <- toList v
  result <- dropWhileM (\x -> fmap isTruthy (callFn fn [x])) elems
  mkList result
builtinDropWhile _ _ = err "dropWhile" 2

dropWhileM :: Monad m => (a -> m Bool) -> [a] -> m [a]
dropWhileM _ []     = return []
dropWhileM p (x:xs) = do
  b <- p x
  if b then dropWhileM p xs else return (x:xs)

builtinSpan :: [Value] -> [(String, Value)] -> IO Value
builtinSpan [fn, v] _ = do
  elems  <- toList v
  (pre, suf) <- spanM (\x -> fmap isTruthy (callFn fn [x])) elems
  preRef <- newIORef pre
  sufRef <- newIORef suf
  return (VTuple [VList preRef, VList sufRef])
builtinSpan _ _ = err "span" 2

spanM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
spanM _ []     = return ([], [])
spanM p (x:xs) = do
  b <- p x
  if b
    then do { (pre, suf) <- spanM p xs; return (x:pre, suf) }
    else return ([], x:xs)

builtinBreakH :: [Value] -> [(String, Value)] -> IO Value
builtinBreakH [fn, v] _ = do
  elems      <- toList v
  (pre, suf) <- spanM (\x -> fmap (not . isTruthy) (callFn fn [x])) elems
  preRef     <- newIORef pre
  sufRef     <- newIORef suf
  return (VTuple [VList preRef, VList sufRef])
builtinBreakH _ _ = err "break" 2

builtinSplitAt :: [Value] -> [(String, Value)] -> IO Value
builtinSplitAt [VInt n, v] _ = do
  elems <- toList v
  let (pre, suf) = splitAt n elems
  preRef <- newIORef pre
  sufRef <- newIORef suf
  return (VTuple [VList preRef, VList sufRef])
builtinSplitAt _ _ = err "splitAt" 2

builtinElem :: [Value] -> [(String, Value)] -> IO Value
builtinElem [x, v] _ = do
  elems <- toList v
  return (VBool (x `elem` elems))
builtinElem _ _ = err "elem" 2

builtinNotElem :: [Value] -> [(String, Value)] -> IO Value
builtinNotElem [x, v] _ = do
  elems <- toList v
  return (VBool (x `notElem` elems))
builtinNotElem _ _ = err "notElem" 2

builtinLookup :: [Value] -> [(String, Value)] -> IO Value
builtinLookup [key, v] _ = do
  elems <- toList v
  let result = go elems
  case result of
    Nothing -> return VNone
    Just x  -> return x
  where
    go [] = Nothing
    go (VTuple [k, val]:rest) = if k == key then Just val else go rest
    go (_:rest) = go rest
builtinLookup _ _ = err "lookup" 2

builtinMaximum :: [Value] -> [(String, Value)] -> IO Value
builtinMaximum [v] _ = do
  elems <- toList v
  case elems of
    [] -> throwIO (MFValueError { mfMsg = "maximum: empty list", mfPos = noPos })
    _  -> return (maximum elems)
builtinMaximum _ _ = err "maximum" 1

builtinMinimum :: [Value] -> [(String, Value)] -> IO Value
builtinMinimum [v] _ = do
  elems <- toList v
  case elems of
    [] -> throwIO (MFValueError { mfMsg = "minimum: empty list", mfPos = noPos })
    _  -> return (minimum elems)
builtinMinimum _ _ = err "minimum" 1

builtinSumH :: [Value] -> [(String, Value)] -> IO Value
builtinSumH [v] _ = do
  elems <- toList v
  foldM addVals (VInt 0) elems
  where
    addVals (VInt a)   (VInt b)   = return (VInt   (a+b))
    addVals (VFloat a) (VFloat b) = return (VFloat (a+b))
    addVals (VInt a)   (VFloat b) = return (VFloat (fromIntegral a + b))
    addVals (VFloat a) (VInt b)   = return (VFloat (a + fromIntegral b))
    addVals a b = throwIO (MFTypeError { mfMsg = "sum: cannot add " ++ typeNameOf a ++ " and " ++ typeNameOf b, mfPos = noPos })
builtinSumH _ _ = err "sum" 1

builtinProductH :: [Value] -> [(String, Value)] -> IO Value
builtinProductH [v] _ = do
  elems <- toList v
  foldM mulVals (VInt 1) elems
  where
    mulVals (VInt a)   (VInt b)   = return (VInt   (a*b))
    mulVals (VFloat a) (VFloat b) = return (VFloat (a*b))
    mulVals (VInt a)   (VFloat b) = return (VFloat (fromIntegral a * b))
    mulVals (VFloat a) (VInt b)   = return (VFloat (a * fromIntegral b))
    mulVals a b = throwIO (MFTypeError { mfMsg = "product: cannot multiply " ++ typeNameOf a, mfPos = noPos })
builtinProductH _ _ = err "product" 1

-- =============================================================================
-- HIGHER-ORDER FUNCTIONS
-- =============================================================================

builtinFoldr :: [Value] -> [(String, Value)] -> IO Value
builtinFoldr [fn, acc, v] _ = do
  elems <- toList v
  foldrM (\x a -> callFn fn [x, a]) acc elems
  where
    foldrM _ a []     = return a
    foldrM f a (x:xs) = do rest <- foldrM f a xs; f x rest
builtinFoldr _ _ = err "foldr" 3

builtinFoldl :: [Value] -> [(String, Value)] -> IO Value
builtinFoldl [fn, acc, v] _ = do
  elems <- toList v
  foldM (\a x -> callFn fn [a, x]) acc elems
builtinFoldl _ _ = err "foldl" 3

builtinFoldl1 :: [Value] -> [(String, Value)] -> IO Value
builtinFoldl1 [fn, v] _ = do
  elems <- toList v
  case elems of
    []     -> throwIO (MFValueError { mfMsg = "foldl1: empty list", mfPos = noPos })
    (x:xs) -> foldM (\a e -> callFn fn [a, e]) x xs
builtinFoldl1 _ _ = err "foldl1" 2

builtinFoldr1 :: [Value] -> [(String, Value)] -> IO Value
builtinFoldr1 [fn, v] _ = do
  elems <- toList v
  case elems of
    [] -> throwIO (MFValueError { mfMsg = "foldr1: empty list", mfPos = noPos })
    _  -> let go [x]    = return x
              go (x:xs) = do rest <- go xs; callFn fn [x, rest]
              go []     = error "impossible"
          in go elems
builtinFoldr1 _ _ = err "foldr1" 2

builtinScanl :: [Value] -> [(String, Value)] -> IO Value
builtinScanl [fn, acc, v] _ = do
  elems  <- toList v
  result <- scanlM (\a x -> callFn fn [a, x]) acc elems
  mkList result
  where
    scanlM _ a []     = return [a]
    scanlM f a (x:xs) = do
      a' <- f a x
      rest <- scanlM f a' xs
      return (a : rest)
builtinScanl _ _ = err "scanl" 3

builtinScanr :: [Value] -> [(String, Value)] -> IO Value
builtinScanr [fn, acc, v] _ = do
  elems  <- toList v
  result <- scanrM (\x a -> callFn fn [x, a]) acc elems
  mkList result
  where
    scanrM _ a []     = return [a]
    scanrM f a (x:xs) = do
      rest <- scanrM f a xs
      let a' = head rest
      r <- f x a'
      return (r : rest)
builtinScanr _ _ = err "scanr" 3

builtinScanl1 :: [Value] -> [(String, Value)] -> IO Value
builtinScanl1 [fn, v] _ = do
  elems <- toList v
  case elems of
    []     -> mkList []
    (x:xs) -> do
      let go a []     = return [a]
          go a (e:es) = do a' <- callFn fn [a, e]; rest <- go a' es; return (a:rest)
      fmap (x:) (mapM id []) >>= \_ -> do
        result <- go x xs
        mkList result
builtinScanl1 _ _ = err "scanl1" 2

builtinScanr1 :: [Value] -> [(String, Value)] -> IO Value
builtinScanr1 [fn, v] _ = do
  elems <- toList v
  case elems of
    [] -> mkList []
    _  -> do
      let go [x]    = return [x]
          go (x:xs) = do
            rest <- go xs
            r    <- callFn fn [x, head rest]
            return (r : rest)
          go [] = return []
      result <- go elems
      mkList result
builtinScanr1 _ _ = err "scanr1" 2

builtinMapM_ :: [Value] -> [(String, Value)] -> IO Value
builtinMapM_ [fn, v] _ = do
  elems <- toList v
  mapM_ (\x -> callFn fn [x]) elems
  return VNone
builtinMapM_ _ _ = err "mapM_" 2

builtinForM_ :: [Value] -> [(String, Value)] -> IO Value
builtinForM_ [v, fn] _ = do
  elems <- toList v
  mapM_ (\x -> callFn fn [x]) elems
  return VNone
builtinForM_ _ _ = err "forM_" 2

builtinMapM :: [Value] -> [(String, Value)] -> IO Value
builtinMapM [fn, v] _ = do
  elems   <- toList v
  results <- mapM (\x -> callFn fn [x]) elems
  mkList results
builtinMapM _ _ = err "mapM" 2

builtinForM :: [Value] -> [(String, Value)] -> IO Value
builtinForM [v, fn] _ = do
  elems   <- toList v
  results <- mapM (\x -> callFn fn [x]) elems
  mkList results
builtinForM _ _ = err "forM" 2

builtinSequence :: [Value] -> [(String, Value)] -> IO Value
builtinSequence [v] _ = do
  elems   <- toList v
  results <- sequence (map return elems)
  mkList results
builtinSequence _ _ = err "sequence" 1

builtinSequence_ :: [Value] -> [(String, Value)] -> IO Value
builtinSequence_ _ _ = return VNone

-- =============================================================================
-- ZIP FUNCTIONS
-- =============================================================================

builtinZip2 :: [Value] -> [(String, Value)] -> IO Value
builtinZip2 [v1, v2] _ = do
  l1 <- toList v1
  l2 <- toList v2
  let pairs = zipWith (\a b -> VTuple [a, b]) l1 l2
  mkList pairs
builtinZip2 _ _ = err "zip" 2

builtinZip3 :: [Value] -> [(String, Value)] -> IO Value
builtinZip3 [v1, v2, v3] _ = do
  l1 <- toList v1
  l2 <- toList v2
  l3 <- toList v3
  let triples = zipWith3 (\a b c -> VTuple [a, b, c]) l1 l2 l3
  mkList triples
builtinZip3 _ _ = err "zip3" 3

builtinUnzip :: [Value] -> [(String, Value)] -> IO Value
builtinUnzip [v] _ = do
  pairs <- toList v
  let as = [a | VTuple (a:_) <- pairs]
      bs = [b | VTuple (_:b:_) <- pairs]
  aRef <- newIORef as
  bRef <- newIORef bs
  return (VTuple [VList aRef, VList bRef])
builtinUnzip _ _ = err "unzip" 1

builtinUnzip3 :: [Value] -> [(String, Value)] -> IO Value
builtinUnzip3 [v] _ = do
  triples <- toList v
  let as = [a | VTuple (a:_)     <- triples]
      bs = [b | VTuple (_:b:_)   <- triples]
      cs = [c | VTuple (_:_:c:_) <- triples]
  aRef <- newIORef as
  bRef <- newIORef bs
  cRef <- newIORef cs
  return (VTuple [VList aRef, VList bRef, VList cRef])
builtinUnzip3 _ _ = err "unzip3" 1

builtinZipWith :: [Value] -> [(String, Value)] -> IO Value
builtinZipWith [fn, v1, v2] _ = do
  l1 <- toList v1
  l2 <- toList v2
  results <- mapM (\(a, b) -> callFn fn [a, b]) (zip l1 l2)
  mkList results
builtinZipWith _ _ = err "zipWith" 3

builtinZipWith3 :: [Value] -> [(String, Value)] -> IO Value
builtinZipWith3 [fn, v1, v2, v3] _ = do
  l1 <- toList v1
  l2 <- toList v2
  l3 <- toList v3
  results <- mapM (\(a, b, c) -> callFn fn [a, b, c]) (zip3 l1 l2 l3)
  mkList results
builtinZipWith3 _ _ = err "zipWith3" 4

-- =============================================================================
-- PREDICATES
-- =============================================================================

builtinAnyH :: [Value] -> [(String, Value)] -> IO Value
builtinAnyH [fn, v] _ = do
  elems  <- toList v
  result <- anyM (\x -> fmap isTruthy (callFn fn [x])) elems
  return (VBool result)
  where
    anyM _ []     = return False
    anyM p (x:xs) = do b <- p x; if b then return True else anyM p xs
builtinAnyH [v] _ = do
  elems <- toList v
  return (VBool (any isTruthy elems))
builtinAnyH _ _ = err "any" 2

builtinAllH :: [Value] -> [(String, Value)] -> IO Value
builtinAllH [fn, v] _ = do
  elems  <- toList v
  result <- allM (\x -> fmap isTruthy (callFn fn [x])) elems
  return (VBool result)
  where
    allM _ []     = return True
    allM p (x:xs) = do b <- p x; if b then allM p xs else return False
builtinAllH [v] _ = do
  elems <- toList v
  return (VBool (all isTruthy elems))
builtinAllH _ _ = err "all" 2

-- =============================================================================
-- NUMBER UTILITIES
-- =============================================================================

builtinOdd :: [Value] -> [(String, Value)] -> IO Value
builtinOdd [VInt n]   _ = return (VBool (odd n))
builtinOdd [VFloat f] _ = return (VBool (odd (round f :: Int)))
builtinOdd _ _ = err "odd" 1

builtinEven :: [Value] -> [(String, Value)] -> IO Value
builtinEven [VInt n]   _ = return (VBool (even n))
builtinEven [VFloat f] _ = return (VBool (even (round f :: Int)))
builtinEven _ _ = err "even" 1

builtinGcd :: [Value] -> [(String, Value)] -> IO Value
builtinGcd [VInt a, VInt b] _ = return (VInt (gcd a b))
builtinGcd [a, b] _ = do
  na <- coerceInt noPos "gcd" a
  nb <- coerceInt noPos "gcd" b
  return (VInt (gcd na nb))
builtinGcd _ _ = err "gcd" 2

builtinLcm :: [Value] -> [(String, Value)] -> IO Value
builtinLcm [VInt a, VInt b] _ = return (VInt (lcm a b))
builtinLcm [a, b] _ = do
  na <- coerceInt noPos "lcm" a
  nb <- coerceInt noPos "lcm" b
  return (VInt (lcm na nb))
builtinLcm _ _ = err "lcm" 2

builtinAbsH :: [Value] -> [(String, Value)] -> IO Value
builtinAbsH [VInt n]   _ = return (VInt   (abs n))
builtinAbsH [VFloat f] _ = return (VFloat (abs f))
builtinAbsH _ _ = err "abs" 1

builtinSignum :: [Value] -> [(String, Value)] -> IO Value
builtinSignum [VInt n]   _ = return (VInt (signum n))
builtinSignum [VFloat f] _ = return (VFloat (signum f))
builtinSignum _ _ = err "signum" 1

builtinNegate :: [Value] -> [(String, Value)] -> IO Value
builtinNegate [VInt n]   _ = return (VInt   (negate n))
builtinNegate [VFloat f] _ = return (VFloat (negate f))
builtinNegate _ _ = err "negate" 1

builtinFloor :: [Value] -> [(String, Value)] -> IO Value
builtinFloor [VFloat f] _ = return (VInt (floor f))
builtinFloor [VInt n]   _ = return (VInt n)
builtinFloor _ _ = err "floor" 1

builtinCeiling :: [Value] -> [(String, Value)] -> IO Value
builtinCeiling [VFloat f] _ = return (VInt (ceiling f))
builtinCeiling [VInt n]   _ = return (VInt n)
builtinCeiling _ _ = err "ceiling" 1

builtinTruncate :: [Value] -> [(String, Value)] -> IO Value
builtinTruncate [VFloat f] _ = return (VInt (truncate f))
builtinTruncate [VInt n]   _ = return (VInt n)
builtinTruncate _ _ = err "truncate" 1

builtinRoundH :: [Value] -> [(String, Value)] -> IO Value
builtinRoundH [VFloat f] _ = return (VInt (round f))
builtinRoundH [VInt n]   _ = return (VInt n)
builtinRoundH _ _ = err "round" 1

builtinFromIntegral :: [Value] -> [(String, Value)] -> IO Value
builtinFromIntegral [VInt n] _ = return (VFloat (fromIntegral n))
builtinFromIntegral [v]      _ = return v
builtinFromIntegral _ _ = err "fromIntegral" 1

builtinToInteger :: [Value] -> [(String, Value)] -> IO Value
builtinToInteger = builtinFromIntegral

builtinFromInteger :: [Value] -> [(String, Value)] -> IO Value
builtinFromInteger [VInt n] _ = return (VInt n)
builtinFromInteger _ _ = err "fromInteger" 1

-- =============================================================================
-- INFINITE LISTS
-- =============================================================================

builtinIterate :: [Value] -> [(String, Value)] -> IO Value
builtinIterate [fn, seed] _ = do
  ref <- newIORef (makeLazy seed)
  return (VLazyList ref)
  where
    makeLazy v = LCons v (callFn fn [v] >>= \v' -> return (makeLazy v'))
builtinIterate _ _ = err "iterate" 2

builtinRepeat :: [Value] -> [(String, Value)] -> IO Value
builtinRepeat [v] _ = do
  ref <- newIORef (makeLazy v)
  return (VLazyList ref)
  where makeLazy x = LCons x (return (makeLazy x))
builtinRepeat _ _ = err "repeat" 1

builtinReplicate :: [Value] -> [(String, Value)] -> IO Value
builtinReplicate [VInt n, v] _ = mkList (replicate n v)
builtinReplicate [nv, v] _ = do
  n <- coerceInt noPos "replicate" nv
  mkList (replicate n v)
builtinReplicate _ _ = err "replicate" 2

builtinCycle :: [Value] -> [(String, Value)] -> IO Value
builtinCycle [v] _ = do
  elems <- toList v
  if null elems
    then throwIO (MFValueError { mfMsg = "cycle: empty list", mfPos = noPos })
    else do
      ref <- newIORef (makeLazy elems)
      return (VLazyList ref)
  where makeLazy xs = let x = head xs; rest = tail xs ++ [x]
                      in  LCons x (return (makeLazy rest))
builtinCycle _ _ = err "cycle" 1

-- =============================================================================
-- STRING FUNCTIONS
-- =============================================================================

builtinWords :: [Value] -> [(String, Value)] -> IO Value
builtinWords [VStr s] _ = mkList (map VStr (words s))
builtinWords _ _ = err "words" 1

builtinUnwords :: [Value] -> [(String, Value)] -> IO Value
builtinUnwords [v] _ = do
  elems <- toList v
  let strs = [s | VStr s <- elems]
  return (VStr (unwords strs))
builtinUnwords _ _ = err "unwords" 1

builtinLines :: [Value] -> [(String, Value)] -> IO Value
builtinLines [VStr s] _ = mkList (map VStr (lines s))
builtinLines _ _ = err "lines" 1

builtinUnlines :: [Value] -> [(String, Value)] -> IO Value
builtinUnlines [v] _ = do
  elems <- toList v
  let strs = [s | VStr s <- elems]
  return (VStr (unlines strs))
builtinUnlines _ _ = err "unlines" 1

builtinShow :: [Value] -> [(String, Value)] -> IO Value
builtinShow [v] _ = return (VStr (valueToStr v))
builtinShow _ _ = err "show" 1

builtinRead :: [Value] -> [(String, Value)] -> IO Value
builtinRead [VStr s] _ =
  case reads s :: [(Int, String)] of
    [(n, "")] -> return (VInt n)
    _         -> case reads s :: [(Double, String)] of
      [(f, "")] -> return (VFloat f)
      _         -> case s of
        "True"  -> return (VBool True)
        "False" -> return (VBool False)
        "None"  -> return VNone
        _       -> return (VStr s)
builtinRead _ _ = err "read" 1

-- =============================================================================
-- DATA.LIST FUNCTIONS
-- =============================================================================

builtinNub :: [Value] -> [(String, Value)] -> IO Value
builtinNub [v] _ = toList v >>= mkList . nub
builtinNub _ _ = err "nub" 1

builtinSort :: [Value] -> [(String, Value)] -> IO Value
builtinSort [v] _ = toList v >>= mkList . sort
builtinSort _ _ = err "sort" 1

builtinSortBy :: [Value] -> [(String, Value)] -> IO Value
builtinSortBy [fn, v] _ = do
  elems <- toList v
  let cmpIO a b = do
        result <- callFn fn [a, b]
        case result of
          VInt n   -> return (compare n 0)
          VBool b  -> return (if b then LT else GT)
          _        -> return (compare a b)
  -- Use bubble sort (simple; for large lists use Data.List.sortBy with IO)
  sorted <- ioSort cmpIO elems
  mkList sorted
builtinSortBy _ _ = err "sortBy" 2

ioSort :: (a -> a -> IO Ordering) -> [a] -> IO [a]
ioSort _ []  = return []
ioSort _ [x] = return [x]
ioSort cmp xs = do
  let (l, r) = splitAt (length xs `div` 2) xs
  sl <- ioSort cmp l
  sr <- ioSort cmp r
  isMerge cmp sl sr
  where
    isMerge _ [] ys = return ys
    isMerge _ xs [] = return xs
    isMerge c (x:xs) (y:ys) = do
      o <- c x y
      case o of
        GT -> fmap (y:) (isMerge c (x:xs) ys)
        _  -> fmap (x:) (isMerge c xs (y:ys))

builtinSortOn :: [Value] -> [(String, Value)] -> IO Value
builtinSortOn [fn, v] _ = do
  elems  <- toList v
  keyed  <- mapM (\e -> do k <- callFn fn [e]; return (k, e)) elems
  let sorted = sortBy (\(k1,_) (k2,_) -> compare k1 k2) keyed
  mkList (map snd sorted)
builtinSortOn _ _ = err "sortOn" 2

builtinGroupBy :: [Value] -> [(String, Value)] -> IO Value
builtinGroupBy [fn, v] _ = do
  elems  <- toList v
  groups <- groupByM (\a b -> fmap isTruthy (callFn fn [a, b])) elems
  refs   <- mapM (\g -> newIORef g >>= \r -> return (VList r)) groups
  mkList refs
  where
    groupByM _ []     = return []
    groupByM p (x:xs) = do
      (same, rest) <- spanM (p x) xs
      groups       <- groupByM p rest
      return ((x:same) : groups)
    spanM _ []     = return ([], [])
    spanM p (x:xs) = do
      b <- p x
      if b
        then do (pre, suf) <- spanM p xs; return (x:pre, suf)
        else return ([], x:xs)
builtinGroupBy _ _ = err "groupBy" 2

builtinGroup :: [Value] -> [(String, Value)] -> IO Value
builtinGroup [v] _ = do
  elems  <- toList v
  let groups = group elems
  refs   <- mapM (\g -> newIORef g >>= \r -> return (VList r)) groups
  mkList refs
builtinGroup _ _ = err "group" 1

builtinTails :: [Value] -> [(String, Value)] -> IO Value
builtinTails [v] _ = do
  elems <- toList v
  refs  <- mapM (\t -> newIORef t >>= \r -> return (VList r)) (tails elems)
  mkList refs
builtinTails _ _ = err "tails" 1

builtinInits :: [Value] -> [(String, Value)] -> IO Value
builtinInits [v] _ = do
  elems <- toList v
  refs  <- mapM (\t -> newIORef t >>= \r -> return (VList r)) (inits elems)
  mkList refs
builtinInits _ _ = err "inits" 1

builtinPermutations :: [Value] -> [(String, Value)] -> IO Value
builtinPermutations [v] _ = do
  elems <- toList v
  let perms = permutations elems
  refs  <- mapM (\p -> newIORef p >>= \r -> return (VList r)) perms
  mkList refs
builtinPermutations _ _ = err "permutations" 1

builtinSubsequences :: [Value] -> [(String, Value)] -> IO Value
builtinSubsequences [v] _ = do
  elems <- toList v
  let subs = subsequences elems
  refs  <- mapM (\s -> newIORef s >>= \r -> return (VList r)) subs
  mkList refs
builtinSubsequences _ _ = err "subsequences" 1

builtinTranspose :: [Value] -> [(String, Value)] -> IO Value
builtinTranspose [v] _ = do
  outer <- toList v
  inner <- mapM toList outer
  let transposed = transpose inner
  refs  <- mapM (\row -> newIORef row >>= \r -> return (VList r)) transposed
  mkList refs
builtinTranspose _ _ = err "transpose" 1

builtinIntercalate :: [Value] -> [(String, Value)] -> IO Value
builtinIntercalate [sep, v] _ = do
  elems <- toList v
  case sep of
    VStr s -> do
      strs <- mapM (\e -> case e of VStr t -> return t; _ -> return (valueToStr e)) elems
      return (VStr (intercalate s strs))
    _ -> do
      sepList <- toList sep
      lists   <- mapM toList elems
      mkList (intercalate sepList lists)
builtinIntercalate _ _ = err "intercalate" 2

builtinIntersperse :: [Value] -> [(String, Value)] -> IO Value
builtinIntersperse [x, v] _ = case v of
  VStr s -> return (VStr (intersperse (head (valueToStr x)) s))
  _      -> toList v >>= mkList . intersperse x
builtinIntersperse _ _ = err "intersperse" 2

builtinIsPrefixOf :: [Value] -> [(String, Value)] -> IO Value
builtinIsPrefixOf [VStr pre, VStr str] _ = return (VBool (isPrefixOf pre str))
builtinIsPrefixOf [v1, v2] _ = do
  l1 <- toList v1
  l2 <- toList v2
  return (VBool (isPrefixOf l1 l2))
builtinIsPrefixOf _ _ = err "isPrefixOf" 2

builtinIsSuffixOf :: [Value] -> [(String, Value)] -> IO Value
builtinIsSuffixOf [VStr suf, VStr str] _ = return (VBool (isSuffixOf suf str))
builtinIsSuffixOf [v1, v2] _ = do
  l1 <- toList v1
  l2 <- toList v2
  return (VBool (isSuffixOf l1 l2))
builtinIsSuffixOf _ _ = err "isSuffixOf" 2

builtinIsInfixOf :: [Value] -> [(String, Value)] -> IO Value
builtinIsInfixOf [VStr inf, VStr str] _ = return (VBool (isInfixOf inf str))
builtinIsInfixOf [v1, v2] _ = do
  l1 <- toList v1
  l2 <- toList v2
  return (VBool (isInfixOf l1 l2))
builtinIsInfixOf _ _ = err "isInfixOf" 2

builtinStripPrefix :: [Value] -> [(String, Value)] -> IO Value
builtinStripPrefix [VStr pre, VStr str] _ =
  return (case stripPrefix pre str of
    Just r  -> VStr r
    Nothing -> VNone)
builtinStripPrefix _ _ = err "stripPrefix" 2

builtinNubBy :: [Value] -> [(String, Value)] -> IO Value
builtinNubBy [fn, v] _ = do
  elems  <- toList v
  result <- nubByM (\a b -> fmap isTruthy (callFn fn [a, b])) elems
  mkList result
  where
    nubByM _ []     = return []
    nubByM p (x:xs) = do
      filtered <- filterM (\y -> fmap not (p x y)) xs
      rest     <- nubByM p filtered
      return (x:rest)
    filterM _ []     = return []
    filterM p (x:xs) = do
      b  <- p x
      ys <- filterM p xs
      return (if b then x:ys else ys)
builtinNubBy _ _ = err "nubBy" 2

builtinPartition :: [Value] -> [(String, Value)] -> IO Value
builtinPartition [fn, v] _ = do
  elems <- toList v
  (yes, no) <- partitionM (\x -> fmap isTruthy (callFn fn [x])) elems
  yesRef <- newIORef yes
  noRef  <- newIORef no
  return (VTuple [VList yesRef, VList noRef])
  where
    partitionM _ []     = return ([], [])
    partitionM p (x:xs) = do
      b <- p x
      (ys, ns) <- partitionM p xs
      return (if b then (x:ys, ns) else (ys, x:ns))
builtinPartition _ _ = err "partition" 2

builtinFind :: [Value] -> [(String, Value)] -> IO Value
builtinFind [fn, v] _ = do
  elems  <- toList v
  result <- findM (\x -> fmap isTruthy (callFn fn [x])) elems
  return (case result of
    Nothing -> VNone
    Just x  -> x)
  where
    findM _ []     = return Nothing
    findM p (x:xs) = do
      b <- p x
      if b then return (Just x) else findM p xs
builtinFind _ _ = err "find" 2

builtinFindIndex :: [Value] -> [(String, Value)] -> IO Value
builtinFindIndex [fn, v] _ = do
  elems  <- toList v
  result <- findIndexM (\x -> fmap isTruthy (callFn fn [x])) elems 0
  return (case result of
    Nothing -> VNone
    Just i  -> VInt i)
  where
    findIndexM _ []     _ = return Nothing
    findIndexM p (x:xs) i = do
      b <- p x
      if b then return (Just i) else findIndexM p xs (i+1)
builtinFindIndex _ _ = err "findIndex" 2

builtinElemIndex :: [Value] -> [(String, Value)] -> IO Value
builtinElemIndex [x, v] _ = do
  elems <- toList v
  return (case elemIndex x elems of
    Nothing -> VNone
    Just i  -> VInt i)
builtinElemIndex _ _ = err "elemIndex" 2

-- =============================================================================
-- FUNCTION COMBINATORS
-- =============================================================================

builtinIdH :: [Value] -> [(String, Value)] -> IO Value
builtinIdH [v] _ = return v
builtinIdH _ _ = err "id" 1

builtinConst :: [Value] -> [(String, Value)] -> IO Value
builtinConst [a] _ = return (VBuiltin "const_result" (\_ _ -> return a))
builtinConst [a, _] _ = return a
builtinConst _ _ = err "const" 1

builtinFlip :: [Value] -> [(String, Value)] -> IO Value
builtinFlip [fn] _ = return (VBuiltin "flipped"
  (\args _ -> case args of
    [a, b] -> callFn fn [b, a]
    _      -> throwIO (MFTypeError { mfMsg = "flip: expected 2 arguments", mfPos = noPos })))
builtinFlip _ _ = err "flip" 1

builtinCurry :: [Value] -> [(String, Value)] -> IO Value
builtinCurry [fn] _ = return (VBuiltin "curried"
  (\[a] _ -> return (VBuiltin "curried_inner"
    (\[b] _ -> callFn fn [VTuple [a, b]]
    ))))
builtinCurry _ _ = err "curry" 1

builtinUncurry :: [Value] -> [(String, Value)] -> IO Value
builtinUncurry [fn] _ = return (VBuiltin "uncurried"
  (\[VTuple [a, b]] _ -> callFn fn [a] >>= \f -> callFn f [b]))
builtinUncurry _ _ = err "uncurry" 1

builtinUntil :: [Value] -> [(String, Value)] -> IO Value
builtinUntil [pred_, fn, seed] _ = do
  let go v = do
        b <- callFn pred_ [v]
        if isTruthy b
          then return v
          else callFn fn [v] >>= go
  go seed
builtinUntil _ _ = err "until" 3

builtinFix :: [Value] -> [(String, Value)] -> IO Value
builtinFix [fn] _ = do
  ref <- newIORef VNone
  let fix_ = VBuiltin "fix_result" (\args _ -> do
        r <- readIORef ref
        callFn r args)
  writeIORef ref fix_
  callFn fn [fix_]
builtinFix _ _ = err "fix" 1

builtinOn :: [Value] -> [(String, Value)] -> IO Value
builtinOn [f, g] _ = return (VBuiltin "on_result"
  (\[a, b] _ -> do
    ga <- callFn g [a]
    gb <- callFn g [b]
    callFn f [ga, gb]))
builtinOn _ _ = err "on" 2

builtinComparing :: [Value] -> [(String, Value)] -> IO Value
builtinComparing [fn] _ = return (VBuiltin "comparing_result"
  (\[a, b] _ -> do
    ka <- callFn fn [a]
    kb <- callFn fn [b]
    return (VInt (case compare ka kb of LT -> -1; EQ -> 0; GT -> 1))))
builtinComparing _ _ = err "comparing" 1

-- =============================================================================
-- MAYBE FUNCTIONS
-- =============================================================================

builtinFromMaybe :: [Value] -> [(String, Value)] -> IO Value
builtinFromMaybe [def, VNone] _ = return def
builtinFromMaybe [_, v]       _ = return v
builtinFromMaybe _ _ = err "fromMaybe" 2

builtinMaybe :: [Value] -> [(String, Value)] -> IO Value
builtinMaybe [def, _, VNone] _ = return def
builtinMaybe [_, fn, v]      _ = callFn fn [v]
builtinMaybe _ _ = err "maybe" 3

builtinCatMaybes :: [Value] -> [(String, Value)] -> IO Value
builtinCatMaybes [v] _ = do
  elems <- toList v
  mkList [x | x <- elems, x /= VNone]
builtinCatMaybes _ _ = err "catMaybes" 1

builtinMapMaybe :: [Value] -> [(String, Value)] -> IO Value
builtinMapMaybe [fn, v] _ = do
  elems   <- toList v
  results <- mapM (\x -> callFn fn [x]) elems
  mkList [r | r <- results, r /= VNone]
builtinMapMaybe _ _ = err "mapMaybe" 2

builtinListToMaybe :: [Value] -> [(String, Value)] -> IO Value
builtinListToMaybe [v] _ = do
  elems <- toList v
  return (case elems of
    []    -> VNone
    (x:_) -> x)
builtinListToMaybe _ _ = err "listToMaybe" 1

builtinMaybeToList :: [Value] -> [(String, Value)] -> IO Value
builtinMaybeToList [VNone] _ = mkList []
builtinMaybeToList [v]     _ = mkList [v]
builtinMaybeToList _ _ = err "maybeToList" 1

-- =============================================================================
-- MATH FUNCTIONS
-- =============================================================================

mathFn1 :: String -> (Double -> Double) -> [Value] -> [(String, Value)] -> IO Value
mathFn1 _ f [VInt n]   _ = return (VFloat (f (fromIntegral n)))
mathFn1 _ f [VFloat x] _ = return (VFloat (f x))
mathFn1 name _ _ _ = err name 1

mathFn2 :: String -> (Double -> Double -> Double) -> [Value] -> [(String, Value)] -> IO Value
mathFn2 _ f [a, b] _ = do
  fa <- coerceNum noPos "" a
  fb <- coerceNum noPos "" b
  return (VFloat (f fa fb))
mathFn2 name _ _ _ = err name 2

builtinSqrt    = mathFn1 "sqrt"    sqrt
builtinLog     = mathFn1 "log"     log
builtinExp     = mathFn1 "exp"     exp
builtinSin     = mathFn1 "sin"     sin
builtinCos     = mathFn1 "cos"     cos
builtinTan     = mathFn1 "tan"     tan
builtinAsin    = mathFn1 "asin"    asin
builtinAcos    = mathFn1 "acos"    acos
builtinAtan    = mathFn1 "atan"    atan
builtinSinh    = mathFn1 "sinh"    sinh
builtinCosh    = mathFn1 "cosh"    cosh
builtinTanh    = mathFn1 "tanh"    tanh
builtinAtan2   = mathFn2 "atan2"   atan2
builtinLogBase = mathFn2 "logBase" logBase

-- =============================================================================
-- CHAR FUNCTIONS
-- =============================================================================

charFn1 :: String -> (Char -> Bool) -> [Value] -> [(String, Value)] -> IO Value
charFn1 _ p [VStr [c]] _ = return (VBool (p c))
charFn1 _ p [VStr s]   _ = return (VBool (all p s))
charFn1 name _ _ _ = err name 1

builtinIsAlpha   = charFn1 "isAlpha"   isAlpha
builtinIsDigit   = charFn1 "isDigit"   isDigit
builtinIsSpace   = charFn1 "isSpace"   isSpace
builtinIsUpper   = charFn1 "isUpper"   isUpper
builtinIsLower   = charFn1 "isLower"   isLower

builtinToUpper :: [Value] -> [(String, Value)] -> IO Value
builtinToUpper [VStr s] _ = return (VStr (map toUpper s))
builtinToUpper _ _ = err "toUpper" 1

builtinToLower :: [Value] -> [(String, Value)] -> IO Value
builtinToLower [VStr s] _ = return (VStr (map toLower s))
builtinToLower _ _ = err "toLower" 1

builtinDigitToInt :: [Value] -> [(String, Value)] -> IO Value
builtinDigitToInt [VStr [c]] _ = return (VInt (digitToInt c))
builtinDigitToInt _ _ = err "digitToInt" 1

builtinIntToDigit :: [Value] -> [(String, Value)] -> IO Value
builtinIntToDigit [VInt n] _ = return (VStr [intToDigit n])
  where intToDigit d | d < 10    = toEnum (d + fromEnum '0')
                     | otherwise = toEnum (d - 10 + fromEnum 'a')
builtinIntToDigit _ _ = err "intToDigit" 1

-- =============================================================================
-- BOOL FUNCTIONS
-- =============================================================================

builtinNot :: [Value] -> [(String, Value)] -> IO Value
builtinNot [v] _ = return (VBool (not (isTruthy v)))
builtinNot _ _ = err "not" 1

builtinAndH :: [Value] -> [(String, Value)] -> IO Value
builtinAndH [v] _ = do
  elems <- toList v
  return (VBool (all isTruthy elems))
builtinAndH _ _ = err "and" 1

builtinOrH :: [Value] -> [(String, Value)] -> IO Value
builtinOrH [v] _ = do
  elems <- toList v
  return (VBool (any isTruthy elems))
builtinOrH _ _ = err "or" 1

-- =============================================================================
-- IO FUNCTIONS
-- =============================================================================

builtinPutStr :: [Value] -> [(String, Value)] -> IO Value
builtinPutStr [VStr s] _ = putStr s >> return VNone
builtinPutStr [v] _ = putStr (valueToStr v) >> return VNone
builtinPutStr _ _ = err "putStr" 1

builtinPutStrLn :: [Value] -> [(String, Value)] -> IO Value
builtinPutStrLn [VStr s] _ = putStrLn s >> return VNone
builtinPutStrLn [v] _ = putStrLn (valueToStr v) >> return VNone
builtinPutStrLn _ _ = err "putStrLn" 1

builtinGetLine :: [Value] -> [(String, Value)] -> IO Value
builtinGetLine [] _ = fmap VStr getLine
builtinGetLine _ _ = err "getLine" 0

builtinReadLn :: [Value] -> [(String, Value)] -> IO Value
builtinReadLn [] _ = do
  line <- getLine
  case reads line :: [(Int, String)] of
    [(n, "")] -> return (VInt n)
    _         -> return (VStr line)
builtinReadLn _ _ = err "readLn" 0

builtinWriteFile :: [Value] -> [(String, Value)] -> IO Value
builtinWriteFile [VStr path, VStr content] _ = do
  writeFile path content
  return VNone
builtinWriteFile _ _ = err "writeFile" 2

builtinReadFile :: [Value] -> [(String, Value)] -> IO Value
builtinReadFile [VStr path] _ = fmap VStr (readFile path)
builtinReadFile _ _ = err "readFile" 1

builtinAppendFile :: [Value] -> [(String, Value)] -> IO Value
builtinAppendFile [VStr path, VStr content] _ = do
  appendFile path content
  return VNone
builtinAppendFile _ _ = err "appendFile" 2

-- =============================================================================
-- HELPERS
-- =============================================================================

toList :: Value -> IO [Value]
toList (VList ref)   = readIORef ref
toList (VTuple vs)   = return vs
toList (VStr s)      = return (map (VStr . (:[])) s)
toList (VSet ref)    = readIORef ref
toList (VIterator ref) = readIORef ref
toList v = throwIO (MFTypeError
  { mfMsg = "object is not iterable: " ++ typeNameOf v, mfPos = noPos })

mkList :: [Value] -> IO Value
mkList vs = newIORef vs >>= return . VList

err :: String -> Int -> IO a
err name n = throwIO (MFTypeError
  { mfMsg = name ++ "() takes exactly " ++ show n ++ " argument(s)"
  , mfPos = noPos })

foldM :: (b -> a -> IO b) -> b -> [a] -> IO b
foldM _ acc []     = return acc
foldM f acc (x:xs) = f acc x >>= \acc' -> foldM f acc' xs

-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Evaluator.hs  -- Tree-walking interpreter: AST -> IO Value
-- =============================================================================
module Evaluator
  ( evalProgram
  , evalStmt
  , evalExpr
  , applyFunction
  , initGlobalEnv
  ) where

import Types
import Environment
import PatternMatch
import Pretty
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, isPrefixOf, isSuffixOf, sort, nub)
import Data.Char (toLower, toUpper, isDigit, isAlpha)
import Control.Exception (throwIO, catch, try, SomeException, evaluate)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe, mapMaybe)

-- Import builtins
import Builtins.Python (pythonBuiltinList)
import Builtins.Haskell (haskellBuiltinList)

-- =============================================================================
-- GLOBAL ENVIRONMENT INITIALIZATION
-- =============================================================================

initGlobalEnv :: IO Env
initGlobalEnv = do
  env <- newGlobalEnv
  -- Register Python builtins
  mapM_ (\(n, f) -> defineVar n (VBuiltin n f) env) pythonBuiltinList
  -- Register Haskell builtins
  mapM_ (\(n, f) -> defineVar n (VBuiltin n f) env) haskellBuiltinList
  -- Constants
  defineVar "pi"    (VFloat 3.141592653589793) env
  defineVar "e"     (VFloat 2.718281828459045) env
  defineVar "inf"   (VFloat (1/0))             env
  defineVar "nan"   (VFloat (0/0))             env
  defineVar "True"  (VBool True)               env
  defineVar "False" (VBool False)              env
  defineVar "None"  VNone                      env
  -- Register pipeline helpers
  defineVar "compose" composeBuiltin env
  defineVar "pipe"    pipeBuiltin    env
  return env

composeBuiltin :: Value
composeBuiltin = VBuiltin "compose"
  (\[f, g] _ -> return (VBuiltin "composed"
    (\args _ -> do
      gresult <- applyFunction g args
      applyFunction f [gresult])))

pipeBuiltin :: Value
pipeBuiltin = VBuiltin "pipe"
  (\[v, f] _ -> applyFunction f [v])

-- =============================================================================
-- PROGRAM EVALUATION
-- =============================================================================

evalProgram :: Env -> Program -> IO Value
evalProgram env (Program stmts) = do
  evalStmts env stmts
  return VNone

evalStmts :: Env -> [Stmt] -> IO Value
evalStmts env []     = return VNone
evalStmts env (s:ss) = do
  result <- evalStmt env s
  case result of
    Just v  -> return v   -- propagate return/break/continue
    Nothing -> evalStmts env ss

-- Returns Just v if a return/break/continue occurred, Nothing otherwise
evalStmt :: Env -> Stmt -> IO (Maybe Value)

-- Expression statement
evalStmt env (SExpr e) = do
  _ <- evalExpr env e
  return Nothing

-- Variable assignment
evalStmt env (SAssign pat expr) = do
  val <- evalExpr env expr
  ok  <- matchPattern env pat val
  if ok
    then return Nothing
    else throwIO (MFPatternFail
      { mfMsg = "Assignment pattern match failed"
      , mfPos = noPos })

-- Augmented assignment (x += 5)
evalStmt env (SAugAssign name op expr) = do
  lhs <- lookupVarIO noPos name env
  rhs <- evalExpr env expr
  result <- evalBinOp op lhs rhs
  found  <- assignVar name result env
  if found
    then return Nothing
    else do
      defineVar name result env
      return Nothing

-- Function definition
evalStmt env (SFunDef name params retTy body) = do
  let closure = VClosure params body env name
  defineVar name closure env
  return Nothing

-- Let definition
evalStmt env (SLetDef name _ expr) = do
  val <- evalExpr env expr
  defineVar name val env
  return Nothing

-- If/elif/else
evalStmt env (SIf branches elsePart) = do
  result <- evalBranches env branches
  case result of
    Just stmts -> runBlock stmts
    Nothing    -> case elsePart of
      Just stmts -> runBlock stmts
      Nothing    -> return Nothing
  where
    runBlock stmts = evalStmts env stmts >>= \v ->
      return (if v == VNone then Nothing else Just v)
    evalBranches _ [] = return Nothing
    evalBranches e ((cond, stmts):rest) = do
      cv <- evalExpr e cond
      if isTruthy cv
        then return (Just stmts)
        else evalBranches e rest

-- While loop
evalStmt env (SWhile cond body elsePart) = do
  result <- whileLoop
  case result of
    Just v -> return (Just v)
    Nothing -> case elsePart of
      Just stmts -> evalStmts env stmts >> return Nothing
      Nothing    -> return Nothing
  where
    whileLoop = do
      cv <- evalExpr env cond
      if not (isTruthy cv)
        then return Nothing
        else do
          r <- runBody body
          case r of
            Just (Left _)  -> return Nothing  -- break
            Just (Right v) -> return (Just v) -- return
            Nothing        -> whileLoop
    runBody stmts = do
      result <- evalStmtsLoop env stmts
      return result

-- For loop
evalStmt env (SFor pat iterExpr body elsePart) = do
  iterVal <- evalExpr env iterExpr
  elems   <- valueToList iterVal
  result  <- forLoop elems
  case result of
    Just (Left _)  -> return Nothing  -- break
    Just (Right v) -> return (Just v) -- return value
    Nothing        -> case elsePart of
      Just stmts -> evalStmts env stmts >> return Nothing
      Nothing    -> return Nothing
  where
    forLoop []     = return Nothing
    forLoop (x:xs) = do
      loopEnv <- newChildEnv "<for>" env
      ok      <- matchPattern loopEnv pat x
      if not ok
        then forLoop xs
        else do
          r <- evalStmtsLoop loopEnv body
          case r of
            Just (Left _)  -> return (Just (Left ()))
            Just (Right v) -> return (Just (Right v))
            Nothing        -> forLoop xs

-- Return
evalStmt env (SReturn Nothing)  = return (Just VNone)
evalStmt env (SReturn (Just e)) = do
  v <- evalExpr env e
  return (Just v)

-- Break / Continue (handled via signals in loop contexts)
evalStmt _ SBreak    = throwIO MFBreakSignal
evalStmt _ SContinue = throwIO MFContinueSignal

-- Match statement
evalStmt env (SMatch scrutinee arms) = do
  val    <- evalExpr env scrutinee
  result <- tryArms val arms
  case result of
    Nothing -> throwIO (MFPatternFail
      { mfMsg = "Non-exhaustive patterns in match"
      , mfPos = noPos })
    Just v  -> return Nothing
  where
    tryArms _ []     = return Nothing
    tryArms v (a:as) = do
      matchResult <- tryMatchArm env v a
      case matchResult of
        Nothing           -> tryArms v as
        Just (body, armEnv) -> do
          -- Evaluate guard if present
          let arm = a
          guardOk <- case maGuard arm of
            Nothing -> return True
            Just g  -> fmap isTruthy (evalExpr armEnv g)
          if guardOk
            then do
              evalStmts armEnv body
              return (Just VNone)
            else tryArms v as

-- Try/Catch
evalStmt env (STryCatch tryBody handlers elsePart finallyPart) = do
  result <- try (evalStmts env tryBody)
  handled <- case result of
    Right v -> do
      case elsePart of
        Just stmts -> evalStmts env stmts
        Nothing    -> return v
      return (Just v)
    Left (ex :: MiniFlowError) -> do
      let exName = exceptionName ex
          exMsg  = case ex of
            MFRuntimeError m _ -> m
            MFTypeError m _    -> m
            MFNameError m _    -> m
            _                  -> show ex
      findHandler handlers ex exName exMsg
  case finallyPart of
    Just stmts -> evalStmts env stmts >> return Nothing
    Nothing    -> return Nothing
  where
    exceptionName (MFRuntimeError{})  = "RuntimeError"
    exceptionName (MFTypeError{})     = "TypeError"
    exceptionName (MFNameError{})     = "NameError"
    exceptionName (MFIndexError{})    = "IndexError"
    exceptionName (MFKeyError{})      = "KeyError"
    exceptionName (MFValueError{})    = "ValueError"
    exceptionName (MFDivisionByZero{})= "ZeroDivisionError"
    exceptionName (MFPatternFail{})   = "PatternMatchFailure"
    exceptionName (MFImportError{})   = "ImportError"
    exceptionName _                   = "Exception"

    findHandler [] ex _ _ = throwIO ex
    findHandler ((exType, varName, handlerBody):rest) ex exName exMsg = do
      let matches = case exType of
            Nothing  -> True
            Just t   -> t == exName || t == "Exception"
      if matches
        then do
          handlerEnv <- newChildEnv "<except>" env
          defineVar varName (VStr exMsg) handlerEnv
          evalStmts handlerEnv handlerBody
          return (Just VNone)
        else findHandler rest ex exName exMsg

-- Record definition
evalStmt env (SRecordDef name fields) = do
  -- Register a constructor function
  let constructor = VBuiltin name (\args kwargs -> do
        let pairs = zipWith (\(FieldDef fname _ _) v -> (fname, v)) fields args
            kwPairs = [(k, v) | (k, v) <- kwargs]
            allPairs = pairs ++ kwPairs
        ref <- newIORef (Map.fromList allPairs)
        return (VRecord name ref))
  defineVar name constructor env
  return Nothing

-- Import
evalStmt env (SImport spec) = do
  -- Simplified: just define module name as a string for now
  case spec of
    ImportModule mname alias -> do
      let key = fromMaybe mname alias
      defineVar key (VModule mname Map.empty) env
    ImportFrom mname names _ -> do
      -- In a full implementation, load the module and import names
      return ()
    ImportAll mname -> return ()
  return Nothing

-- Pass / Global / Nonlocal / Delete / Raise / Assert
evalStmt _ SPass = return Nothing
evalStmt _ (SGlobal _) = return Nothing
evalStmt _ (SNonlocal _) = return Nothing
evalStmt env (SDelete exprs) = do
  mapM_ (deleteExpr env) exprs
  return Nothing
evalStmt env (SRaise Nothing) = throwIO (MFRuntimeError { mfMsg = "Exception", mfPos = noPos })
evalStmt env (SRaise (Just e)) = do
  v <- evalExpr env e
  throwIO (MFRaiseSignal v)
evalStmt env (SAssert cond msgExpr) = do
  cv <- evalExpr env cond
  if isTruthy cv
    then return Nothing
    else do
      msg <- case msgExpr of
        Nothing -> return "AssertionError"
        Just e  -> fmap valueToStr (evalExpr env e)
      throwIO (MFRuntimeError { mfMsg = "AssertionError: " ++ msg, mfPos = noPos })

deleteExpr :: Env -> Expr -> IO ()
deleteExpr env (EVar name) = defineVar name VNone env  -- simplified
deleteExpr env (EIndex e idx) = do
  container <- evalExpr env e
  key       <- evalExpr env idx
  case container of
    VList ref -> do
      elems <- readIORef ref
      case key of
        VInt i -> writeIORef ref (deleteAt i elems)
        _      -> return ()
    VDict ref -> modifyIORef ref (Map.delete key)
    _         -> return ()
deleteExpr _ _ = return ()

deleteAt :: Int -> [a] -> [a]
deleteAt i xs = take i xs ++ drop (i+1) xs

-- =============================================================================
-- LOOP BODY HELPERS
-- =============================================================================

-- Runs statements, catching break/continue signals
evalStmtsLoop :: Env -> [Stmt] -> IO (Maybe (Either () Value))
evalStmtsLoop env stmts =
  catch (evalStmts env stmts >> return Nothing) handler
  where
    handler MFBreakSignal    = return (Just (Left ()))
    handler MFContinueSignal = return Nothing
    handler (MFReturnSignal v) = return (Just (Right v))
    handler ex = throwIO ex

whileBody :: Env -> [Stmt] -> IO Bool
whileBody env stmts = do
  result <- evalStmtsLoop env stmts
  case result of
    Nothing          -> return True   -- continue loop
    Just (Left _)    -> return False  -- break
    Just (Right _)   -> return False  -- return (propagate)

-- =============================================================================
-- EXPRESSION EVALUATION
-- =============================================================================

evalExpr :: Env -> Expr -> IO Value

-- Literals
evalExpr _ (EInt n)    = return (VInt n)
evalExpr _ (EFloat f)  = return (VFloat f)
evalExpr _ (EStr s)    = return (VStr s)
evalExpr _ (EBool b)   = return (VBool b)
evalExpr _ ENone       = return VNone
evalExpr _ EEllipsis   = return (VStr "...")

-- F-string / string interpolation
evalExpr env (EFStr parts) = do
  strs <- mapM evalFPart parts
  return (VStr (concat strs))
  where
    evalFPart (FStrLit s)  = return s
    evalFPart (FStrExpr rawExpr) = do
      -- In a full implementation, re-parse and eval rawExpr
      -- For now, try to look up as a variable name
      let name = rawExpr
      result <- lookupVar name env
      case result of
        Just v  -> valueToStrIO v
        Nothing -> return ("{" ++ rawExpr ++ "}")
    valueToStrIO v = displayValue v

displayValue :: Value -> IO String
displayValue VNone          = return "None"
displayValue (VBool True)   = return "True"
displayValue (VBool False)  = return "False"
displayValue (VInt n)       = return (show n)
displayValue (VFloat f)     = return (formatFlt f)
displayValue (VStr s)       = return s
displayValue (VList ref)    = do
  vs <- readIORef ref
  strs <- mapM reprVal vs
  return ("[" ++ intercalate ", " strs ++ "]")
displayValue (VTuple vs)    = do
  strs <- mapM reprVal vs
  return ("(" ++ intercalate ", " strs ++ ")")
displayValue (VDict ref)    = do
  m <- readIORef ref
  pairs <- mapM (\(k,v) -> do ks <- reprVal k; vs <- reprVal v; return (ks ++ ": " ++ vs))
                (Map.toAscList m)
  return ("{" ++ intercalate ", " pairs ++ "}")
displayValue (VSet ref)     = do
  vs <- readIORef ref
  strs <- mapM reprVal vs
  return (case strs of { [] -> "set()"; _ -> "{" ++ intercalate ", " strs ++ "}" })
displayValue v              = return (valueToStr v)

reprVal :: Value -> IO String
reprVal (VStr s) = return (show s)
reprVal v        = displayValue v

formatFlt :: Double -> String
formatFlt f
  | isInfinite f && f > 0 = "inf"
  | isInfinite f && f < 0 = "-inf"
  | isNaN f               = "nan"
  | f == fromIntegral (round f :: Int) && abs f < 1e15
                          = show (round f :: Int) ++ ".0"
  | otherwise             = show f

-- Variable lookup
evalExpr env (EVar name) = lookupVarIO noPos name env

-- Field access: obj.field
evalExpr env (EField expr field) = do
  obj <- evalExpr env expr
  case obj of
    VRecord _ ref -> do
      fields <- readIORef ref
      case Map.lookup field fields of
        Just v  -> return v
        Nothing -> throwIO (MFKeyError
          { mfMsg = "'" ++ typeNameOf obj ++ "' object has no attribute '" ++ field ++ "'"
          , mfPos = noPos })
    VModule _ exports -> case Map.lookup field exports of
      Just v  -> return v
      Nothing -> throwIO (MFKeyError
        { mfMsg = "module has no attribute '" ++ field ++ "'"
        , mfPos = noPos })
    VStr s -> evalStringMethod s field
    VList ref -> evalListMethod ref field
    VDict ref -> evalDictMethod ref field
    _ -> throwIO (MFTypeError
      { mfMsg = "'" ++ typeNameOf obj ++ "' object has no attribute '" ++ field ++ "'"
      , mfPos = noPos })

-- Indexing: expr[idx]
evalExpr env (EIndex expr idx) = do
  container <- evalExpr env expr
  key       <- evalExpr env idx
  evalIndex container key

evalIndex :: Value -> Value -> IO Value
evalIndex (VList ref) (VInt i) = do
  elems <- readIORef ref
  let n = length elems
      j = if i < 0 then n + i else i
  if j < 0 || j >= n
    then throwIO (MFIndexError { mfMsg = "list index out of range", mfPos = noPos })
    else return (elems !! j)
evalIndex (VStr s) (VInt i) = do
  let n = length s
      j = if i < 0 then n + i else i
  if j < 0 || j >= n
    then throwIO (MFIndexError { mfMsg = "string index out of range", mfPos = noPos })
    else return (VStr [s !! j])
evalIndex (VDict ref) key = do
  m <- readIORef ref
  case Map.lookup key m of
    Just v  -> return v
    Nothing -> throwIO (MFKeyError { mfMsg = "key not found: " ++ valueToStr key, mfPos = noPos })
evalIndex (VTuple vs) (VInt i) = do
  let n = length vs
      j = if i < 0 then n + i else i
  if j < 0 || j >= n
    then throwIO (MFIndexError { mfMsg = "tuple index out of range", mfPos = noPos })
    else return (vs !! j)
evalIndex v k = throwIO (MFTypeError
  { mfMsg = "'" ++ typeNameOf v ++ "' object is not subscriptable with " ++ typeNameOf k
  , mfPos = noPos })

-- Slicing: expr[a:b:c]
evalExpr env (ESlice expr mA mB mC) = do
  container <- evalExpr env expr
  a <- mapM (evalExpr env) mA
  b <- mapM (evalExpr env) mB
  c <- mapM (evalExpr env) mC
  evalSlice container a b c

evalSlice :: Value -> Maybe Value -> Maybe Value -> Maybe Value -> IO Value
evalSlice (VList ref) mA mB mC = do
  elems <- readIORef ref
  let n     = length elems
      step  = maybe 1 toInt mC
      start = case mA of
        Nothing -> if step > 0 then 0 else n-1
        Just v  -> normalizeIdx (toInt v) n
      end   = case mB of
        Nothing -> if step > 0 then n else -1
        Just v  -> normalizeIdx (toInt v) n
      sliced = if step > 0
               then [elems !! i | i <- [start, start+step .. end-1], i >= 0, i < n]
               else [elems !! i | i <- [start, start+step .. end+1], i >= 0, i < n]
  ref' <- newIORef sliced
  return (VList ref')
evalSlice (VStr s) mA mB mC = do
  let n     = length s
      step  = maybe 1 toInt mC
      start = case mA of
        Nothing -> if step > 0 then 0 else n-1
        Just v  -> normalizeIdx (toInt v) n
      end   = case mB of
        Nothing -> if step > 0 then n else -1
        Just v  -> normalizeIdx (toInt v) n
      sliced = if step > 0
               then [s !! i | i <- [start, start+step .. end-1], i >= 0, i < n]
               else [s !! i | i <- [start, start+step .. end+1], i >= 0, i < n]
  return (VStr sliced)
evalSlice v _ _ _ = throwIO (MFTypeError
  { mfMsg = "'" ++ typeNameOf v ++ "' object is not sliceable", mfPos = noPos })

normalizeIdx :: Int -> Int -> Int
normalizeIdx i n = if i < 0 then max 0 (n + i) else min n i

-- Binary operations
evalExpr env (EBinOp op lhs rhs) = do
  -- Short-circuit for and/or
  case op of
    OpAnd -> do
      lv <- evalExpr env lhs
      if not (isTruthy lv) then return lv else evalExpr env rhs
    OpOr -> do
      lv <- evalExpr env lhs
      if isTruthy lv then return lv else evalExpr env rhs
    _ -> do
      lv <- evalExpr env lhs
      rv <- evalExpr env rhs
      evalBinOp op lv rv

evalBinOp :: BinOp -> Value -> Value -> IO Value

-- Arithmetic
evalBinOp OpAdd (VInt a)   (VInt b)   = return (VInt   (a + b))
evalBinOp OpAdd (VFloat a) (VFloat b) = return (VFloat (a + b))
evalBinOp OpAdd (VInt a)   (VFloat b) = return (VFloat (fromIntegral a + b))
evalBinOp OpAdd (VFloat a) (VInt b)   = return (VFloat (a + fromIntegral b))
evalBinOp OpAdd (VStr a)   (VStr b)   = return (VStr (a ++ b))
evalBinOp OpAdd (VList r1) (VList r2) = do
  l1 <- readIORef r1; l2 <- readIORef r2
  ref <- newIORef (l1 ++ l2); return (VList ref)
evalBinOp OpSub (VInt a)   (VInt b)   = return (VInt   (a - b))
evalBinOp OpSub (VFloat a) (VFloat b) = return (VFloat (a - b))
evalBinOp OpSub (VInt a)   (VFloat b) = return (VFloat (fromIntegral a - b))
evalBinOp OpSub (VFloat a) (VInt b)   = return (VFloat (a - fromIntegral b))
evalBinOp OpMul (VInt a)   (VInt b)   = return (VInt   (a * b))
evalBinOp OpMul (VFloat a) (VFloat b) = return (VFloat (a * b))
evalBinOp OpMul (VInt a)   (VFloat b) = return (VFloat (fromIntegral a * b))
evalBinOp OpMul (VFloat a) (VInt b)   = return (VFloat (a * fromIntegral b))
evalBinOp OpMul (VStr s)   (VInt n)   = return (VStr (concat (replicate n s)))
evalBinOp OpMul (VInt n)   (VStr s)   = return (VStr (concat (replicate n s)))
evalBinOp OpDiv (VInt a)   (VInt b)   = if b == 0 then throwIO (MFDivisionByZero noPos)
                                        else return (VFloat (fromIntegral a / fromIntegral b))
evalBinOp OpDiv (VFloat a) (VFloat b) = if b == 0 then throwIO (MFDivisionByZero noPos)
                                        else return (VFloat (a / b))
evalBinOp OpDiv (VInt a)   (VFloat b) = if b == 0 then throwIO (MFDivisionByZero noPos)
                                        else return (VFloat (fromIntegral a / b))
evalBinOp OpDiv (VFloat a) (VInt b)   = if b == 0 then throwIO (MFDivisionByZero noPos)
                                        else return (VFloat (a / fromIntegral b))
evalBinOp OpFloorDiv (VInt a) (VInt b) = if b == 0 then throwIO (MFDivisionByZero noPos)
                                         else return (VInt (a `div` b))
evalBinOp OpFloorDiv (VFloat a) (VFloat b) = if b == 0 then throwIO (MFDivisionByZero noPos)
                                              else return (VFloat (fromIntegral (floor (a / b) :: Int)))
evalBinOp OpMod  (VInt a)   (VInt b)   = if b == 0 then throwIO (MFDivisionByZero noPos)
                                         else return (VInt (a `mod` b))
evalBinOp OpMod  (VFloat a) (VFloat b) = return (VFloat (a - fromIntegral (floor (a/b) :: Int) * b))
evalBinOp OpPow  (VInt a)   (VInt b)   = return (VInt   (a ^ b))
evalBinOp OpPow  (VFloat a) (VFloat b) = return (VFloat (a ** b))
evalBinOp OpPow  (VInt a)   (VFloat b) = return (VFloat (fromIntegral a ** b))
evalBinOp OpPow  (VFloat a) (VInt b)   = return (VFloat (a ** fromIntegral b))

-- Comparisons
evalBinOp OpEq  a b = return (VBool (a == b))
evalBinOp OpNeq a b = return (VBool (a /= b))
evalBinOp OpLt  a b = return (VBool (a < b))
evalBinOp OpGt  a b = return (VBool (a > b))
evalBinOp OpLtEq a b = return (VBool (a <= b))
evalBinOp OpGtEq a b = return (VBool (a >= b))

-- Membership
evalBinOp OpIn x (VList ref) = do
  elems <- readIORef ref
  return (VBool (x `elem` elems))
evalBinOp OpIn (VStr c) (VStr s) = return (VBool (c `isInfixOf` s))
  where isInfixOf sub str = any (isPrefixOf sub) (tails str)
        tails []     = [[]]
        tails xs@(_:rest) = xs : tails rest
        isPrefixOf [] _ = True
        isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys
        isPrefixOf _ [] = False
evalBinOp OpIn x (VDict ref) = do
  m <- readIORef ref
  return (VBool (Map.member x m))
evalBinOp OpIn x (VSet ref) = do
  elems <- readIORef ref
  return (VBool (x `elem` elems))
evalBinOp OpNotIn x container = do
  VBool b <- evalBinOp OpIn x container
  return (VBool (not b))

-- Bitwise
evalBinOp OpBitAnd (VInt a) (VInt b) = return (VInt (a .&. b))
  where (.&.) = (Prelude.&)
evalBinOp OpBitOr  (VInt a) (VInt b) = return (VInt (a .|. b))
  where (.|.) = (Prelude.|)
evalBinOp OpBitXor (VInt a) (VInt b) = return (VInt (a `xor` b))
  where xor x y = (x .|. y) - (x .&. y)
evalBinOp OpShiftL (VInt a) (VInt b) = return (VInt (a * 2^b))
evalBinOp OpShiftR (VInt a) (VInt b) = return (VInt (a `div` 2^b))

-- String concat (<>)
evalBinOp OpConcat (VStr a) (VStr b) = return (VStr (a ++ b))
evalBinOp OpConcat (VList r1) (VList r2) = do
  l1 <- readIORef r1; l2 <- readIORef r2
  ref <- newIORef (l1 ++ l2); return (VList ref)

-- Cons (:)
evalBinOp OpCons x (VList ref) = do
  elems <- readIORef ref
  ref'  <- newIORef (x : elems)
  return (VList ref')

-- Fallback
evalBinOp op a b = throwIO (MFTypeError
  { mfMsg = "unsupported operand types for " ++ show op ++
            ": '" ++ typeNameOf a ++ "' and '" ++ typeNameOf b ++ "'"
  , mfPos = noPos })

-- Unary operations
evalExpr env (EUnOp op expr) = do
  v <- evalExpr env expr
  evalUnOp op v

evalUnOp :: UnOp -> Value -> IO Value
evalUnOp OpNeg (VInt n)   = return (VInt   (negate n))
evalUnOp OpNeg (VFloat f) = return (VFloat (negate f))
evalUnOp OpNot v          = return (VBool (not (isTruthy v)))
evalUnOp OpBitNot (VInt n) = return (VInt (complement n))
  where complement x = -(x + 1)
evalUnOp OpAbs (VInt n)   = return (VInt   (abs n))
evalUnOp OpAbs (VFloat f) = return (VFloat (abs f))
evalUnOp op v = throwIO (MFTypeError
  { mfMsg = "unsupported operand type for unary op: '" ++ typeNameOf v ++ "'"
  , mfPos = noPos })

-- =============================================================================
-- PIPE OPERATOR:  lhs |> rhs
-- Apply lhs as argument to rhs function
-- =============================================================================
evalExpr env (EPipe lhs rhs) = do
  lval <- evalExpr env lhs
  fn   <- evalExpr env rhs
  applyFunction fn [lval]

-- =============================================================================
-- COMPOSE OPERATOR: f . g
-- Creates a new function h(x) = f(g(x))
-- =============================================================================
evalExpr env (ECompose f g) = do
  fv <- evalExpr env f
  gv <- evalExpr env g
  return (VBuiltin "<composed>"
    (\args _ -> do
      gresult <- applyFunction gv args
      applyFunction fv [gresult]))

-- =============================================================================
-- BIND OPERATOR: ma >> f
-- Applies f to the result of ma (like monadic bind but simplified)
-- =============================================================================
evalExpr env (EBind ma f) = do
  a  <- evalExpr env ma
  fn <- evalExpr env f
  applyFunction fn [a]

-- Function application: f(args...)
evalExpr env (EApp fn args) = do
  fnVal   <- evalExpr env fn
  (posArgs, kwArgs) <- evalArgs env args
  applyFunctionKw fnVal posArgs kwArgs

evalArgs :: Env -> [Arg] -> IO ([Value], [(String, Value)])
evalArgs env args = do
  pos <- concat <$> mapM (evalPosArg env) [a | a@(ArgPos _) <- args]
                                  -- simplified: handle all arg types
  kw  <- mapM (evalKwArg env) [a | a@(ArgKw _ _) <- args]
  stars <- concat <$> mapM (evalStarArg env) [a | a@(ArgStar _) <- args]
  return (pos ++ stars, kw)

evalPosArg :: Env -> Arg -> IO [Value]
evalPosArg env (ArgPos e) = fmap (:[]) (evalExpr env e)
evalPosArg _ _ = return []

evalKwArg :: Env -> Arg -> IO (String, Value)
evalKwArg env (ArgKw k v) = do val <- evalExpr env v; return (k, val)
evalKwArg _ _ = return ("", VNone)

evalStarArg :: Env -> Arg -> IO [Value]
evalStarArg env (ArgStar e) = do
  v <- evalExpr env e
  valueToList v
evalStarArg _ _ = return []

-- Lambda expression
evalExpr env (ELambda params body) = do
  return (VClosure params [SReturn (Just body)] env "<lambda>")

-- If-then-else expression
evalExpr env (EIf cond thenE elseE) = do
  cv <- evalExpr env cond
  if isTruthy cv
    then evalExpr env thenE
    else evalExpr env elseE

-- Match expression (returns a value)
evalExpr env (EMatch scrutinee arms) = do
  val    <- evalExpr env scrutinee
  result <- tryArms val arms
  case result of
    Nothing -> throwIO (MFPatternFail
      { mfMsg = "Non-exhaustive patterns in match expression"
      , mfPos = noPos })
    Just v  -> return v
  where
    tryArms _ []     = return Nothing
    tryArms v (arm:rest) = do
      matchResult <- tryMatchArm env v arm
      case matchResult of
        Nothing -> tryArms v rest
        Just (body, armEnv) -> do
          guardOk <- case maGuard arm of
            Nothing -> return True
            Just g  -> fmap isTruthy (evalExpr armEnv g)
          if guardOk
            then do
              -- Execute body statements, collect return value
              result <- evalStmts armEnv body
              return (Just result)
            else tryArms v rest

-- List literal
evalExpr env (EList elems) = do
  vals <- mapM (evalExpr env) elems
  ref  <- newIORef vals
  return (VList ref)

-- Tuple literal
evalExpr env (ETuple elems) = do
  vals <- mapM (evalExpr env) elems
  return (VTuple vals)

-- Dict literal
evalExpr env (EDict kvs) = do
  pairs <- mapM (\(k, v) -> do
    kv <- evalExpr env k
    vv <- evalExpr env v
    return (kv, vv)) kvs
  ref <- newIORef (Map.fromList pairs)
  return (VDict ref)

-- Set literal
evalExpr env (ESet elems) = do
  vals <- mapM (evalExpr env) elems
  ref  <- newIORef (nub vals)
  return (VSet ref)

-- Range: [a..b] or [a..b:step]
evalExpr env (ERange startE endE stepE) = do
  startV <- evalExpr env startE
  endV   <- evalExpr env endE
  stepV  <- maybe (return (VInt 1)) (evalExpr env) stepE
  let start = toInt startV
      end_  = toInt endV
      step  = toInt stepV
  when (step == 0) $ throwIO (MFValueError { mfMsg = "range step cannot be zero", mfPos = noPos })
  let nums = if step > 0
               then [start, start+step .. end_]
               else [start, start+step .. end_]
  ref <- newIORef (map VInt nums)
  return (VList ref)

-- List comprehension: [expr for pat in iter if cond]
evalExpr env (EListComp body clauses) = do
  results <- runComprehension env clauses (\e -> evalExpr e body)
  ref     <- newIORef results
  return (VList ref)

runComprehension :: Env -> [CompClause] -> (Env -> IO Value) -> IO [Value]
runComprehension env [] bodyFn = fmap (:[]) (bodyFn env)
runComprehension env (CCFor pat iterE : rest) bodyFn = do
  iterVal <- evalExpr env iterE
  elems   <- valueToList iterVal
  concat <$> mapM (\x -> do
    loopEnv <- newChildEnv "<listcomp>" env
    ok <- matchPattern loopEnv pat x
    if ok
      then runComprehension loopEnv rest bodyFn
      else return []) elems
runComprehension env (CCIf condE : rest) bodyFn = do
  cv <- evalExpr env condE
  if isTruthy cv
    then runComprehension env rest bodyFn
    else return []

-- Dict comprehension: {k: v for ...}
evalExpr env (EDictComp keyE valE clauses) = do
  pairs <- runComprehension2 env clauses (\e -> do
    k <- evalExpr e keyE
    v <- evalExpr e valE
    return (k, v))
  ref <- newIORef (Map.fromList pairs)
  return (VDict ref)

runComprehension2 :: Env -> [CompClause] -> (Env -> IO a) -> IO [a]
runComprehension2 env [] bodyFn = fmap (:[]) (bodyFn env)
runComprehension2 env (CCFor pat iterE : rest) bodyFn = do
  iterVal <- evalExpr env iterE
  elems   <- valueToList iterVal
  concat <$> mapM (\x -> do
    loopEnv <- newChildEnv "<dictcomp>" env
    ok <- matchPattern loopEnv pat x
    if ok
      then runComprehension2 loopEnv rest bodyFn
      else return []) elems
runComprehension2 env (CCIf condE : rest) bodyFn = do
  cv <- evalExpr env condE
  if isTruthy cv
    then runComprehension2 env rest bodyFn
    else return []

-- Set comprehension
evalExpr env (ESetComp body clauses) = do
  results <- runComprehension env clauses (\e -> evalExpr e body)
  ref     <- newIORef (nub results)
  return (VSet ref)

-- Record creation
evalExpr env (ERecordCreate name fields) = do
  pairs <- mapM (\(k, v) -> do val <- evalExpr env v; return (k, val)) fields
  ref   <- newIORef (Map.fromList pairs)
  return (VRecord name ref)

-- Record update (functional)
evalExpr env (ERecordUpdate expr updates) = do
  base <- evalExpr env expr
  case base of
    VRecord name ref -> do
      fields <- readIORef ref
      newPairs <- mapM (\(k, v) -> do val <- evalExpr env v; return (k, val)) updates
      let newFields = Map.union (Map.fromList newPairs) fields
      ref' <- newIORef newFields
      return (VRecord name ref')
    _ -> throwIO (MFTypeError { mfMsg = "Record update requires a record value", mfPos = noPos })

-- Type annotation (just evaluate the expression, ignore annotation at runtime)
evalExpr env (EAnnotated e _) = evalExpr env e

-- =============================================================================
-- FUNCTION APPLICATION
-- =============================================================================

applyFunction :: Value -> [Value] -> IO Value
applyFunction fn args = applyFunctionKw fn args []

applyFunctionKw :: Value -> [Value] -> [(String, Value)] -> IO Value
applyFunctionKw (VBuiltin _ f) posArgs kwArgs = f posArgs kwArgs

applyFunctionKw (VClosure params body closureEnv name) posArgs kwArgs = do
  -- Create a new scope with closure as parent
  callEnv <- newChildEnv ("<call:" ++ name ++ ">") closureEnv
  -- Bind parameters
  bindParams callEnv params posArgs kwArgs
  -- Execute body
  result  <- runFunctionBody callEnv body
  return result

applyFunctionKw v _ _ = throwIO (MFTypeError
  { mfMsg = "'" ++ typeNameOf v ++ "' object is not callable"
  , mfPos = noPos })

bindParams :: Env -> [Param] -> [Value] -> [(String, Value)] -> IO ()
bindParams env params posArgs kwArgs = go params posArgs
  where
    go []           []       = return ()
    go []           (_:_)    = return ()  -- extra args ignored (Python-like)
    go (p:ps)       args     = case p of
      ParamSimple name -> case lookup name kwArgs of
        Just v  -> defineVar name v env >> go ps args
        Nothing -> case args of
          []     -> return ()  -- no default, skip (would be NameError at use)
          (a:as) -> defineVar name a env >> go ps as
      ParamTyped name _ -> go (ParamSimple name : ps) args
      ParamDefault name defExpr -> case lookup name kwArgs of
        Just v  -> do defineVar name v env; go ps args
        Nothing -> case args of
          []     -> do
            defVal <- evalExpr env defExpr
            defineVar name defVal env
            go ps []
          (a:as) -> defineVar name a env >> go ps as
      ParamPat pat -> case args of
        []     -> return ()
        (a:as) -> do ok <- matchPattern env pat a; go ps as
      ParamStar name -> do
        ref <- newIORef args
        defineVar name (VList ref) env
      ParamDStar name -> do
        let m = Map.fromList [(VStr k, v) | (k, v) <- kwArgs]
        ref <- newIORef m
        defineVar name (VDict ref) env

runFunctionBody :: Env -> [Stmt] -> IO Value
runFunctionBody env stmts =
  catch (evalStmts env stmts >> return VNone) handler
  where
    handler (MFReturnSignal v) = return v
    handler ex = throwIO ex

-- =============================================================================
-- VALUE TO LIST
-- =============================================================================

valueToList :: Value -> IO [Value]
valueToList (VList ref)    = readIORef ref
valueToList (VTuple vs)    = return vs
valueToList (VStr s)       = return (map (VStr . (:[])) s)
valueToList (VSet ref)     = readIORef ref
valueToList (VIterator ref) = readIORef ref
valueToList (VDict ref)    = fmap (map fst . Map.toList) (readIORef ref)
valueToList (VLazyList ref) = do
  ll <- readIORef ref
  go ll
  where
    go LNil          = return []
    go (LCons v mkRest) = do
      rest <- mkRest
      vs   <- go rest
      return (v : vs)
valueToList v = throwIO (MFTypeError
  { mfMsg = "object is not iterable: " ++ typeNameOf v, mfPos = noPos })

-- =============================================================================
-- STRING METHODS
-- =============================================================================

evalStringMethod :: String -> String -> IO Value
evalStringMethod s method = case method of
  "upper"      -> return (VStr (map toUpper s))
  "lower"      -> return (VStr (map toLower s))
  "strip"      -> return (VStr (trim s))
  "lstrip"     -> return (VStr (dropWhile isSpace s))
  "rstrip"     -> return (VStr (reverse (dropWhile isSpace (reverse s))))
  "split"      -> return (VBuiltin "split" (\args _ -> do
                    let sep = case args of { [VStr sep] -> sep; _ -> " " }
                    ref <- newIORef (map VStr (splitOn sep s))
                    return (VList ref)))
  "join"       -> return (VBuiltin "join" (\[v] _ -> do
                    elems <- valueToList v
                    strs  <- mapM (displayValue) elems
                    return (VStr (intercalate s strs))))
  "replace"    -> return (VBuiltin "replace" (\[VStr old, VStr new] _ ->
                    return (VStr (replace old new s))))
  "startswith" -> return (VBuiltin "startswith" (\[VStr pre] _ ->
                    return (VBool (isPrefixOf pre s))))
  "endswith"   -> return (VBuiltin "endswith" (\[VStr suf] _ ->
                    return (VBool (isSuffixOf suf s))))
  "find"       -> return (VBuiltin "find" (\[VStr sub] _ ->
                    return (VInt (findSubstr sub s))))
  "count"      -> return (VBuiltin "count" (\[VStr sub] _ ->
                    return (VInt (countOccurrences sub s))))
  "format"     -> return (VBuiltin "format" (\args _ -> do
                    result <- applyFormat s args []
                    return (VStr result)))
  "encode"     -> return (VStr s)
  "decode"     -> return (VStr s)
  "isalpha"    -> return (VBool (not (null s) && all isAlpha s))
  "isdigit"    -> return (VBool (not (null s) && all isDigit s))
  "isalnum"    -> return (VBool (not (null s) && all (\c -> isAlpha c || isDigit c) s))
  "isspace"    -> return (VBool (not (null s) && all isSpace s))
  "isupper"    -> return (VBool (not (null s) && all isUpper s))
  "islower"    -> return (VBool (not (null s) && all isLower s))
  "center"     -> return (VBuiltin "center" (\[VInt n] _ -> do
                    let pad = max 0 (n - length s)
                        lp  = pad `div` 2
                        rp  = pad - lp
                    return (VStr (replicate lp ' ' ++ s ++ replicate rp ' '))))
  "ljust"      -> return (VBuiltin "ljust" (\[VInt n] _ ->
                    return (VStr (s ++ replicate (max 0 (n - length s)) ' '))))
  "rjust"      -> return (VBuiltin "rjust" (\[VInt n] _ ->
                    return (VStr (replicate (max 0 (n - length s)) ' ' ++ s))))
  "zfill"      -> return (VBuiltin "zfill" (\[VInt n] _ ->
                    return (VStr (replicate (max 0 (n - length s)) '0' ++ s))))
  "title"      -> return (VStr (titleCase s))
  "swapcase"   -> return (VStr (map swapCase s))
  "expandtabs" -> return (VStr s)  -- simplified
  _            -> throwIO (MFKeyError
    { mfMsg = "str has no method '" ++ method ++ "'"
    , mfPos = noPos })

titleCase :: String -> String
titleCase []       = []
titleCase (' ':cs) = ' ' : titleCase cs
titleCase (c:cs)   = toUpper c : map toLower (takeWhile (/= ' ') cs)
                     ++ titleCase (dropWhile (/= ' ') cs)

swapCase :: Char -> Char
swapCase c | isUpper c = toLower c
            | isLower c = toUpper c
            | otherwise = c

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

splitOn :: String -> String -> [String]
splitOn _ "" = [""]
splitOn sep str = go str
  where
    sepLen = length sep
    go "" = [""]
    go s  = if isPrefixOf sep s
              then "" : go (drop sepLen s)
              else case go (tail s) of
                []     -> [[head s]]
                (w:ws) -> (head s : w) : ws

replace :: String -> String -> String -> String
replace _ _ "" = ""
replace old new s
  | old `isPrefixOf` s = new ++ replace old new (drop (length old) s)
  | otherwise          = head s : replace old new (tail s)

findSubstr :: String -> String -> Int
findSubstr sub str = go 0 str
  where
    go _ []  = -1
    go i s   = if sub `isPrefixOf` s then i else go (i+1) (tail s)

countOccurrences :: String -> String -> Int
countOccurrences sub str = go str 0
  where
    go "" acc = acc
    go s  acc = if sub `isPrefixOf` s
                  then go (drop (length sub) s) (acc + 1)
                  else go (tail s) acc

applyFormat :: String -> [Value] -> [(String, Value)] -> IO String
applyFormat [] _ _ = return ""
applyFormat ('{':'}':rest) (a:args) kwargs = do
  s <- displayValue a
  r <- applyFormat rest args kwargs
  return (s ++ r)
applyFormat ('{':cs) args kwargs =
  let (key, rest) = break (== '}') cs
  in case rest of
    ('}':r) -> do
      val <- case reads key :: [(Int, String)] of
               [(n, "")] -> return (if n < length args then args !! n else VNone)
               _         -> case lookup key kwargs of
                              Just v  -> return v
                              Nothing -> return VNone
      s <- displayValue val
      r' <- applyFormat r args kwargs
      return (s ++ r')
    _ -> fmap ('{':) (applyFormat cs args kwargs)
applyFormat (c:cs) args kwargs = fmap (c:) (applyFormat cs args kwargs)

-- =============================================================================
-- LIST METHODS
-- =============================================================================

evalListMethod :: IORef [Value] -> String -> IO Value
evalListMethod ref method = case method of
  "append"  -> return (VBuiltin "append"  (\[v] _ -> modifyIORef ref (++ [v]) >> return VNone))
  "extend"  -> return (VBuiltin "extend"  (\[v] _ -> do
                 elems <- valueToList v
                 modifyIORef ref (++ elems)
                 return VNone))
  "insert"  -> return (VBuiltin "insert"  (\[VInt i, v] _ -> do
                 modifyIORef ref (\xs -> take i xs ++ [v] ++ drop i xs)
                 return VNone))
  "remove"  -> return (VBuiltin "remove"  (\[v] _ -> do
                 modifyIORef ref (removeFirst v)
                 return VNone))
  "pop"     -> return (VBuiltin "pop"     (\args _ -> do
                 elems <- readIORef ref
                 case args of
                   [] -> do
                     let v = last elems
                     writeIORef ref (init elems)
                     return v
                   [VInt i] -> do
                     let v = elems !! i
                     writeIORef ref (deleteAt i elems)
                     return v
                   _ -> return VNone))
  "clear"   -> return (VBuiltin "clear"   (\_ _ -> writeIORef ref [] >> return VNone))
  "copy"    -> return (VBuiltin "copy"    (\_ _ -> do
                 elems <- readIORef ref
                 ref'  <- newIORef elems
                 return (VList ref')))
  "count"   -> return (VBuiltin "count"   (\[v] _ -> do
                 elems <- readIORef ref
                 return (VInt (length (filter (== v) elems)))))
  "index"   -> return (VBuiltin "index"   (\[v] _ -> do
                 elems <- readIORef ref
                 case elemIndex v elems of
                   Just i  -> return (VInt i)
                   Nothing -> throwIO (MFValueError { mfMsg = "value not in list", mfPos = noPos })))
  "reverse" -> return (VBuiltin "reverse" (\_ _ -> modifyIORef ref reverse >> return VNone))
  "sort"    -> return (VBuiltin "sort"    (\_ kwargs -> do
                 modifyIORef ref sort
                 return VNone))
  _         -> throwIO (MFKeyError
    { mfMsg = "list has no method '" ++ method ++ "'"
    , mfPos = noPos })

removeFirst :: Eq a => a -> [a] -> [a]
removeFirst _ []     = []
removeFirst x (y:ys) = if x == y then ys else y : removeFirst x ys

modifyIORef :: IORef a -> (a -> a) -> IO ()
modifyIORef ref f = readIORef ref >>= writeIORef ref . f

elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex _ []     = Nothing
elemIndex x (y:ys) = if x == y then Just 0 else fmap (+1) (elemIndex x ys)

-- =============================================================================
-- DICT METHODS
-- =============================================================================

evalDictMethod :: IORef (Map Value Value) -> String -> IO Value
evalDictMethod ref method = case method of
  "keys"    -> return (VBuiltin "keys"    (\_ _ -> do
                 m <- readIORef ref
                 r <- newIORef (Map.keys m)
                 return (VList r)))
  "values"  -> return (VBuiltin "values"  (\_ _ -> do
                 m <- readIORef ref
                 r <- newIORef (Map.elems m)
                 return (VList r)))
  "items"   -> return (VBuiltin "items"   (\_ _ -> do
                 m <- readIORef ref
                 let pairs = map (\(k,v) -> VTuple [k,v]) (Map.toList m)
                 r <- newIORef pairs
                 return (VList r)))
  "get"     -> return (VBuiltin "get"     (\args _ -> do
                 m <- readIORef ref
                 case args of
                   [k]      -> return (Map.findWithDefault VNone k m)
                   [k, def] -> return (Map.findWithDefault def k m)
                   _        -> return VNone))
  "update"  -> return (VBuiltin "update"  (\[v] _ -> do
                 case v of
                   VDict r2 -> do
                     m2 <- readIORef r2
                     modifyIORef ref (Map.union m2)
                   _ -> return ()
                 return VNone))
  "pop"     -> return (VBuiltin "pop"     (\args _ -> do
                 m <- readIORef ref
                 case args of
                   [k] -> do
                     writeIORef ref (Map.delete k m)
                     return (Map.findWithDefault VNone k m)
                   [k, def] -> do
                     writeIORef ref (Map.delete k m)
                     return (Map.findWithDefault def k m)
                   _ -> return VNone))
  "clear"   -> return (VBuiltin "clear"   (\_ _ -> writeIORef ref Map.empty >> return VNone))
  "copy"    -> return (VBuiltin "copy"    (\_ _ -> do
                 m <- readIORef ref
                 r <- newIORef m
                 return (VDict r)))
  "setdefault" -> return (VBuiltin "setdefault" (\args _ -> do
                    m <- readIORef ref
                    case args of
                      [k]      -> do
                        let v = Map.findWithDefault VNone k m
                        modifyIORef ref (Map.insertWith (\_ old -> old) k VNone)
                        return v
                      [k, def] -> do
                        let v = Map.findWithDefault def k m
                        modifyIORef ref (Map.insertWith (\_ old -> old) k def)
                        return v
                      _ -> return VNone))
  _         -> throwIO (MFKeyError
    { mfMsg = "dict has no method '" ++ method ++ "'"
    , mfPos = noPos })

-- =============================================================================
-- HELPER: when
-- =============================================================================

when :: Bool -> IO () -> IO ()
when True  action = action
when False _      = return ()

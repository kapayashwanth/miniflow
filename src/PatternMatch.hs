-- =============================================================================
-- MiniFlow Language Interpreter
-- src/PatternMatch.hs  -- Pattern matching engine
-- =============================================================================
module PatternMatch
  ( matchPattern
  , matchPatterns
  , tryMatchArm
  , destructure
  ) where

import Types
import Environment
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (throwIO, catch, SomeException)

-- =============================================================================
-- CORE MATCHING
-- =============================================================================

-- | Try to match a value against a pattern in the given environment.
--   Returns True if the match succeeded (and binds any variables).
--   Returns False if the match fails (no side effects on failure).
matchPattern :: Env -> Pattern -> Value -> IO Bool
matchPattern env pat val = case pat of

  -- Wildcard always succeeds, no bindings
  PWild -> return True

  -- Variable: always succeeds, binds name to value
  PVar name -> do
    defineVar name val env
    return True

  -- Literal patterns: succeed only if value equals literal
  PNum n  -> return (val == VFloat n || val == VInt (round n))
  PStr s  -> return (val == VStr s)
  PBool b -> return (val == VBool b)
  PNone   -> return (val == VNone)

  -- List pattern: matches list with exactly matching length
  PList pats -> case val of
    VList ref -> do
      elems <- readIORef ref
      if length elems /= length pats
        then return False
        else matchAll env pats elems
    _ -> return False

  -- Tuple pattern
  PTuple pats -> case val of
    VTuple elems ->
      if length elems /= length pats
        then return False
        else matchAll env pats elems
    _ -> return False

  -- Cons pattern: h:t matches head and tail of non-empty list
  PCons hpat tpat -> case val of
    VList ref -> do
      elems <- readIORef ref
      case elems of
        []     -> return False
        (x:xs) -> do
          h <- matchPattern env hpat x
          if h
            then do
              tailRef <- newIORef xs
              matchPattern env tpat (VList tailRef)
            else return False
    _ -> return False

  -- Record pattern: matches record by name, then checks fields
  PRecord rname fieldPats -> case val of
    VRecord vname fieldsRef -> do
      if rname /= vname
        then return False
        else do
          fields <- readIORef fieldsRef
          matchFields env fieldPats fields
    _ -> return False

  -- As pattern: bind the whole value, then match sub-pattern
  PAs inner name -> do
    defineVar name val env
    matchPattern env inner val

  -- Or pattern: try left, then right
  POr left right -> do
    result <- matchPattern env left val
    if result
      then return True
      else matchPattern env right val

  -- Typed pattern: match underlying pattern (type check is optional)
  PTyped inner _ty -> matchPattern env inner val

-- | Match a list of patterns against a list of values (all must succeed)
matchAll :: Env -> [Pattern] -> [Value] -> IO Bool
matchAll _ [] [] = return True
matchAll _ [] _  = return False
matchAll _ _  [] = return False
matchAll env (p:ps) (v:vs) = do
  ok <- matchPattern env p v
  if ok
    then matchAll env ps vs
    else return False

-- | Match record fields: each named field pattern must succeed
matchFields :: Env -> [(String, Pattern)] -> Map String Value -> IO Bool
matchFields _ [] _ = return True
matchFields env ((fname, fpat):rest) fields =
  case Map.lookup fname fields of
    Nothing -> return False
    Just v  -> do
      ok <- matchPattern env fpat v
      if ok
        then matchFields env rest fields
        else return False

-- =============================================================================
-- MULTI-PATTERN MATCHING (for function parameter lists)
-- =============================================================================

-- | Match a list of argument values against a list of patterns.
--   Used for function application with pattern-matching parameters.
matchPatterns :: Env -> [Param] -> [Value] -> [(String, Value)] -> IO Bool
matchPatterns env params posArgs kwArgs = go params posArgs
  where
    go [] [] = return True
    go [] _  = return False  -- too many args
    go (ParamSimple name : ps) (v:vs) = do
      defineVar name v env
      go ps vs
    go (ParamTyped name _ty : ps) (v:vs) = do
      defineVar name v env
      go ps vs
    go (ParamDefault name defExpr : ps) vs = do
      -- Use kwarg if present, else positional, else default
      let val = case lookup name kwArgs of
                  Just kv -> Just kv
                  Nothing -> case vs of
                    (v:_) -> Just v
                    []    -> Nothing
      case val of
        Just v -> defineVar name v env >> go ps (drop 1 vs)
        Nothing -> return False  -- no default evaluation here; caller handles
    go (ParamPat pat : ps) (v:vs) = do
      ok <- matchPattern env pat v
      if ok then go ps vs else return False
    go (ParamStar name : _) remaining = do
      ref <- newIORef remaining
      defineVar name (VList ref) env
      return True
    go (ParamDStar name : _) _ = do
      let kvMap = Map.fromList [(VStr k, v) | (k, v) <- kwArgs]
      ref <- newIORef kvMap
      defineVar name (VDict ref) env
      return True
    go _ [] = return False

-- =============================================================================
-- MATCH ARM EVALUATION
-- =============================================================================

-- | Try a single match arm against a scrutinee value.
--   Returns Just the arm body if matched, Nothing otherwise.
--   Binding side effects only occur if the arm matches.
tryMatchArm :: Env -> Value -> MatchArm -> IO (Maybe ([Stmt], Env))
tryMatchArm env val (MatchArm pat guardM body) = do
  -- Create a fresh child environment for bindings
  armEnv <- newChildEnv "<match_arm>" env
  ok <- matchPattern armEnv pat val
  if not ok
    then return Nothing
    else case guardM of
      Nothing -> return (Just (body, armEnv))
      Just _guardExpr -> do
        -- Guard expression evaluation is handled by the Evaluator
        -- We pass the unevaluated guard back; caller evaluates it
        return (Just (body, armEnv))

-- =============================================================================
-- DESTRUCTURING ASSIGNMENT
-- =============================================================================

-- | Destructure a value into bindings in the environment.
--   Used for:  let (a, b) = (1, 2)
--              for (k, v) in dict.items()
destructure :: Env -> Pattern -> Value -> IO ()
destructure env pat val = do
  ok <- matchPattern env pat val
  if ok
    then return ()
    else throwIO (MFPatternFail
      { mfMsg = "Pattern match failed in destructuring"
      , mfPos = noPos
      })

-- =============================================================================
-- EXHAUSTIVENESS CHECKING (static, warning-only)
-- =============================================================================

data MatchExhaustiveness
  = Exhaustive
  | NonExhaustive [String]  -- which cases are missing
  deriving (Show)

checkExhaustiveness :: [MatchArm] -> MatchExhaustiveness
checkExhaustiveness arms =
  let pats = map maPattern arms
  in if any isWildOrVar pats
       then Exhaustive
       else NonExhaustive ["(unknown cases)"]

isWildOrVar :: Pattern -> Bool
isWildOrVar PWild     = True
isWildOrVar (PVar _)  = True
isWildOrVar (PAs _ _) = True
isWildOrVar _         = False

-- =============================================================================
-- PATTERN PRINTING (for error messages)
-- =============================================================================

showPattern :: Pattern -> String
showPattern PWild               = "_"
showPattern (PVar name)         = name
showPattern (PNum n)            = show n
showPattern (PStr s)            = show s
showPattern (PBool True)        = "True"
showPattern (PBool False)       = "False"
showPattern PNone               = "None"
showPattern (PList pats)        = "[" ++ commaSep (map showPattern pats) ++ "]"
showPattern (PTuple pats)       = "(" ++ commaSep (map showPattern pats) ++ ")"
showPattern (PCons h t)         = showPattern h ++ ":" ++ showPattern t
showPattern (PRecord name flds) = name ++ " {" ++ commaSep (map showField flds) ++ "}"
  where showField (k, p) = k ++ ": " ++ showPattern p
showPattern (PAs inner name)    = showPattern inner ++ " as " ++ name
showPattern (POr l r)           = showPattern l ++ " | " ++ showPattern r
showPattern (PTyped p ty)       = showPattern p ++ " : " ++ showType ty

commaSep :: [String] -> String
commaSep []     = ""
commaSep [x]    = x
commaSep (x:xs) = x ++ ", " ++ commaSep xs

-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Pretty.hs  -- Pretty-printing values, AST nodes, and errors
-- =============================================================================
module Pretty
  ( prettyValue
  , prettyValueIO
  , prettyExpr
  , prettyStmt
  , prettyType
  , prettyError
  , reprValue
  , displayValue
  , prettyList
  , prettyDict
  ) where

import Types
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)
import Numeric (showHex, showOct)

-- =============================================================================
-- VALUE PRETTY-PRINTING
-- =============================================================================

-- | Human-readable display string (like Python's str())
displayValue :: Value -> IO String
displayValue VNone          = return "None"
displayValue (VBool True)   = return "True"
displayValue (VBool False)  = return "False"
displayValue (VInt n)       = return (show n)
displayValue (VFloat f)     = return (formatFloat f)
displayValue (VStr s)       = return s
displayValue (VTuple vs)    = do
  strs <- mapM displayValue vs
  case strs of
    [s] -> return ("(" ++ s ++ ",)")
    _   -> return ("(" ++ intercalate ", " strs ++ ")")
displayValue (VList ref)    = do
  vs <- readIORef ref
  strs <- mapM reprValue vs
  return ("[" ++ intercalate ", " strs ++ "]")
displayValue (VDict ref)    = do
  m <- readIORef ref
  pairs <- mapM (\(k,v) -> do
    ks <- reprValue k
    vs <- reprValue v
    return (ks ++ ": " ++ vs)) (Map.toAscList m)
  return ("{" ++ intercalate ", " pairs ++ "}")
displayValue (VSet ref)     = do
  vs <- readIORef ref
  strs <- mapM reprValue vs
  case strs of
    [] -> return "set()"
    _  -> return ("{" ++ intercalate ", " strs ++ "}")
displayValue (VClosure{vcName=n}) = return ("<function " ++ n ++ ">")
displayValue (VBuiltin n _) = return ("<built-in function " ++ n ++ ">")
displayValue (VRecord name ref) = do
  fields <- readIORef ref
  pairs  <- mapM (\(k,v) -> do
    vs <- reprValue v
    return (k ++ "=" ++ vs)) (Map.toAscList fields)
  return (name ++ "(" ++ intercalate ", " pairs ++ ")")
displayValue (VType t)      = return ("<class '" ++ t ++ "'>")
displayValue (VLazyList _)  = return "<lazy_list>"
displayValue (VIterator _)  = return "<list_iterator>"
displayValue (VModule n _)  = return ("<module '" ++ n ++ "'>")
displayValue (VNativeExn e) = return e

-- | Python repr() style (strings get quotes, etc.)
reprValue :: Value -> IO String
reprValue (VStr s)    = return (show s)   -- adds quotes and escapes
reprValue (VBool True) = return "True"
reprValue (VBool False) = return "False"
reprValue VNone       = return "None"
reprValue v           = displayValue v

-- | Synchronous version (without IO; uses unsafe reads where needed)
prettyValue :: Value -> String
prettyValue VNone          = "None"
prettyValue (VBool True)   = "True"
prettyValue (VBool False)  = "False"
prettyValue (VInt n)       = show n
prettyValue (VFloat f)     = formatFloat f
prettyValue (VStr s)       = s
prettyValue (VTuple vs)    = "(" ++ intercalate ", " (map prettyValue vs) ++ ")"
prettyValue (VList _)      = "<list>"
prettyValue (VDict _)      = "<dict>"
prettyValue (VSet _)       = "<set>"
prettyValue (VClosure{vcName=n}) = "<function " ++ n ++ ">"
prettyValue (VBuiltin n _) = "<built-in function " ++ n ++ ">"
prettyValue (VRecord n _)  = "<" ++ n ++ " record>"
prettyValue (VType t)      = "<class '" ++ t ++ "'>"
prettyValue (VLazyList _)  = "<lazy_list>"
prettyValue (VIterator _)  = "<iterator>"
prettyValue (VModule n _)  = "<module '" ++ n ++ "'>"
prettyValue (VNativeExn e) = e

-- | IO version that reads IORef contents for accurate printing
prettyValueIO :: Value -> IO String
prettyValueIO = displayValue

-- =============================================================================
-- NUMBER FORMATTING
-- =============================================================================

formatFloat :: Double -> String
formatFloat f
  | isInfinite f && f > 0 = "inf"
  | isInfinite f && f < 0 = "-inf"
  | isNaN f               = "nan"
  | f == fromIntegral (round f :: Int) && abs f < 1e15
                          = show (round f :: Int) ++ ".0"
  | otherwise             = show f

-- =============================================================================
-- LIST / DICT PRETTY HELPERS
-- =============================================================================

prettyList :: [Value] -> IO String
prettyList vs = do
  strs <- mapM reprValue vs
  return ("[" ++ intercalate ", " strs ++ "]")

prettyDict :: Map Value Value -> IO String
prettyDict m = do
  pairs <- mapM (\(k,v) -> do
    ks <- reprValue k
    vs <- reprValue v
    return (ks ++ ": " ++ vs)) (Map.toAscList m)
  return ("{" ++ intercalate ", " pairs ++ "}")

-- =============================================================================
-- AST PRETTY-PRINTING
-- =============================================================================

prettyExpr :: Expr -> String
prettyExpr (EInt n)       = show n
prettyExpr (EFloat f)     = formatFloat f
prettyExpr (EStr s)       = show s
prettyExpr (EBool True)   = "True"
prettyExpr (EBool False)  = "False"
prettyExpr ENone          = "None"
prettyExpr EEllipsis      = "..."
prettyExpr (EVar name)    = name
prettyExpr (EField e f)   = prettyExpr e ++ "." ++ f
prettyExpr (EIndex e i)   = prettyExpr e ++ "[" ++ prettyExpr i ++ "]"
prettyExpr (ESlice e a b c) =
  prettyExpr e ++ "[" ++
  maybe "" prettyExpr a ++ ":" ++
  maybe "" prettyExpr b ++
  maybe "" (\s -> ":" ++ prettyExpr s) c ++ "]"
prettyExpr (EList es)     = "[" ++ intercalate ", " (map prettyExpr es) ++ "]"
prettyExpr (ETuple es)    = "(" ++ intercalate ", " (map prettyExpr es) ++ ")"
prettyExpr (EDict kvs)    =
  "{" ++ intercalate ", " (map (\(k,v) -> prettyExpr k ++ ": " ++ prettyExpr v) kvs) ++ "}"
prettyExpr (ESet es)      = "{" ++ intercalate ", " (map prettyExpr es) ++ "}"
prettyExpr (ERange a b s) =
  "[" ++ prettyExpr a ++ ".." ++ prettyExpr b ++
  maybe "" (\st -> ":" ++ prettyExpr st) s ++ "]"
prettyExpr (EBinOp op l r) =
  "(" ++ prettyExpr l ++ " " ++ prettyOp op ++ " " ++ prettyExpr r ++ ")"
prettyExpr (EUnOp op e)   = prettyUnOp op ++ prettyExpr e
prettyExpr (EPipe l r)    = prettyExpr l ++ " |> " ++ prettyExpr r
prettyExpr (ECompose f g) = prettyExpr f ++ " . " ++ prettyExpr g
prettyExpr (EBind ma f)   = prettyExpr ma ++ " >> " ++ prettyExpr f
prettyExpr (EApp f args)  =
  prettyExpr f ++ "(" ++ intercalate ", " (map prettyArg args) ++ ")"
prettyExpr (ELambda ps e) =
  "lambda " ++ intercalate ", " (map prettyParam ps) ++ " -> " ++ prettyExpr e
prettyExpr (EIf c t f)    =
  prettyExpr t ++ " if " ++ prettyExpr c ++ " else " ++ prettyExpr f
prettyExpr (EListComp e cs) =
  "[" ++ prettyExpr e ++ " " ++ unwords (map prettyCompClause cs) ++ "]"
prettyExpr (EDictComp k v cs) =
  "{" ++ prettyExpr k ++ ": " ++ prettyExpr v ++ " " ++
  unwords (map prettyCompClause cs) ++ "}"
prettyExpr (ESetComp e cs) =
  "{" ++ prettyExpr e ++ " " ++ unwords (map prettyCompClause cs) ++ "}"
prettyExpr (EMatch e arms) =
  "match " ++ prettyExpr e ++ " { " ++
  intercalate " | " (map prettyArm arms) ++ " }"
prettyExpr (EAnnotated e ty) = prettyExpr e ++ " : " ++ prettyType ty
prettyExpr (ERecordCreate n fs) =
  n ++ " { " ++ intercalate ", " (map (\(k,v) -> k ++ " = " ++ prettyExpr v) fs) ++ " }"
prettyExpr (ERecordUpdate e fs) =
  prettyExpr e ++ " { " ++ intercalate ", " (map (\(k,v) -> k ++ " = " ++ prettyExpr v) fs) ++ " }"
prettyExpr (EFStr parts) = "f\"" ++ concatMap showFPart parts ++ "\""
  where
    showFPart (FStrLit s)  = s
    showFPart (FStrExpr e) = "{" ++ e ++ "}"

prettyArg :: Arg -> String
prettyArg (ArgPos e)     = prettyExpr e
prettyArg (ArgKw k v)    = k ++ "=" ++ prettyExpr v
prettyArg (ArgStar e)    = "*" ++ prettyExpr e
prettyArg (ArgDStar e)   = "**" ++ prettyExpr e

prettyParam :: Param -> String
prettyParam (ParamSimple n)    = n
prettyParam (ParamTyped n ty)  = n ++ ": " ++ prettyType ty
prettyParam (ParamDefault n e) = n ++ "=" ++ prettyExpr e
prettyParam (ParamStar n)      = "*" ++ n
prettyParam (ParamDStar n)     = "**" ++ n
prettyParam (ParamPat p)       = showPattern p

prettyCompClause :: CompClause -> String
prettyCompClause (CCFor p e) = "for " ++ showPattern p ++ " in " ++ prettyExpr e
prettyCompClause (CCIf  e)   = "if " ++ prettyExpr e

prettyArm :: MatchArm -> String
prettyArm (MatchArm p g _) =
  showPattern p ++ maybe "" (\g -> " if " ++ prettyExpr g) g ++ " -> ..."

prettyOp :: BinOp -> String
prettyOp OpAdd     = "+"
prettyOp OpSub     = "-"
prettyOp OpMul     = "*"
prettyOp OpDiv     = "/"
prettyOp OpFloorDiv = "//"
prettyOp OpMod     = "%"
prettyOp OpPow     = "**"
prettyOp OpEq      = "=="
prettyOp OpNeq     = "!="
prettyOp OpLt      = "<"
prettyOp OpGt      = ">"
prettyOp OpLtEq    = "<="
prettyOp OpGtEq    = ">="
prettyOp OpAnd     = "and"
prettyOp OpOr      = "or"
prettyOp OpBitAnd  = "&"
prettyOp OpBitOr   = "|"
prettyOp OpBitXor  = "^"
prettyOp OpShiftL  = "<<"
prettyOp OpShiftR  = ">>"
prettyOp OpConcat  = "<>"
prettyOp OpCons    = ":"
prettyOp OpIn      = "in"
prettyOp OpNotIn   = "not in"

prettyUnOp :: UnOp -> String
prettyUnOp OpNeg    = "-"
prettyUnOp OpNot    = "not "
prettyUnOp OpBitNot = "~"
prettyUnOp OpAbs    = "abs "

prettyStmt :: Stmt -> String
prettyStmt (SExpr e)              = prettyExpr e
prettyStmt (SAssign p e)          = showPattern p ++ " = " ++ prettyExpr e
prettyStmt (SAugAssign n op e)    = n ++ " " ++ prettyOp op ++ "= " ++ prettyExpr e
prettyStmt (SFunDef n ps ret _)   =
  "def " ++ n ++ "(" ++ intercalate ", " (map prettyParam ps) ++ ")" ++
  maybe "" (\t -> " -> " ++ prettyType t) ret ++ ": ..."
prettyStmt (SLetDef n t e)        =
  "let " ++ n ++ maybe "" (\ty -> ": " ++ prettyType ty) t ++ " = " ++ prettyExpr e
prettyStmt (SIf branches els)     = "if ..."
prettyStmt (SWhile c _ _)         = "while " ++ prettyExpr c ++ ": ..."
prettyStmt (SFor p e _ _)         = "for " ++ showPattern p ++ " in " ++ prettyExpr e ++ ": ..."
prettyStmt (SReturn me)           = "return" ++ maybe "" (\e -> " " ++ prettyExpr e) me
prettyStmt SBreak                 = "break"
prettyStmt SContinue              = "continue"
prettyStmt (SMatch e _)           = "match " ++ prettyExpr e ++ ": ..."
prettyStmt (STryCatch _ _ _ _)    = "try: ..."
prettyStmt (SRecordDef n _)       = "record " ++ n ++ " { ... }"
prettyStmt (SImport spec)         = prettyImport spec
prettyStmt SPass                  = "pass"
prettyStmt (SGlobal ns)           = "global " ++ intercalate ", " ns
prettyStmt (SNonlocal ns)         = "nonlocal " ++ intercalate ", " ns
prettyStmt (SDelete es)           = "del " ++ intercalate ", " (map prettyExpr es)
prettyStmt (SRaise me)            = "raise" ++ maybe "" (\e -> " " ++ prettyExpr e) me
prettyStmt (SAssert e me)         = "assert " ++ prettyExpr e ++
                                    maybe "" (\m -> ", " ++ prettyExpr m) me

prettyImport :: ImportSpec -> String
prettyImport (ImportModule n a)  = "import " ++ n ++ maybe "" (" as " ++) a
prettyImport (ImportFrom n ns _) = "from " ++ n ++ " import " ++ intercalate ", " ns
prettyImport (ImportAll n)       = "from " ++ n ++ " import *"

-- =============================================================================
-- TYPE PRETTY-PRINTING
-- =============================================================================

prettyType :: TypeExpr -> String
prettyType = showType

-- =============================================================================
-- ERROR PRETTY-PRINTING
-- =============================================================================

prettyError :: MiniFlowError -> String
prettyError (MFSyntaxError msg pos)   = formatErr "SyntaxError"  msg pos
prettyError (MFTypeError   msg pos)   = formatErr "TypeError"    msg pos
prettyError (MFNameError   msg pos)   = formatErr "NameError"    msg pos
prettyError (MFIndexError  msg pos)   = formatErr "IndexError"   msg pos
prettyError (MFKeyError    msg pos)   = formatErr "KeyError"     msg pos
prettyError (MFValueError  msg pos)   = formatErr "ValueError"   msg pos
prettyError (MFRuntimeError msg pos)  = formatErr "RuntimeError" msg pos
prettyError (MFPatternFail msg pos)   = formatErr "PatternMatchFailure" msg pos
prettyError (MFImportError msg pos)   = formatErr "ImportError"  msg pos
prettyError (MFDivisionByZero pos)    = formatErr "ZeroDivisionError"
                                        "division by zero" pos
prettyError (MFOverflowError msg pos) = formatErr "OverflowError" msg pos
prettyError MFStopIteration           = "StopIteration"
prettyError MFBreakSignal             = "break (outside loop)"
prettyError MFContinueSignal          = "continue (outside loop)"
prettyError (MFReturnSignal _)        = "return (outside function)"
prettyError (MFRaiseSignal v)         = "Exception: " ++ prettyValue v

formatErr :: String -> String -> SourcePos -> String
formatErr errType msg pos =
  "\x1b[31m" ++ errType ++ "\x1b[0m: " ++ msg ++
  "\n  --> " ++ show pos

-- =============================================================================
-- PATTERN PRETTY-PRINTING (needed by PatternMatch.hs via showPattern)
-- =============================================================================

showPattern :: Pattern -> String
showPattern PWild               = "_"
showPattern (PVar name)         = name
showPattern (PNum n)
  | n == fromIntegral (round n :: Int) = show (round n :: Int)
  | otherwise                   = show n
showPattern (PStr s)            = show s
showPattern (PBool True)        = "True"
showPattern (PBool False)       = "False"
showPattern PNone               = "None"
showPattern (PList pats)        = "[" ++ intercalate ", " (map showPattern pats) ++ "]"
showPattern (PTuple pats)       = "(" ++ intercalate ", " (map showPattern pats) ++ ")"
showPattern (PCons h t)         = showPattern h ++ ":" ++ showPattern t
showPattern (PRecord n flds)    = n ++ " {" ++ intercalate ", " (map showField flds) ++ "}"
  where showField (k, p) = k ++ ": " ++ showPattern p
showPattern (PAs inner name)    = showPattern inner ++ " as " ++ name
showPattern (POr l r)           = showPattern l ++ " | " ++ showPattern r
showPattern (PTyped p ty)       = showPattern p ++ ": " ++ prettyType ty
showPattern (PIndex _ _)        = "<index-assign>"
showPattern (PField _ f)        = "<field-assign:" ++ f ++ ">"

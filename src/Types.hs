-- =============================================================================
-- MiniFlow Language Interpreter
-- src/Types.hs  -- All core data types shared across every module
-- =============================================================================
{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Exception (Exception, throwIO)

-- =============================================================================
-- SOURCE POSITION
-- =============================================================================

data SourcePos = SourcePos
  { spFile :: String
  , spLine :: Int
  , spCol  :: Int
  } deriving (Eq)

instance Show SourcePos where
  show (SourcePos f l c) = f ++ ":" ++ show l ++ ":" ++ show c

noPos :: SourcePos
noPos = SourcePos "<unknown>" 0 0

-- =============================================================================
-- TOKENS  (Lexer output)
-- =============================================================================

data Token
  -- Literals
  = TInt    Int
  | TFloat  Double
  | TStr    String
  | TBool   Bool
  | TNone
  -- Identifiers & Keywords
  | TIdent  String
  | TDef
  | TLet
  | TIn
  | TIf
  | TElif
  | TElse
  | TFor
  | TWhile
  | TReturn
  | TBreak
  | TContinue
  | TMatch
  | TCase
  | TRecord
  | TTry
  | TExcept
  | TFinally
  | TImport
  | TFrom
  | TAs
  | TLambda
  | TAnd
  | TOr
  | TNot
  | TTrue
  | TFalse
  -- Arithmetic operators
  | TPlus
  | TMinus
  | TStar
  | TSlash
  | TDSlash      -- //
  | TPercent
  | TDStar       -- **
  -- Comparison operators
  | TEq          -- ==
  | TNeq         -- !=
  | TLt
  | TGt
  | TLtEq        -- <=
  | TGtEq        -- >=
  -- Assignment
  | TAssign      -- =
  | TPlusEq      -- +=
  | TMinusEq     -- -=
  | TStarEq      -- *=
  | TSlashEq     -- /=
  -- Logical / Bitwise
  | TAmp         -- &
  | TPipeSym     -- |
  | TCaret       -- ^
  | TTilde       -- ~
  | TShiftL      -- <<
  | TShiftR      -- >>
  -- Functional operators
  | TPipeRight   -- |>
  | TCompose     -- .  (as compose when between functions)
  | TBind        -- >>
  | TArrow       -- ->
  | TFatArrow    -- =>
  | TDot         -- .  (as field access)
  -- Punctuation
  | TLParen
  | TRParen
  | TLBrace
  | TRBrace
  | TLBracket
  | TRBracket
  | TComma
  | TColon
  | TSemicolon
  | THash        -- #  (comment start, handled by lexer)
  | TAt          -- @  (decorator)
  | TDotDot      -- ..
  | TDotDotDot   -- ...
  | TBackslash
  | TUnderscore
  -- String interpolation
  | TFStr  [FStrPart]
  -- Block structure
  | TNewline
  | TIndent
  | TDedent
  | TEOF
  deriving (Show, Eq)

data FStrPart
  = FStrLit  String
  | FStrExpr String       -- the raw substring to re-parse
  deriving (Show, Eq)

data TokenInfo = TokenInfo
  { tiToken :: Token
  , tiPos   :: SourcePos
  } deriving (Show, Eq)

-- =============================================================================
-- ABSTRACT SYNTAX TREE
-- =============================================================================

-- Type annotations (optional / gradual typing)
data TypeExpr
  = TyInt
  | TyFloat
  | TyStr
  | TyBool
  | TyNone
  | TyAny
  | TyList   TypeExpr
  | TyTuple  [TypeExpr]
  | TyDict   TypeExpr TypeExpr
  | TySet    TypeExpr
  | TyFun    [TypeExpr] TypeExpr
  | TyMaybe  TypeExpr
  | TyUnion  [TypeExpr]
  | TyNamed  String [TypeExpr]
  deriving (Show, Eq)

-- Patterns (for match expressions, function params, for-loop vars)
data Pattern
  = PWild                              -- _
  | PVar    String                     -- x
  | PNum    Double                     -- 42 or 3.14
  | PStr    String                     -- "hello"
  | PBool   Bool                       -- True / False
  | PNone                              -- None
  | PList   [Pattern]                  -- [a, b, c]
  | PTuple  [Pattern]                  -- (a, b, c)
  | PCons   Pattern Pattern            -- h:t
  | PRecord String [(String, Pattern)] -- Point { x: a, y: b }
  | PAs     Pattern String             -- pat as name
  | POr     Pattern Pattern            -- pat1 | pat2
  | PTyped  Pattern TypeExpr           -- pat : Type
  | PIndex  Expr Expr                  -- container[key] (assignment target)
  | PField  Expr String                -- obj.field (assignment target)
  deriving (Show, Eq)

-- A single match arm
data MatchArm = MatchArm
  { maPattern :: Pattern
  , maGuard   :: Maybe Expr
  , maBody    :: [Stmt]
  } deriving (Show, Eq)

-- Comprehension clause (for / if)
data CompClause
  = CCFor  Pattern Expr
  | CCIf   Expr
  deriving (Show, Eq)

-- Binary operators
data BinOp
  = OpAdd | OpSub | OpMul | OpDiv | OpFloorDiv | OpMod | OpPow
  | OpEq  | OpNeq | OpLt  | OpGt  | OpLtEq    | OpGtEq
  | OpAnd | OpOr
  | OpBitAnd | OpBitOr | OpBitXor | OpShiftL | OpShiftR
  | OpConcat   -- <>  string concat
  | OpCons     -- :   list cons
  | OpIn       -- x in xs
  | OpNotIn    -- x not in xs
  deriving (Show, Eq)

-- Unary operators
data UnOp
  = OpNeg | OpNot | OpBitNot | OpAbs
  deriving (Show, Eq)

-- Expressions
data Expr
  -- Literals
  = EInt    Int
  | EFloat  Double
  | EStr    String
  | EBool   Bool
  | ENone
  | EFStr   [FStrPart]           -- f"Hello {name}!"
  -- Collections
  | EList   [Expr]
  | ETuple  [Expr]
  | EDict   [(Expr, Expr)]
  | ESet    [Expr]
  | ERange  Expr Expr (Maybe Expr) -- start .. end [step]
  -- Variables & field access
  | EVar    String
  | EField  Expr String            -- expr.field
  | EIndex  Expr Expr              -- expr[index]
  | ESlice  Expr (Maybe Expr) (Maybe Expr) (Maybe Expr)  -- expr[a:b:c]
  -- Operators
  | EBinOp  BinOp Expr Expr
  | EUnOp   UnOp  Expr
  -- Functional
  | EPipe   Expr Expr              -- lhs |> rhs
  | ECompose Expr Expr             -- f . g
  | EBind   Expr Expr              -- ma >> f
  -- Function application
  | EApp    Expr [Arg]             -- f(x, y, key=val)
  -- Abstraction
  | ELambda [Param] Expr
  -- Control flow expressions
  | EIf     Expr Expr Expr         -- if cond then t else f
  | EMatch  Expr [MatchArm]
  -- Comprehensions
  | EListComp Expr [CompClause]
  | EDictComp Expr Expr [CompClause]
  | ESetComp  Expr [CompClause]
  -- Records
  | ERecordCreate String [(String, Expr)]
  | ERecordUpdate Expr [(String, Expr)]
  -- Type annotation
  | EAnnotated Expr TypeExpr
  -- Misc
  | EEllipsis
  deriving (Show, Eq)

-- Function call argument
data Arg
  = ArgPos   Expr              -- positional
  | ArgKw    String Expr       -- keyword  name=val
  | ArgStar  Expr              -- *args
  | ArgDStar Expr              -- **kwargs
  deriving (Show, Eq)

-- Function parameter
data Param
  = ParamSimple  String
  | ParamTyped   String TypeExpr
  | ParamDefault String Expr
  | ParamStar    String           -- *args
  | ParamDStar   String           -- **kwargs
  | ParamPat     Pattern          -- pattern param (Haskell style)
  deriving (Show, Eq)

-- Record field definition
data FieldDef = FieldDef
  { fdName    :: String
  , fdType    :: TypeExpr
  , fdDefault :: Maybe Expr
  } deriving (Show, Eq)

-- Statements
data Stmt
  = SExpr   Expr
  | SAssign Pattern Expr
  | SAugAssign String BinOp Expr     -- x += 5
  | SFunDef  String [Param] (Maybe TypeExpr) [Stmt]  -- def name(params) -> type: body
  | SLetDef  String (Maybe TypeExpr) Expr             -- let x : Type = expr
  | SIf      [(Expr, [Stmt])] (Maybe [Stmt])          -- if/elif chain + else
  | SWhile   Expr [Stmt] (Maybe [Stmt])               -- while/else
  | SFor     Pattern Expr [Stmt] (Maybe [Stmt])       -- for/else
  | SReturn  (Maybe Expr)
  | SBreak
  | SContinue
  | SMatch   Expr [MatchArm]
  | STryCatch [Stmt] [(Maybe String, String, [Stmt])] (Maybe [Stmt]) (Maybe [Stmt])
    -- try body, [(exType, name, handler)], else, finally
  | SRecordDef String [FieldDef]
  | SImport  ImportSpec
  | SPass
  | SGlobal  [String]
  | SNonlocal [String]
  | SDelete  [Expr]
  | SRaise   (Maybe Expr)
  | SAssert  Expr (Maybe Expr)
  deriving (Show, Eq)

data ImportSpec
  = ImportModule String (Maybe String)             -- import foo [as bar]
  | ImportFrom   String [String] (Maybe String)    -- from foo import [a,b] [as c]
  | ImportAll    String                            -- from foo import *
  deriving (Show, Eq)

-- Top-level program
newtype Program = Program { progStmts :: [Stmt] }
  deriving (Show, Eq)

-- =============================================================================
-- RUNTIME VALUES
-- =============================================================================

type Env = IORef EnvFrame

data EnvFrame = EnvFrame
  { envBindings :: Map String (IORef Value)
  , envParent   :: Maybe Env
  , envName     :: String     -- for stack traces
  }

-- The MiniFlow value type
data Value
  = VInt    Int
  | VFloat  Double
  | VBool   Bool
  | VStr    String
  | VNone
  | VList   (IORef [Value])
  | VTuple  [Value]
  | VDict   (IORef (Map Value Value))
  | VSet    (IORef [Value])           -- unordered, dedup
  | VClosure
      { vcParams  :: [Param]
      , vcBody    :: [Stmt]
      , vcEnv     :: Env
      , vcName    :: String
      }
  | VBuiltin
      { vbName :: String
      , vbFn   :: [Value] -> [(String, Value)] -> IO Value
      }
  | VRecord String (IORef (Map String Value))
  | VType   String
  | VLazyList (IORef LazyList)        -- for iterate/repeat/cycle
  | VIterator (IORef [Value])         -- consumed iterator
  | VModule   String (Map String Value)
  | VNativeExn String                 -- raised exception value

-- Lazy list for infinite sequences
data LazyList
  = LNil
  | LCons Value (IO LazyList)

instance Show Value where
  show (VInt n)        = show n
  show (VFloat d)      = show d
  show (VBool True)    = "True"
  show (VBool False)   = "False"
  show (VStr s)        = show s
  show VNone           = "None"
  show (VList _)       = "<list>"
  show (VTuple vs)     = "(" ++ concatMap (\v -> show v ++ ", ") vs ++ ")"
  show (VDict _)       = "<dict>"
  show (VSet _)        = "<set>"
  show (VClosure{vcName=n}) = "<function " ++ n ++ ">"
  show (VBuiltin{vbName=n}) = "<builtin " ++ n ++ ">"
  show (VRecord n _)   = "<" ++ n ++ " instance>"
  show (VType n)       = "<type " ++ n ++ ">"
  show (VLazyList _)   = "<lazy_list>"
  show (VIterator _)   = "<iterator>"
  show (VModule n _)   = "<module " ++ n ++ ">"
  show (VNativeExn s)  = "<exception " ++ s ++ ">"

-- Value must be Ord for use as Dict keys
instance Eq Value where
  VInt a    == VInt b    = a == b
  VFloat a  == VFloat b  = a == b
  VInt a    == VFloat b  = fromIntegral a == b
  VFloat a  == VInt b    = a == fromIntegral b
  VBool a   == VBool b   = a == b
  VStr a    == VStr b    = a == b
  VNone     == VNone     = True
  VTuple as == VTuple bs = as == bs
  VType a   == VType b   = a == b
  _         == _         = False

instance Ord Value where
  compare (VInt a)   (VInt b)   = compare a b
  compare (VFloat a) (VFloat b) = compare a b
  compare (VInt a)   (VFloat b) = compare (fromIntegral a) b
  compare (VFloat a) (VInt b)   = compare a (fromIntegral b)
  compare (VStr a)   (VStr b)   = compare a b
  compare (VBool a)  (VBool b)  = compare a b
  compare (VTuple a) (VTuple b) = compare a b
  compare VNone      VNone      = EQ
  compare a          b          = compare (typeNameOf a) (typeNameOf b)

typeNameOf :: Value -> String
typeNameOf (VInt _)      = "int"
typeNameOf (VFloat _)    = "float"
typeNameOf (VBool _)     = "bool"
typeNameOf (VStr _)      = "str"
typeNameOf VNone         = "NoneType"
typeNameOf (VList _)     = "list"
typeNameOf (VTuple _)    = "tuple"
typeNameOf (VDict _)     = "dict"
typeNameOf (VSet _)      = "set"
typeNameOf (VClosure{})  = "function"
typeNameOf (VBuiltin{})  = "builtin_function"
typeNameOf (VRecord n _) = n
typeNameOf (VType _)     = "type"
typeNameOf (VLazyList _) = "lazy_list"
typeNameOf (VIterator _) = "iterator"
typeNameOf (VModule n _) = "module:" ++ n
typeNameOf (VNativeExn e)= "exception:" ++ e

-- =============================================================================
-- ERRORS  (also used as Haskell exceptions via Exception typeclass)
-- =============================================================================

data MiniFlowError
  = MFSyntaxError   { mfMsg :: String, mfPos :: SourcePos }
  | MFTypeError     { mfMsg :: String, mfPos :: SourcePos }
  | MFNameError     { mfMsg :: String, mfPos :: SourcePos }
  | MFIndexError    { mfMsg :: String, mfPos :: SourcePos }
  | MFKeyError      { mfMsg :: String, mfPos :: SourcePos }
  | MFValueError    { mfMsg :: String, mfPos :: SourcePos }
  | MFRuntimeError  { mfMsg :: String, mfPos :: SourcePos }
  | MFPatternFail   { mfMsg :: String, mfPos :: SourcePos }
  | MFImportError   { mfMsg :: String, mfPos :: SourcePos }
  | MFDivisionByZero{ mfPos :: SourcePos }
  | MFOverflowError { mfMsg :: String, mfPos :: SourcePos }
  | MFStopIteration
  | MFBreakSignal
  | MFContinueSignal
  | MFReturnSignal  Value
  | MFRaiseSignal   Value
  deriving (Show)

instance Exception MiniFlowError

-- =============================================================================
-- TYPE CHECKER TYPES
-- =============================================================================

data TyScheme = TyScheme [String] TypeExpr
  deriving (Show, Eq)

type TyEnv = Map String TyScheme

data TypeWarning
  = TWIncompatible SourcePos TypeExpr TypeExpr String
  | TWUnboundVar   SourcePos String
  | TWReturnType   SourcePos TypeExpr TypeExpr
  deriving (Show)

formatTypeWarning :: TypeWarning -> String
formatTypeWarning (TWIncompatible pos t1 t2 ctx) =
  "[TypeWarning] " ++ show pos ++ ": In " ++ ctx ++
  ", expected " ++ showType t1 ++ " but got " ++ showType t2
formatTypeWarning (TWUnboundVar pos v) =
  "[TypeWarning] " ++ show pos ++ ": Unbound variable '" ++ v ++ "'"
formatTypeWarning (TWReturnType pos declared actual) =
  "[TypeWarning] " ++ show pos ++ ": Declared return type " ++
  showType declared ++ " but got " ++ showType actual

showType :: TypeExpr -> String
showType TyInt           = "Int"
showType TyFloat         = "Float"
showType TyStr           = "Str"
showType TyBool          = "Bool"
showType TyNone          = "None"
showType TyAny           = "Any"
showType (TyList t)      = "List[" ++ showType t ++ "]"
showType (TyTuple ts)    = "Tuple[" ++ commas (map showType ts) ++ "]"
showType (TyDict k v)    = "Dict[" ++ showType k ++ ", " ++ showType v ++ "]"
showType (TySet t)       = "Set[" ++ showType t ++ "]"
showType (TyFun ps r)    = "(" ++ commas (map showType ps) ++ ") -> " ++ showType r
showType (TyMaybe t)     = "Maybe[" ++ showType t ++ "]"
showType (TyUnion ts)    = commas (map showType ts)
showType (TyNamed n [])  = n
showType (TyNamed n as)  = n ++ "[" ++ commas (map showType as) ++ "]"

commas :: [String] -> String
commas = foldr1 (\a b -> a ++ ", " ++ b)

-- =============================================================================
-- UTILITY  (used everywhere)
-- =============================================================================

isTruthy :: Value -> Bool
isTruthy (VBool False) = False
isTruthy VNone         = False
isTruthy (VInt 0)      = False
isTruthy (VFloat 0.0)  = False
isTruthy (VStr "")     = False
-- Lists, dicts, sets: truthy if non-empty (checked at runtime via IORef)
isTruthy _             = True

isNumeric :: Value -> Bool
isNumeric (VInt _)   = True
isNumeric (VFloat _) = True
isNumeric _          = False

toDouble :: Value -> Double
toDouble (VInt n)   = fromIntegral n
toDouble (VFloat f) = f
toDouble _          = error "toDouble: not a number"

toInt :: Value -> Int
toInt (VInt n)   = n
toInt (VFloat f) = round f
toInt _          = error "toInt: not a number"

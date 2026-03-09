# Changelog

All notable changes to **MiniFlow** will be documented in this file.

This project adheres to [Semantic Versioning](https://semver.org/) and
[Keep a Changelog](https://keepachangelog.com/) conventions.

---

## [Unreleased]

### Planned
- Static type inference (Hindley-Milner)
- Bytecode compilation target
- Module / package manager (`mfpkg`)
- JIT compilation via LLVM

---

## [1.0.0] — 2026-03-09

### Added
- Full interpreter pipeline: Lexer → Parser → Evaluator
- **150+ built-in functions** (60 Python-style, 90 Haskell-style)
- **Pipe operator** `|>` — Unix-style value threading
- **Compose operator** `.` — function composition
- **Bind operator** `>>` — monadic-style sequencing
- **Pattern matching** with guards (`match / case`)
- **List comprehensions** with multiple `for`/`if` clauses
- **Dictionary and set comprehensions**
- **Lazy lists** — `iterate`, `repeat`, `cycle`, `take`, `takeWhile`
- **Closures** with full lexical scoping
- **Records / structs** with field access and mutation
- **f-string interpolation** — `f"Hello, {name}!"`
- **Haskell-style range literals** — `[1..10]`, `[2,4..20]`
- **Tuple destructuring** in `let` and `for` statements
- **Error handling** — `try / except / finally`
- **REPL** with multi-line input, `:load`, `:env`, `:history`, `:t`
- **Type annotations** (gradual — checked at runtime only)
- **Default parameter values** — `def f(x, y=0):`
- **Variadic parameters** — `*args`, `**kwargs`
- **String methods** via dot syntax — `"hello".upper()`
- **All Python string operations** — `split`, `join`, `strip`, `replace`, …
- **Full `Data.List` surface** — `nub`, `sortBy`, `groupBy`, `intercalate`, …
- **Maybe helpers** — `fromMaybe`, `catMaybes`, `mapMaybe`
- **Math built-ins** — `sqrt`, `sin`, `cos`, `tan`, `log`, `exp`, `floor`, `ceil`
- **Import / from-import** statement (namespace isolation)
- **Decorator syntax** (`@decorator`) — parsed, extensible
- **Block comments** `{- … -}` and line comments `#`

### Fixed (during development)
- Lazy list `LCons`/`LNil` only — removed phantom `LLazy` constructor
- `VLazyList` support added to `map`/`filter`/`takeWhile` in Python.hs and Haskell.hs
- Paren-depth tracking in `insertIndentDedent` — suppresses INDENT/DEDENT inside `()[]{}` for implicit line continuation
- `let (a,b) = expr` tuple destructuring in Parser
- `exprToPattern` now handles `EIndex` and `EField` → `PIndex`/`PField`
- `SAssign (PVar name)` uses `assignVar` (scope-chain walk) + `defineVar` fallback
- `SAssign (PIndex …)` handles dict/list indexed assignment
- `SAssign (PField …)` handles record field assignment
- `PIndex` / `PField` added to `Pattern` and `PatternMatch.hs`
- `showPattern` in `Pretty.hs` extended for `PIndex` and `PField`

---

## Format Reference

```
### Added   — new features
### Changed — changes in existing functionality
### Deprecated — soon-to-be removed features
### Removed — now removed features
### Fixed   — any bug fixes
### Security — vulnerability patches
```

[Unreleased]: https://github.com/YOUR_USERNAME/miniflow-interpreter/compare/v1.0.0...HEAD
[1.0.0]: https://github.com/YOUR_USERNAME/miniflow-interpreter/releases/tag/v1.0.0

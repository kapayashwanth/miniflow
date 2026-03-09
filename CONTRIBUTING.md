# Contributing to MiniFlow

Thank you for your interest in contributing to MiniFlow!
This guide explains how to set up your environment, submit changes, and follow our conventions.

---

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [Project Structure](#project-structure)
- [How to Contribute](#how-to-contribute)
- [Commit Convention](#commit-convention)
- [Pull Request Process](#pull-request-process)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
- [Reporting Bugs](#reporting-bugs)
- [Requesting Features](#requesting-features)

---

## Code of Conduct

This project follows the [Contributor Covenant Code of Conduct](CODE_OF_CONDUCT.md).
By participating, you agree to uphold these standards.

---

## Getting Started

1. **Fork** the repository on GitHub.
2. **Clone** your fork:
   ```bash
   git clone https://github.com/YOUR_USERNAME/miniflow-interpreter.git
   cd miniflow-interpreter
   ```
3. **Add the upstream remote**:
   ```bash
   git remote add upstream https://github.com/ORIGINAL_OWNER/miniflow-interpreter.git
   ```

---

## Development Setup

### Requirements

| Tool   | Version  | Install                                      |
|--------|----------|----------------------------------------------|
| GHC    | ≥ 8.10   | `ghcup install ghc`                         |
| Stack  | ≥ 2.7    | `ghcup install stack`                       |
| HLint  | any      | `stack install hlint` *(optional)*          |
| Ormolu | any      | `stack install ormolu` *(optional)*         |

### Build

```bash
# Quick compile check
ghc -isrc Main.hs -o miniflow

# Or use the VS Code task
stack build
```

### Run tests

```bash
# Run all example files and check they exit cleanly
bash scripts/run_all_tests.sh

# Or individual file
runhaskell -isrc Main.hs examples/sample.mf
```

---

## Project Structure

```
src/
 ├── Types.hs          ← All ADTs (Token / Expr / Stmt / Value / Error)
 ├── Lexer.hs          ← Hand-written lexer
 ├── Parser.hs         ← Recursive descent parser
 ├── Evaluator.hs      ← Tree-walking interpreter
 ├── Environment.hs    ← Scope chain
 ├── PatternMatch.hs   ← Pattern engine
 ├── Pretty.hs         ← Value printer
 ├── REPL.hs           ← Interactive REPL
 └── Builtins/
     ├── Core.hs       ← Shared utilities
     ├── Python.hs     ← Python-style builtins
     └── Haskell.hs    ← Haskell-style builtins

examples/             ← Integration test files (.mf)
docs/                 ← Architecture, grammar, assets
```

---

## How to Contribute

### Types of contributions welcome

| Type                     | Notes                                            |
|--------------------------|--------------------------------------------------|
| Bug fixes                | Always welcome — see [Reporting Bugs](#reporting-bugs) |
| New built-in functions   | Add to `Builtins/Python.hs` or `Builtins/Haskell.hs` |
| New syntax features      | Discuss in an issue first                        |
| Documentation            | Improve README, GRAMMAR.md, ARCHITECTURE.md      |
| New example `.mf` files  | Add to `examples/` with clear naming            |
| Performance improvements | Benchmark before & after                         |

### Workflow

```
1. Create an issue describing the change (for non-trivial work)
2. Create a feature branch from main:
       git checkout -b feat/my-feature
3. Make your changes
4. Run tests (see below)
5. Commit following the convention below
6. Push and open a Pull Request
```

---

## Commit Convention

We follow [Conventional Commits](https://www.conventionalcommits.org/):

```
<type>(<scope>): <short description>

[optional body]

[optional footer]
```

| Type       | When to use                            |
|------------|----------------------------------------|
| `feat`     | A new feature                          |
| `fix`      | A bug fix                              |
| `docs`     | Documentation only                     |
| `refactor` | Code restructuring (no behavior change)|
| `test`     | New or updated tests                   |
| `chore`    | Build scripts, CI, tooling             |
| `perf`     | Performance improvement                |

**Examples:**
```
feat(lexer): add support for raw string literals r"..."
fix(evaluator): correct scoping for nested closures
docs(grammar): add formal BNF for comprehensions
test(examples): add test_fib_memoized.mf
```

---

## Pull Request Process

1. Ensure your branch is up to date with `upstream/main` before opening a PR.
2. Fill in the PR template completely.
3. Every PR must:
   - Pass the GHC build (`ghc -isrc Main.hs`)
   - Not break any existing example files
   - Include a new example file if adding language syntax
4. Request a review from at least one maintainer.
5. Squash commits before merge if the PR has many WIP commits.

---

## Coding Standards

### Haskell Style

- 2-space indentation
- Lines ≤ 100 characters
- Use `where` clauses for local helpers, not `let` chains in do-blocks
- Prefer pattern matching over `if/then/else` where natural
- Add a short comment above each top-level function explaining _what_ it does (not _how_)
- Avoid partial functions (`head`, `tail`, `fromJust`) without a guard

### Naming Conventions

| Context            | Convention              | Example                  |
|--------------------|-------------------------|--------------------------|
| Types / Constructors | `PascalCase`          | `TokenInfo`, `VFunction` |
| Functions          | `camelCase`             | `evalExpr`, `lookupVar`  |
| Local bindings     | `camelCase`             | `trimmed`, `newEnv`      |
| Constants          | `camelCase` or `UPPER`  | `noPos`, `MAX_DEPTH`     |
| Type variables     | single lowercase        | `a`, `b`, `env`          |

### MiniFlow (.mf) Style

- 4-space indentation
- Snake_case for variable names
- Descriptive function names

---

## Testing

Until a formal test suite is added, testing is done by running example files:

```bash
# All core examples should produce no errors and correct output
runhaskell -isrc Main.hs examples/sample.mf
runhaskell -isrc Main.hs examples/pipes.mf
runhaskell -isrc Main.hs examples/fibonacci.mf
```

When adding a bug fix, add a minimal `.mf` file to `examples/` that
_reproduces_ the bug and _verifies_ the fix.

---

## Reporting Bugs

Open a GitHub Issue using the **Bug Report** template.

Please include:
- MiniFlow version (`v1.0`)
- GHC version (`ghc --version`)
- OS and shell
- Minimal reproducible `.mf` code snippet
- Expected behaviour
- Actual behaviour / error message

---

## Requesting Features

Open a GitHub Issue using the **Feature Request** template.

Please describe:
- What problem this solves
- Proposed syntax / semantics
- Any prior art in Python / Haskell / Elixir

---

Thank you for making MiniFlow better!

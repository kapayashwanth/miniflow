# MiniFlow Language Interpreter

> A fully-featured interpreted programming language implemented in **Haskell**, blending the best of **Python** and **Haskell** — with pipes, pattern matching, list comprehensions, closures, and 150+ built-in functions.

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Project Structure](#project-structure)
- [Installation](#installation)
- [Running the Interpreter](#running-the-interpreter)
- [Language Syntax](#language-syntax)
- [Built-in Functions](#built-in-functions)
- [Pipe & Pipeline Operators](#pipe--pipeline-operators)
- [Examples](#examples)
- [Time & Space Complexity](#time--space-complexity)
- [Online Hosting](#online-hosting)
- [Team](#team)

---

## Overview

**MiniFlow** is a small, interpreted programming language designed and built as a university mini-project for the course **23CSE212 — Principles of Programming Languages (PFPL)**.

It demonstrates every core concept of language design:

| Concept | Implementation |
|---|---|
| Lexical Analysis | `src/Lexer.hs` |
| Parsing (LL1 Grammar) | `src/Parser.hs` |
| Abstract Syntax Tree | `src/Types.hs` |
| Tree-Walking Interpreter | `src/Evaluator.hs` |
| Pattern Matching Engine | `src/PatternMatch.hs` |
| Scoped Environments | `src/Environment.hs` |
| Python Built-ins (60+) | `src/Builtins/Python.hs` |
| Haskell Built-ins (90+) | `src/Builtins/Haskell.hs` |
| Interactive REPL | `src/REPL.hs` |

**Total source code: 7,982 lines of Haskell**

---

## Features

### Language Features
- ✅ Variables (`let x = 10`)
- ✅ Functions with default parameters (`def f(x, y=0):`)
- ✅ Lambda / anonymous functions (`\x -> x * 2`)
- ✅ Closures and lexical scoping
- ✅ Recursion and tail calls
- ✅ Conditionals (`if / elif / else`)
- ✅ Loops (`for`, `while`)
- ✅ Pattern matching with guards (`match / case`)
- ✅ List comprehensions (`[x*x for x in range(10) if odd(x)]`)
- ✅ Dictionary and set comprehensions
- ✅ Records / structs (`record Point { x: Float, y: Float }`)
- ✅ Error handling (`try / except`)
- ✅ String interpolation (`f"Hello, {name}!"`)
- ✅ Type annotations (gradual typing)
- ✅ Modules and imports

### Functional Operators
- ✅ **Pipe** `|>` — pass value into function
- ✅ **Compose** `.` — chain functions
- ✅ **Bind** `>>` — monadic-style sequencing
- ✅ **Range** `[1..10]` — Haskell-style range literals

### Built-in Functions
- ✅ All **Python** standard functions (60+)
- ✅ All **Haskell Prelude** + `Data.List` functions (90+)
- ✅ Math, string, list, dict, set, IO, char operations

---

## Project Structure

```
Mini_Project/
│
├── Main.hs                        ← Entry point (run this)
│
├── src/
│   ├── Types.hs                   ← All data types (tokens, AST, values, errors)
│   ├── Lexer.hs                   ← Tokenizer / lexical analysis
│   ├── Parser.hs                  ← Recursive descent parser
│   ├── Environment.hs             ← Runtime variable scopes
│   ├── Evaluator.hs               ← Tree-walking interpreter
│   ├── PatternMatch.hs            ← Pattern matching engine
│   ├── Pretty.hs                  ← Pretty-printing for output
│   ├── REPL.hs                    ← Interactive REPL
│   └── Builtins/
│       ├── Core.hs                ← Shared utilities
│       ├── Python.hs              ← Python-style built-ins
│       └── Haskell.hs             ← Haskell-style built-ins
│
├── examples/
│   ├── sample.mf                  ← Full language demo
│   ├── pipes.mf                   ← Pipe/compose/bind demos
│   └── fibonacci.mf               ← 8 Fibonacci implementations
│
├── COMPLEXITY.md                  ← Time & space complexity analysis
├── STEP_BY_STEP.md                ← Setup and usage guide
└── README.md                      ← This file
```

---

## Installation

### Requirements
- **GHC** (Glasgow Haskell Compiler) 8.x or higher
- **Windows / macOS / Linux**

### Install GHC

**Windows:**
```bash
# Download GHCup installer from:
https://www.haskell.org/ghcup/

# Then in PowerShell:
ghcup install ghc
ghcup install cabal
```

**macOS:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

**Linux (Ubuntu/Debian):**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
# or
sudo apt install ghc
```

**Verify installation:**
```bash
ghc --version
# GHC version 9.x.x
```

---

## Running the Interpreter

### Clone the Repository
```bash
git clone https://github.com/YOUR_USERNAME/miniflow-interpreter.git
cd miniflow-interpreter
```

### Run an Example File
```bash
runhaskell -isrc Main.hs examples/sample.mf
```

### Run All Examples
```bash
runhaskell -isrc Main.hs examples/sample.mf
runhaskell -isrc Main.hs examples/pipes.mf
runhaskell -isrc Main.hs examples/fibonacci.mf
```

### Start the REPL (Interactive Mode)
```bash
runhaskell -isrc Main.hs
```

### Compile for Faster Execution
```bash
ghc -isrc -o miniflow Main.hs
./miniflow examples/sample.mf
./miniflow                        # REPL mode
```

### Run Your Own Code
```bash
# Create mycode.mf, then:
runhaskell -isrc Main.hs mycode.mf
```

---

## Language Syntax

### Variables
```python
let x = 10
let name = "Alice"
let nums = [1, 2, 3, 4, 5]
let point = {x: 3.0, y: 4.0}
```

### Functions
```python
def add(a, b):
    a + b

def greet(name, msg="Hello"):
    f"{msg}, {name}!"

# Recursive
def factorial(n):
    if n <= 1: 1
    else: n * factorial(n - 1)
```

### Lambda
```python
let double  = \x -> x * 2
let add5    = lambda x: x + 5
let compose = \f g x -> f(g(x))
```

### Pipe Operator
```python
# a |> f  is the same as  f(a)
5 |> double |> str |> print

# Pipeline
[1..10] |> filter(even) |> map(lambda x: x*x) |> sum |> print
```

### Function Composition
```python
# f . g  creates h(x) = f(g(x))
let shout    = str.upper . str.strip
let process  = str . double . increment
```

### Pattern Matching
```python
def describe(n):
    match n:
        case 0:         "zero"
        case 1:         "one"
        case n if n < 0: "negative"
        case _:         "other"
```

### List Comprehension
```python
let squares = [x*x for x in range(1, 11)]
let filtered = [x for x in range(20) if odd(x) if x > 5]
let matrix = [(i, j) for i in range(3) for j in range(3)]
```

### Records
```python
record Point { x: Float, y: Float }
record Person { name: String, age: Int }

let p = Point { x: 3.0, y: 4.0 }
print(p.x, p.y)
```

### Error Handling
```python
try:
    let result = 10 / 0
except ZeroDivisionError as e:
    print("Error:", e)
```

### Loops
```python
for i in range(5):
    print(i)

while x > 0:
    x = x - 1
```

---

## Built-in Functions

### Python-Style (60+)

| Category | Functions |
|---|---|
| I/O | `print`, `input`, `open` |
| Types | `int`, `float`, `str`, `bool`, `list`, `dict`, `set`, `tuple` |
| Numbers | `abs`, `round`, `min`, `max`, `sum`, `pow`, `divmod` |
| Strings | `chr`, `ord`, `format`, `repr`, `len` |
| Collections | `range`, `enumerate`, `zip`, `map`, `filter`, `sorted`, `reversed` |
| Functional | `reduce`, `any`, `all`, `zip_longest` |
| Inspection | `type`, `isinstance`, `callable`, `hasattr`, `getattr`, `dir` |
| Encoding | `bin`, `oct`, `hex`, `hash` |

### Haskell-Style (90+)

| Category | Functions |
|---|---|
| List basics | `head`, `tail`, `init`, `last`, `null`, `length`, `reverse` |
| Sublists | `take`, `drop`, `takeWhile`, `dropWhile`, `span`, `splitAt` |
| Folds | `foldr`, `foldl`, `foldl1`, `foldr1`, `scanl`, `scanr` |
| Mapping | `map`, `filter`, `concatMap`, `mapM`, `forM` |
| Zipping | `zip`, `zip3`, `unzip`, `zipWith`, `zipWith3` |
| Searching | `elem`, `notElem`, `find`, `findIndex`, `elemIndex`, `lookup` |
| Sorting | `sort`, `sortBy`, `sortOn`, `nub`, `nubBy`, `group`, `groupBy` |
| Math | `sqrt`, `sin`, `cos`, `tan`, `log`, `exp`, `floor`, `ceiling` |
| Numbers | `odd`, `even`, `gcd`, `lcm`, `abs`, `signum`, `negate` |
| Strings | `words`, `unwords`, `lines`, `unlines`, `show`, `read` |
| Combinators | `id`, `const`, `flip`, `curry`, `uncurry`, `fix`, `until` |
| Infinite | `iterate`, `repeat`, `replicate`, `cycle` |
| Data.List | `intercalate`, `intersperse`, `transpose`, `permutations`, `subsequences` |
| Maybe | `fromMaybe`, `catMaybes`, `mapMaybe`, `listToMaybe` |

---

## Pipe & Pipeline Operators

### Pipe `|>`
```python
# value |> function
5 |> double              # double(5) = 10
"hello" |> str.upper     # "HELLO"

# Chained pipeline
[1..20] |> filter(even) |> map(lambda x: x**2) |> sum |> print
```

### Compose `.`
```python
# f . g  →  h(x) = f(g(x))
let shout = str.upper . str.strip
shout("  hello  ")       # "HELLO"
```

### Bind `>>`
```python
# ma >> f  →  f(ma)
5 >> add1 >> double >> str >> print
```

---

## Examples

### Fibonacci (multiple styles)
```python
# Recursive
def fib(n):
    match n:
        case 0: 0
        case 1: 1
        case _: fib(n-1) + fib(n-2)

# Haskell-style infinite list
let fibs = iterate(lambda p: (p[1], p[0]+p[1]), (0,1))
           |> map(lambda p: p[0])
           |> take(10)
```

### FizzBuzz via pipeline
```python
def fizzbuzz(n):
    if n % 15 == 0: "FizzBuzz"
    elif n % 3 == 0: "Fizz"
    elif n % 5 == 0: "Buzz"
    else: str(n)

range(1, 21) |> map(fizzbuzz) |> print
```

### Word frequency
```python
let text = "the quick brown fox jumps over the lazy dog"
text.split(" ") |> sorted |> group
|> map(lambda g: (head(g), length(g))) |> print
```

---

## Time & Space Complexity

See [COMPLEXITY.md](COMPLEXITY.md) for full analysis. Summary:

| Component | Time | Space |
|---|---|---|
| Lexer | O(n) | O(n) |
| Parser | O(n) | O(n) |
| Evaluator | O(n log n) avg | O(n) |
| Pipe `\|>` operator | O(1) overhead | O(1) |
| `sort` | O(n log n) | O(n) |
| `foldr/foldl` | O(n) | O(1) |
| `map/filter` | O(n × f) | O(r) |
| Pattern match | O(arms × pattern) | O(bindings) |

---

## Online Hosting

### Replit (Easiest)
1. Go to **https://replit.com**
2. New Repl → Language: **Haskell**
3. Upload all project files
4. In Shell: `runhaskell -isrc Main.hs examples/sample.mf`
5. Share the Replit URL

### Wandbox (No signup)
1. Go to **https://wandbox.org**
2. Select **Haskell (GHC)**
3. Paste and run

---

## Course Information

| Field | Details |
|---|---|
| Course | 23CSE212 — Principles of Programming Languages |
| Semester | SEM 4 |
| Project | MiniFlow Language Interpreter |
| Language | Haskell (GHC) |
| Lines of Code | 7,982 |

---

## License

MIT License — free to use, modify, and distribute.

# MiniFlow Language Interpreter — Time & Space Complexity Analysis

## Overview

This document provides a comprehensive analysis of the time and space complexity
for every major component of the MiniFlow interpreter and every built-in function.

---

## 1. LEXER (src/Lexer.hs)

| Operation          | Time Complexity | Space Complexity | Notes |
|--------------------|----------------|------------------|-------|
| `tokenize`         | O(n)           | O(n)             | n = source length in chars |
| `nextToken`        | O(k)           | O(1)             | k = token length |
| `lexString`        | O(s)           | O(s)             | s = string literal length |
| `lexNumber`        | O(d)           | O(d)             | d = digits in number |
| `lexIdent`         | O(w)           | O(w)             | w = word length |
| `lexFStringParts`  | O(n)           | O(n)             | n = f-string length |
| Full tokenization  | **O(n)**       | **O(n)**         | linear pass over source |

---

## 2. PARSER (src/Parser.hs)

| Operation               | Time Complexity | Space Complexity | Notes |
|-------------------------|----------------|------------------|-------|
| `parseProgram`          | O(t)           | O(t)             | t = token count |
| `parseExpr`             | O(t)           | O(d)             | d = nesting depth |
| `parsePipeExpr`         | O(t)           | O(d)             | d = pipe chain depth |
| `parseLambdaExpr`       | O(p + t)       | O(p)             | p = param count |
| `parseListComp`         | O(t)           | O(c)             | c = clause count |
| `parseMatchArms`        | O(a * t)       | O(a)             | a = arm count |
| `parsePattern`          | O(p)           | O(p)             | p = pattern complexity |
| Full parse              | **O(t)**       | **O(t)**         | AST size ≤ token count |

The parser is LL(1) (one-token lookahead), making it linear.

---

## 3. ENVIRONMENT (src/Environment.hs)

| Operation       | Time Complexity   | Space Complexity | Notes |
|-----------------|-------------------|------------------|-------|
| `newGlobalEnv`  | O(1)              | O(1)             | empty map |
| `newChildEnv`   | O(1)              | O(1)             | just an IORef |
| `lookupVar`     | O(log k + d)      | O(1)             | k = bindings in frame, d = chain depth |
| `defineVar`     | O(log k)          | O(1)             | Map.insert |
| `assignVar`     | O(d * log k)      | O(1)             | walks chain |
| `extendEnv`     | O(b log b)        | O(b)             | b = bindings count |
| `envToList`     | O(n)              | O(n)             | n = total visible bindings |
| `envDepth`      | O(d)              | O(d)             | d = scope depth |

**Note:** Using `Data.Map.Strict` gives O(log k) per key operation.

---

## 4. EVALUATOR (src/Evaluator.hs)

### Expression Evaluation

| Expression        | Time Complexity        | Space Complexity | Notes |
|-------------------|------------------------|------------------|-------|
| Literal           | O(1)                   | O(1)             | |
| Variable lookup   | O(log k + d)           | O(1)             | |
| Binary op (arith) | O(1)                   | O(1)             | |
| Binary op (list+) | O(n)                   | O(n)             | list concat |
| Unary op          | O(1)                   | O(1)             | |
| List literal      | O(n)                   | O(n)             | |
| Tuple literal     | O(n)                   | O(n)             | |
| Dict literal      | O(n log n)             | O(n)             | Map.fromList |
| List comprehension| O(n * f)               | O(r)             | f = body eval cost, r = result size |
| Dict comprehension| O(n * f * log r)       | O(r)             | |
| Set comprehension | O(n * f * r)           | O(r)             | dedup is O(r) extra |
| F-string          | O(p)                   | O(p)             | p = part count |
| Field access      | O(log k)               | O(1)             | |
| Indexing          | O(log k) or O(i)       | O(1)             | dict: log k, list: O(i) |
| Slicing           | O(s)                   | O(s)             | s = slice length |
| Match expr        | O(a * p)               | O(b)             | a = arms, p = pattern match cost, b = bindings |
| Lambda creation   | O(1)                   | O(e)             | e = captured env size |
| **Pipe** `\|>`    | O(eval(lhs) + apply)   | O(1) extra       | constant overhead |
| **Compose** `.`   | O(1) (returns closure) | O(e)             | application is O(eval(g)+eval(f)) |
| **Bind** `>>`     | O(eval(ma) + apply(f)) | O(1) extra       | |

### Statement Evaluation

| Statement          | Time Complexity   | Space Complexity | Notes |
|--------------------|-------------------|------------------|-------|
| Assign             | O(match + eval)   | O(bindings)      | |
| FunDef             | O(1)              | O(e)             | just creates closure |
| If/elif chain      | O(b * cond + body)| O(d)             | b = branches |
| While loop         | O(i * body)       | O(d)             | i = iterations |
| For loop           | O(n * body)       | O(n)             | n = iterable size |
| Match stmt         | O(a * p + body)   | O(bindings)      | |
| Try/Catch          | O(try + handler)  | O(d)             | |
| Return             | O(expr)           | O(1)             | uses exception |

### Function Application

| Scenario            | Time Complexity       | Space Complexity |
|---------------------|-----------------------|------------------|
| Builtin call        | O(f)                  | O(args)          |
| Closure call        | O(param_bind + body)  | O(params + env)  |
| Recursive call (depth d) | O(d * body)    | O(d)             |

---

## 5. PATTERN MATCHING (src/PatternMatch.hs)

| Pattern              | Time Complexity | Space Complexity |
|----------------------|----------------|------------------|
| Wildcard `_`         | O(1)           | O(1)             |
| Variable `x`         | O(log k)       | O(1)             |
| Literal              | O(1)           | O(1)             |
| List `[p1, p2, ...]` | O(n * p)       | O(n)             |
| Tuple `(p1, p2, ...)` | O(n * p)      | O(n)             |
| Cons `h:t`           | O(p_h + p_t)   | O(1)             |
| Record `R {f: p}`    | O(log k + p)   | O(b)             |
| As `p as x`          | O(p + log k)   | O(1)             |
| Or `p1 \| p2`        | O(p1 + p2)     | O(max(b1,b2))    |
| `tryMatchArm` (all arms) | O(a * p)  | O(b_max)         |

---

## 6. PYTHON BUILT-IN FUNCTIONS (src/Builtins/Python.hs)

| Function         | Time        | Space   | Notes |
|------------------|-------------|---------|-------|
| `print(*args)`   | O(Σ len(str(a))) | O(1) | IO, not in-memory |
| `input(prompt)`  | O(1) call   | O(L)   | L = input line length |
| `len(x)`         | O(1) list/tuple, O(n) string | O(1) | |
| `range(n)`       | O(n)        | O(n)   | creates list |
| `range(a,b,s)`   | O((b-a)/s)  | O(n)   | |
| `enumerate(xs)`  | O(n)        | O(n)   | |
| `zip(*iters)`    | O(min(n_i)) | O(n)   | |
| `map(f, xs)`     | O(n * f)    | O(n)   | |
| `filter(f, xs)`  | O(n * f)    | O(r)   | r = result size |
| `sorted(xs)`     | O(n log n)  | O(n)   | Haskell's sort |
| `sorted(xs,key)` | O(n log n * k) | O(n) | k = key fn cost |
| `reversed(xs)`   | O(n)        | O(n)   | |
| `sum(xs)`        | O(n)        | O(1)   | fold |
| `min(xs)`        | O(n)        | O(1)   | |
| `max(xs)`        | O(n)        | O(1)   | |
| `any(xs)`        | O(n) worst  | O(1)   | short-circuits |
| `all(xs)`        | O(n) worst  | O(1)   | short-circuits |
| `abs(x)`         | O(1)        | O(1)   | |
| `round(x,n)`     | O(1)        | O(1)   | |
| `int(x)`         | O(s) str    | O(1)   | s = string length |
| `float(x)`       | O(s) str    | O(1)   | |
| `str(x)`         | O(n)        | O(n)   | n = value size |
| `bool(x)`        | O(1)        | O(1)   | |
| `list(x)`        | O(n)        | O(n)   | |
| `dict(pairs)`    | O(n log n)  | O(n)   | Map.fromList |
| `set(xs)`        | O(n²) worst | O(n)   | using nub |
| `chr(n)`         | O(1)        | O(1)   | |
| `ord(c)`         | O(1)        | O(1)   | |
| `type(x)`        | O(1)        | O(1)   | |
| `isinstance(x,t)` | O(1)       | O(1)   | |
| `isinstance(x,(t1,t2,...))` | O(k) | O(1) | k = types count |
| `hasattr(obj,n)` | O(log k)    | O(1)   | |
| `getattr(obj,n)` | O(log k)    | O(1)   | |
| `setattr(obj,n,v)` | O(log k)  | O(1)   | |
| `dir(obj)`       | O(k)        | O(k)   | k = field count |
| `reduce(f,xs)`   | O(n * f)    | O(1)   | |
| `reduce(f,xs,i)` | O(n * f)    | O(1)   | |
| `hash(x)`        | O(s) str    | O(1)   | s = string length |
| `callable(x)`    | O(1)        | O(1)   | |
| `next(it)`       | O(1)        | O(1)   | |
| `iter(xs)`       | O(n)        | O(n)   | copies list |
| `format(fmt,...)`| O(n)        | O(n)   | n = format string length |
| `repr(x)`        | O(n)        | O(n)   | |
| `divmod(a,b)`    | O(1)        | O(1)   | |
| `pow(a,b)`       | O(log b)    | O(1)   | fast exponentiation |
| `pow(a,b,m)`     | O(log b)    | O(1)   | modular exp |
| `bin/oct/hex(n)` | O(log n)    | O(log n)| |
| `product(xs)`    | O(n)        | O(1)   | fold |

---

## 7. HASKELL BUILT-IN FUNCTIONS (src/Builtins/Haskell.hs)

| Function            | Time          | Space   | Notes |
|---------------------|---------------|---------|-------|
| `head(xs)`          | O(1)          | O(1)    | |
| `tail(xs)`          | O(n)          | O(n)    | copies list |
| `init(xs)`          | O(n)          | O(n)    | |
| `last(xs)`          | O(n)          | O(1)    | traversal |
| `null(xs)`          | O(1)          | O(1)    | |
| `length(xs)`        | O(n)          | O(1)    | |
| `reverse(xs)`       | O(n)          | O(n)    | |
| `concat(xss)`       | O(Σn_i)       | O(Σn_i) | |
| `concatMap(f,xs)`   | O(n * f)      | O(r)    | |
| `take(n,xs)`        | O(n)          | O(n)    | |
| `drop(n,xs)`        | O(n)          | O(m-n)  | |
| `takeWhile(p,xs)`   | O(n * p)      | O(r)    | |
| `dropWhile(p,xs)`   | O(n * p)      | O(r)    | |
| `span(p,xs)`        | O(n * p)      | O(n)    | |
| `break_(p,xs)`      | O(n * p)      | O(n)    | |
| `splitAt(n,xs)`     | O(n)          | O(n)    | |
| `elem(x,xs)`        | O(n)          | O(1)    | linear search |
| `notElem(x,xs)`     | O(n)          | O(1)    | |
| `lookup(k,assoc)`   | O(n)          | O(1)    | assoc list |
| `maximum(xs)`       | O(n)          | O(1)    | |
| `minimum(xs)`       | O(n)          | O(1)    | |
| `sum(xs)`           | O(n)          | O(1)    | strict fold |
| `product(xs)`       | O(n)          | O(1)    | |
| `foldr(f,z,xs)`     | O(n * f)      | O(n)    | builds call stack |
| `foldl(f,z,xs)`     | O(n * f)      | O(1)    | tail recursive |
| `foldl1(f,xs)`      | O(n * f)      | O(1)    | |
| `foldr1(f,xs)`      | O(n * f)      | O(n)    | |
| `scanl(f,z,xs)`     | O(n * f)      | O(n)    | |
| `scanr(f,z,xs)`     | O(n * f)      | O(n)    | |
| `zip(xs,ys)`        | O(min(m,n))   | O(min)  | |
| `zip3(xs,ys,zs)`    | O(min(m,n,p)) | O(min)  | |
| `unzip(pairs)`      | O(n)          | O(n)    | |
| `zipWith(f,xs,ys)`  | O(min(m,n)*f) | O(r)    | |
| `zipWith3(f,...)`   | O(min*f)      | O(r)    | |
| `any(p,xs)`         | O(n*p) worst  | O(1)    | short-circuit |
| `all(p,xs)`         | O(n*p) worst  | O(1)    | short-circuit |
| `odd(n)`            | O(1)          | O(1)    | |
| `even(n)`           | O(1)          | O(1)    | |
| `gcd(a,b)`          | O(log(min))   | O(1)    | Euclidean |
| `lcm(a,b)`          | O(log(min))   | O(1)    | |
| `abs(x)`            | O(1)          | O(1)    | |
| `signum(x)`         | O(1)          | O(1)    | |
| `floor/ceiling/round/truncate` | O(1) | O(1) | |
| `iterate(f,x)`      | O(1) create   | O(1)    | lazy; O(k) for take(k) |
| `repeat(x)`         | O(1) create   | O(1)    | lazy; O(k) for take(k) |
| `replicate(n,x)`    | O(n)          | O(n)    | |
| `cycle(xs)`         | O(1) create   | O(k)    | k = taken elements |
| `words(s)`          | O(n)          | O(w)    | w = word count |
| `unwords(xs)`       | O(n)          | O(n)    | |
| `lines(s)`          | O(n)          | O(l)    | l = line count |
| `unlines(xs)`       | O(n)          | O(n)    | |
| `show(x)`           | O(n)          | O(n)    | |
| `read(s)`           | O(n)          | O(1)    | |
| `nub(xs)`           | O(n²)         | O(n)    | O(n log n) with Set |
| `sort(xs)`          | O(n log n)    | O(n)    | merge sort |
| `sortBy(cmp,xs)`    | O(n log n * c)| O(n)    | c = comparator cost |
| `sortOn(f,xs)`      | O(n log n * f)| O(n)    | |
| `groupBy(eq,xs)`    | O(n * eq)     | O(n)    | |
| `group(xs)`         | O(n)          | O(n)    | |
| `tails(xs)`         | O(n²)         | O(n²)   | all suffixes |
| `inits(xs)`         | O(n²)         | O(n²)   | all prefixes |
| `permutations(xs)`  | O(n! * n)     | O(n!)   | exponential! |
| `subsequences(xs)`  | O(2^n * n)    | O(2^n)  | exponential! |
| `transpose(xss)`    | O(Σn_i)       | O(Σn_i) | |
| `intercalate(sep,xs)` | O(n * s)   | O(n)    | s = sep length |
| `intersperse(x,xs)` | O(n)          | O(n)    | |
| `isPrefixOf(pre,xs)` | O(p)         | O(1)    | p = prefix length |
| `isSuffixOf(suf,xs)` | O(s + n)     | O(1)    | |
| `isInfixOf(inf,xs)` | O(n * i)      | O(1)    | i = infix length |
| `stripPrefix(pre,xs)` | O(p)        | O(n-p)  | |
| `nubBy(eq,xs)`      | O(n² * eq)    | O(n)    | |
| `partition(p,xs)`   | O(n * p)      | O(n)    | |
| `find(p,xs)`        | O(n * p)      | O(1)    | |
| `findIndex(p,xs)`   | O(n * p)      | O(1)    | |
| `elemIndex(x,xs)`   | O(n)          | O(1)    | |
| `id(x)`             | O(1)          | O(1)    | |
| `const(a,b)`        | O(1)          | O(1)    | |
| `flip(f)`           | O(1) create   | O(1)    | |
| `curry(f)`          | O(1) create   | O(1)    | |
| `uncurry(f)`        | O(1) create   | O(1)    | |
| `until(p,f,x)`      | O(k * (p+f))  | O(1)    | k = iterations |
| `fix(f)`            | O(k * f)      | O(d)    | d = recursion depth |
| `on(f,g)`           | O(1) create   | O(1)    | |
| `comparing(f)`      | O(1) create   | O(1)    | |
| `fromMaybe(d,v)`    | O(1)          | O(1)    | |
| `catMaybes(xs)`     | O(n)          | O(r)    | |
| `mapMaybe(f,xs)`    | O(n * f)      | O(r)    | |
| **Math functions**  | O(1)          | O(1)    | sqrt, sin, cos, etc. |
| **Char functions**  | O(1) single char | O(1)  | isAlpha, toLower, etc. |
| `not(b)`            | O(1)          | O(1)    | |
| `and(xs)`           | O(n) worst    | O(1)    | short-circuit |
| `or(xs)`            | O(n) worst    | O(1)    | |

---

## 8. PIPE / COMPOSE / BIND OPERATORS

| Operation          | Time Complexity            | Space Complexity |
|--------------------|---------------------------|------------------|
| `a \|> f`          | O(eval(a) + apply(f,[a])) | O(1) overhead    |
| `f . g` (create)   | O(1)                       | O(env)           |
| `(f . g)(x)` (apply) | O(eval(g,x) + eval(f,_)) | O(1) overhead    |
| `ma >> f`          | O(eval(ma) + apply(f))    | O(1) overhead    |
| Pipeline of k fns  | O(Σ apply(f_i))            | O(1) overhead    |

The pipe operator has **O(1) overhead** per application — it is simply
syntactic sugar for function application with reversed argument order.

---

## 9. MEMORY MODEL

| Structure          | Space           | Notes |
|--------------------|----------------|-------|
| `VInt n`           | O(1)            | boxed integer |
| `VFloat f`         | O(1)            | 64-bit double |
| `VBool b`          | O(1)            | |
| `VStr s`           | O(n)            | n = string length |
| `VList ref`        | O(n)            | IORef + list |
| `VTuple vs`        | O(n)            | n = tuple length |
| `VDict ref`        | O(n log n)      | Map (BST) |
| `VSet ref`         | O(n)            | nub'd list |
| `VClosure`         | O(env)          | captures env |
| `VRecord`          | O(k)            | k = field count |
| `VLazyList`        | O(forced)       | only what's been forced |
| Environment frame  | O(k)            | k = bindings |
| Scope chain (depth d) | O(d * k_avg) | sum of all frames |

---

## 10. OVERALL INTERPRETER COMPLEXITY

For a program with:
- **n** = total AST nodes
- **d** = maximum scope depth
- **k** = average bindings per scope
- **i** = total iterations (loops + recursion)
- **c** = maximum list/collection size

| Phase        | Time                   | Space            |
|--------------|------------------------|------------------|
| Lexing       | O(source_chars)        | O(tokens)        |
| Parsing      | O(tokens)              | O(AST_nodes)     |
| Evaluation   | O(n * log(d*k) + i*n) | O(d*k + c)       |
| **Total**    | **O(n * log(d*k) + i*n)** | **O(n + d*k + c)** |

For typical programs:
- `d*k` is bounded by program size → effectively **O(n log n)** time
- Space is **O(n)** for the AST + O(live values)

---

## 11. COMPLEXITY OF LANGUAGE FEATURES

| Feature              | Time               | Space  | Notes |
|----------------------|--------------------|--------|-------|
| Variable lookup      | O(log k + d)       | O(1)   | Map lookup per frame |
| Function call        | O(p + body)        | O(p+e) | p=params, e=env |
| Recursion (depth r)  | O(r * body)        | O(r)   | stack |
| Tail recursion       | O(r * body)        | O(1)   | if TCO (Haskell does) |
| Pattern match        | O(a * p)           | O(b)   | arms * pattern cost |
| List comprehension   | O(n * f)           | O(r)   | |
| Pipe chain (k pipes) | O(Σ apply_i)       | O(1)   | |
| Compose (k fns)      | O(1) create, O(Σ apply) apply | O(k closures) |
| Closures (capture)   | O(1)               | O(env) | |
| Infinite list take n | O(n * f)           | O(n)   | f = iterate fn |
| permutations(n)      | O(n! * n)          | O(n!)  | AVOID for n > 10 |
| subsequences(n)      | O(2^n)             | O(2^n) | AVOID for n > 20 |

---

## 12. ONLINE COMPILER HOSTING OPTIONS

### Option 1: Replit (Recommended)
```
1. Go to https://replit.com
2. Click "Create Repl"
3. Choose "Haskell" template
4. Upload all files:
   - Main.hs
   - src/Types.hs, src/Lexer.hs, src/Parser.hs, ...
   - examples/*.mf
5. In the shell, run:
   runhaskell -isrc Main.hs examples/sample.mf
6. Share the Replit URL
```

### Option 2: GitHub + GitHub Codespaces
```
1. Push project to GitHub repo
2. Open in GitHub Codespaces
3. GHC is available by default
4. Run: runhaskell -isrc Main.hs examples/sample.mf
```

### Option 3: Wandbox (paste & run)
```
1. Go to https://wandbox.org
2. Select "Haskell (GHC)"
3. Paste Main.hs content + inline modules
4. Click "Run"
```

### Option 4: Try Haskell Online
```
1. Go to https://www.tryhaskell.org
2. Good for testing individual expressions
3. Not suitable for multi-file projects
```

### Run Command (for any setup)
```bash
# Single-module approach (all in one file):
runhaskell Main.hs examples/sample.mf

# Multi-module approach with source path:
runhaskell -isrc Main.hs examples/sample.mf

# With GHC compilation (faster):
ghc -isrc -o miniflow Main.hs
./miniflow examples/sample.mf

# REPL mode:
./miniflow
# or
runhaskell -isrc Main.hs
```

### Required GHC Extensions/Packages
```
base         (built-in)
containers   (Data.Map, Data.Set)
transformers (State, Reader monads)
```

Install via:
```bash
cabal install containers transformers
# or with Stack:
stack install containers transformers
```

---

## Summary

The MiniFlow interpreter achieves:
- **Lexer**: O(n) time, O(n) space — optimal
- **Parser**: O(n) time, O(n) space — LL(1) recursive descent
- **Evaluator**: O(n log n) average, O(n) space — tree walking
- **Built-in functions**: Matching Python/Haskell standard library complexities
- **Pipe operator**: O(1) overhead — zero-cost abstraction
- **Pattern matching**: O(arms × pattern_size) — efficient for typical programs

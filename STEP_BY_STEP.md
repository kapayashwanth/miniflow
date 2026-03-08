# MiniFlow — Step-by-Step Setup & Usage Guide

This guide walks you through everything from installing Haskell to writing your own MiniFlow programs.

---

## PART 1 — INSTALLATION

### Step 1: Install GHC (Haskell Compiler)

#### Windows
1. Open your browser and go to:
   ```
   https://www.haskell.org/ghcup/
   ```
2. Download the **GHCup** installer for Windows
3. Run the installer — it installs GHC automatically
4. Open **Command Prompt** and verify:
   ```bash
   ghc --version
   ```
   Expected output:
   ```
   The Glorious Glasgow Haskell Compilation System, version 9.x.x
   ```

#### macOS
1. Open **Terminal**
2. Run:
   ```bash
   curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
   ```
3. Follow the on-screen prompts
4. Restart Terminal, then verify:
   ```bash
   ghc --version
   ```

#### Linux (Ubuntu / Debian)
```bash
sudo apt update
sudo apt install ghc
ghc --version
```

---

### Step 2: Download the Project

#### Option A — Clone from GitHub
```bash
git clone https://github.com/YOUR_USERNAME/miniflow-interpreter.git
cd miniflow-interpreter
```

#### Option B — Download ZIP
1. Go to the GitHub repository page
2. Click **Code → Download ZIP**
3. Extract the ZIP
4. Open terminal and `cd` into the extracted folder

#### Option C — Already have the files
```bash
cd C:\Users\kapay\SEM4\23CSE212_PFPL\Mini_Project
```

---

### Step 3: Verify Project Structure

Run this to confirm all files are in place:

```bash
# Windows
dir
dir src
dir examples

# macOS / Linux
ls
ls src/
ls examples/
```

You should see:
```
Main.hs
src/
  Types.hs
  Lexer.hs
  Parser.hs
  Environment.hs
  Evaluator.hs
  PatternMatch.hs
  Pretty.hs
  REPL.hs
  Builtins/
    Core.hs
    Python.hs
    Haskell.hs
examples/
  sample.mf
  pipes.mf
  fibonacci.mf
README.md
COMPLEXITY.md
STEP_BY_STEP.md
```

---

## PART 2 — RUNNING EXAMPLE PROGRAMS

### Step 4: Open Terminal in the Project Folder

**Windows:**
- Press `Win + R`, type `cmd`, press Enter
- Type:
  ```bash
  cd C:\Users\kapay\SEM4\23CSE212_PFPL\Mini_Project
  ```

**macOS / Linux:**
- Open Terminal
- Type:
  ```bash
  cd /path/to/Mini_Project
  ```

---

### Step 5: Run the Full Demo

```bash
runhaskell -isrc Main.hs examples/sample.mf
```

**What you should see:**
```
Hello from MiniFlow
x = 10  y = 3.14
x + y = 13.14
...
Sum of squares of even [1..10]: 220
...
All done! MiniFlow is running correctly.
```

---

### Step 6: Run the Pipe Operator Demo

```bash
runhaskell -isrc Main.hs examples/pipes.mf
```

**What you should see:**
```
=== PIPE OPERATOR |> ===
5 |> double = 10
sum of squares of evens [1..10] = 220
...
=== FUNCTION COMPOSITION . ===
...
=== BIND OPERATOR >> ===
...
```

---

### Step 7: Run the Fibonacci Demo

```bash
runhaskell -isrc Main.hs examples/fibonacci.mf
```

**What you should see:**
```
=== Fibonacci Implementations ===
Naive fib(10): 55
Tail-recursive fib(20): 6765
Loop fib(30): 832040
Infinite list fibs: [0,1,1,2,3,5,8,13,21,34]
...
```

---

## PART 3 — WRITING YOUR OWN CODE

### Step 8: Create a New `.mf` File

Open any text editor (Notepad, VS Code, Notepad++) and create a file named `mycode.mf` inside the `Mini_Project` folder.

#### Starter Code:
```python
# My first MiniFlow program

# 1. Variables
let name = "Student"
let score = 95

# 2. Print
print("Hello,", name)
print("Your score is:", score)

# 3. Function
def grade(s):
    if s >= 90:   "A"
    elif s >= 80: "B"
    elif s >= 70: "C"
    else:         "F"

print("Grade:", grade(score))

# 4. Pipeline
let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
let result = numbers |> filter(even) |> map(lambda x: x * x) |> sum
print("Sum of squares of even numbers:", result)

# 5. List comprehension
let squares = [x*x for x in range(1, 6)]
print("Squares:", squares)
```

---

### Step 9: Run Your File

```bash
runhaskell -isrc Main.hs mycode.mf
```

**Expected output:**
```
Hello, Student
Your score is: 95
Grade: A
Sum of squares of even numbers: 220
Squares: [1, 4, 9, 16, 25]
```

---

## PART 4 — USING THE REPL

### Step 10: Start the REPL

```bash
runhaskell -isrc Main.hs
```

You will see:
```
╔══════════════════════════════════════════════════════════════╗
║           MiniFlow Language Interpreter v1.0                ║
║   Type :help for help, :q to quit                           ║
╚══════════════════════════════════════════════════════════════╝

miniflow>
```

---

### Step 11: Try REPL Commands

Type these one by one and press Enter:

```
miniflow> print("Hello, World!")
Hello, World!

miniflow> let x = 42
miniflow> x * 2
84

miniflow> [1,2,3,4,5] |> sum
15

miniflow> [1..10] |> filter(even) |> map(lambda n: n*n)
[4, 16, 36, 64, 100]

miniflow> def square(n) = n * n
miniflow> square(7)
49

miniflow> foldr(lambda a,b: a+b, 0, [1..100])
5050

miniflow> :t sqrt(144)
sqrt(144) :: float

miniflow> :env

miniflow> :q
Goodbye!
```

---

### Step 12: Load a File Inside REPL

```
miniflow> :load examples/sample.mf
Loaded: examples/sample.mf
miniflow> factorial(10)
3628800
```

---

## PART 5 — COMPILE FOR SPEED (OPTIONAL)

### Step 13: Compile to Native Executable

Instead of using `runhaskell` every time (which re-compiles), compile once:

```bash
ghc -isrc -o miniflow Main.hs
```

This creates a `miniflow` executable. Then run it directly:

```bash
# Windows
miniflow.exe examples/sample.mf
miniflow.exe

# macOS / Linux
./miniflow examples/sample.mf
./miniflow
```

Compiled programs run **5-10x faster** than `runhaskell`.

---

## PART 6 — UPLOADING TO GITHUB

### Step 14: Initialize Git Repository

```bash
cd C:\Users\kapay\SEM4\23CSE212_PFPL\Mini_Project
git init
git add .
git commit -m "Initial commit: MiniFlow Language Interpreter (7982 lines)"
```

---

### Step 15: Create GitHub Repository

1. Go to **https://github.com**
2. Click **New Repository** (top right **+** button)
3. Fill in:
   - Repository name: `miniflow-interpreter`
   - Description: `A Python+Haskell inspired language interpreter in Haskell (7982 lines)`
   - Visibility: **Public**
4. Click **Create Repository**

---

### Step 16: Push to GitHub

Copy the commands GitHub shows you, or use:

```bash
git remote add origin https://github.com/YOUR_USERNAME/miniflow-interpreter.git
git branch -M main
git push -u origin main
```

Replace `YOUR_USERNAME` with your actual GitHub username.

---

### Step 17: Verify on GitHub

1. Open **https://github.com/YOUR_USERNAME/miniflow-interpreter**
2. You should see all files:
   - `README.md` displayed at the bottom (auto-rendered)
   - `Main.hs`, `src/`, `examples/` folders
   - `COMPLEXITY.md`, `STEP_BY_STEP.md`

---

## PART 7 — ONLINE COMPILER (NO INSTALLATION)

### Step 18: Run on Replit

1. Go to **https://replit.com**
2. Click **+ Create Repl**
3. Select language: **Haskell**
4. Name it: `MiniFlow`
5. Upload files:
   - Click the **Files** panel on the left
   - Create folders `src/` and `src/Builtins/` and `examples/`
   - Upload each `.hs` and `.mf` file
6. In the **Shell** tab, run:
   ```bash
   runhaskell -isrc Main.hs examples/sample.mf
   ```
7. Click **Share** to get a public link

---

## PART 8 — QUICK SYNTAX CHEAT SHEET

```python
# ── Variables ──────────────────────────────────────
let x      = 10
let name   = "Alice"
let nums   = [1, 2, 3, 4, 5]
let info   = {"key": "value"}

# ── Functions ──────────────────────────────────────
def add(a, b):
    a + b

def greet(name, msg="Hello"):
    f"{msg}, {name}!"

# ── Lambda ─────────────────────────────────────────
let double = \x -> x * 2
let add5   = lambda x: x + 5

# ── Pipe |> ────────────────────────────────────────
5 |> double |> str |> print
[1..10] |> filter(odd) |> sum |> print

# ── Compose . ──────────────────────────────────────
let shout  = str.upper . str.strip
let f      = double . add5

# ── Conditionals ───────────────────────────────────
if x > 0:
    print("positive")
elif x == 0:
    print("zero")
else:
    print("negative")

# ── Loops ──────────────────────────────────────────
for i in range(5):
    print(i)

while x > 0:
    x = x - 1

# ── Pattern Matching ───────────────────────────────
match x:
    case 0:         "zero"
    case 1:         "one"
    case n if n<0:  "negative"
    case _:         "other"

# ── List Comprehension ─────────────────────────────
let squares = [x*x for x in range(10) if odd(x)]

# ── Records ────────────────────────────────────────
record Point { x: Float, y: Float }
let p = Point { x: 3.0, y: 4.0 }
print(p.x, p.y)

# ── Error Handling ─────────────────────────────────
try:
    10 / 0
except ZeroDivisionError as e:
    print("Error:", e)

# ── Haskell Functions ──────────────────────────────
foldr(add, 0, [1..10])
foldl(lambda a,x: a*x, 1, [1..5])
scanl(add, 0, [1,2,3,4,5])
takeWhile(lambda x: x < 5, [1..10])
zipWith(add, [1,2,3], [10,20,30])
nub([1,2,1,3,2,4])
sortOn(len, ["banana","apple","fig"])
```

---

## PART 9 — TROUBLESHOOTING

| Problem | Cause | Solution |
|---|---|---|
| `runhaskell: command not found` | GHC not installed | Install from haskell.org/ghcup |
| `Could not find module 'Types'` | Missing `-isrc` flag | Use `runhaskell -isrc Main.hs` |
| `No such file or directory` | Wrong folder | `cd` into `Mini_Project` first |
| `parse error on input` | Syntax error in `.mf` file | Check indentation and colons |
| Very slow execution | Using `runhaskell` | Compile with `ghc -isrc -o miniflow Main.hs` |
| `permutations` hangs | List too large | Only use on lists with ≤ 8 elements |

---

## Summary of All Commands

```bash
# Run example files
runhaskell -isrc Main.hs examples/sample.mf
runhaskell -isrc Main.hs examples/pipes.mf
runhaskell -isrc Main.hs examples/fibonacci.mf

# Run your own file
runhaskell -isrc Main.hs mycode.mf

# Start REPL
runhaskell -isrc Main.hs

# Compile (one time)
ghc -isrc -o miniflow Main.hs

# Run compiled version
./miniflow examples/sample.mf    # macOS/Linux
miniflow.exe examples/sample.mf  # Windows

# Git commands
git init
git add .
git commit -m "MiniFlow interpreter"
git remote add origin https://github.com/YOUR_USERNAME/miniflow-interpreter.git
git push -u origin main
```

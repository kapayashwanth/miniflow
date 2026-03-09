# MiniFlow Language Grammar

> Formal BNF / EBNF grammar specification for MiniFlow v1.0

---

## Notation

| Symbol         | Meaning                              |
|----------------|--------------------------------------|
| `→`            | Production rule                      |
| `*`            | Zero or more                         |
| `+`            | One or more                          |
| `?`            | Optional (zero or one)               |
| `\|`           | Alternation                          |
| `( … )`        | Grouping                             |
| `'token'`      | Terminal token (literal)             |
| `UPPER`        | Token class from lexer               |
| `lower`        | Non-terminal                         |

Indentation-sensitive blocks are delimited by `INDENT` / `DEDENT` tokens
injected by the lexer (Python-style).

---

## Top Level

```bnf
program       → statement* EOF

statement     → simple_stmt NEWLINE
              | compound_stmt

simple_stmt   → let_stmt
              | assign_stmt
              | augassign_stmt
              | return_stmt
              | break_stmt
              | continue_stmt
              | import_stmt
              | expr_stmt

compound_stmt → def_stmt
              | if_stmt
              | for_stmt
              | while_stmt
              | match_stmt
              | try_stmt
              | record_stmt
```

---

## Declarations

```bnf
let_stmt      → 'let' pattern '=' expr

def_stmt      → 'def' IDENT '(' param_list? ')' (':' type_ann)? ':' block

param_list    → param (',' param)*
param         → IDENT ('=' expr)?       -- positional with optional default
              | '*' IDENT               -- variadic *args
              | '**' IDENT              -- keyword **kwargs

record_stmt   → 'record' IDENT '{' field_decl (',' field_decl)* '}'
field_decl    → IDENT ':' type_ann

import_stmt   → 'import' IDENT ('as' IDENT)?
              | 'from' IDENT 'import' IDENT (',' IDENT)*
```

---

## Control Flow

```bnf
if_stmt       → 'if' expr ':' block
                ('elif' expr ':' block)*
                ('else' ':' block)?

for_stmt      → 'for' pattern 'in' expr ':' block

while_stmt    → 'while' expr ':' block

return_stmt   → 'return' expr?
break_stmt    → 'break'
continue_stmt → 'continue'
```

---

## Pattern Matching

```bnf
match_stmt    → 'match' expr ':' INDENT case_clause+ DEDENT

case_clause   → 'case' pattern ('if' expr)? ':' block

pattern       → '_'                            -- wildcard
              | IDENT                          -- variable binding
              | literal                        -- exact value match
              | '(' pattern (',' pattern)+ ')' -- tuple
              | '[' pattern (',' pattern)* ']' -- list
              | pattern ':' pattern            -- cons (head:tail)
              | pattern 'if' expr              -- guarded pattern
              | IDENT '{' field_pat* '}'       -- record destructuring
              | '(' pattern ')'                -- grouped

field_pat     → IDENT ':' pattern (',' IDENT ':' pattern)*
```

---

## Exception Handling

```bnf
try_stmt      → 'try' ':' block
                except_clause*
                ('finally' ':' block)?

except_clause → 'except' (IDENT ('as' IDENT)?)? ':' block
```

---

## Expressions

```bnf
expr          → pipe_expr

pipe_expr     → bind_expr ('|>' bind_expr)*

bind_expr     → or_expr ('>>' or_expr)*

or_expr       → and_expr ('or' and_expr)*

and_expr      → not_expr ('and' not_expr)*

not_expr      → 'not' not_expr
              | compare_expr

compare_expr  → bitor_expr (compare_op bitor_expr)*
compare_op    → '==' | '!=' | '<' | '>' | '<=' | '>=' | 'in' | 'not' 'in'
              | 'is' | 'is' 'not'

bitor_expr    → bitxor_expr ('|' bitxor_expr)*
bitxor_expr   → bitand_expr ('^' bitand_expr)*
bitand_expr   → shift_expr ('&' shift_expr)*
shift_expr    → add_expr (('<<' | '>>') add_expr)*

add_expr      → mul_expr (('+' | '-') mul_expr)*

mul_expr      → unary_expr (('*' | '/' | '//' | '%') unary_expr)*

unary_expr    → ('-' | '+' | '~') unary_expr
              | power_expr

power_expr    → compose_expr ('**' unary_expr)?

compose_expr  → postfix_expr ('.' postfix_expr)*   -- function composition

postfix_expr  → primary
                ( '(' arg_list? ')'                -- function call
                | '[' expr ']'                     -- index
                | '.' IDENT                        -- field access
                )*

primary       → literal
              | IDENT
              | '(' expr ')'
              | '(' expr (',' expr)+ ')'           -- tuple
              | '[' (expr (',' expr)*)? ']'         -- list literal
              | '[' expr 'for' comp_clause+ ('if' expr)* ']'  -- list comp
              | '{' (kv_pair (',' kv_pair)*)? '}'  -- dict literal
              | '{' expr (',' expr)* '}'            -- set literal
              | '{' expr 'for' comp_clause+ '}'     -- set comp
              | '{' kv_pair 'for' comp_clause+ '}'  -- dict comp
              | '[' expr '..' expr? ']'             -- range literal
              | lambda_expr
              | backslash_lambda
              | fstring

comp_clause   → 'for' pattern 'in' expr
lambda_expr   → 'lambda' param_list? ':' expr
backslash_lambda → '\' IDENT+ '->' expr

arg_list      → arg (',' arg)*
arg           → expr
              | IDENT '=' expr        -- keyword argument
              | '*' expr              -- unpack
              | '**' expr             -- keyword unpack

kv_pair       → expr ':' expr
```

---

## Literals

```bnf
literal       → INT
              | FLOAT
              | STRING
              | FSTRING
              | 'True'
              | 'False'
              | 'None'

INT           → [0-9]+
FLOAT         → [0-9]+ '.' [0-9]+  |  [0-9]+ ('e'|'E') [+-]? [0-9]+
STRING        → '"' (char | escape)* '"'
              | "'" (char | escape)* "'"
FSTRING       → 'f"' (char | '{' expr '}')* '"'
```

---

## Types (Annotations, not enforced)

```bnf
type_ann      → 'Int' | 'Float' | 'String' | 'Bool' | 'None'
              | 'List' '[' type_ann ']'
              | 'Dict' '[' type_ann ',' type_ann ']'
              | 'Tuple' '[' type_ann (',' type_ann)* ']'
              | IDENT                         -- user-defined record type
              | type_ann '->' type_ann        -- function type
              | '(' type_ann ')'
```

---

## Blocks

```bnf
block         → NEWLINE INDENT statement+ DEDENT
              | simple_stmt                   -- inline single-statement block
```

---

## Operator Precedence Table (highest to lowest)

| Precedence | Operators                             | Associativity |
|:----------:|---------------------------------------|---------------|
| 12         | `( )` `[ ]` `.` (field)              | Left          |
| 11         | `**`                                  | Right         |
| 10         | `*`  `/`  `//`  `%`                  | Left          |
| 9          | `+`  `-`  (binary)                    | Left          |
| 8          | `<<`  `>>`                            | Left          |
| 7          | `&`                                   | Left          |
| 6          | `^`                                   | Left          |
| 5          | `\|`                                  | Left          |
| 4          | Comparisons: `==` `!=` `<` `>` …     | Non-assoc     |
| 3          | `not`                                 | Right (unary) |
| 2          | `and`                                 | Left          |
| 1          | `or`                                  | Left          |
| 0          | `\|>` `.` (compose)  `>>`            | Left          |

---

## Lexical Categories

### Keywords

```
let   def   if   elif   else   for   while   return   break   continue
match case  try  except finally import from  as       record  in      is
and   or    not  lambda True   False  None
```

### Operators

```
+   -   *   /   //   **   %         arithmetic
==  !=  <   >   <=   >=             comparison
=   +=  -=  *=  /=                  assignment
&   |   ^   ~   <<   >>             bitwise
|>                                  pipe
.                                   compose (between functions) / field access
>>                                  bind
->                                  arrow (lambda, type annotations)
```

### Delimiters

```
(  )   [  ]   {  }   ,   :   ;
```

### Special Sequences

```
..       range constructor  [1..10]
...      spread / rest      (*args)
#        line comment
{- -}    block comment
f"..."   f-string literal
\        lambda shorthand
```

---

*See [ARCHITECTURE.md](ARCHITECTURE.md) for implementation details.*

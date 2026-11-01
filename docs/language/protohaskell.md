# ProtoHaskell Language Reference

This document describes the language accepted by the compiler.  It follows
the ProtoHaskell surface language from chapters 8–17 of *Write You a
Haskell*, with a few deliberate simplifications noted at the end.

## Modules

```haskell
module Main where

id x = x
```

The module header is optional.  A file is a sequence of top-level
declarations.  There are no imports or module namespaces; all names share one
module scope.  A file normally defines `main`; the file runner evaluates it if
present and otherwise prints inferred types.

## Layout

The lexer inserts virtual braces and semicolons.  Both layout and explicit
braces are accepted:

```haskell
whereBlock x = y
  where
    y = x + 1

caseBlock x = case x of
  Just y  -> y
  Nothing -> 0

explicit = do { putStrLn "a"; putStrLn "b" }
```

Important layout details:

- a new declaration starts at the indentation of the first declaration in the
  block;
- `where` closes a preceding `do`/`case-of` block and attaches to the
  enclosing declaration;
- layout insertion is suspended inside parentheses and brackets; use explicit
  `{}`/`;` for multi-line blocks there.

Line comments (`--`) and nested block comments (`{- ... -}`) are supported.

## Declarations

### Function equations

```haskell
f x y = x + y

map f []       = []
map f (x:xs)   = f x : map f xs
```

Multiple equations for one name are grouped by the renamer.  Equations may
use constructor, literal, tuple, list, variable, wildcard, and as-patterns.
They may carry guards and `where` blocks:

```haskell
sign x
  | x > 0     = 1
  | x < 0     = -1
  | otherwise = 0

sumTo n = go 0 1
  where
    go acc i
      | i > n     = acc
      | otherwise = go (acc + i) (i + 1)
```

Pattern guards and comma-separated guards are supported:

```haskell
positive (Just x)
  | x > 0 = x
  | otherwise = 0

bounded x
  | Just y <- x, y > 0, y < 100 = y
  | otherwise = 0
```

Top-level declarations are mutually recursive.  The inferencer orders
non-recursive dependencies before their users and treats a remaining dependency
cycle as one generalization group.

### Type signatures

```haskell
id :: forall a. a -> a
id x = x

same :: Eq2 a => a -> a -> Bool
same x y = eq2 x y
```

A signature is checked by skolemizing its quantified variables and unifying
the skolemized type with the inferred type.  Local signatures in `let`/`where`
are accepted and checked; expression annotations `(e :: T)` are checked too.

### Data declarations

Regular syntax:

```haskell
data Bool2 = False2 | True2
data Maybe2 a = Nothing2 | Just2 a
data Pair a b = Pair a b
```

GADT-style syntax is accepted:

```haskell
data Person where
  Person :: [Char] -> Int -> Person
```

Record syntax creates selector functions:

```haskell
data Person = Person { name :: [Char], age :: Int }

main = name (Person "Ada" 36)
```

The record constructor is still applied positionally
(`Person "Ada" 36`); record construction and update syntax (`Person { name = ... }`,
`p { age = ... }`) is not part of this implementation.  A field shared by
several constructors produces a selector with one equation per constructor.

### Type synonyms

```haskell
type String2 = [Char]
type Pair a = (a, a)
```

Synonyms are expanded before kind checking.  A synonym must be fully applied;
recursive synonyms are rejected.  `String` is a built-in synonym for `[Char]`.

### Fixity declarations

```haskell
infixl 6 <+>
infixr 5 <:>
infix 4 <==>
```

Precedence is an integer; associativity is left, right, or none.  Built-in
fixities follow the Haskell report for the operators the Prelude exposes.
Fixity declarations are collected in a prepass, so they may appear after their
first use in a file.

### Type classes and instances

```haskell
class Eq2 a where
  eq2 :: a -> a -> Bool

instance Eq2 Int where
  eq2 x y = x == y

instance Eq2 a => Eq2 [a] where
  eq2 xs ys = ...
```

Only single-parameter classes are supported.  A class may have superclasses:

```haskell
class Eq2 a => Ord2 a where
  cmp2 :: a -> a -> Bool
```

Instance contexts and superclass dictionaries are resolved during
elaboration.  Default methods and method-specific class constraints are not
supported; each method signature must contain only the class constraint over
the class variable.  `deriving` is rejected with an explicit diagnostic.

## Expressions

```haskell
42                    -- integer
'a'                   -- character
"abc"                 -- [Char]
True                  -- built-in Bool constructor
\x -> x               -- lambda
(\x -> x) 42          -- application
let x = 1 in x + 2    -- let
if cond then a else b -- conditional
case x of             -- case with alternatives
  Just y -> y
  Nothing -> 0
do { x <- action; print x }  -- IO do-notation
```

### Operators and sections

```haskell
1 + 2 * 3
7 `div` 2
(+ 1)
(1 +)
(+)
```

Operators are resolved through the fixity table.  A parenthesized operator is
a function value; a section becomes a one-argument lambda.  Prefix minus is
negation, implemented through `negate` (not the predecessor function).

### Lists, tuples, ranges, comprehensions

```haskell
[]
[1,2,3]
[1..5]
[1,3..9]
[2..]
(1, True)
(1, 2, 3)
[x * 2 | x <- [1,2,3], x > 1]
```

Lists and tuples desugar to constructor applications.  Ranges use the
`enumFrom`, `enumFromThen`, `enumFromTo`, and `enumFromThenTo` primitives and
can be infinite.  List comprehensions support generators, guards, and `let`
statements.

### Do notation and IO

`do` desugars to `>>=` and `>>`.  `IO` is built into the interpreter; it is not
implemented through a user-visible `Monad` class.  Available primitives are
`return`, `>>=`, `>>`, `fail`, `getLine`, `putStr`, `putStrLn`, and `print`.

## Patterns

Supported patterns:

```haskell
x                 -- variable
_                 -- wildcard
42                -- literal
Just x            -- constructor, including nesting
(x, y)            -- tuple
[x, y]            -- list
p@(Just x)        -- as-pattern
```

Patterns are non-linear only when the same variable appears twice; that is
rejected.  Case alternatives and equations may use guards and `where`.
Pattern matching is compiled to nested Core cases; a failed match raises a
structured runtime error.

## Types and kinds

```haskell
Int
Bool
Char
[Char]
(a, b)
Maybe a
Either a b
a -> b
forall a. a -> a
Eq2 a => a -> a -> Bool
```

Type constructors carry kinds.  Data declarations may have higher-kinded
parameters:

```haskell
data T1 f a = T1 (f a)
-- T1 :: (* -> *) -> * -> *

getT (T1 x) = x
-- getT :: forall f a. T1 f a -> f a
```

Kind inference unifies parameter kinds with their uses and defaults
unconstrained parameters to `*`.  Type synonyms are expanded before kind
checking.

## Built-in values

The prelude is seeded in Haskell rather than parsed from a `.fun` file.  It
contains:

- arithmetic: `+`, `-`, `*`, `/`, `div`, `mod`, `quot`, `rem`, `negate`,
  `succ`, `pred`, `iszero`, `add`;
- comparisons: `==`, `/=`, `<`, `<=`, `>`, `>=` (monomorphic over `Int`);
- booleans: `True`, `False`, `not`, `&&`, `||`, `otherwise`;
- lists: `[]`, `:`, `++`, `map`, `filter`, `foldr`, `foldl`, `head`, `tail`,
  `null`, `length`, `take`, `drop`, `reverse`, `concat`, `concatMap`, `zip`,
  `enumFrom`, `enumFromThen`, `enumFromTo`, `enumFromThenTo`;
- tuples: `fst`, `snd` (pairs);
- functions: `id`, `const`, `compose`, `.`, `flip`, `$`, `seq`, `fix`;
- errors: `error`, `undefined`;
- rendering and IO: `show`, `print`, `putStr`, `putStrLn`, `getLine`,
  `return`, `>>=`, `>>`, `fail`.

Arithmetic and comparison primitives are monomorphic `Int` operations.  There
are no overloaded literals, defaulting rules, or built-in `Num`/`Eq`/`Ord`
classes; user-defined classes demonstrate the dictionary mechanism.

## Deliberate boundaries

- No modules/imports.
- No full GADT type system; GADT syntax is accepted for ordinary data
  declarations.
- No polymorphic recursion, defaulting, or overloaded literals.
- No class default methods or method-specific class constraints.
- No `deriving` generation.
- No record construction/update syntax.
- No negative literal patterns.
- No layout insertion inside parentheses/brackets.
- No newtypes, type families, or other GHC extensions.
- No Core type checker, optimizer, STG/Imp, LLVM, or native backend.

These are recorded as boundaries rather than silent gaps; diagnostics call
them out where the parser or renamer would otherwise accept them.

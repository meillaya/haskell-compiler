# Declaration Semantics

## Program shape

A source file is a module: an optional `module Name where` header followed by
top-level declarations.  There is no required final expression and no
imports.  A file normally defines `main` for the file runner; the REPL accepts
declarations and expressions on separate lines.

```haskell
module Main where

id x = x

map f []     = []
map f (x:xs) = f x : map f xs
```

## Visibility and recursion

Top-level declarations are **mutually recursive**.  Every top-level function,
data constructor, class method, and record selector is visible to every other
top-level declaration.  Forward references are allowed:

```haskell
main = later
later = 1
```

Shadowing is resolved lexically; the renamer assigns every binder a unique
`ResolvedName`.  Duplicate top-level bindings are rejected where a duplicate is
truly a duplicate (datatypes, classes, class methods, record fields, single
signatures).  Multiple equations for the same function are the ordinary
Haskell mechanism for pattern matching and are grouped, not rejected.

## Function equations

Each equation is parsed into a pattern list, a right-hand side (plain or
guarded), and a `where` block.  The renamer groups equations by name; the
inferencer treats the group as one recursive binding; the elaborator compiles
the equations to nested Core cases with a failure continuation that tries the
next equation.

```haskell
sign x
  | x > 0     = 1
  | x < 0     = -1
  | otherwise = 0
```

Guards can be expressions, pattern guards, or comma-separated combinations.
Pattern variables from a guard scope over its right-hand side and over
subsequent guards in the same clause.

## Let and where

`let` and `where` bindings are recursive.  A `where` block scopes over all
guards and the right-hand side of its equation/alternative; it may refer to the
equation's pattern variables.  Lowering turns both into `CLetRec`.

```haskell
sumTo n = go 0 1
  where
    go acc i
      | i > n     = acc
      | otherwise = go (acc + i) (i + 1)
```

Local declarations may include signatures.  Local signatures are checked
against the inferred type; they are not yet generalized into polymorphic local
schemes beyond the inferred/local generalization rules.

## Data declarations

Regular, GADT-style, and record declarations are all represented as a data
name, parameter list, and constructor list:

```haskell
data Maybe2 a = Nothing2 | Just2 a
data Person = Person { name :: [Char], age :: Int }
data G where
  MkG :: Int -> G
```

Record fields generate selector functions.  A field shared by several
constructors produces a selector with one equation per constructor.  Record
construction/update syntax is not implemented; constructors are positional.

## Classes and instances

Class and instance declarations are first-class top-level declarations.
Class methods are top-level values with qualified schemes.  Instances are
matched by class name and head type; instance contexts become dictionary
parameters during elaboration.

## Lowering boundary

Parsing and renaming operate on the `Program`-shaped module AST.  Desugaring,
kind checking, inference, and elaboration happen only after the declaration
contract has been enforced.  The important invariants are:

- no unresolved names reach inference;
- every binder is unique;
- function equations are grouped before inference;
- dictionaries are explicit before evaluation.

# Declaration Annotations and Signatures

## Surface form

Top-level signatures use `::` and may quantify type variables and carry
contexts:

```haskell
id :: forall a. a -> a
id x = x

same :: Eq2 a => a -> a -> Bool
same x y = eq2 x y
```

Local signatures are accepted in `let` and `where` blocks.  Expression
annotations use `(e :: T)`.  Pattern annotations use `(p :: T)`.

## Checking rules

1. The kind checker expands synonyms and converts the signature to a
   `Scheme`, rejecting unknown type constructors and ill-kinded applications.
2. The inferencer skolemizes the signature's quantified variables.
3. The body is inferred with the signature predicates as givens.
4. The inferred type is unified with the skolemized type.
5. Body constraints must be entailed by the signature predicates or solved by
   instances; otherwise the program is rejected.
6. The declared scheme is stored for later uses, so recursive and subsequent
   uses are polymorphic.

## Annotation limits

Expression/pattern annotations currently support the same type language as
signatures except that a class context inside an expression annotation is
rejected.  `forall` in an expression annotation is accepted but its variables
are treated as unification variables, not a new skolem scope.

## Failure modes

- malformed type syntax;
- unknown type constructor;
- kind mismatch;
- annotation/inferred type mismatch;
- annotation context not entailed by the declared type;
- unsupported context in an expression annotation;
- signature for an undefined top-level value.

All failures use the normalized phase/source/span/message diagnostic contract.

## Unchanged invariants

- final expression is not required in file mode;
- top-level declarations are mutually recursive;
- duplicate names are rejected where Haskell requires uniqueness;
- no imports or module namespaces.

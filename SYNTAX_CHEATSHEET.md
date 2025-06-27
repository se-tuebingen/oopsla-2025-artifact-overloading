# Variational Core Cheat Sheet

This cheat sheet describes the "Variational Core" language that is accepted by the artifact.

## Literals

```ml
// line comment

42          // integers
3.14        // doubles
true, false // booleans
"hello"     // strings
```

## Functions

```ml
λ x => x                   // lambda (single param)
λ (x, y) => x              // lambda (multiple params)
\x => x                    // backslash syntax
fun x => x                 // function keyword
f(x)                       // function call
f(x, y)                    // multiple arguments
f(x)(y)                    // curried application
```

## Control Flow

```ml
if condition then expr1 else expr2
```

## Let Bindings

Let bindings in Variational Core are _monomorphic_, they do not generalize in any way whatsoever!

```ml
let x = 42 in x               // let expression
let x = 42; x                 // semicolon syntax
let rec f = λ x => f(x) in f  // recursive binding
```

## Choices

Each choice has a dimension variable (here `a`)
and alternatives (here `expr1`, `expr2`, `expr3`):

```ml
a<expr1, expr2, expr3>    // choice over expressions
a‹expr1, expr2, expr3›    // alternative syntax (web-safe)
```

We use choices for overloading, for example `x + y` is _manually_ desugared to some:
```ml
let add = a<addInt, addString>;
add(x, y)
```
where `a` is a fresh dimension and `addInt, addString` are the available overloads for addition.

Note that the alternatives can be arbitrary expressions.

## Externs / Assumptions

The (core) language of the artifact doesn't have any builtins of its own.

```ml
assume intToString: Int => String; expr    // function type
assume id: ∀A. A => A; expr                // polymorphic type
assume const: ∀(A, B). A => B => A; expr   // multiple type vars
```

## Types

```ml
Int, String, Bool, Double, ...    // base types (anything free -- not bound in a forall)
A => B                            // function type (single arg)
(A, B) => C                       // function type (multiple args)
() => A                           // function type (no args)
∀(A, B) . (A => B) => A => B      // polymorphic type
forall A . A => A                 // alternative syntax
```

## Further References

The `src/syntax.rs` source file specifies a parser-combinator-based parser in the `parser` submodule (grep for `mod parser`).
There are also many tests in the `tests` submodule in the same file and a lot of runnable examples in `examples/`.
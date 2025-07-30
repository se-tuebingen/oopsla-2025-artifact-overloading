# ARCHITECTURE

This document describes the high-level architecture of the artifact,
inspired by matklad's https://matklad.github.io/2021/02/06/ARCHITECTURE.md.html.

This is a research prototype implementing a type-checker for "Variational Core" --
a calculus with choice operators that enable overloading resolution via constraint solving and BDD-based world counting.

## Bird's Eye View

The artifact accepts input source code and produces:
1. An inferred type for the expression
2. (Variational) constraints generated during type inference
3. Upper and lower (variational) bounds for each type variable
4. A count of valid "worlds" (overload resolution solutions)
5. Optionally, all valid solutions enumerated

The input consists of expressions in a ML-style lambda calculus
extended with choice operators of the form `a<expr1, expr2, ...>`
where `a` is a dimension (choice variable) and the alternatives are different overloaded expressions.

See `examples/*.over` for many examples of the syntax and [`SYNTAX_CHEATSHEET.md`](./SYNTAX_CHEATSHEET.md) for a cheat sheet.

## Entry Points

`src/main.rs` contains the CLI entry point that parses files and calls the compiler.

`src/lib.rs` contains the WASM entry point `wasm_parse_and_test` plus the `process_expr` function used by both entry points which orchestrates the entire compilation pipeline:
parsing → constraint generation → constraint solving → overload resolution (= BDD construction → world counting).

## Code Map

### `src/syntax.rs`

**API Boundary**: This module defines the core syntax trees and parsing infrastructure.

Contains the fundamental data structures:
- `Expr`: The expression AST with lambda abstractions, applications, let bindings, and choice operators
- `Type`: Type representation including base types, function types, type variables, and type schemes
- `Choices`: Type alias for choice variable constraints
- Parser combinators for expressions and types

Also contains timing macros and WASM compatibility shims.

**Architecture Invariant**: The syntax module is independent of inference and solving -- it only knows about parsing and pretty-printing.

**Architecture Invariant**: Choice operators `a<expr1, expr2, ...>` are first-class syntax constructs, making overloading explicit in the AST.

### `src/rename.rs`

Gathering choices, renaming anonymous choices and `assume` synonyms.

### `src/infer.rs`

Constraint gathering and constraint solving.

Key types:
- `Constraint`: Represents subtyping constraints with associated world requirements
- `Bounded`: Lower and upper bounds for type variables during constraint solving
- `InferenceContext`: Manages fresh variable generation and environments
  - `infer` is the main entry point for constraint gathering
- `ConstraintSolver`: Implements constraint solving via bound propagation
  - `solve_constraint` shows how a single constraint is solved

**Architecture Invariant**: The constraint solver is a separate phase from constraint gathering that operates on the constraint graph (represented as only its edges).
The output is not only the resulting type, which is not substituted into, but also the errors encountered along the way.

**Architecture Invariant**: Constraints are variational, they can be annotated with world requirements:
a constraint `T1 <:^a1 T2` means the subtyping relationship holds only in the worlds where choice `a = 1` is made.

### `src/bdd.rs`

Overload resolution consisting of Binary Decision Diagram (BDD) construction and world counting.

Core functionality:
- `BddMapping`: Bidirectional mapping between worlds and BDD variables
- `count_worlds`: Constructs a BDD from variational error constraints and counts satisfying assignments
- `solutions`: Enumerates all valid choice combinations

### `src/lib.rs`

Contains:
- `ProcessingResult`: Sum type representing successful compilation or inference errors
- `compile_expr`: Main compilation function with pretty-printed output
- `compile_expr_json`: JSON API variant for web interface
- `process_expr`: Core pipeline implementation
- `mod json`: Submodule for JSON serialization of `ProcessingResult` to be used on the website

### `src/main.rs`

Command-line interface and snapshot test harness.

**Architecture Invariant**: The CLI is the only part that knows about files and I/O. The core library operates purely on in-memory data structures.

## Cross-Cutting Concerns

Here's what didn't fit into the above.

### Variational Constraint Solving

Traditional type inference generates constraints like `T1 <: T2`.
This system generates _variational_ constraints like `T1 <:^a1b2 T2`, meaning the constraint only applies in worlds where choice `a` takes alternative 1 and choice `b` takes alternative 2.

The constraint solver remembers all the error constraints like `Int <:^a1 String`,
using them for overload resolution as such an error constraints marks that worlds where `a = 1` are not valid (and therefore can be rejected by the resolution).

### Overload resolution using BDDs ("World Counting")

Enumerating all possible choice combinations is exponential.
The system encodes world constraints as a BDD instead, which we believe allows polynomial-time counting for many cases and efficient enumeration when needed.

The BDD construction treats each choice dimension as mutually exclusive BDD variables (exactly one alternative must be chosen).

### Development Workflow

Use `nix develop` to get a Nix development shell to develop in, then open up VSCode with the rust-analyzer extension in that shell (`code .`).
There are many tests runnable with `cargo test`, example files trigger automatic snapshot testing via `insta`.
Use `cargo insta review` after modifying examples to either accept or reject the change.

The `--short` flag of the CLI skips solution enumeration for faster feedback.

We recommend using the website for quick iteration.

#### Adding a new example / modifying an existing one

In a Nix development shell:
- If the example is new, put a file containing it called `$name.over` into `examples/`
- Run `cargo test` to regenerate the snapshots
- Run `cargo insta review` and either accept or reject the snapshot change based on its diff
- If the example is new, add the example to `examples.json` (just a `name` and a `path` is enough)
- Run `python3 make-examples.py examples.json` to regenerate `examples.json`
- Refresh the website to see the new example in the dropdown

### Implementation Choices

The code heavily prioritizes clarity and presentation over optimization.
Even nested data structures are cloned liberally to avoid lifetime complexity that would obscure the algorithms.

All output is deterministic (sorted) for reliable snapshot testing and benchmarking.

# derive.c

A symbolic differentiation engine written in C built in a few hours. It builds expression trees and computes their derivatives analytically using standard calculus rules.

## Supported operations

- Arithmetic: addition, subtraction, multiplication, division, negation
- Exponentiation (generalized power rule: `d/dx(f^g)`)
- Trigonometric: `sin`, `cos`
- Logarithm: `log`, you cannot specify the base, it is always the natural logarithm.

Expressions are automatically simplified during construction (e.g. multiplying by 0 or 1, adding 0).

## Building and running

```sh
gcc -o derive derive.c
./derive
```

## Implementation notes

### Expression tree

Expressions are represented as a tagged union (`expr`) forming a tree. Each node carries a type (leaf, binary, or unary), a reference count for memory management, and min/max depth metadata. Three global constants (`zero`, `one`, `two`) are cached and never freed.

### Simplification

The `simplify()` function recursively reduces an expression tree using algebraic identities:

- **Multiplication**: `x * 0 -> 0`, `x * 1 -> x`, `x * x -> x^2`, constant folding
- **Addition**: `x + 0 -> x`, `x + x -> 2x`, `x + (-y) -> x - y`, constant folding
- **Subtraction**: `x - 0 -> x`, `0 - x -> -x`, `x - (-y) -> x + y`, constant folding
- **Division**: `0 / x -> 0`, `x / 1 -> x`, constant folding
- **Exponentiation**: `x^0 -> 1`, `x^1 -> x`, `1^x -> 1`, `0^x -> 0`, constant folding
- **Negation**: `-0 -> 0`, `-(-x) -> x`, constant folding
- **Logarithm**: `ln(1) -> 0`, constant folding

Terms are also reordered to place constants before symbols when possible.

### Symbol assignment

`assign(f, sym, val)` performs in-place substitution, replacing every occurrence of `sym` in the expression tree `f` with `val`. This is the building block for evaluation.

### Function calculation

`calc(f, sym, val)` evaluates an expression at a point:

1. Deep-copies the expression tree (preserving the original)
2. Assigns the integer value to the symbol via `assign()`
3. Simplifies the result

For example, given `f = a^2 + b`, calling `calc(f, a, 3)` produces `9 + b`.
This is a fun exercise project. Currently, functions must be defined directly in the code. A future improvement could be adding a simple parser with a REPL for interactive use.

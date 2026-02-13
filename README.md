# derive.c

A symbolic differentiation engine written in C built in a few hours. It builds expression trees and computes their derivatives analytically using standard calculus rules.

## Supported operations

- Arithmetic: addition, subtraction, multiplication, division, negation
- Exponentiation (generalized power rule: `d/dx(f^g)`)
- Trigonometric: `sin`, `cos`
- Logarithm: `log`

Expressions are automatically simplified during construction (e.g. multiplying by 0 or 1, adding 0).

## Building and running

```sh
gcc -o derive derive.c
./derive
```

## Example

The included `main` computes the derivative of `x^(x + 3) * cos(x)`, producing:

```
x^(x + 3) * cos(x)
x^(x + 3) * cos(x) * (1 * log(x) + (x + 3) * 1 / x) + x^(x + 3) * -sin(x)
```

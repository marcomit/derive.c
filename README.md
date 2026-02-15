# derive.c

A symbolic differentiation engine written in C built in a few hours. It parses expressions from text, builds expression trees, computes their derivatives analytically, and simplifies the results. Includes an interactive REPL.

## Supported operations

- Arithmetic: addition, subtraction, multiplication, division, negation
- Exponentiation (generalized power rule: `d/dx(f^g)`)
- Trigonometric: `sin`, `cos`
- Logarithm: `log`
- User-defined functions: `f = x^2 + 3*x`
- Symbolic differentiation: `f'(x)`
- Evaluation at a point: `f(x=5)`

Expressions are automatically simplified (e.g. multiplying by 0 or 1, adding 0, constant folding, double negation elimination, canonical ordering of terms).

## Building and running

```sh
gcc -o derive derive.c -lm
./derive
```

## REPL

The program starts an interactive session where you can type expressions, define functions, differentiate, and evaluate:

```
>> f = x^3 + 2*x
x^3 + 2 * x

>> f'(x)
3 * x^2 + 2

>> f'(x)'(x)
6 * x

>> f(x=4)
72

>> sin(x)^2 + cos(x)^2
sin(x)^2 + cos(x)^2

>> (sin(x)^2 + cos(x)^2)(x=0)
1

>> exit
Goodbye by marcomit!
```

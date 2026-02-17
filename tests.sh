#!/bin/bash
set -e

PASS=0
FAIL=0

assert_eq() {
    local input="$1"
    local expected="$2"
    local actual
    actual=$(echo "$input" | ./derive 2>&1 | tr -d '\n')

    if [ "$actual" = "$expected" ]; then
        PASS=$((PASS + 1))
    else
        FAIL=$((FAIL + 1))
        echo "FAIL: '$input'"
        echo "  expected: '$expected'"
        echo "  actual:   '$actual'"
    fi
}

# Basic arithmetic
assert_eq "1 + 2" "3"
assert_eq "3 * 4" "12"
assert_eq "10 / 2" "5"
assert_eq "5 - 3" "2"
assert_eq "2^3" "8"

# Symbols
assert_eq "x + 0" "x"
assert_eq "x * 1" "x"
assert_eq "x * 0" "0"
assert_eq "x^1" "x"
assert_eq "x^0" "1"

# Derivatives
assert_eq "(x^2)'(x)" "x^2 * 2 / x"
assert_eq "(x^3)'(x)" "x^3 * 3 / x"
assert_eq "(sin(x))'(x)" "cos(x)"
assert_eq "(cos(x))'(x)" "-sin(x)"
assert_eq "(log(x))'(x)" "1 / x"
assert_eq "(3*x + 5)'(x)" "3"

# Evaluation
assert_eq "x^2(x=3)" "9"
assert_eq "(x^2 + 1)(x=4)" "17"

# Integration
assert_eq "integrate(x^2, 0, 3)" "9"
assert_eq "integrate(1, 0, 5)" "5"

# Taylor series
assert_eq "taylor(sin(x), 0, 5)" "0.00833333 * x^5 + -0.166667 * x^3 + x"
assert_eq "taylor(cos(x), 0, 4)" "1 + -0.5 * x^2 + 0.0416667 * x^4"

echo ""
echo "Results: $PASS passed, $FAIL failed"

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi

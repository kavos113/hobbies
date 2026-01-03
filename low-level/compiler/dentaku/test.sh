#!/bin/bash
assert() {
    expected="$1"
    input="$2"

    ./acc "$input" > tmp.s
    gcc -o tmp tmp.s
    ./tmp
    actual="$?"

    if [ "$actual" = "$expected" ]; then
        echo "$input => $actual"
    else 
        echo "$input => $expected expected, but got $actual"
        exit 1
    fi
}

assert 34 "10-4+28"
assert 18 "  29 - 10+ 4  - 5"
assert 13 "3 + 2*5"
assert 39 "(3 + 10) * (4 - 1)"
assert 4 "(6 + 2 * 3) / 3"
assert 5 "-10 + 15"
assert 6 "-3 * (-2)"
assert 1 "3 + 5 == 8"
assert 0 "4 * 3 + 2 * (1+5) > 100"

echo OK.
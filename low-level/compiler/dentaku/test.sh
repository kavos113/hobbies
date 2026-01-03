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

echo OK.
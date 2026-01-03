#!/bin/bash

# file paths
assert() {
    expected=$(<"$1")
    expected=$(echo "$expected" | tr -d '[:space:]')
    input="$2"

    ./acc "$input" tmp.s
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

tc_name="01_single_local_var"

tc_dir="./testcase/$tc_name"

for src_file in "$tc_dir"/*.src; do
    [ -e "$src_file" ] || continue

    test_name="${src_file%.src}"
    ans_file="${test_name}.ans"

    if [ -f "$ans_file" ]; then
        assert $ans_file $src_file
    fi
done

echo OK.
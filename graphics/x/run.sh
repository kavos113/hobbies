#!/bin/bash

build() {
  cmake -B build -S . -DCMAKE_BUILD_TYPE=Debug -G Ninja
  cmake --build build
}

run() {
  "./build/$1/x-$1"
}

case "$1" in
    build)
        build
        ;;
    run)
        run "$2"
        ;;
    *)
        echo "Usage:"
        echo "  $0 build"
        echo "  $0 run <project>"
        exit 1
        ;;
esac
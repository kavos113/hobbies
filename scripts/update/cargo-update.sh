#!/bin/bash

echo "searching for cargo updates..."

pkgs=$(find . -name "target" -prune -o -name "Cargo.toml" -print)
for pkg in $pkgs; do
  dir=$(dirname "$pkg")
  echo "updating packages in $dir"

  (
    cd "$dir" || exit
    cargo update
  )
done
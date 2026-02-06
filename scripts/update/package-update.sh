#!/bin/bash

echo "searching for package updates..."

pkgs=$(find . -name "node_modules" -prune -o -name "package.json" -print)
for pkg in $pkgs; do
  dir=$(dirname "$pkg")
  echo "updating packages in $dir"

  (
    cd "$dir" || exit
    
    if [ -f "package-lock.json" ]; then
        npm install
    elif [ -f "pnpm-lock.yaml" ]; then
        pnpm up --lockfile-only
    fi
  )
done
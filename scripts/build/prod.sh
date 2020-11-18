#!/usr/bin/env bash

if [[ "$1" == *"frontend"* ]]; then
  COMPILER='ghcjs'
else
  COMPILER='ghc'
fi
echo "Using compiler: $COMPILER"
nix-build --show-trace -o build/"$1" -A "$COMPILER.$1"

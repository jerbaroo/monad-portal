#!/usr/bin/env bash

if [[ "$*" == *'--ghc'* ]]; then
  COMPILER='ghc';
elif [[ "$*" == *'--ghcjs'* ]]; then
  COMPILER='ghcjs';
elif [[ "$1" == *'frontend'* ]]; then
  COMPILER='ghcjs';
else
  COMPILER='ghc';
fi
echo "Using compiler: $COMPILER"
nix-build --show-trace -o build/"$1" -A "$COMPILER.$1"

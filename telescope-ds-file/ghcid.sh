#!/bin/sh

nix-shell -p ghcid haskell.compiler.ghc884 --run 'ghcid -c "cabal new-repl"'

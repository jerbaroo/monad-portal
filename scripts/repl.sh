#!/bin/sh

. scripts/util.sh
nix-shell -A shells.ghc --run "cabal new-repl $(flags) demo-backend"

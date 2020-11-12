#!/usr/bin/env bash

. scripts/util.sh
nix-shell -A shells.ghc --run \
  "ghcid -c \"cabal new-repl $(flags) --repl-options=-fdiagnostics-color=always\" $1"

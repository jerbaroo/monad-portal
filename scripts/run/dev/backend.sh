#!/bin/sh

. scripts/util.sh
nix-shell -A shells.ghc --run \
  "ghcid -W -c \"cabal new-repl $(flags) --repl-options=-fdiagnostics-color=always\" demo-backend -T Main.main"

#!/bin/sh

nix-shell -A shells.ghc --run \
  "ghcid -c 'cabal new-repl --repl-options -fdiagnostics-color=always' $@"

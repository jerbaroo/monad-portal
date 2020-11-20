#!/usr/bin/env bash

. scripts/util.sh
nix-shell -A shells.ghc --run "cabal new-test $(flags) \
  telescope telescope-ds-file"

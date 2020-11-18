#!/usr/bin/env bash

. scripts/util.sh
nix-shell -A shells.ghc --run "cabal new-build $(flags) $1"

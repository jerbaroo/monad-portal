#!/bin/sh

. scripts/util.sh
nix-shell -A shells.ghc --run "cabal new-build $(flags) demo-backend"

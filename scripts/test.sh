#!/bin/sh

. scripts/util.sh
nix-shell -A shells.ghc --run "cabal new-test $(flags) telescope-ds-file"

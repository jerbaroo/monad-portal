#!/usr/bin/env bash

. scripts/util.sh
if [[ -z "$1" ]]; then PKGS="$(packages 'GHC')" ; else PKGS="$1"; fi
nix-shell -A shells.ghc --run "cabal new-build $(flags) $PKGS"

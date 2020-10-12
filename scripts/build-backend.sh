#!/bin/sh

nix-shell -A shells.ghc --run "cabal new-build demo-backend"

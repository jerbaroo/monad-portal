#!/bin/sh

nix-shell -A shells.ghc --run 'cabal new-run demo-backend'

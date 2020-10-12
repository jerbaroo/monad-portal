#!/bin/sh

nix-shell -A shells.ghc --run 'cabal new-test --test-show-details=streaming telescope'

#!/bin/sh

nix-shell -A shells.ghc --run \
  'cabal new-repl --repl-options -fno-warn-missing-fields demo-backend'

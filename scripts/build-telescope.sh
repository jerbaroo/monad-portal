#!/bin/sh

nix-shell -A shells.ghc --run 'cabal new-build telescope telescope-ds-file telescope-server telescope-server-api'

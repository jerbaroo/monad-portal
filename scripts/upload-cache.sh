#!/usr/bin/env bash

echo "Building with GHC..."
nix-build -A shells.ghc | cachix push jerbaroo\
  && echo "Building with GHCJS..."\
  && nix-build -A shells.ghcjs | cachix push jerbaroo

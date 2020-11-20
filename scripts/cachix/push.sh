#!/usr/bin/env bash

. scripts/util.sh
nix-build --show-trace $NIX_GHC_ATTRS $NIX_GHCJS_ATTRS | cachix push $CACHIX_USER

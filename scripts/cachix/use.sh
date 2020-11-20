#!/usr/bin/env bash

. scripts/util.sh
cachix use $CACHIX_USER && nix-build --show-trace $NIX_GHC_ATTRS $NIX_GHCJS_ATTRS

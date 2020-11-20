#!/usr/bin/env bash

. scripts/util.sh
nix-build --show-trace -A ghc.telescope -A ghc.telescope-ds-file

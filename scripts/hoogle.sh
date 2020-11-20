#!/usr/bin/env bash

if [[ -z "$1" ]]; then
  PORT=5000
else
  PORT="$1"
fi

nix-shell -A shells.ghc --arg withHoogle true --run "hoogle server --local --port=$PORT"

#!/bin/sh

if [ -z "$1" ]; then
  PORT=3001
else
  PORT="$1"
fi

nix-shell -A shells.ghc --arg withHoogle true --run "hoogle server --local --port=$PORT"

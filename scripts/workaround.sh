#!/bin/sh

workaround () {
  if [[ "$OSTYPE" == 'darwin'* ]]; then
    echo 'Temporary workaround for https://github.com/jerbaroo/new-telescope/issues/15'
    if [[ "$(ghc --version)" == *'8.6.5' ]]; then
      echo "Running: $1"
      eval "$1"
    else
      echo "$(ghc --version)"
      echo 'Make sure you are on GHC 8.6.5 (use the tool ghcup)'
    fi
  else
    echo "Running in nix-shell: $1"
    nix-shell -A shells.ghc --run "$1"
  fi
}

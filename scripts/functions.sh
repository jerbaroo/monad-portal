#!/bin/sh

flags () {
  echo "\
--ghc-options=-Wall \
--ghc-options=-fno-warn-missing-fields \
--ghc-options=-threaded \
--test-show-details=streaming"
}

workaround () {
  if [[ "$OSTYPE" == 'darwin'* ]]; then
    echo 'Temporary workaround for https://github.com/jerbaroo/new-telescope/issues/15'
    if [[ "$(ghc --version)" == *'8.6.5' ]]; then
      echo "Running: $@"
      eval "$@"
    else
      echo "$(ghc --version)"
      echo 'Make sure you are on GHC 8.6.5 (use the tool ghcup)'
    fi
  else
    echo "Running in nix-shell: $@"
    nix-shell -A shells.ghc --run "$@"
  fi
}

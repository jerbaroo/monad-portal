#!/bin/sh

. scripts/util.sh
ghcid -c "cabal new-repl $(flags) --repl-options=-fdiagnostics-color=always" "$@"

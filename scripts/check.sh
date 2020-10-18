#!/bin/sh

. scripts/functions.sh
workaround "ghcid -c 'cabal new-repl $(flags) --repl-options=-fdiagnostics-color=always' $@"

#!/bin/sh

. scripts/workaround.sh
workaround "ghcid -c 'cabal new-repl --repl-options -fdiagnostics-color=always' $@"

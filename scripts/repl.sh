#!/bin/sh

. scripts/functions.sh
workaround "cabal new-repl $(flags) demo-backend"

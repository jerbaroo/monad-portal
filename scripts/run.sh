#!/bin/sh

. scripts/functions.sh
workaround "cabal new-run $(flags) $@"

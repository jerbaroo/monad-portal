#!/bin/sh

. scripts/functions.sh
workaround "cabal new-build $(flags) $@"

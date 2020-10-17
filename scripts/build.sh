#!/bin/sh

. scripts/workaround.sh
workaround "cabal new-build $@"

#!/bin/sh

. scripts/workaround.sh
workaround "cabal new-run $@"

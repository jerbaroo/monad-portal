#!/bin/sh

. scripts/functions.sh
workaround "cabal new-test $(flags) telescope-ds-file"

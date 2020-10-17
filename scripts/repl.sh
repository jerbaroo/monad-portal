#!/bin/sh

. scripts/workaround.sh
workaround 'cabal new-repl --repl-options -fno-warn-missing-fields demo-backend'

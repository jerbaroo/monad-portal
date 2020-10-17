#!/bin/sh

. scripts/workaround.sh
workaround 'cabal new-test --test-show-details=streaming telescope-ds-file'

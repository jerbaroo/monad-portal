#!/bin/sh

cabal new-build all
cabal new-test all --test-show-details=streaming    

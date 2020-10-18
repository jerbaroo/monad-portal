#!/bin/sh

nix-build -o build/demo-backend -A ghc.demo-backend

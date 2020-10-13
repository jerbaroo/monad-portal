#!/bin/sh

nix-build -o demo-frontend-ghc -A ghc.demo-frontend && \
  ./demo-frontend-ghc/bin/demo-frontend

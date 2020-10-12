#!/bin/sh

nix-build -o demo-frontend-ghcjs -A ghcjs.demo-frontend

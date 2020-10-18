#!/bin/sh

nix-build -o build/demo-frontend -A ghcjs.demo-frontend

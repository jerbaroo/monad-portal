#!/bin/sh

nix-build -o static-files -A ghcjs.demo-frontend

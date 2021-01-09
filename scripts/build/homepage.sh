#!/usr/bin/env bash
set -euo pipefail

. scripts/util.sh

cp README.md homepage/homepage
mkdir -p homepage/homepage/docs
cp docs/*.md homepage/homepage/docs
rm headers.txt markdown.html || true
nix-shell -A shells.ghc --run "cabal new-clean && cabal new-run $(flags) homepage"

mv {headers.txt,markdown.html} homepage/homepage-frontend
rm index.html || true
nix-shell -A shells.ghc --run "cabal new-clean && cabal new-run $(flags) homepage-frontend"

rm -rf homepage-build
mkdir -p homepage-build/diagram
cp diagram/diagram.png homepage-build/diagram
cp homepage/homepage-frontend/*.{png,ttf} homepage-build
mv index.html homepage-build

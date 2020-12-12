#!/usr/bin/env bash

. scripts/util.sh

cp README.md homepage/homepage
mkdir -p homepage/homepage/docs
cp docs/*.md homepage/homepage/docs
rm headers.txt highlight.css markdown.html
nix-shell -A shells.ghc --run "cabal new-clean && cabal new-run $(flags) homepage"

mv {headers.txt,highlight.css,markdown.html} homepage/homepage-frontend
rm index.html
nix-shell -A shells.ghc --run "cabal new-clean && cabal new-run $(flags) homepage-frontend"

rm -rf homepage-build
mkdir -p homepage-build/diagram
cp diagram/{diagram,GitHub_Logo}.png homepage-build/diagram
mv index.html homepage-build

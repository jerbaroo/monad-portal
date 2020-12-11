#!/usr/bin/env bash

. scripts/util.sh

cp README.md homepage
mkdir -p homepage/docs
cp docs/*.md homepage/docs
rm headers.txt highlight.css markdown.html
nix-shell -A shells.ghc --run "cabal new-clean && cabal new-run $(flags) homepage"

cp {headers.txt,highlight.css,markdown.html} homepage-frontend
./scripts/build/prod.sh homepage-frontend

rm -rf homepage-build
mkdir -p homepage-build/diagram
cp diagram/diagram.png homepage-build/diagram
cp diagram/GitHub_Logo.png homepage-build/diagram
cp build/homepage-frontend/bin/homepage-frontend.jsexe/* homepage-build

#!/usr/bin/env bash

cp README.md homepage
mkdir -p homepage/docs
cp docs/*.md homepage/docs
./scripts/build/prod.sh homepage

cp homepage/headers.txt   homepage-frontend
cp homepage/highlight.css homepage-frontend
cp homepage/markdown.html homepage-frontend
./scripts/build/prod.sh homepage-frontend

rm -rf homepage-build
mkdir -p homepage-build/diagram
cp diagram/diagram.png homepage-build/diagram
cp -r diagram/GitHub_Logo.png homepage-build/diagram
cp build/homepage-frontend/bin/homepage-frontend.jsexe/* homepage-build

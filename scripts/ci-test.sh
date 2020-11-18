#!/usr/bin/env bash

PACKAGES=(
  "testing-backend"
  "testing-common"
  "testing-frontend"
  "todolist-backend"
  "todolist-common"
  "todolist-frontend"
  "telescope"
  "telescope-ds-file"
  "telescope-ds-reflex-dom"
  "telescope-ds-test"
  "telescope-server"
  "telescope-server-api"
  "telescope-server-api-types"
)

for PACKAGE in "${PACKAGES[@]}"
do
 LIST="$LIST -A ghc.$PACKAGE"
done

nix-build --show-trace $LIST

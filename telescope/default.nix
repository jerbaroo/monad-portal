{compiler ? "ghc884"}:

let
  pkgs = import <nixpkgs> { };

in
  pkgs.haskell.packages.${compiler}.callPackage ./package.nix {
    zlib = pkgs.zlib;
  }

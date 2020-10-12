{ system ? builtins.currentSystem }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;

  packages = {
    demo-backend         = ./demo-backend;
    demo-common          = ./demo-common;
    demo-frontend        = ./demo-frontend;
    telescope            = ./telescope;
    telescope-ds-file    = ./telescope-ds-file;
    telescope-server     = ./telescope-server;
    telescope-server-api = ./telescope-server-api;
  };

  shells = {
    ghc   = ["demo-common" "demo-frontend"];
    ghcjs = ["demo-common" "demo-frontend"];
  };
})

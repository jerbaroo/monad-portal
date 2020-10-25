{ system ? builtins.currentSystem, withHoogle ? false }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  inherit withHoogle;
  packages = {
    demo-backend            = ./demo-backend;
    demo-common             = ./demo-common;
    demo-frontend           = ./demo-frontend;
    telescope               = ./telescope;
    telescope-ds-file       = ./telescope-ds-file;
    telescope-ds-reflex-dom = ./telescope-ds-reflex-dom;
    telescope-server        = ./telescope-server;
    telescope-server-api    = ./telescope-server-api;
  };
  shells = {
    ghc   = ["demo-backend" "demo-common" "demo-frontend"];
    ghcjs = [               "demo-common" "demo-frontend"];
  };
  overrides = self: super: {
    servant-websockets = self.callHackage "servant-websockets" "2.0.0" {};
  };
})

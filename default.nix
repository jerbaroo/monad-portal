{ system ? builtins.currentSystem, withHoogle ? false }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  inherit withHoogle;
  packages = {
    demo-backend                = ./apps/demo-backend;
    demo-common                 = ./apps/demo-common;
    demo-frontend               = ./apps/demo-frontend;
    todolist-backend            = ./apps/todolist-backend;
    todolist-common             = ./apps/todolist-common;
    todolist-frontend           = ./apps/todolist-frontend;
    telescope                   = ./telescope;
    telescope-ds-file           = ./telescope-ds-file;
    telescope-ds-reflex-dom     = ./telescope-ds-reflex-dom;
    telescope-server            = ./telescope-server;
    telescope-server-api        = ./telescope-server-api;
    telescope-server-api-types  = ./telescope-server-api-types;
  };
  shells = {
    ghc = [
      "demo-backend"
      "demo-common"
      "demo-frontend"
      "todolist-backend"
      "todolist-common"
      "todolist-frontend"
      ## Telescope packages ##
      "telescope"
      "telescope-ds-file"
      "telescope-ds-reflex-dom"
      "telescope-server"
      "telescope-server-api"
      "telescope-server-api-types"
    ];
    ghcjs = [
      "demo-common"
      "demo-frontend"
      "todolist-common"
      "todolist-frontend"
      ## Telescope packages ##
      "telescope"
      "telescope-ds-file"
      "telescope-ds-reflex-dom"
      "telescope-server"
      "telescope-server-api"
      "telescope-server-api-types"
    ];
  };
  overrides = self: super: {
    servant-websockets = self.callHackage "servant-websockets" "2.0.0" {};
  };
})

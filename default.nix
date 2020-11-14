{ system ? builtins.currentSystem, withHoogle ? false }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  inherit withHoogle;
  packages = {
    ## Apps ##
    todolist-backend            = ./apps/todolist-backend;
    todolist-common             = ./apps/todolist-common;
    todolist-frontend           = ./apps/todolist-frontend;
    testing-backend             = ./apps/testing-backend;
    testing-common              = ./apps/testing-common;
    testing-frontend            = ./apps/testing-frontend;
    ## Telescope packages ##
    telescope                   = ./telescope;
    telescope-ds-file           = ./telescope-ds-file;
    telescope-ds-reflex-dom     = ./telescope-ds-reflex-dom;
    telescope-ds-test           = ./telescope-ds-test;
    telescope-server            = ./telescope-server;
    telescope-server-api        = ./telescope-server-api;
    telescope-server-api-types  = ./telescope-server-api-types;
  };
  shells = {
    ghc = [
    ## Apps ##
      "todolist-backend"
      "todolist-common"
      "todolist-frontend"
      "testing-backend"
      "testing-common"
      "testing-frontend"
      ## Telescope packages ##
      "telescope"
      "telescope-ds-file"
      "telescope-ds-reflex-dom"
      "telescope-ds-test"
      "telescope-server"
      "telescope-server-api"
      "telescope-server-api-types"
    ];
    ghcjs = [
      ## Apps ##
      "todolist-common"
      "todolist-frontend"
      "testing-common"
      "testing-frontend"
      ## Telescope packages ##
      "telescope"
      "telescope-ds-file"
      "telescope-ds-test"
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

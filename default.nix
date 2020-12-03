{ system ? builtins.currentSystem, withHoogle ? false }:
(import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
  useWarp = true;
  inherit withHoogle;
  packages = {
    ## Telescope packages ##
    telescope                   = ./telescope;
    telescope-ds-file           = ./telescope-ds-file;
    telescope-ds-reflex-dom     = ./telescope-ds-reflex-dom;
    telescope-ds-test           = ./telescope-ds-test;
    telescope-server            = ./telescope-server;
    telescope-server-api        = ./telescope-server-api;
    telescope-server-api-types  = ./telescope-server-api-types;
    ## Apps ##
    chatroom-backend            = ./apps/chatroom-backend;
    chatroom-common             = ./apps/chatroom-common;
    chatroom-frontend           = ./apps/chatroom-frontend;
    testing-backend             = ./apps/testing-backend;
    testing-common              = ./apps/testing-common;
    testing-frontend            = ./apps/testing-frontend;
    todolist-backend            = ./apps/todolist-backend;
    todolist-common             = ./apps/todolist-common;
    todolist-frontend           = ./apps/todolist-frontend;
  };
  shells = {
    ghc = [ ## BEGIN-GHC ##
      ## Telescope packages ##
      "telescope"
      "telescope-ds-file"
      "telescope-ds-reflex-dom"
      "telescope-ds-test"
      "telescope-server"
      "telescope-server-api"
      "telescope-server-api-types"
      ## Apps ##
      "chatroom-backend"
      "chatroom-common"
      "chatroom-frontend"
      "testing-backend"
      "testing-common"
      "testing-frontend"
      "todolist-backend"
      "todolist-common"
      "todolist-frontend"
    ]; ## END-GHC ##
    ghcjs = [ ## BEGIN-GHCJS ##
      ## Telescope packages ##
      "telescope"
      "telescope-ds-test"
      "telescope-ds-reflex-dom"
      "telescope-server-api-types"
      ## Apps ##
      "chatroom-common"
      "chatroom-frontend"
      "testing-common"
      "testing-frontend"
      "todolist-common"
      "todolist-frontend"
    ]; ## END-GHCJS ##
  };
  overrides = self: super: {
    flat               = self.callHackage "flat"               "0.4.4" {};
    servant-websockets = self.callHackage "servant-websockets" "2.0.0" {};
  };
})

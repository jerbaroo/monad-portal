{ reflex-platform ? ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "reflex-frp";
    repo = "reflex-platform";
    rev = "6fbaf9b5dafd3e1afc538049654fb8ab8ce64965";
    sha256 = "cTTdkjpAQfg6bhTGBpAYMMQNtokTwpOq7WPV+pTQRKg=";
    })
}:
(import reflex-platform {}).project ({ pkgs, ... }:{
  useWarp = true;
  packages = {
    monad-portal = ./monad-portal;
    # telescope-ds-file           = ./telescope-ds-file;
    # telescope-ds-reflex-dom     = ./telescope-ds-reflex-dom;
    # telescope-ds-test           = ./telescope-ds-test;
    # telescope-server            = ./telescope-server;
    # telescope-server-api        = ./telescope-server-api;
    # telescope-server-api-types  = ./telescope-server-api-types;
    # chatroom-backend            = ./apps/chatroom-backend;
    # chatroom-common             = ./apps/chatroom-common;
    # chatroom-frontend           = ./apps/chatroom-frontend;
    # testing-backend             = ./apps/testing-backend;
    # testing-common              = ./apps/testing-common;
    # testing-frontend            = ./apps/testing-frontend;
    # todolist-backend            = ./apps/todolist-backend;
    # todolist-common             = ./apps/todolist-common;
    # todolist-frontend           = ./apps/todolist-frontend;
  };
  shells = {
    ghc = [
      "monad-portal"
      # "telescope-ds-file"
      # "telescope-ds-reflex-dom"
      # "telescope-ds-test"
      # "telescope-server"
      # "telescope-server-api"
      # "telescope-server-api-types"
      # "chatroom-backend"
      # "chatroom-common"
      # "chatroom-frontend"
      # "testing-backend"
      # "testing-common"
      # "testing-frontend"
      # "todolist-backend"
      # "todolist-common"
      # "todolist-frontend"
    ];
    ghcjs = [
      "monad-portal"
      # "telescope-ds-test"
      # "telescope-ds-reflex-dom"
      # "telescope-server-api-types"
      # "chatroom-common"
      # "chatroom-frontend"
      # "testing-common"
      # "testing-frontend"
      # "todolist-common"
      # "todolist-frontend"
    ];
  };
  overrides = self: super: {
    flat               = self.callHackage "flat"               "0.4.4"   {};
    servant-websockets = self.callHackage "servant-websockets" "2.0.0"   {};
  };
})

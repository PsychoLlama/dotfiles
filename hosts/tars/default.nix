{ config, pkgs, ... }:

{
  home = {
    username = "overlord";
    homeDirectory = "/home/overlord";
    stateVersion = "22.11";
  };

  presets.terminal-environment.enable = true;

  programs = {
    home-manager.enable = true;

    git = {
      userName = "Jesse Gibson";
      userEmail = "JesseTheGibson@gmail.com";
    };
  };
}

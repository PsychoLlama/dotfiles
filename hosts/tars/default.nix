{ config, ... }:

let
  inherit (config.psychollama.identity) username name email;
in

{
  psychollama.identity = {
    username = "overlord";
    name = "Jesse Gibson";
    email = "JesseTheGibson@gmail.com";
  };

  home = {
    inherit username;
    homeDirectory = "/home/${username}";
    stateVersion = "22.11";
  };

  programs = {
    home-manager.enable = true;

    git.settings.user = {
      inherit name email;
    };
  };
}

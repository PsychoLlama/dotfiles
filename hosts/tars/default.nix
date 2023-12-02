{
  home = {
    username = "overlord";
    homeDirectory = "/home/overlord";
    stateVersion = "22.11";
  };

  # TODO: This was removed. Add an equivalent profile.
  presets.terminal-environment.enable = true;

  programs = {
    home-manager.enable = true;

    git = {
      userName = "Jesse Gibson";
      userEmail = "JesseTheGibson@gmail.com";
    };
  };
}

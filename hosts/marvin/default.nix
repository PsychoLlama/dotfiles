{ config, pkgs, ... }:

{
  dotfiles = {
    packageSet = "nixpkgs-unstable";

    user = {
      name = "overlord";
      packages = with pkgs.unstable; [
        (nerdfonts.override { fonts = [ "FiraCode" ]; })
        fira-code
      ];
    };
  };

  home-manager.users.${config.dotfiles.user.name} = {
    home.stateVersion = "22.05";

    # TODO: This was removed. Create an equivalent profile.
    presets.terminal-environment.enable = true;

    programs = {
      alacritty.settings.font.normal.family = "Fira Code";

      git = {
        userName = "Jesse Gibson";
        userEmail = "JesseTheGibson@gmail.com";
      };
    };
  };

  homebrew = {
    enable = true;
    casks = [ "firefox" ];
    onActivation.cleanup = "zap";
  };

  system = {
    stateVersion = 4;

    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };

    defaults = {
      dock = {
        autohide = true;
        orientation = "left";
        show-recents = false;
        static-only = true;
      };

      NSGlobalDomain = {
        AppleInterfaceStyle = "Dark";
        AppleShowAllExtensions = true;
        InitialKeyRepeat = 18;
        KeyRepeat = 2;
        NSAutomaticQuoteSubstitutionEnabled = false;
      };
    };
  };

  services.nix-daemon.enable = true;
  programs.zsh.enable = true;
  fonts.fontDir.enable = true;
}

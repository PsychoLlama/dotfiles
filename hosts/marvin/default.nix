{ config, inputs, ... }:

{
  environment.variables.GIT_CONFIG_SYSTEM = "/etc/gitconfig";

  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  system.defaults = {
    dock = {
      autohide = true;
      orientation = "left";
      show-recents = false;
      static-only = true;
    };

    NSGlobalDomain = {
      NSAutomaticQuoteSubstitutionEnabled = false;
      InitialKeyRepeat = 20;
      KeyRepeat = 2;
      AppleInterfaceStyle = "Dark";
      AppleShowAllExtensions = true;
    };
  };

  dotfiles = {
    user.account = "marvin";
    package-set = "nixpkgs-unstable";

    toolkit = {
      development.enable = true;
      files.enable = true;
      networking.enable = true;
    };
  };

  home-manager.users.${config.dotfiles.user.account} = {
    imports = [ inputs.self.nixosModules.home-manager ];
    home.stateVersion = "22.05";

    programs.git = {
      userName = "Jesse Gibson";
      userEmail = "JesseTheGibson@gmail.com";
    };

    presets = {
      terminal-environment.enable = true;
      fonts.enable = true;
    };
  };

  homebrew = {
    enable = true;
    casks = [ "firefox" ];
    cleanup = "zap";
  };

  programs.zsh.enable = true;
  services.nix-daemon.enable = true;
  fonts.fontDir.enable = true;

  system.stateVersion = 4;
}

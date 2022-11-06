{ config, ... }:

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

    editor = {
      enable = true;
      linter.enable = true;
    };

    toolkit = {
      networking.enable = true;
      js-development.enable = true;

      files = {
        enable = true;
        replace = true;
      };

      development = {
        enable = true;
        aliases.enable = true;
      };
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

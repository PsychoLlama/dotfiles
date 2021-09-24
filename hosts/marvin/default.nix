{ config, unstable, ... }:

{
  programs.zsh.enable = true;
  system.keyboard = {
    enableKeyMapping = true;
    remapCapsLockToEscape = true;
  };

  dotfiles = {
    user.account = "marvin";
    dev-shell.enable = true;
    toolkit.js-development.enable = true;
  };

  system.defaults.dock = {
    autohide = true;
    orientation = "left";
    show-recents = false;
  };

  dotfiles.editor = {
    enable = true;
    linter.enable = true;
  };

  system.stateVersion = 4;
}

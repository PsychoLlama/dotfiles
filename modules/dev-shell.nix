{ config, pkgs, nixpkgs-unstable, lib, ... }:

# dev-shell
#
# Configures everything you need to launch a fancy development shell.

let
  df = config.dotfiles;
  cfg = df.dev-shell;

in {
  options.dotfiles.dev-shell = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable a fancy development shell";
      default = df.kitchen-sink.enable;
    };

    tmux.config = mkOption {
      type = types.path;
      description = "Set the tmux config file";
      default = ../config/tmux.conf;
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        programs.tmux = {
          enable = true;
          keyMode = mkDefault "vi";
          escapeTime = mkDefault 0;
          historyLimit = mkDefault 10000;
          customPaneNavigationAndResize = mkDefault true;
          extraConfig = builtins.readFile cfg.tmux.config;
        };

        # home-manager only configures zsh - I still need to make it the
        # default.
        # TODO: Find some cleaner way to do this.
        users.users.${df.user.account}.shell = pkgs.zsh;

        fonts.fonts = [ nixpkgs-unstable.fira-code ];
        console.font = "Fira Code";
      })
    ];
}
